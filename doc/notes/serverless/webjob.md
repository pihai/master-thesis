# KUDU

Kudu ist die Deployment-Engine hinter Azure App Service. Es ist aber nicht auf Azure beschränkt, sondern kann theoretisch überall laufen. Im Prinzip geht es einfach darum, den Code der Web Site an den dafür vorgesehenen Platz zu bringen. Jede erstellte Azure Web Site hat zusätzlich eine Kudu Site, die auf alles Zugriff hat, auf dass auch die eigentliche Seite Zugriff hat.

Kudu erlaubt es z.B.:

- Webseiten über ein Versionsverwaltungsprogramm zu deployen
- Benachrichtigungen nach einem Deployment (Web-Hooks)
- Site extensions
- Web Jobs laufen zu lassen
- uvm

Prinzipiell gibt es in Azure drei Möglichkeiten eine Web Site zu deployen:

- FTP
- Webdeploy (z.B. über Visual Studio Publish oder Console)
- Kudu (Versionsverwaltungsbasiert z.B. git oder Mercurial)

Selbst wenn kein Versionsverwaltungs-basierte Deplyoment Art verwendet wird, gibt es immer eine Kudu Site. Weil Kudo eben auch Diagnose Möglichkeiten, Site-Extensions und Web-Job Funktionalität bietet.

Beispielhaft einfaches Szenario mit GitHub Deployment:

1. Code wird in ein GitHub Repo gepusht
2. Auf der Github seite ist ist ein WebHook registriert, den GitHub nach einem Push aufruft (foo.scm.bla/deploy)
3. Kudu wird von GitHub über WebHook aufgerufen (FetchHandler.cs)
4. Kudu holt die Änderungen vom Repository
5. Kudu führt eine Deployment-Script aus, dass zum jeweiligen App-Typ pass (Node.js, Asp.Net, php, ...) (SiteBuilderFactory.cs)
6. Deployment Script verwendet z.B. für Asp.Net MsBuild um die Seite zu erstellen

Um die Kudu Web Site aufzurufen muss nur *scm* (Source Control Management) der URL der eigentlichen Web Site angehängt werden: 'https://mysite.scm.azurewebsites.net/'. Normalerweise wird Single Sign On des Azure Portals verwendet, es ist aber auch möglich sich per Basic Auth anzumelden.

Eine Azure Web Site hat Zugriff auf zwei verschiedene Festplatten. Eine langsame Festplatte in Azure Storage, damit alle Instanzen der Festplatte auf die selben Daten Zugriff haben. Die zweite Festplatte ist die lokale Festplatte der jeweiligen VM Instanz (D:\local).

Meistens sind Deployments nicht *in_place*, d.h. sie werden in einem Temp-Ordner gebuildet und dann auf den Web Server umgeleitet.

## Continuous Jobs

Continuous Jobs laufen immer, solange sie nicht deaktiviert sind. Damit sie immer laufen können, muss *Always On* aktiviert sein. Daher funktioniert das nur mit mindestens Basic-Tier. Je nachdem ob der Job ein Singelton Job ist und ob bereits ein Job auf einer anderen Instanz das Lock bekommen hat, wird der Job gestartet. Andernfalls wird periodisch versucht das Singelton Lock zu bekommen.

Die Klasse `BaseJobRunner` startet in der Methode `RunJobInstance` einen Job ganz einfach mit `Process.Start()`. Zusätzlich werden dem Prozess einige Umgebungsvariablen gesetzt, mit dem der Job Informationen wie Pfade abfragen kann.

Ein Continuous Job wird einfach, wenn notwendig, beendet. Der Job kann aber auch auf überprüfen ob eine Shutdown-Datei an einem bestimmten Ort liegt. Ist das der Fall, so hat er fünf Sekunden Zeit sich zu beenden.

Normalerweise soll ein kontinuierlicher Job so gebaut sein, dass er immer läuft (z.B. durch eine while(true) Schleife im Main). Endet der Job Prozess trotzdem, so wird er sofort wieder gestaret, ausser er ist weniger als zwei Minuten gelaufen.

Aufgaben der verschiedenen Klassen:

- `ContinuousJob`: Enthält allgemeine Informationen über Typ, Name und Run-Command des Jobs. Aber auf eine Referenz zum `IScriptHost`, der die Pfade und Argumente für den jeweiligen Host enthält.
- `ContinuousJobRunner`: Kümmert sich darum, dass der Job immer läuft. Dafür wird ein eigener Thread gestartet. Soll ein Job beendet werden, so wird versucht den Thread mit einem Timeout von fünf Sekunden zu joinen. Gelingt das nicht, wid er mit `Thread.Abort()` hat beendet. Anschließend werden noch die Prozesse der Script-Hosts beendet.
- `ContinuousJobsManager`: Diese Klasse verwaltet ein Dictionary mit dem Job-Namen als Schlüssel und einem `ContinuousJobRunner` als Wert. Ein Großteil der Funktionalität ist aber bereits in der nachfolgenden Basisklasse implementiert.
- `JobsManagerBase`: Zu den Aufgaben dieser Klasse zählt z.B. das Warten auf Änderungen im Wurzelverzeichnis der Jobs. Damit kann ein hinzugefügter Job gestartet, ein entfernter Job beendet oder ein geänderter Job neu gestartet werden. Ausserdem ist diese Klasse für die Bestimmung des auszuführenden Executables oder Script Files zuständig.

Erzeugt wird der `ContinuousJobsManager` als Singelton in der Klasse `NinjectServices`.

## Triggered Jobs

Neben den dauern laufenden Jobs kann auch ein Schedule in Form eines Cron-Ausdrucks definiert werden, an dem der Job laufen soll. Die Architektur ist sehr ähnlich zu jener der Continuous Jobs, was nicht weiter verwundert, da sie einige Basisklassen teilen.

Aufgaben der verschiedenen Klassen:

- `TriggeredJob`: Analog zu `ContinuousJob`, lediglich zusätzlich mit den Informationen wann der Job ausgeführt wurde.
- `TriggeredJobRunner`: Diese Klasse leitet von `BaseJobRunner` ab und bietet zusätzlich mit der Methode `StartJobRun` die Möglichkeit den einmal Job auszuführen. Jeder Run verwendet ein Lock-File, damit nicht mehere Runs gleichzeitig ausgeführt werden. Da Triggered Jobs nicht permanent laufen, müssen sie nicht wie Continuous Jobs, das Shutdown-File beobachten. Sondern bei jeder Ausfrühung kann nachgesehen werden ob der Job überhaupt laufen soll. Der Runner gibt ein WaitHandle nach aussen, mit dem ein Verwender auf das Fertigstellen eines Job Runs warten kann. Standardmäßig bekommt ein Job Run aber nur 30 Sekunden Zeit um zu terminieren. Die Ausführung des Jobs selbst wird einfach am Thread Pool über die selbe Methode der Basisklasse die auch der `ContinuousJobRunner` verwendet, gestartet.
- `TriggeredJobsManager`: In dieser Klasse wird ein `ConcurrentDictionary` mit dem Job Namens als Schlüssel und einem `TriggeredJobRunner` als Wert, verwaltet. Wurde ein Job ausgeführt, wird ein WebHook ausgelöst. Die Function `InvokeTriggeredJob` wird entweder über einen HTTP-Request oder vom `TriggeredJobsScheduler` aufgerufen.
- `JobsController`: Neben verschiedenen Administrativen Funktionen wie dem Auflisten von Jobs, bietet der JobsController auch die Möglichkeiten einen Triggered Job per Web-Request auszuführen. Vorausgesetzt der Web-Request ist mit den Azure Portal Credentials oder Kudu Deployment Credentials authentifiziert. Der Controller leiten den Aufruf einfach an den `TriggeredJobsManager` weiter.
- `TriggeredJobSchedule`: In dieser Klasse befindet sich ein Timer, der dann ein Event feuert, wenn der Job das nächste mal auszuführen ist.
- `TriggeredJobsScheduler`: Die Aufgabe dieser Klasse ist es, die Jobs zur richtigen Zeit auszuführen. Dazu verwaltet sie in Dictionary in dem für jeden Job eine Wert des Typs `TriggeredJobSchedule` enthalten ist. Ändern sich die Einstellungen eines Jobs in der Datei 'settings.job', werden die betroffenen Jobs-Schedules aktualisiert. Feuert der Timer eines `TriggeredJobSchedule` so wird es ausgeführt, oder wenn er in der Zeitpunkt in der Zukunft liegt neu eingetaktet.

Wie auch der Manager der Continuous Jobs leben der Manager und der Scheduler für die Triggered Jobs in der Klasse `NinjectServices` als Singelton.

# Web Jobs SDK

Die Azure Web Jobs SDK hilft das erstellen von Web Jobs stark zu vereinfachen. Primär wird das ermöglicht durch eine Interaktion mit verschiedenen Azure Services auf deklartive Art und Weise. Die beiden Basiskonstrukte dafür sind Bindings und Triggers, die im weiteren noch detailiert erörtert werden. Ausserdem bietet die SDK eine eine Dashboard Seite, in der verschiedene Tracing und Diagnose Funktionen angeboten werden.

In der Core SDK sind bereits einige integrale Services von Azure, wie z.B. Storage und Services Bus enthalten. Die SDK ist aber erweiterbar aufgebaut, sodass eigene Bindings und Trigger möglich sind. Weitere vorgefertigte Anbindungsmöglichkeiten befinden sich in den SDK Extensions Projekt.

## Web Jobs SDK Extensions

Es gibt zwei verschiedene Arten von Bindings:

- *Trigger Bindings*: Überwachen exterene Ereignisse und lösen beim eintreffen die entsprechende Job Funktion aus. 
- *Non-Trigger Bindings*: Erlauben den einfachen Zugriff auf exterene Datenquellen, wie z.B. einer Queue

## Binding Attributes

Das Binding wird in Form von Attributen auf den Parametern einer Job Funktion deklariert:

```
public static void ProcessOrders(
        [QueueTrigger("order")] Order order, 
        [Blob("orders/{OrderId}")] out string orderBlob)
{
  ...
}
```

## Klassen-Übersicht

- `JobHost`
  - Ist der Einstiegspunkt eines SDK Web Jobs. In Program Main muss einfach die Methode `RundAndBlock` aufgerufen werden.
  - Delegiert die ganze Arbeit eigentlich an die `JobHostContextFactory`. Dieser liefert eine Instanz von `JobHostContext`.
  - Blockiert so lange bis `Stop` aufgerufen wird, oder das Shutdown File am vorgesehen Platz liegt.
- `JobHostContextFactory`
  - Erzeugt den `JobHostContext`
  - TBD
- `DynamicHostIdProvider`
  - Generiert eine Host id basierend auf dem Assembly Namen der ersten Funktion und persistiert diese im Storage
  - Die id muss entweder direkt in `JobHostconfiguration` gesetzt sein, oder wird über diese Klasse generiert und persistiert
  - Im Blob Storage unter der Url `azure-webjobs-hosts/ids/<storage-account-name>/(Unkown|name-of-first-found-functions)` wird die Id (Guid) des Hosts gespeichert
  - Hier gibt es aber unterschiede zwischen Basic und Dynamic Tier 
- `JobHostContext`
  - Container für `IFunctionIndexLookup`, `IFunctionExecutor`, `IListener` usw.
- `IFunctionIndexLookup`
  - Gibt für einen gegebenen Funktionsnamen oder `MethodInfo` eine `FunctionDefinition` zurück
- `FunctionDefinition`
  - Container für `FunctionDecriptor`, `FunctionInstanceFactory` und `IListenerFactory`
- `FunctionDescriptor`
  - Enthält Meta-Daten wie Name und Parameter zu einer Funktion
  - Enthält auch eine Referenz zur `MethodInfo` der Funktion
- 


### Finden von Funktionen

- Die `JobHostContextFactory` stößt das finden der Funktionen an, indem es auf dem `IFunctionIndexProvider` die Funktionen `GetAsync` aufruft, die einen `IFunctionIndex` liefert
- Zu erst müssen die in Frage kommenden Typen/Kassen gefunden werden. Dazu bietet der `ITypeLocator` ein Funktion die all diese Typen zurückgibt
  - Es werden nur Assemblies in betracht gezogen, die entweder die das Core Web Jobs Assembly referenzieren oder ein Web Jobs Extension Assembly
  - Aus diesen Assemblies werden Typen betrachet, die:
    - `public class` sind und
    - nicht `abstract` sind, keine generischen Typ-Parameter haben
    - Klasse darf aber `sealed` sein 
  - Die gefundenen Typen werden anschließend dem `FunctionIndexer` übergeben
  - Der `FunctionIndexer` sieht in allen Methoden der gefundenen Typen nach, ob Functionen enthalten sind, die folgende Eigenschaften besitzen:
    - Darf keine generischen Parameter haben
    - Hat mindestens einen Parameter
    - Methode besitzt eine JobAttribut oder
    - Ein Parameter der Methode besitzt ein JobAttribute
  - Ein JobAttribut ist dadurch gekennzeichnet, dass es aus dem Core Web Jobs Assembly kommt oder einem Extension Assembly
- Die gefundenen `MethodInfo`'s werden anschließend vom `FunctionIndex` in den `FunctionIndex` genauer über dessen Schnittstelle `IFunctionIndexCollector` eingefügt
  - Zuerst werden alle Parameter untersucht, ob sie ein 'Trigger-Binding' besitzen
  - Jede Funktion darf höchstens ein 'Trigger-Binding' besitzen
  - Für die restlichen Parameter wird versucht ein 'Binding' zu erstellen, gelingt dies nicht, wird eine Exception geworfen
  - D.h. jede Funktion muss entweder ein Trigger-Binding, Non-Trigger-Binding oder das `NoAutomaticTrigger` Attribut auf der Methode haben
  - Funktionen müssen void oder Task als Rückgabewert haben
  - Für jede Funktion wird ein `FunctionDescriptor` erstellt der im wesentlichen die `MethodInfo` und den Namen enthält. Zusätzlich hat er aber auch eine Liste von `ParameterDescriptor`en. Non-Trigger und Trigger-Bindings können einen `ParameterDescriptor` erstellen
  - `FunctionInvoker` 

### Erweiterbarkeit

TODO ExtensionRegistry usw




FOO Start

JobHostContextFactory:
- Schreibt das file 'WebJobsSdk.marker', über dass Kudu erkennt ob die SDK verwendet wird
- Erzeugt einen JobHostContext

DynamicHostIdProvider:



JobHostContext: 




FOO END






- viele wichtige bindings sind in webjobs-sdk (core)
- core sdk ist aber erweiterbar. zusätzliche extension sind in webjobs-sdk-extensions definiert
- zwei arten von bindings:
  - trigger binding z.B. QueueTrigger: Monitoren externe Datenquellen und lösen aus wenn ein Ereignis eintritt
  - non-trigger binding z.B. Table: Erlaubt zugriff auf externen Datenquelle
- Attribute legen nur deklartiv die benötigten informationen für das binding fest
- die eigentliche arbeit erledigt der binding provider


# KUDU Revised

- KUDU is itself a site extension (and can manage private site extensions)
- There are a lot of pre-installed site extensions: %ProgramFiles(x86)%\SiteExtensions
- Private site extensions are installed by the user and part of the website files
- Some pre-installed extensions:
  - KUDU (of course)
  - AzureJobs (this is the Web Jobs Dashboard extension)
  - Functions (The functions runtime - WebJobs.Script.Host bundled as a site extension)
- In the WebJobs Dashboard functions are not shown as web job. But there is a functions tab on the dashboard site which lists all functions and the number of successful and failed executions

# WebJobsSDK Script

- The `Functions` site extension is installed by default
- It can also be installed as private extension (specific version) via the binary `Functions.private`
- If HTTP is not needed `WebJobs.Script.Host` can also be run as continuous web job


# Summary

- Binding Extensions Overview (https://github.com/Azure/azure-webjobs-sdk-extensions/wiki/Binding-Extensions-Overview)
  - There are two types of bindings
    - Trigger Bindings
    - Non-Trigger Bindings
  - There is a template for building a custom binding (https://github.com/Azure/azure-webjobs-sdk-extensions/tree/master/src/Sample.Extension)
- Binding Process (https://github.com/Azure/azure-webjobs-sdk-extensions/wiki/The-Binding-Process)
  - The binding can be devided in two phases
  - On `JobHost` startup it registers all it's own and all extension `ITriggerBindingProvider` and `IBindingProvider` 
  - The host tries for each function to binding each parameter
    - The first binding wins
    - The host has it's internal representation of all jobs/functions and their bindings
    - For each `ITriggerBinding` it retries a listener and registers the listener
    - 