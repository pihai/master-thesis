#$region = "eastus"
#$region = "northeurope"
#$region = "westeurope"
$region = "westus"
$resGrpName = "coldStartFunTestGrp-$region"
$appName = [guid]::NewGuid()
$timingAppName = "$appName-test"

# login (will prompt a login dialog)
Try {
  Get-AzureRmContext
} Catch {
  if ($_ -like "*Login-AzureRmAccount to login*") {
    Login-AzureRmAccount
  }
}

$ctx = Get-AzureRmContext
Get-AzureRmSubscription -SubscriptionId $ctx.Subscription.SubscriptionId -TenantId $ctx.Tenant.TenantId

# Get-AzureSubscription
# Select-AzureSubscription -SubscriptionId $ctx.Subscription.SubscriptionId -Default

# create a resource group
New-AzureRmResourceGroup -Location $region -Name $resGrpName -Force

# create storage account for timing results - requires a resource group called garbage
$storageResGrpName = "Garbage"
$storageName = "hptimingstorage"
if(!(Test-AzureName -Storage $storageName)) {
    New-AzureRmStorageAccount -ResourceGroupName $storageResGrpName -Name $storageName -SkuName 'Standard_LRS' -Location "westeurope"
}
$storageKeys = Get-AzureRmStorageAccountKey -ResourceGroupName $storageResGrpName -AccountName $storageName
$storageKey = $storageKeys.Item(0).Value
$storageCnnStr = "DefaultEndpointsProtocol=https;AccountName=$storageName;AccountKey=$storageKey"

# create a function app (dyn) within that group
New-AzureRmResourceGroupDeployment -ResourceGroupName $resGrpName -TemplateUri https://raw.githubusercontent.com/Azure/azure-quickstart-templates/master/101-function-app-create-dynamic/azuredeploy.json -TemplateParameterObject @{ appName = $appName }
New-AzureRmResourceGroupDeployment -ResourceGroupName $resGrpName -TemplateUri https://raw.githubusercontent.com/Azure/azure-quickstart-templates/master/101-function-app-create-dynamic/azuredeploy.json -TemplateParameterObject @{ appName = $timingAppName }

function Set-AppSetting($ResGrp, $AppName, $Slot, $Settings) {
    $app = Get-AzureRMWebAppSlot -ResourceGroupName $ResGrp -Name $AppName -Slot $Slot
    $appSettingList = $app.SiteConfig.AppSettings

    $hash = @{}
    ForEach ($kvp in $appSettingList) {
        $hash[$kvp.Name] = $kvp.Value
    }
    ForEach ($h in $settings.GetEnumerator()) {
        $hash[$h.Name] = $h.Value
    }

    Set-AzureRMWebAppSlot -ResourceGroupName $ResGrp -Name $AppName -AppSettings $hash -Slot $Slot
}

Set-AppSetting -ResGrp $resGrpName -AppName $timingAppName -Slot "production" -Settings @{ "url" = "https://$appName.azurewebsites.net/api/httpTrigger"; "location" = $region; "timingCnnStr" = $storageCnnStr }

function Deploy-Function($ResGrp, $AppName, $FuncName, $CodeFileName) {
    $funConfigFile = Get-Content "$FuncName/function.json" | Out-String
    $funcConfigJsonOjb = ConvertFrom-Json $funConfigFile
    $codeFileContent = "$(Get-Content -Path "$FuncName/$CodeFileName" -Raw)"

    $props = 
        @{
            config = $funcConfigJsonOjb
            files = @{
                "$CodeFileName" = $codeFileContent
            }
        }

    New-AzureRmResource -ResourceGroupName $ResGrp -ResourceType Microsoft.Web/sites/functions -ResourceName $AppName/$FuncName -PropertyObject $props -ApiVersion 2015-08-01 -Force
}

Deploy-Function -ResGrp $resGrpName -AppName $appName -FuncName "httpTrigger" "index.js"
Deploy-Function -ResGrp $resGrpName -AppName $timingAppName -FuncName "timerTrigger" "run.csx"

#Remove-AzureRmResourceGroup -Name "coldStartFunTestGrp-westeurope"

#Measure-Command { Invoke-WebRequest -Uri "https://418346e8-e563-4073-9d3b-531f036cba86.azurewebsites.net/api/httpTrigger?code=5fysvEP1lz1FjEr7eXh7EJEZzGqTg5DwSNeCY42JBiGsKfHP4giYLA==&name=hello" }
