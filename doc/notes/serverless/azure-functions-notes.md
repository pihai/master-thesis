# What is Serverless Computing? Exploring Azure Functions

[Link](https://www.hanselman.com/blog/WhatIsServerlessComputingExploringAzureFunctions.aspx)

- ...

# Introducing Azure Functions

[Link](https://azure.microsoft.com/en-us/blog/introducing-azure-functions/)

- Data is everywhere
- Scheduled or batched data processing
- Ever compressed-Time tables
- Free to write code, not setting up infrastructure
- Azure Functions is an extension to the Azure application platform
- Triggered from any event
- Connecting to data-sources and messaging systems
- Easy react and process events
- Additional browser based interface
- Variety of languages
- Triggers (WebHooks, ...)
- Innovative way to process data (bindings)
- Complete infrastructure abstraction
- Function runtime, UI, WebJobs SDK all open-source, can run in "any" environment

# Azure Functions: The Journey

[Link](https://blogs.msdn.microsoft.com/appserviceteam/2016/04/27/azure-functions-the-journey/)

- laverages existing app service building blocks -> e.g. web jobs sdk
- web jobs sdk
  - existed for some years
  - DECLARATIVE programming model
  - build background processing jobs
  - Focus on business logic rather than storage operations
- Functions runtime
  - add a JSON description model to allow leveraging other progrmming languages
  - In memory adapter between arbitrary languages and the .net webjobs sdk runtime
  - generates the c# method stub on the fly and calls the method body of the actual function
  - Same trigger/binding runtime for all languages
  - Every effort put into Web Jobs SDK will be immediatelly available to all supported languages
- The functions runtime runs as SCM site extension: D:\Program Files (x86)\SiteExtensions\Functions
- Dynamic Compute removed the ability to create a web app. Just run functions as a service
- Simple file system model. Portal can easily interact via KUDU api on those files
- Extensiblity
  - Web Jobs SDK is extensible

# Making Azure Functions more “serverless”

[Link](https://blogs.msdn.microsoft.com/appserviceteam/2016/11/15/making-azure-functions-more-serverless/)

- solving business and application problems, not infrastruktur problems
- dropped the need to pre-define the memory limit
- algorithms have dropped the cost of GB-sec about a factor of 5, compared to manual memory settings
- get the right resources, pay the least

# Azure Functions developers guide

[Link](https://docs.microsoft.com/en-us/azure/azure-functions/functions-reference)

- Function is the primary concept: Code + Configuration
- Function App: Multiple functions managed together within the same app (share pricing and deplyoment)
  - One function app can container functions written in many languages
- Script Host: WebJobs SDK Host, listens for triggers, runs code, ..
- Web Host: Sits in front of the Script host to make http calls possible
- folder structure:
  - wwwroot
    - host.json
      - function-name
        - run.csx
        - function.json
  - node functions folder usually contains the node_modules folder

# Announcing general availability of Azure Functions

[Link](https://azure.microsoft.com/en-us/blog/announcing-general-availability-of-azure-functions/)

- billed in msec steps with 100 msec minimum?
- dynamically scale up and down