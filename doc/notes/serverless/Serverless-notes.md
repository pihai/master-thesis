# Serverless

Notes for: [Link](https://martinfowler.com/bliki/Serverless.html)

- Traditional
  - Long-lived server, handling the application logic
- Serverless Architecture
   - No server
   - Some aspects of the server are handled by the client
   - The rest is implmeneted via third party services and RPCs in FaaS

# AWS Lambda is Everywhere

Notes for: [Link](https://read.acloud.guru/aws-lambda-everywhere-f28c855e44b9#.w8fpv3t4x)

- Event driven functions
- Lambda originated in the storage tier (actions triggered by storage operations)
- Lambda running on edge servers
- AWS Greengrass (lambda on the device?)

# Evolution of Business Logic from Monoliths through Microservices, to Functions

Notes for: [Link](https://medium.com/@adrianco/evolution-of-business-logic-from-monoliths-through-microservices-to-functions-ff464b95a44d#.y6z6q8xym)

- Time to value
- The point of running software is to deliver business value
- Delivery authomation
  - 10 years ago the best way to deliver business logic was a monlith
  - In the past software delivery was expensive (buy, install, configure, pre-provision extra capacity, bad utilization, ...)
  - automation
  - infrastructure as code (chef, puppet)
  - DevOps
  - Operation teams adopted agile principles
  - Deploy capacity just in time and pay by the hour -> better utilization and more cost effective
  - Next big wave
    - Docker
    - More isolation than process but less than servers
    - Startup times in the seconds
    - Fixed dependencies
    - Saving memory
    - For spiky workloads lambda is better suited than containers
- Hardware Capabilities
  - Network evolved form 1GBit interfaces to 25Gib
  - Software protocols got better
  - In the past everything was tied into a single monolith or a few big services, beause of the high latency
  - HDDs were replaced with SSDs
  - Few centralized RDBMS were replaced with lots of NoSql DBs
  - DBs have been created especially for SSDs (Dynamo DB)
  - SSDs are another enabler for the move away from monoliths
- Organizational Changes
  - In the Past big teams working on the same monolith
  - Management overhead, meetings and tickets
  - In a microservices only small teams which are fully responsible for their service
  - Shift from project to product: Replace many project managers with fewer product managers
- Lambda means fewer code: less boilerplate and platform code
  - This means that code can be production ready within days
  - Because they glue together well tested an robust services and storages
  - Soluations are automatically scalable, low cost, fast to deploy, ...
  - Model house with clay vs. lego example
  - More constrained but much faster and cheaper


# microXchg 2017 - Adrian Cockcroft: Shrinking Microservices to Functions

Notes for: [Link](https://www.youtube.com/watch?v=ZgxZCXouBkY)

- Adrian Cockcroft (former Netflix) now works for Amazon
- Buiness lock is hidden within a monolith
  - Then it was distributed as microservices
  - And finally broken into functions
- Three dimensions have driven this evolution
  - Networking/Messaging
    - Technology change
    - 10 years ago
      - big java application, connected to a big schema database
      - Then broken up into web-services (ws-*, soap, xml, ...)
      - Only 1 Gbit network
      - But networks were slow at that time and messages were big
      - Therefore messaging was rarely used
    - Today
      - 25 Gbit network
      - More lightweight serialization formats (not xml)
      - No 100-1000 times faster
    - Enabled single responsiblity (Was not possible because of performance concerns in the past)
  - Efficency
    - 10 years ago
      - Few releases
      - Manual processes
      - Manual delivery of software
    - Today
      - Agile
      - DevOps -> Automate everything
      - Update machines via chef/puppet
      - Public-Cloud
      - Elastic Scale
      - Give the devs an API to operations
      - Blue/Green Deployment (start new code on new machines, leave the old as is and finally switch) (No inplace updates -> create new ones) (immutable infrastructure)
      - Can deliver more often because now it is fast
      - VMs still needed minutes to start (which is not acceptable in very spiky load scenarios)
      - Containers gave the ability to start within seconds
      - Standardization/Reuse of containers (whichever is on docker hub) (well tested)
  - Lambda
    - Code waiting until a request comes
    - Then the code gets deployed withing 100s of milliseconds
    - Following calls will start in 10s of milliseconds
    - By per 100ms instead of per hour with VMs
- More quickly: Modeling a house with plastic vs lego bricks
  - first one will take very long. createive process. but all possiblities
  - second one will be much quicker. less creativity. standardized components. glue logic. constraining.
- Each team should "own" the service. High trust is necessary
- Where is it going?
  - Batch scripting for infrastructure (event driven)
  - Event driven infrastructure (e.g. attach automatically a volume to a new machine)
  - Lambda at edge nodes (low latency) (modify images at the edge node. not storing multiple versions)
  - IoT: Lambda on the device
- Don't think about reuse but about replacability

# Migrating to Serverless - an experience report - Gojko Adzic

Notes for: [Link](https://vimeo.com/205453444)

- We still think about reserved capacity
- Monitoring is very good because the providers need it for billing
- A/B testing is cheap because two deplyoments don't cost twice
- Glue together other platform service

