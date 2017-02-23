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

# microXchg 2017 - Adrian Cockcroft: Shrinking Microservices to Functions

Notes for: [Link](https://www.youtube.com/watch?v=ZgxZCXouBkY)

# Evolution of Business Logic from Monoliths through Microservices, to Functions

Notes for: [Link](https://medium.com/@adrianco/evolution-of-business-logic-from-monoliths-through-microservices-to-functions-ff464b95a44d#.y6z6q8xym)

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