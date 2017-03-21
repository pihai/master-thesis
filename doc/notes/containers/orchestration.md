# Container orchastretation

- https://www.youtube.com/watch?v=_uw1ISM_uRU&t=1407s
- Docker Swarm uses the same command API as normal Docker -> Same commands can be used or both
- Swarm is for clustering. Group of machines appear as a single ressource
  - Swarm has different scheduling strategies
    - Random
    - Binpack (Fill on machine after another)
    - Spread (Spread evenly)
    - Pluggable
  - Filters
    - Node gets a certain label
    - Constrains which containers can be run on a node
  

# Azure Friday (Kubernetes)

- Kubernetes was started at Google
- Container Orchastrator
- Container is a reliable and reproducable way of packaging software
- Handles rollout without downtime
- Azure Container Service?
- Forget about the machines (no setting up of SSL, ...) and think about containers

# Container Orchestration Wars (Youtube)

- https://www.youtube.com/watch?v=C_u4_l84ED8
- Wiki: Orchestration is arrangement, coordination and managment of complex computer systems
- Functional capabilities:
  - Scheduling: Placeent, Rolling Deployments, Upgrades, Collocation, ...
  - Resource Management: Memory, CPU, Volumes, Ports, ...
  - Service Management: Labels, Groups/Namespaces, Dependencies, Load Balancing, Read Check
- Like a distributed operating system
  - User Space (Source code, CI, Artifact store)
  - System Space (Logging, ...)
  - Kernel (Orchestration)
- Docker Swarm
- Kubernetes (More than orchestration)
