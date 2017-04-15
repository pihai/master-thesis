% 1. Create a network, so that containers can communicate with each other
%    docker network create erlang-network

% 2. Create a container, attach it to the network, set the host name, register a dns entry in the network, start the beam-vm with a short name and set a cookie (connected erlang vms need the same cookie)
%    - A short name is just the host name, while a full name is a fully qualified domain name (foo.bar.com)
%    - The cookie is like a shared secret which every node in the cluster needs to know
%    docker run --rm -it --net=erlang-network -h nodeA --net-alias=nodeA erlang erl -sname alice -setcookie mycookie
%    docker run --rm -it --net=erlang-network -h nodeB --net-alias=nodeB erlang erl -sname bob -setcookie mycookie
%    docker run --rm -it --net=erlang-network -h nodeC --net-alias=nodeC erlang erl -sname charly -setcookie mycookie

% 4. Show the node id with: node().

% 5. Show all connected nodes with: nodes().
%    At this point the list should be empty

% 6. One of the following commands should connect the nodes:
%    - spawn(<node-id>, ...).
%    - net_kernel:connect_node(<node-id>).
%    - net_adm:ping(Node).

% 7. At this point at least two nodes should be connected: nodes().

% 8. Nodes are automatically propagated to all the other connected nodes

% 9. Multiple nodes can be stated on the same host, but only as long as the name is unique on this host.
%    docker exec -it <container-id-of-nodeC> erl -sname alice -setcookie mysecret

% 10. We can also monitor the status of a node:
%     monitor_node(<node-id>, true).
%     Terminate the vm of the node
%     flush(). The node should have received a message { nodedown, alice@nodeC }.
