% 1. Create a network, so that containers can communicate with each other
%    docker network create erlang-network

% 2. Create a container, attach it to the network, set the host name, register a dns entry in the network, start the beam-vm with a short name and set a cookie (connected erlang vms need the same cookie)
%    - A short name is just the host name, while a full name is a fully qualified domain name (foo.bar.com)
%    - The cookie is like a shared secret which every node in the cluster needs to know
%    docker run --rm -it --net=erlang-network -h erl1 --net-alias=erl1 erlang erl -sname foo -setcookie mycookie
% 