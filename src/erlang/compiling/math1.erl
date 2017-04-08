-module(math1).
-export([factorial/1]).

factorial(0) -> 2;
factorial(N) -> N * factorial(N-1).

% steps to compile an .erl file into a .beam file with the shell:
% docker run --rm -it -v ${PWD}:/usr/erl erlang
% cd("/usr/erl/compiling").
% c(math1).
% math1:factorial(45).

