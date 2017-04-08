-module(sup).
-export([start/4, init/4]).

start(Name, M, F, A) ->
  spawn(?MODULE, init, [Name, M, F, A]).

init(Name, M, F, A) ->
  process_flag(trap_exit, true),
  loop(Name, M, F, A).

loop(Name, M, F, A) ->
  Pid = spawn_link(M, F, A),
  register(Name, Pid),
  receive
    { 'EXIT', Pid, normal } -> ok;
    { 'EXIT', Pid, Reason } -> 
      loop(Name, M, F, A);
    { 'EXIT', From, _ } -> exit(shutdown)
  end.
