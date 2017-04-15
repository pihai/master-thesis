-module(greeter).
-export([loop/1, upgrade/1]).
loop(State) -> receive
  greet -> 
    io:format("Version ~p.~n",[State]),
    loop(State);
  update ->
    NewState = ?MODULE:upgrade(State),
    ?MODULE:loop(NewState)
end.
upgrade(OldState) -> 
  % upgrade the state if necessary
  OldState + 1.

% test this code:
% docker run --rm -it -v ${PWD}:/code erlang
% cd("/code").
% c(greeter).
% Pid = spawn_link(greeter, loop, [1]).
% Pid ! greet.
% change the text in line 7
% c(greeter).
% Pid ! greet. % still yields the old message
% Pid ! update.
% Pid ! greet. % Now it works