-module(calculator).
-export([init/0, loop/1]).

init() ->
  spawn(fun () -> loop(0) end).

loop(Total) ->
  receive
    { add,  X } -> loop(Total + X);
    { mult, X } -> loop(Total * X);
    { divi, X } -> loop(Total / X);
    result      -> io:write(Total),
									 loop(Total);
    stop    		-> ok
  end.