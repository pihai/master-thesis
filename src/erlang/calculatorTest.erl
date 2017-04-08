% docker run --rm -it -v ${PWD}:/code erlang

cd("/code").
% compile the module
c(calculator). 
% start process and store the id
Pid = calculator:init().
% send messages by process id
Pid ! { add, 6 }.
% register a name for a process id
register(calc_server, Pid).
% send messages by name
calc_server ! { mult, 7 }.
calc_server ! result. % prints 42
calc_server ! stop.
% next message won't arrive,
% but sending succeeds
calc_server ! result.



% SUPERVISION
cd("/code").
% compile the module
c(calculator), c(sup).
sup:start(
  calc_server, 
  calculator, loop, [0]
).
whereis(calc_server).
whereis(calc_server).
calc_server ! {add, 1}.
calc_server ! { divi, 0 }. % crashes the process
whereis(calc_server). % process got restarted
calc_result ! result. % 0