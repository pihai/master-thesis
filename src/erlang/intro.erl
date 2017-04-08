% VS Code Instructions:
% 1. Open a terminal with Ctrl+ö
% 2. `docker run -it --rm erlang`
% 3. Select some code an run it with Alt+Enter in the REPL

% Print something to check if it is working
io:format("Hello world~n", []).

% Functions (Only lambda functions work in the repl)
F = fun(Arg1, Arg2) -> io:format("F1 called with ~p and ~p~n", [Arg1, Arg2]) end.

% Unbind (Variables can only be bound once)
f(F).

% Spawn a new process
P1 = fun() -> io:format("process created") end.
Pid = spawn(P1). % Pid is the unique address

% self() always returns the process id of the current process (even the shell is a normal process with a pid)
self().

% Sending messages
self() ! hello. % sending always returns the message, so that these sends can be chained

% flush() receives all messages of the mailbox of the current process
flush().

% receiving messages
self() ! foo.
receive
  X -> io:format("received: ~p~n", [X])
end.
io:format("here").

% timeouts
receive
  _ -> io:format("received something!~n")
after 2000 ->
  io:format("timed out~n")
end.
io:format("here").

% Ping-Pong
PongF = fun() -> 
    receive
      { ping, From } -> 
        io:format("got ping from ~p~n", [From]),
        From ! pong
    end
  end.
Pong = spawn(PongF).

PingF = fun() -> 
      Pong ! { ping, self() },
      receive
        pong -> io:format("got pong~n")
      end
    end.
Ping = spawn(PingF).

% Keeping state
PongF = fun ThisFun() -> 
          receive
            { ping, From } -> io:format("got ping from ~p~n", [From]), 
                              From ! pong,
                              ThisFun();
            finished       -> io:format("pong finished~n")
          end
        end.
Pong = spawn(PongF).

PingF = fun ThisFun(0) -> 
          io:format("ping finished~n");
        ThisFun(N) ->
          Pong ! { ping, self() },
          receive
            pong -> io:format("got pong~n")
          end,
          ThisFun(N-1)
        end.
Ping = spawn(fun() -> PingF(5) end).

% Linking processes
% we have no idea that the process ended
self(). % print the process id of the shell
Pid3 = spawn(fun() -> io:format("process .."), timer:sleep(1000),  exit("i'm dead now") end).
link(Pid3). % because the shell is linked to Pid3, they die together
self(). % shell is a new process id because it was restarted

% spawn_link (Because linking after spawning introduces a race condition, there is also an atomic version)
self().
Pid4 = spawn_link(fun() -> io:format("process .."), exit("i'm dead now") end).
self().

% trapping
% sometimes we want to control (e.g. restart) other processes. So it's not a good idea to let the supervior process die. trap_exit transforms the exit message in a regular messar which can be received.
process_flag(trap_exit, true).
self().
Pid5 = spawn_link(fun() -> io:format("process .."), exit("i'm dead now") end).
self(). % shell has still the same id
flush(). % we have the exit message in the mailbox
