% docker run --rm -it erlang

f().

Pid = spawn_link(
  fun() -> 
    receive
      a -> io:format("got a~n");
      b -> io:format("got b~n")
    end
  end).

Pid ! a.
Pid ! b.
Pid ! c.
Pid ! a.

% Since the message on top of the message queue is an unmatched message and doesn't go away, no further actions are performed after c was sent