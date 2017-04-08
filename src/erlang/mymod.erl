-module(mymod).
-export([foo/0, bar/1, baz/0]).

foo() -> io:format("foo()~n").
bar(X) -> io:format("bar(~p)~n", [X]).


priv() -> io:format("priv~n").

baz() -> spawn_link(?MODULE, priv, []).

