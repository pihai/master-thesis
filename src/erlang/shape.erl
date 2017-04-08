% docker run --rm -it -v ${PWD}:/usr/erl erlang

-module(shape).
-export([area/1]).

area({square, Width}) -> Width * Width;
area({rectangle, Width, Height}) -> Width * Height;
area({circle, Radius}) -> math:pi() * Radius * Radius;
area(_) -> erlang:error(unknown_shape).
