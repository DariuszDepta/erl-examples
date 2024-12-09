-module(atomicus).
-export([start/1]).

start(N) ->
  statistics(runtime),
  statistics(wall_clock),
  s(N),
  {_, Time1} = statistics(runtime),
  {_, Time2} = statistics(wall_clock),
  io:format("System time: ~p [ms], wall time: ~p [ms]~n", [Time1, Time2]).

s(0) ->
  ok;
s(N) when is_integer(N) ->
  L = [$a | integer_to_list(N)],
  _A = list_to_atom(L),
  s(N - 1).
