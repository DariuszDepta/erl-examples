-module(processes).
-export([max/1]).

max(N) when is_integer(N) ->
  Max = erlang:system_info(process_limit),
  io:format("Maximum allowed processes: ~p~n", [Max]),
  io:format("Requested number of processes: ~B~n", [N]),
  statistics(runtime),
  statistics(wall_clock),
  L = for(1, N, fun() -> spawn(fun() -> wait() end) end),
  {_, Time1} = statistics(runtime),
  {_, Time2} = statistics(wall_clock),
  lists:foreach(fun(Pid) -> Pid!die end, L),
  U1 = Time1 * 1000 / N,
  U2 = Time2 * 1000 / N,
  io:format("Process spawn time: ~p [µs], wall time: ~p [µs]~n", [U1, U2]),
  io:format("Total time: ~p [ms], wall time: ~p [ms]~n", [Time1, Time2]).

wait() ->
  receive
    die -> void
  end.

for(N, N, F) -> [F()];
for(I, N, F) -> [F()|for(I+1, N, F)].
