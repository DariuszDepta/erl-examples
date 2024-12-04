-module(counter).
-export([start/0, loop/1, initialize/1, initialize/2, query/1, inc/1, dec/1]).

start() ->
  spawn(counter, loop, [undefined]).

initialize(Counter) ->
  Counter!{self(), initialize, zero},
  receive
    A -> A
  end.

initialize(Counter, Value) when is_integer(Value)->
  Counter!{self(), initialize, set, Value},
  receive
    A -> A
  end.

inc(Counter) ->
  Counter!{self(), execute, inc},
  receive
    A -> A
  end.

dec(Counter) ->
  Counter!{self(), execute, dec},
  receive
    A -> A
  end.

query(Counter)->
  Counter!{self(), query, value},
  receive
    A -> A
  end.

loop(Value) ->
  receive
    {Caller, initialize, zero} ->
      Caller ! {initialize_result, 0},
      loop(0);
    {Caller, initialize, set, NewValue} when is_integer(NewValue) ->
      Caller ! {initialize_result, NewValue},
      loop(NewValue);
    {Caller, execute, set, NewValue} when is_integer(NewValue) ->
      Caller ! {execute_result, NewValue},
      loop(NewValue);
    {Caller, execute, inc} when Value < 255 ->
      NewValue = Value + 1,
      Caller ! {execute_result, NewValue},
      loop(NewValue);
    {Caller, execute, inc} ->
      Caller ! {execute_result, Value},
      loop(Value);
    {Caller, execute, dec} when Value > 0 ->
      NewValue = Value - 1,
      Caller ! {execute_result, NewValue},
      loop(NewValue);
    {Caller, execute, dec} ->
      Caller ! {execute_result, Value},
      loop(Value);
    {Caller, query, value} ->
      Caller ! {query_result, Value}
  end,
  loop(Value).
