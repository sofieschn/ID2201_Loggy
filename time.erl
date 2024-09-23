-module(time).
-export([zero/0, inc/2, merge/2, leq/2]).

zero() -> 
    0.

inc(Name, Time) ->
    Time+1.

merge(Timei, Timej) ->
    lists:max([Timei, Timej]).

leq(Timei, Timej) ->
    Timei =< Timej.




