-module(time).
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).

zero() -> 
    0.

%take the time and increment it by 1.
inc(_Name, Time) ->
    Time+1.

% merge the two Lamport time stamps (i.e., take the maximum value)
merge(Timei, Timej) ->
    lists:max([Timei, Timej]).


% true if Timei is less than or equal to Timej
leq(Timei, Timej) ->
    Timei =< Timej.

% return a clock that can keep track of the nodes;
clock(Nodes) ->
    lists:foldl(fun(Node, Acc) -> maps:put(Node, zero(), Acc) end, #{}, Nodes).

% return a clock that has been updated given that we have received a log message from a node at a given time;
update(Node, Time, Clock) ->
    maps:put(Node, Time, Clock).

% is it safe to log an event that happened at a given time, true or false?
safe(Time, Clock) ->
    lists:all(fun({_, NodeTime}) -> NodeTime =< Time end, maps:to_list(Clock)).







