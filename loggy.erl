
-module(loggy).
-export([start/1, stop/1]).


start(Nodes) ->
    spawn_link(fun() ->init(Nodes) end).

stop(Loggy) ->
   Loggy ! stop.

init(_) ->
    % Initialize the clock and holdback queue
    Clock = time:clock(Nodes),
    Queue = [],
    loop(Clock, Queue).

loop(Clock, Queue) ->
    receive
        {log, From, Time, Msg} ->
            % Update the clock when a new log message is received
            NewClock = time:update(From, Time, Clock),
            
            % Add the message to the holdback queue
            NewQueue = [{Time, From, Msg} | Queue],

            % Attempt to deliver any messages that are now safe to print
            {SafeMessages, RemainingQueue} = deliver_safe(NewClock, NewQueue, []),

            % Print the safe messages
            lists:foreach(fun({T, F, M}) -> log(F, T, M) end, SafeMessages),
            
            % Continue the loop with the updated clock and queue
            loop(NewClock, RemainingQueue);

        stop -> 
            ok
    end.

    deliver_safe(Clock, [{Time, From, Msg} | Rest], Acc) ->
        if
            time:safe(Time, Clock) ->
                % If the message is safe, add it to the accumulated list
                deliver_safe(Clock, Rest, [{Time, From, Msg} | Acc]);
            true ->
                % If not safe, hold it back
                {lists:reverse(Acc), [{Time, From, Msg} | Rest]}
        end.
 
log(From, Time, Msg) ->
    io:format("log: ~w ~w ~p~n", [Time, From, Msg]).

