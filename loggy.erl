
-module(loggy).
-export([start/1, stop/1]).

% starts the logger with the existing nodes by passing the nodes to the initializer 
start(Nodes) ->
    spawn_link(fun() ->init(Nodes) end).

% stops the logger
stop(Loggy) ->
   Loggy ! stop.

init(Nodes) ->
    % Initialize the clock and holdback queue
    Clock = time:clock(Nodes),
    Queue = [],
    % passe the created clock and que to the loop function
    loop(Clock, Queue).

%loops through the messages recursively for each worker
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

% we dont use the clock here since we are just adding to the list 
deliver_safe(_Clock, [], Acc) ->
    {lists:reverse(Acc), []}; % Return accumulated safe messages

deliver_safe(Clock, [{Time, From, Msg} | Rest], Acc) ->
    case time:safe(Time, Clock) of
        true ->
            % If the message is safe, add it to the accumulated list
            deliver_safe(Clock, Rest, [{Time, From, Msg} | Acc]);
        false ->
            % If not safe, hold it back
            {lists:reverse(Acc), [{Time, From, Msg} | Rest]}
    end.
 
log(From, Time, Msg) ->
    io:format("log: ~w ~w ~p~n", [Time, From, Msg]).

