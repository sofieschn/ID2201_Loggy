-module(worker).
-export([start/5, stop/1, peers/2]).

start(Name, Logger, Seed, Sleep, Jitter) ->
    spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).

stop(Worker) ->
    Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter) ->
    rand:seed(exsplus, {Seed, Seed + 1, Seed + 2}),
    % Initialize the Lamport clock
    Clock = time:zero(),  
    receive
        {peers, Peers} ->
            % Pass the initialized clock into the loop function
            loop(Name, Log, Peers, Sleep, Jitter, Clock);
        stop -> 
            ok
    end.

peers(Wrk, Peers) ->
   Wrk ! {peers, Peers}.

   loop(Name, Log, Peers, Sleep, Jitter, Clock) ->
    Wait = rand:uniform(Sleep),
    receive
        {msg, ReceivedTime, Msg} ->
            % Message received from another worker
            % Merge the current clock with the received message's clock
            NewClock = time:inc(Name, time:merge(Clock, ReceivedTime)),
            % Log the message with the updated clock
            Log ! {log, Name, NewClock, {received, Msg}},
            % Continue the loop with the updated clock
            loop(Name, Log, Peers, Sleep, Jitter, NewClock);
                   
         % Handle the stop message (stop the worker)
        stop -> 
            ok;
        
        Error ->
            % Log any errors that occur
            Log ! {log, Name, Clock, {error, Error}}
    after Wait ->
        Selected = select(Peers),
        
        % Increment the clock before sending the message
        NewClock = time:inc(Name, Clock),
        
        Message = {hello, rand:uniform(100)},
        
        % Send the message with the updated clock
        Selected ! {msg, NewClock, Message},
        
    % random delay
        jitter(Jitter), 
        
        % Log the sending event with the updated clock
        Log ! {log, Name, NewClock, {sending, Message}},
        
        % Continue the loop with the updated clock
        loop(Name, Log, Peers, Sleep, Jitter, NewClock)
    end.


select(Peers) ->
    lists:nth(rand:uniform(length(Peers)), Peers).
    
jitter(0) -> ok;
jitter(Jitter) -> timer:sleep(rand:uniform(Jitter)).
