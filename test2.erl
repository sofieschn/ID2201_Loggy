-module(test2).
-export([run/2]).

run(Sleep, Jitter) ->
    %start the logger with four different beatles :P 
    Log = loggy:start([john, paul, ringo, george]),

    % start the workers 
    A = worker:start(john, Log, 13, Sleep, Jitter),
    B = worker:start(paul, Log, 23, Sleep, Jitter),
    C = worker:start(ringo, Log, 36, Sleep, Jitter),
    D = worker:start(george, Log, 49, Sleep, Jitter),

    % Set peers for each worker
    worker:peers(A, [B, C, D]),
    worker:peers(B, [A, C, D]),
    worker:peers(C, [A, B, D]),
    worker:peers(D, [A, B, C]),

    % Let them run for 5 seconds 
    timer:sleep(5000),

    % Stop the logger and workers
    loggy:stop(Log),
    worker:stop(A),
    worker:stop(B),
    worker:stop(C),
    worker:stop(D).








