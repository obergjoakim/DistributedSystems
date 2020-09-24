-module(testVec).
-export([run/3]).

run(Sleep, Jitter,TestSleep) ->
    Log = loggerVec:start([john, paul, ringo, george]),
    A = workerVec:start(john, Log, 13, Sleep, Jitter),
    B = workerVec:start(paul, Log, 23, Sleep, Jitter),
    C = workerVec:start(ringo, Log, 36, Sleep, Jitter),
    D = workerVec:start(george, Log, 49, Sleep, Jitter),
    workerVec:peers(A, [B, C, D]),
    workerVec:peers(B, [A, C, D]),
    workerVec:peers(C, [A, B, D]),
    workerVec:peers(D, [A, B, C]),
    timer:sleep(TestSleep),
    loggerVec:stop(Log),
    workerVec:stop(A),
    workerVec:stop(B),
    workerVec:stop(C),
    workerVec:stop(D).