-module(barr_client).
-compile(export_all).
-compile(nowarn_export_all).

% Spawn 3 clients
% Each client will wait at barrier, wait, go through, and rearrive at barrier
% Expected behavior: you see abc before 123
start() ->
    B = barr:make(3),
    spawn(?MODULE, client1, [B]),
    spawn(?MODULE, client2, [B]),
    spawn(?MODULE, client3, [B]),
    ok.

client1(B) ->
    io:format("a"),
    barr:synch(B),
    io:format("1"),
    client1(B).

client2(B) ->
    io:format("b"),
    barr:synch(B),
    io:format("2"),
    client2(B).

client3(B) ->
    io:format("c"),
    barr:synch(B),
    io:format("3"),
    client3(B).
