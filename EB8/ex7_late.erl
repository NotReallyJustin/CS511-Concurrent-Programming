-module(ex7_late).
-compile(export_all).
-compile(nowarn_export_all).

start(P, J) ->
    % Spawn the server, the timer to track whether it got late, and patriots and jets fans
    % Initially, it didn't get late
    S = spawn(?MODULE, server, [0, false]),
    [spawn(?MODULE, patriots, [S]) || _ <- lists:seq(1, P)],
    [spawn(?MODULE, jets, [S]) || _ <- lists:seq(1, J)],
    spawn(?MODULE, itGotLate, [3000, S]).

itGotLate(Time, S) ->
    % A timer to track whether it got late
    timer:sleep(Time),
    R = make_ref(),
    S ! {self(), R, itGotLate},
    receive
        {S, R, ok} ->
            done
    end.

patriots(S) ->
    % S = Reference to PID of server
    S ! {patriots},
    io:format("Let a patriot in ~n", []).

jets(S) ->
    % S = Reference to PID of server
    
    % Send a message requesting jets entry
    S ! {jets, self()},

    % Wait for permission to enter
    receive
        {canLetOneIn} ->
            io:format("Let a jets in ~n", [])
    end.

server(Extra_Patriots, false) ->
    % Counters for Patriots available for justifying ingress of Jets
    % false == it did not get late yet
    
    receive
        % if we have a patriot, increment the counter
        {patriots} ->
            server(Extra_Patriots + 1, false);

        {jets, Jets_PID} when Extra_Patriots > 1 ->
            % If you have 2+ extra patriots, you can let a jets in
            % Send one of them the message
            Jets_PID ! {canLetOneIn},
            server(Extra_Patriots - 2, false);

        {Timer_PID, Reference, itGotLate} ->
            % If we got a "we got late" message, send an ok and switch server state now
            io:format("it got late! ~n", []),
            Timer_PID ! {Timer_PID, Reference, ok},
            server(Extra_Patriots)
    end.

server(Extra_Patriots) ->
    % Counters for Patriots available for justifying ingress of Jets, true = it got late
    
    receive
        % if we have a patriot, increment the counter. But lowkey it doesn't matter
        {patriots} ->
            server(Extra_Patriots + 1);

        % if we have a jet, it doesn't matter too lol just let them in
        {jets, Jets_PID}->
            % If you have 2+ extra patriots, you can let a jets in
            % Send one of them the message
            Jets_PID ! {canLetOneIn},
            server(Extra_Patriots)
    end.
