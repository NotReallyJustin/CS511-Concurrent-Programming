-module(ex7).
-compile(export_all).
-compile(nowarn_export_all).

start(P, J) ->

    % Spawn jets patriots server
    % 1 jets fan for every 2 patriots fans
    S = spawn(?MODULE, server, [0]),

    % Spawn P patriot fans and J jets fans
    [spawn(?MODULE, jets, [S]) || _ <- lists:seq(1, J)],
    [spawn(?MODULE, patriots, [S]) || _ <- lists:seq(1, P)].


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

server(Extra_Patriots) ->
    % Counters for Patriots available for justifying ingress of Jets
    
    receive
        % if we have a patriot, increment the counter
        {patriots} ->
            server(Extra_Patriots + 1);

        {jets, Jets_PID} when Extra_Patriots > 1 ->
            % If you have 2+ extra patriots, you can let a jets in
            % Send one of them the message
            Jets_PID ! {canLetOneIn},
            server(Extra_Patriots - 2)
    end.
