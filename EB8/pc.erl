-module(pc).
-compile(export_all).
-compile(nowarn_export_all).

% Producer consumer example

start(P, C, Size) ->
    % Size of buffer, producer producing, consumers consuming, # of sushi in slots
    S = spawn(?MODULE, server, [Size, 0, 0, 0]),

    % Spawn consumers
    % Pass in Server_PID
    [spawn(?MODULE, consumer, [S]) || _ <- lists:seq(1, C)],
    [spawn(?MODULE, producer, [S]) || _ <- lists:seq(1, P)].

% Server to handle the producers and consumers
% Occupied == slots with sushi (slots with an item).
% No race condition bc we have a centralized server!
server(Size, PP, CC, Occupied) ->
    
    receive
        % You can consume when a slot has a sushi and another consumer isn't eating the sushi 
        {start_consume, From} when Occupied - CC > 0 ->
            From ! {ok},

            % Update server
            server(Size, PP, CC + 1, Occupied);

        {stop_consume, _From} ->
            % We finished eating a sushi. # consumers - 1, and # available sushi - 1
            server(Size, PP, CC - 1, Occupied - 1);
        
        % You can produce if there's an empty slot and another producer isn't currently trying to put a sushi in that slot
        % Occupied == # sushi in slots, PP == # producers putting sushi in slots
        {start_produce, From} when Occupied + PP < Size ->
            From ! {ok},

            % Update server
            server(Size, PP + 1, CC, Occupied);

        {stop_produce, _From} ->
            % We finished putting a sushi. # Producers - 1, and # available sushi + 1
            server(Size, PP - 1, CC, Occupied + 1)
    end.

consumer(S) ->
    % Start consuming
    S ! {start_consume, self()},

    % Get confirmation and starts consumimg
    receive
        {ok} ->
            timer:sleep(1000),
            S!{stop_consume, self()}        % Tell server you're no longer consuming
    end.

producer(S) ->
    % starts producing
    S ! {start_produce, self()},

    receive
        {ok} ->
            timer:sleep(1000),
            S!{stop_produce, self()}        % Tell server you're no longer producing cos you finished making one sushi
    end.