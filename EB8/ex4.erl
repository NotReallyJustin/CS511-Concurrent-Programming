-module(ex4).
-compile(export_all).
-compile(nowarn_export_all).

% Timer program

start() ->
    % Spawns child processes and timer process.
    % The goal is for the timer process to send ticks to all child processes

    Child_PIDs = [spawn(?MODULE, client, []) || _ <- lists:seq(1, 4)],
    
    % Spawn timer
    TICK_INTERVAL_MS = 1000,
    spawn(?MODULE, timer, [TICK_INTERVAL_MS, Child_PIDs]).

client() ->
    % The client will just process the tick and then print that it recieved the tick
    receive
        {tick} ->
            io:format("Counter: PID ~p got a tick!~n", [self()]),
            client()        % Continue the client
    end.

timer(Tick_Interval, Client_PIDs) ->
    % Every $Tick_Interval, notify all Client_PIDs with {tick}
    timer:sleep(Tick_Interval),

    % Notify all clients
    % We can use list comprehension for this! For PID in Client_PID, we just PID ! {tick}
    [PID ! {tick} || PID <- Client_PIDs],

    % Continue timer
    timer(Tick_Interval, Client_PIDs).