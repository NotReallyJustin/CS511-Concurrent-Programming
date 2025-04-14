-module(ex5).
-compile(export_all).
-compile(nowarn_export_all).

% Timer program

start() ->
    % Spawn timer process. We start with no PIDs to notify
    TICK_INTERVAL_MS = 1000,
    Timer_PID = spawn(?MODULE, timer, [TICK_INTERVAL_MS, []]),

    % Now, spawn child PIDs.
    % These will be registered via list comprehension trick
    Child_PIDs = [spawn(?MODULE, client, []) || _ <- lists:seq(1, 3)],
    [Timer_PID ! {register, Child_PID} || Child_PID <- Child_PIDs],   
    
    % Spawn another PID that is not registered. This one shouldn't be printing tick stuff
    spawn(?MODULE, client, []).

client() ->
    % The client will still just process the tick and then print that it recieved the tick
    receive
        {tick} ->
            io:format("Counter: PID ~p got a tick!~n", [self()]),
            client()        % Continue the client
    end.

timer(Tick_Interval, Client_PIDs) ->
    % There's an after clause in receive.
    % Recieve... after... end.
    % After allows you to specify what to do if nothing comes in for $after seconds.

    receive
        % When you receive something, update Client_PID list
        {register, Client_PID} ->
            timer(Tick_Interval, Client_PIDs ++ [Client_PID])

    after 0 ->     
        % Do not buffer like at all. If there's nothing in the mailbox, just run the timer.
        % We'll process the new register messages next batch
        
        timer:sleep(Tick_Interval),

        % Notify all clients
        % We can use list comprehension for this! For PID in Client_PID, we just PID ! {tick}
        [PID ! {tick} || PID <- Client_PIDs],
        
        % Continue timer server
        timer(Tick_Interval, Client_PIDs)
    end.
