-module(ex1).
-compile(export_all).
-compile(nowarn_export_all).

% Hey baby drop it to the floor --> Add to spotify

start(N) -> 
    % Spawns a counter and N turnstile clients. This N is different from turnstile N.
    % Expected: 50 * N turns being logged
    C = spawn(?MODULE, counter_server, [0]),
    [spawn(?MODULE, turnstile, [C, 50]) || _ <- lists:seq(1, N)],
    C.

turnstile(C, N) ->
    % C is the PID of the counter, and N the number of times the turnstile turns
    if
        N == 0 ->
            % if N == 0, read current counter. This might not show up on console since child process
            C ! {read, C},

            % Try to recieve e message from the server and print that
            receive
                {Curr_Counter_Num} -> Curr_Counter_Num
            end;
        true ->
            % Else, by default, move some past turnsile. This means send server a bump message 
            C ! {bump},
            turnstile(C, N - 1)
    end.

counter_server(State) ->
    % State is the current value of the counter
    receive
        % Someone can send a read request to get the current counter number
        % From is their PID
        {read, From} ->
            From ! {State},
            counter_server(State);   % Continue the server

        % Register a turnsile. This basically means server + 1
        {bump} ->
            counter_server(State + 1) % Restart server but with +1 state
    end.

% Helper util to read current state/counter #
get_counter_num(Server_PID) ->

    % Send server a read message. self() gives you your own PID so server knows who to send msg back to
    Server_PID ! {read, self()},

    % Try to recieve e message from the server and print that
    receive
        {Curr_Counter_Num} -> Curr_Counter_Num
    end.