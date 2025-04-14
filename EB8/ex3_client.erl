-module(ex3_client).
-compile(export_all).
-compile(nowarn_export_all).

start() ->
    Server_PID = spawn(?MODULE, server, [0]),
    [spawn(?MODULE, client, [Server_PID]) || _ <- lists:seq(1, 10)].

% Client to increment counter
client(Server_PID) ->

    % Tell server to increment counter
    Server_PID ! {continue},

    % Get PID from server
    Server_PID ! {counter, self()},
    receive
        {Counter} ->
            io:format("Counter: ~p~n", [Counter])
    end.

% Server to count and send back count
server(Counter) ->

    receive
        {continue} ->
            % Update server but with counter increased by 1
            server(Counter + 1);

        {counter, Client_PID} ->
            % Send counter back to client and continue server
            Client_PID ! {Counter},
            server(Counter)
    end.