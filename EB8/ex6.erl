-module(ex6).
-compile(export_all).
-compile(nowarn_export_all).

% Helper function - Determine if something is prime

isPrime(Number) ->
    isPrime_Helper(Number, 2).

isPrime_Helper(Number, Divider) ->

    if
        Number < 2 -> 
            false;

        (Divider * Divider) > Number ->
            true;

        Number rem Divider == 0 ->
            false;

        true ->
            isPrime_Helper(Number, Divider + 1)
    end.

% Create a client and server that does prime & prime helper

prime_server() ->
    % The server will get a number and just return if it is prime

    receive
        {Number, Client_PID} ->
            Client_PID ! {isPrime(Number)},
            prime_server()              % Continue the server
    end.

client(Server_PID, Number) ->
    % The client will just send server a number to see if it's prime. Then, it'll print the result
    Server_PID ! {Number, self()},

    % Wait for response
    receive
        {Is_Prime} ->
            io:format("Is prime for ~p: ~p ~n", [Number, Is_Prime])
    end.

% Start client and server
start() ->
    Server_PID = spawn(?MODULE, prime_server, []),
    spawn(?MODULE, client, [Server_PID, 17]).