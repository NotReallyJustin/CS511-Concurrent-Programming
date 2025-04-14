-module(ex2).
-compile(export_all).
-compile(nowarn_export_all).

start() ->
    % Spawns a server and client
    S = spawn(?MODULE, server, [""]),
    [spawn(?MODULE, client, [S]) || _ <- lists:seq(1, 100000)].

client(S) ->
    S ! {start, self()},
    S ! {add, "h", self()},
    S ! {add, "e", self()},
    S ! {add, "l", self()},
    S ! {add, "l", self()},
    S ! {add, "o", self()},
    S ! {done, self()},
    receive
        {S, Str} ->
            io:format("Done: ~p ~s~n", [self(), Str])
    end.

server(String) ->
    
    % Recieve from client
    receive
        % Start creates a new string for them to concat stuff
        {start, _Client_PID} ->
            server("");
        
        % If you get a concat... concat.
        {add, Letter, _Client_PID} ->
            server(String ++ Letter);
        
        % Send string back to client after this is done
        {done, Client_PID} ->
            % Client returns its own PID with state (string)
            Client_PID ! {self(), String},
            server(String)
    end.
