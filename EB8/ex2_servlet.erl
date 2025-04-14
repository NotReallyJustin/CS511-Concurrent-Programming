-module(ex2_servlet).
-compile(export_all).
-compile(nowarn_export_all).

start() ->
    S = spawn(?MODULE, server, [""]),
    [spawn(?MODULE, client, [S]) || _ <- lists:seq(1, 10)].

client(S) ->
    % Spawn a servlet and then send stuff to servlet
    S ! {start, self()},
    receive
        {Servlet} ->
            ok
    end,

    % Now, send stuff to servlets
    Servlet ! {add, "h", self()},
    Servlet ! {add, "e", self()},
    Servlet ! {add, "l", self()},
    Servlet ! {add, "l", self()},
    Servlet ! {add, "o", self()},
    Servlet ! {done, self()},
    receive
        {Servlet, Str} ->
            io:format("Done: ~p ~s~n", [self(), Str])
    end.

server(String) ->
    % Spawn servlets so each client gets its own instance of the concat program
    % Basically child processes - so a client doesn't intefere with other clients
    
    receive
        {start, Child_PID} ->
            % Spawn a servlet
            % Then, send a servlet bad
            Servlet = spawn(?MODULE, server, [""]),
            Child_PID ! {Servlet},
            server(String);     % Continue server

        {add, Char, _Child_PID} ->
            % Concat the servlet instance
            server(String ++ Char);

        {done, Child_PID} ->
            % Servlet returns the child PID back to parent and stops.
            Child_PID ! {self(), String}
    end.
    