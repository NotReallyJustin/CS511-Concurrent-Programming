-module(ex8).
-compile(export_all).
-compile(nowarn_export_all).

start() ->
    Server_PID = spawn(fun server/0),

    % Spawn 2 childs to play the guessing game
    [spawn(?MODULE, client, [Server_PID]) || _ <- lists:seq(1, 2)].

server() ->
    % This server just listens to requests and spawn servlets when needed
    receive
        {From, Ref, start} ->
            % Spawns a servlet that has a random number
            Servlet_PID = spawn(?MODULE, servlet, [rand:uniform(10)]),

            % Return servlet PID to client so they can guess
            From ! {Ref, Servlet_PID},

            server()     % Continue server
    end.

servlet(Target_Num) ->
    % Spawns a servlet that listens for a target number
    receive
        {Pid, Ref, Number} ->
            % If they have it, pog. If not, tell them to guess again
            if
                Number == Target_Num ->
                    Pid ! {Ref, gotIt};
                true ->
                    Pid ! {Ref, tryAgain},

                    % Continue server
                    servlet(Target_Num)
            end
    end.

client(S) ->
    % Request a servlet
    Ref = make_ref(),
    S ! {self(), Ref, start},

    receive
        {Ref, Servlet_PID} ->
            % Spawn the client servlet connection now since we have a servlet to look forwards to
            io:format("~p is spawning a new game ~n", [self()]),
            clientServlet(Servlet_PID)
    end.

clientServlet(Servlet_PID) ->
    % The client has spawned a servlet! Enter this function to start guessing!
    Guess = rand:uniform(10),
    Ref = make_ref(),
    io:format("~p is guessing ~p ~n", [self(), Guess]),

    Servlet_PID ! {self(), Ref, Guess},

    % Now listen for response
    receive
        % If we got it, we good!
        {Ref, gotIt} ->
            io:format("~p guessed correctly! ~n", [self()]),
            close;
        
        % If we need to try again, indeed we try again
        {Ref, tryAgain} ->
            clientServlet(Servlet_PID)
    end.
    