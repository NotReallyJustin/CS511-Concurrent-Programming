-module(ex9).
-compile(export_all).
-compile(nowarn_export_all).

start() ->
    % Spawn nodes w/ neighbors and tokens
    % Param: list of neighbors, tokens list
    Node1 = spawn(?MODULE, node, [[], [1, 2]]),
    Node2 = spawn(?MODULE, node, [[Node1], [3, 4]]),
    Node5 = spawn(?MODULE, node, [[], [5, 6]]),
    Node4 = spawn(?MODULE, node, [[Node5], [7, 8]]),
    Node3 = spawn(?MODULE, node, [[Node2, Node4], [9, 10]]),

    % Spawns a timer with all the nodes
    spawn(?MODULE, timer, [[Node1, Node2, Node3, Node4, Node5]]).

node(Neighbor_PIDs, Token_List) ->
    % Listen for timer tick.

    receive
        {tick} ->
            % When you get a tick, send a copy of token list to every neighbor process
            [PID ! {tokenlist, Token_List} || PID <- Neighbor_PIDs],

            % continue node
            node(Neighbor_PIDs, Token_List);

        {tokenlist, Token_List2} ->
            % When you get a token list, merge it
            io:format("PID ~p got a token list! New list: ~p ~n", [self(), Token_List ++ Token_List2]),
            node(Neighbor_PIDs, Token_List ++ Token_List2)
    end.

timer(Node_PIDs) ->
    % Timer!

    % Every $Tick_Interval, notify all Node_PIDs with {tick}
    timer:sleep(2000),

    % Notify all clients
    % We can use list comprehension for this! For PID in Client_PID, we just PID ! {tick}
    io:format("Tick!!! ~n", []),
    [PID ! {tick} || PID <- Node_PIDs],

    % Continue timer
    timer(Node_PIDs).