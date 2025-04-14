-module(barr).
-compile(export_all).
-compile(nowarn_export_all).

% Make a barrier
make(N) ->
    spawn(?MODULE, server_loop, [N, N, []]).

% server_loop(size of barrier, curr # processes yet to reach barrier, list of of PIDs of processes at barrier)
server_loop(N, 0, L) ->
    % Notify everyone they can leave
    [PID ! {ok} || PID <- L],

    % Restart cyclic barrier
    server_loop(N, N, []);

server_loop(N, M, L) ->
    % Process a client at the barrier
    % curr # process yet to reach barrier goes down by 1

    receive
        {PID, synch} ->
            server_loop(N, M - 1, L ++ [PID])
    end.

% Util/helper function for the client to notify "hey! I am at the barrier"
% Then, the recieve statement forces the client to wait at the barrier until they get an {ok}
synch(B) ->
    B ! {self(), synch},

    receive
        {ok} -> ok
    end.