-module(basic).
-compile(export_all).
-compile(nowarn_export_all).

% Be mindful that vars are in caps in Erlang
mult(A, B) -> A * B.

double(A) -> 2 * A.

distance({Ax, Ay}, {Bx, By}) -> math:sqrt((Bx - Ax) * (Bx - Ax) + (By - Ay) * (By - Ay)).

my_and(A, B) -> 
    if 
        A == true, B /= false ->
            true;
        true ->
            false
    end.

my_or(A, B) -> 
    if 
        A == false, B == false ->
            false;
        true ->
            true
    end.

my_not(A)  -> 
    if 
        A == true ->
            false;
        true ->             % A bit misleading - true just means it matches everything
            true
    end.