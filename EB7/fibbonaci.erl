-module(fibbonaci).
-compile(export_all).
-compile(nowarn_export_all).

fibbonaci(Number) -> 
    if
        Number == 0 ->
            0;
        Number == 1 ->
            1;
        Number == 2 ->
            2;
        true ->
            fibbonaci(Number - 1) + fibbonaci(Number - 2)
    end.

% Main Fibonacci tail recursion function. But we will have to call a helper function
% @see https://www.geeksforgeeks.org/tail-recursion-fibonacci/
fibbonaci_tail(Number) ->
    fibbonaci_tail(Number, 0, 1).

fibbonaci_tail(Number, A, B) ->
    if
        Number == 0 ->
            A;
        Number == 1 ->
            B;
        true ->
            fibbonaci_tail(Number - 1, B, A + B)
    end.