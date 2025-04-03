-module(list).
-compile(export_all).
-compile(nowarn_export_all).

% Done without list functions
sum([]) -> 0;           % Pattern matching requires matching against empty list
sum([Head | Tail]) -> 
    Head + sum(Tail).

% Done w/ list functions
sum_with_list(List) ->
    lists:sum(List).

maximum([OneElement]) -> OneElement;
maximum([Head | Tail]) -> max(Head, maximum(Tail)).
    
maximum_with_list(List) ->
    lists:max(List).

% Zipping a list just means {list1_item, list2_item}
zip([], []) -> [];
zip([], [_Head | _Tail]) -> throw("List mismatch");
zip([_Head | _Tail], []) -> throw("List mismatch");
zip([List1_Head | List1_Tail], [List2_Head | List2_Tail]) ->
    [{List1_Head, List2_Head} | zip(List1_Tail, List2_Tail)]. % This is [zip, ...zipped array returned later by stuff]

zip_with_list(List1, List2) -> lists:zip(List1, List2).

append([], List2) when is_list(List2) -> List2;     % Dynamic type checking
append([Head | Tail], Item) -> [Head | append(Tail, Item)];
append(_Boo, _Baz) -> throw("One of the stuff isn't a list").  % Error checking. This is a catch all case

append_with_list(List1, List2) -> lists:append(List1, List2).

reverse([OneElement]) -> [OneElement];  
reverse([Head | Tail]) ->
    reverse(Tail) ++ [Head].      % I'd say this is append. Ngl feels like this is a list merge

reverse_with_list(List) -> lists:reverse(List).

evenL([]) -> [];
evenL([Head | Tail]) ->
    if 
        Head rem 2 == 0 -> 
            [Head | evenL(Tail)];
        true ->             % Else case
            evenL(Tail) % Don't wrap in array bc this is already one. You're not spread syntaxing this
    end.

evenL_with_filter(List) ->
    IsEven = fun (Number) -> Number rem 2 == 0 end,     % Function objs must be declared in a function cos global var bad
    lists:filter(IsEven, List).                        % Then you terminate them with a ,

% Retrieves the first $NumToTake elements in a list
take(0, _List) -> [];            % you can pattern match with a straight up number
take(NumToTake, [Head | Tail]) -> [Head | take(NumToTake - 1, Tail)].

take_with_list(NumToTake, List) -> lists:sublist(List, NumToTake).      % Wow sublist is a function

% Gets rid of the first $NumToDrop elements in a list
drop(0, List) -> List;
drop(NumToDrop, [_Head | Tail]) -> drop(NumToDrop - 1, Tail).

% Just like in JS, you can split the list
drop_with_list(NumToDrop, List) -> 
    {_Blah, TrimmedList} = lists:split(NumToDrop, List),
    TrimmedList.

% Map operations on lists
map(_Function, []) -> [];
map(Function, [Head | Tail]) -> [Function(Head) | map(Function, Tail)].

% Filter on lists
filter(_Function, []) -> [];
filter(Function, [Head | Tail]) ->
    % If statements can't do boolean. We must use case
    case Function(Head) of
        true -> [Head | filter(Function, Tail)];
        false -> filter(Function, Tail)
    end.

% Fold on lists
fold(_Function, CumL, []) -> CumL;
fold(Function, CumL, [Head | Tail]) ->          % Function takes (CumL, Curr) like JS
    fold(Function, Function(CumL, Head), Tail).

fold_with_list(Function, CumL, List) -> lists:foldr(Function, CumL, List).

% Test map/filter/etc.
test_map() ->
    AddOne = fun (Number) -> Number + 1 end,
    map(AddOne, [1, 2, 3, 4, 5]).

test_filter() ->
    IsEven = fun (Number) -> Number rem 2 == 0 end,
    filter(IsEven, [1, 2, 3, 4, 5, 6]).

test_fold() ->
    Add = fun(CumL, Curr) -> CumL + Curr end,
    fold(Add, 0, [1, 2, 3, 4, 5]).

test_fold2() ->
    Add = fun(CumL, Curr) -> CumL + Curr end,
    fold_with_list(Add, 0, [1, 2, 3, 4, 5]).