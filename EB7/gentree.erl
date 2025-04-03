-module(gentree).
-compile(export_all).
-compile(nowarn_export_all).

% Just a normal tree now :)
mapGTree(_Function, {empty}) -> {empty};
mapGTree(Function, {node, Number, AllTrees}) ->   
    % Runs function and returns the tree array on a tree

    % Partial function application kind of
    RunFunctionAndReturn = fun (Subtree) -> mapGTree(Function, Subtree) end,
    {
        node,
        Function(Number),
        lists:map(RunFunctionAndReturn, AllTrees)
    }.

% Now, fold tree
foldGTree(_Function, CumL, {empty}) -> CumL;
foldGTree(Function, CumL, {node, Number, AllTrees}) -> 
    % We might just do left, right, center

    % Partial function application
    % fold is (curr, CumL)
    RunFunctionAndReturn = fun (Subtree, CumL2) -> foldGTree(Function, CumL2, Subtree) end,
    LeftRightSum = lists:foldr(RunFunctionAndReturn, CumL, AllTrees),
    Function(LeftRightSum, Number).

%  test file
test() ->
    BinaryTree = {
        node, 4, 
        [
            {
                node, 3,
                [{empty}, {empty}]
            },                      % This tree is              4
            {                       %                       3       2
                node, 2,            %                                   1
                [
                    {
                        empty
                    },
                    {
                        node, 1,
                        [{empty}, {empty}] 
                    }
                ]
            }
        ]
    },
    % AddOne = fun (Number) -> Number + 1 end,
    % mapGTree(AddOne, BinaryTree).
    SumAll = fun (CumL, Curr) -> CumL + Curr end,
    foldGTree(SumAll, 0, BinaryTree).