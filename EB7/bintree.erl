-module(bintree).
-compile(export_all).
-compile(nowarn_export_all).

% Variant type binary tree but we don't rly use that
% -type binary_tree(A) :: {empty} | {node, A, binary_tree(A), binary_tree(A)}.

% sums a binary tree
sumTree({empty}) -> 0;
sumTree({node, Number, LSubTree, RSubTree}) ->
    Number + sumTree(LSubTree) + sumTree(RSubTree).

% maps a binary tree
mapTree(_Function, {empty}) -> {empty};
mapTree(Function, {node, Number, LSubTree, RSubTree}) ->        % Applies function to left and right
    {
        node,
        Function(Number),
        mapTree(Function, LSubTree),
        mapTree(Function, RSubTree)
    }.

% Foldr but tree
foldTree(_Function, CumL, {empty}) -> CumL;             % Function takes (cumL, curr)
foldTree(Function, CumL, {node, Number, LSubTree, RSubTree}) ->
    LeftResult = foldTree(Function, CumL, LSubTree),            % First calculate everything on the left
    CurrentResult = Function(LeftResult, Number),               % Then do current one
    foldTree(Function, CurrentResult, RSubTree).            % Finally return what the right one is doing

% test file
test() ->
    BinaryTree = {
        node, 4, 
        {
            node, 3,
            {empty},
            {empty}
        },                      % This tree is              4
        {                       %                       3       2
            node, 2,            %                                   1
            {
                empty
            },
            {
                node, 1, 
                {empty},
                {empty}
            }
        }
    },
    % sumTree(BinaryTree),
    % AddOne = fun (Number) -> Number + 1 end,
    % mapTree(AddOne, BinaryTree),
    SumAll = fun (CumL, Curr) -> CumL + Curr end,
    foldTree(SumAll, 0, BinaryTree).