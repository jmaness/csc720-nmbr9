:- use_module(library(clpfd)).
:- use_module(library(lambda)).
:- use_module(library(when)).

% Adapted from "The Art of Prolog" (Sterling and Shapiro, p. 401.)
%
play(Game) :-
    initialize(Game, Position),
    display_game(Position),
    play(Position, Result).

play(Position, Result) :-
    game_over(Position, Result), !, announce(Result).

play(Position, Result) :-
    draw_card(Position, Position1),
    choose_move(Position1, Move),
    move(Move, Position1, Position2),
    display_game(Position2),
    !, play(Position2, Result).

choose_move(Position, Move) :-
    findall(M, move(Position, M), Moves),
    heuristic(Heuristic),
    evaluate_and_choose(Moves, Position, Heuristic, Move).

evaluate_and_choose(Moves, Position, Heuristic, Move) :-
    call(Heuristic, Moves, Position, Move).


% initialize NMBR9
initialize(_, game(Deck, nil, [], board([]))) :-
    new_deck(Deck).

new_deck(D) :- random_permutation(
                  [ card(0, 0), card(10, 0),
                    card(1, 1), card(11, 1),
                    card(2, 2), card(12, 2),
                    card(3, 3), card(13, 3),
                    card(4, 4), card(14, 4),
                    card(5, 5), card(15, 5),
                    card(6, 6), card(16, 6),
                    card(7, 7), card(17, 7),
                    card(8, 8), card(18, 8),
                    card(9, 9), card(19, 9) ], D).

draw_card(game([C | NewDeck], _, RevealedCards, Board), game(NewDeck, C, [C | RevealedCards], Board)).


game_over(game([], _, _, Board), Result) :-
    score(Board, Result).

% Draw board
display_game(game(_, _, _, board([]))) :-
    writeln('').

display_game(game(_, card(_, CardValue), _, Board)) :-
    Board = board(Tiles),
    minX(Tiles, MinX),
    maxX(Tiles, MaxX),
    minY(Tiles, MinY),
    maxY(Tiles, MaxY),
    minZ(Tiles, MinZ),
    maxZ(Tiles, MaxZ),
    write('Card = '), write(CardValue), nl, nl,
    printLevels(Board, MinX, MaxX, MinY, MaxY, MinZ, MaxZ),
    nl, nl,
    writeln("================================================").

printLevels(board(Tiles), MinX, MaxX, MinY, MaxY, Z, Z) :-
    printLevel(Tiles, MinX, MaxX, MinY, MaxY), !.

printLevels(Board, MinX, MaxX, MinY, MaxY, MinZ, MaxZ) :-
    Board = board(Tiles),
    levelTiles(Tiles, MaxZ, LevelTiles),
    printLevel(LevelTiles, MinX, MaxX, MinY, MaxY),
    NewMaxZ is MaxZ - 1,
    printLevels(Board, MinX, MaxX, MinY, MaxY, MinZ, NewMaxZ).

printLevel(LevelTiles, BoardMinX, BoardMaxX, Y, Y) :-
    printRow(LevelTiles, BoardMinX, BoardMaxX, Y), !.

printLevel(LevelTiles, BoardMinX, BoardMaxX, BoardMinY, BoardMaxY) :-
    printRow(LevelTiles, BoardMinX, BoardMaxX, BoardMaxY),
    nl,
    Y is BoardMaxY - 1,
    printLevel(LevelTiles, BoardMinX, BoardMaxX, BoardMinY, Y).

printRow(LevelTiles, X, X, Y) :-
    printCell(LevelTiles, X, Y), !.

printRow(LevelTiles, BoardMinX, BoardMaxX, Y) :-
    printCell(LevelTiles, BoardMinX, Y),
    write(" "),
    X is BoardMinX + 1,
    printRow(LevelTiles, X, BoardMaxX, Y).

printCell(LevelTiles, X, Y) :-
    (findTile(LevelTiles, X, Y, Tile) ->
         getTileId(Tile, TileId),
         TileValue is TileId mod 10,
         write(TileValue);
     write(" ")).

findTile([T|Ts], X, Y, Tile) :-
    tileXYCoords(T, XYCoords),
    (memberchk([X, Y], XYCoords) ->
         Tile = T, !;
     findTile(Ts, X, Y, Tile)).


% Greedy heuristic which selects the move that yields the
% greatest immediate score
greedy(Moves, Position, Move) :-
    maplist(\M^move_score(Position, M), Moves, MoveScores),
    foldl(\P^A^move_with_max_score(P, A), MoveScores, [], [Move, _]).

move_score(game(_, _, _, board(Tiles)), Move, [Move, Score]) :-
    score([Move | Tiles], Score).

move_with_max_score([M, S], [], [M, S]).
move_with_max_score([], [M, S], [M, S]).
move_with_max_score([M1, S1], [M2, S2], [M, S]) :-
    (S1 >= S2 -> M = M1, S = S1 ; M = M2, S = S2).


% Highest level heuristic which places a tile at the highest possible level.
% If there are more than one possible move at the highest level, use the
% greedy heuristic for the candidate moves at the highest level.
highest_level(Moves, Position, Move) :-
    maplist(\M^move_score(Position, M), Moves, MoveScores),
    foldl(\P^A^move_with_max_level(P, A), MoveScores, [Move, _]).

move_with_max_level([M1, S1], [M2, S2], Move) :-
    getTileZ(M1, Z1),
    getTileZ(M2, Z2),
    (Z1 > Z2 -> Move = M1 ;
     (Z2 > Z1 -> Move = M2 ;
      move_with_max_score([M1, S1], [M2, S2], Move))).




tile(Id, Points, T) :- maplist(\P^tilePoint(P, Id), Points, T).
tilePoint([Px, Py], Id, [Id, Px, Py, 0]).

tileXYCoords(Tile, Coords) :- maplist(\P^xyCoords(P), Tile, Coords).
tileXYZCoords(Tile, Coords) :- maplist(\P^xyzCoords(P), Tile, Coords).

xyCoords([_, X, Y, _], [X, Y]).
xyzCoords([_, X, Y, Z], [X, Y, Z]).

% Tile templates
zero([[0,0], [1,0], [2,0], [0,1], [2,1], [0,2], [2, 2], [0,3], [1,3], [2,3]]).
one([[1,0],[1,1],[1,2],[0,3],[1,3]]).
two([[0,0], [1,0], [2,0], [0,1], [1,1], [1,2], [2,2], [1,3], [2,3]]).
three([[0,0], [1,0], [2,0], [1,1], [2,1], [2,2], [0.3], [1,3], [2,3]]).
four([[1,0], [2,0], [0,1], [1,1], [2,1], [1,2], [1,3], [2,3]]).
five([[0,0], [1,0], [2,0], [2,1], [0,2], [1,2], [2,2], [0,3], [1,3], [2,3]]).
six([[0,0], [1,0], [2,0], [0,1], [1,1], [2,1], [0,2], [0,3], [1,3]]).
seven([[0,0], [0,1], [1,1], [1,2], [0,3], [1,3], [2,3]]).
eight([[0,0], [1,0], [0,1], [1,1], [1,2], [2,2], [1,3], [2,3]]).
nine([[0,0], [1,0], [0,1], [1,1], [0,2], [1,2], [2,2], [0,3], [1,3], [2,3]]).

card(_, 0, Points) :- zero(Points).
card(_, 1, Points) :- one(Points).
card(_, 2, Points) :- two(Points).
card(_, 3, Points) :- three(Points).
card(_, 4, Points) :- four(Points).
card(_, 5, Points) :- five(Points).
card(_, 6, Points) :- six(Points).
card(_, 7, Points) :- seven(Points).
card(_, 8, Points) :- eight(Points).
card(_, 9, Points) :- nine(Points).

getT([T, _, _, _], T).
getX([_, X, _, _], X).
getY([_, _, Y, _], Y).
getZ([_, _, _, Z], Z).

getXs(Tile, Xs) :- maplist(getX, Tile, Xs).
getYs(Tile, Ys) :- maplist(getY, Tile, Ys).
getZs(Tile, Zs) :- maplist(getZ, Tile, Zs).

getTileId(Tile, Id) :- maplist(getT, Tile, [Id|_]).
getTileZ(Tile, Z) :- getZs(Tile, [Z|_]).

% Find minimum X value across a list of tuples
minX([], 0).
minX(Tiles, Min) :-
    maplist(\T^getXs(T), Tiles, Xs),
    flatten(Xs, Xss),
    min_list(Xss, Min).

% Find maximum X value across a list of tuples
maxX([], 0).
maxX(Tiles, Max) :-
    maplist(\T^getXs(T), Tiles, Xs),
    flatten(Xs, Xss),
    max_list(Xss, Max).

% Find minimum Y value across a list of tuples
minY([], 0).
minY(Tiles, Min) :-
    maplist(\T^getYs(T), Tiles, Ys),
    flatten(Ys, Yss),
    min_list(Yss, Min).

% Find maximum Y value across a list of tuples
%maxY([], 0).
maxY(Tiles, Max) :-
    maplist(\T^getYs(T), Tiles, Ys),
    flatten(Ys, Yss),
    max_list(Yss, Max).

% Find minimum Z value across a list of tuples
%minZ([], 0).
minZ(Tiles, Min) :-
    maplist(\T^getZs(T), Tiles, Zs),
    flatten(Zs, Zss),
    min_list(Zss, Min).

% Find maximum Z value across a list of tuples
%maxZ([], 0).
maxZ(Tiles, Max) :-
    maplist(\T^getZs(T), Tiles, Zs),
    flatten(Zs, Zss),
    max_list(Zss, Max).

% Actually perform the move which updates the game state
move(Move, game(Deck, Card, RevealedCards, board(Tiles)), game(Deck, Card, RevealedCards, board([Move | Tiles]))).

% Generate moves given a game state
move(game(_, card(CardId, CardValue), _, board([])), Move) :-
    card(CardId, CardValue, Points),
    tile(CardId, Points, Move), !.

move(game(_, card(CardId, CardValue), _, board(Tiles)), Move) :-
    card(CardId, CardValue, Points),
    tile(CardId, Points, Tile),
    nextMoveBounds(Tiles, Bounds),
    (NextMoveXmin, NextMoveXmax, NextMoveYmin, NextMoveYmax, NextMoveZmin, NextMoveZmax) = Bounds,
    X in NextMoveXmin..NextMoveXmax,
    Y in NextMoveYmin..NextMoveYmax,
    Z in NextMoveZmin..NextMoveZmax,
    R in 0..3,
    indomain(X), indomain(Y), indomain(Z), indomain(R),
    rotate(R, Tile, RotatedTile),
    translate(RotatedTile, X, Y, Z, Bounds, TranslatedTile),
    isNonintersecting(Tiles, TranslatedTile),
    adjacent(TranslatedTile, Tiles),
    Move = TranslatedTile.

nextMoveBounds(Tiles, Bounds) :-
    minX(Tiles, Xmin),
    maxX(Tiles, Xmax),
    minY(Tiles, Ymin),
    maxY(Tiles, Ymax),
    minZ(Tiles, Zmin),
    maxZ(Tiles, Zmax),
    NextMoveXmin #= Xmin - 4,
    NextMoveXmax #= Xmax + 4,
    NextMoveYmin #= Ymin - 4,
    NextMoveYmax #= Ymax + 4,
    NextMoveZmin #= Zmin,
    NextMoveZmax #= Zmax + 1,
    Bounds = (NextMoveXmin, NextMoveXmax, NextMoveYmin, NextMoveYmax, NextMoveZmin, NextMoveZmax).


% Rotates tile T counter-clockwise N times.
rotate(0, Tile, Tile) :- !.
rotate(N, Tile, RotatedTile) :-
    N #> 0,
    N1 #= N - 1,
    tileXYCoords(Tile, Coords),
    maplist(rotateCoord, Coords, Q),
    getTileId(Tile, Id),
    tile(Id, Q, M),
    rotate(N1, M, RotatedTile).


rotateCoord([A,B], [C,D]) :-
    C is -1 * B,
    D is A.

translate(Tile, X, Y, Z, NewPos) :-
    maplist(\P^translatePoint(P, X, Y, Z), Tile, NewPos).

translate(Tile, X, Y, Z, Bounds, NewPos) :-
    maplist(\P^translatePoint(P, X, Y, Z, Bounds), Tile, NewPos).

translatePoint([T, Px, Py, Pz], X, Y, Z, [T, Px2, Py2, Pz2]) :-
    Px2 #= Px + X,
    Py2 #= Py + Y,
    Pz2 #= Pz + Z.

translatePoint([T, Px, Py, Pz], X, Y, Z, Bounds, [T, Px2, Py2, Pz2]) :-
    (Xmin, Xmax, Ymin, Ymax, Zmin, Zmax) = Bounds,
    Px2 in Xmin..Xmax,
    Py2 in Ymin..Ymax,
    Pz2 in Zmin..Zmax,
    Px2 #= Px + X,
    Py2 #= Py + Y,
    Pz2 #= Pz + Z.

isNonintersecting(Tiles, Tile) :-
    boardTileCoords(Tiles, AllCoords),
    tileXYZCoords(Tile, Coords),
    forall(member(Coord, Coords), #\ tuples_in([Coord], AllCoords)).

unionTileCoords(Acc, Tile, Res) :-
    tileXYZCoords(Tile, Coords),
    union(Acc, Coords, Res).

unionTileXYCoords(Acc, Tile, Res) :-
    tileXYCoords(Tile, Coords),
    union(Acc, Coords, Res).

adjacent(Tile, Tiles) :-
    getZs(Tile, [Z|_]),
    levelTiles(Tiles, Z, LevelTiles),
    adjacentOnSameLevel(Tile, LevelTiles),
    PrecedingLevel is Z - 1,
    levelTiles(Tiles, PrecedingLevel, PrecedingLevelTiles),
    overlapsPrecedingLevel(Tile, PrecedingLevelTiles).

adjacentOnSameLevel(_, []) :- !.
adjacentOnSameLevel(Tile, LevelTiles) :-
    boardTileCoords(LevelTiles, LevelTileCoords),
    tileXYZCoords(Tile, TileCoords),
    adjacentCoords(TileCoords, PossibleAdjacentCoords),
    intersection(PossibleAdjacentCoords, LevelTileCoords, AdjacentCoords),
    length(AdjacentCoords, L),
    L #> 0.

adjacentCoords(Coords, AdjacentCoords) :-
    foldl(\C^A^unionAdjacentCoords(A, C), Coords, [], AdjacentCoords).

unionAdjacentCoords(Acc, [X, Y, Z], Res) :-
    adjacentPoints([X, Y, Z], Points),
    union(Acc, Points, Res).

adjacentPoints([X, Y, Z], [[X1, Y, Z], [X2, Y, Z], [X, Y1, Z], [X, Y2, Z]]) :-
    X1 is X+1,
    X2 is X-1,
    Y1 is Y+1,
    Y2 is Y-1.

overlapsPrecedingLevel(_, []) :- !.  % A tile placed on the bottom layer does not need to overlap any other tiles.
overlapsPrecedingLevel(Tile, LevelTiles) :-
    tileXYCoords(Tile, TileXYCoords),
    boardTileXYCoords(LevelTiles, LevelXYCoords),
    subset(TileXYCoords, LevelXYCoords),
    overlapsMultipleTiles(Tile, LevelTiles).

% helper rules
boardTileCoords(Tiles, AllCoords) :-
    foldl(\T^A^unionTileCoords(A, T), Tiles, [], AllCoords).

boardTileXYCoords(Tiles, AllXYCoords) :-
    foldl(\T^A^unionTileXYCoords(A, T), Tiles, [], AllXYCoords).

levelTiles(Tiles, Z, LevelTiles) :-
    include(\T^(getTileZ(T, Z)), Tiles, LevelTiles).

overlapsMultipleTiles(Tile, Tiles) :-
    overlappedTiles(Tile, Tiles, OverlappedTiles),
    maplist(\T^getTileId(T), OverlappedTiles, Ids),
    sort(Ids, TileIds),  % Removes duplicates
    length(TileIds, L),
    L #> 1.

overlappedTiles(Tile, Tiles, CoveredTiles) :-
    include(\T^(overlaps(Tile, T)), Tiles, CoveredTiles).

overlaps(TopTile, BottomTile) :-
    tileXYCoords(TopTile, TopTileXYCoords),
    tileXYCoords(BottomTile, BottomTileXYCoords),
    intersection(TopTileXYCoords, BottomTileXYCoords, CommonCoords),
    length(CommonCoords, L),
    L #> 0.

score(Tiles, Score) :-
    foldl(\T^A^sumTileScore(T, A), Tiles, 0, Score).

sumTileScore(Tile, Acc, TileScore) :-
    getTileId(Tile, TileId),
    TileValue is TileId mod 10,
    getTileZ(Tile, Z),
    TileScore is Acc + (TileValue * Z).
