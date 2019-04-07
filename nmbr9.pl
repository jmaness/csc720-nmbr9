:- use_module(library(clpfd)).
:- use_module(library(lambda)).
:- use_module(library(when)).

tile(Id, Points, T) :- maplist(\P^tilePoint(P, Id), Points, T).
tilePoint([Px, Py], Id, [Id, Px, Py, 0]).

pointCoords([_, X, Y, Z], [X, Y, Z]).
tileCoords(Tile, Coords) :- maplist(\P^pointCoords(P),Tile,Coords).

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

getT([T, _, _, _], T).
getX([_, X, _, _], X).
getY([_, _, Y, _], Y).
getZ([_, _, _, Z], Z).

getTs(Tile, Ts) :- maplist(getT, Tile, Ts).
getXs(Tile, Xs) :- maplist(getX, Tile, Xs).
getYs(Tile, Ys) :- maplist(getY, Tile, Ys).
getZs(Tile, Zs) :- maplist(getZ, Tile, Zs).

% Find minimum X value across a list of tuples
minX(Tiles, Min) :-
    maplist(\T^getXs(T), Tiles, Xs),
    flatten(Xs, Xss),
    min_list(Xss, Min).

% Find maximum X value across a list of tuples
maxX(Tiles, Max) :-
    maplist(\T^getXs(T), Tiles, Xs),
    flatten(Xs, Xss),
    max_list(Xss, Max).

% Find minimum Y value across a list of tuples
minY(Tiles, Min) :-
    maplist(\T^getYs(T), Tiles, Ys),
    flatten(Ys, Yss),
    min_list(Yss, Min).

% Find maximum Y value across a list of tuples
maxY(Tiles, Max) :-
    maplist(\T^getYs(T), Tiles, Ys),
    flatten(Ys, Yss),
    max_list(Yss, Max).

% Find minimum Z value across a list of tuples
minZ(Tiles, Min) :-
    maplist(\T^getZs(T), Tiles, Zs),
    flatten(Zs, Zss),
    min_list(Zss, Min).

% Find maximum Z value across a list of tuples
maxZ(Tiles, Max) :-
    maplist(\T^getZs(T), Tiles, Zs),
    flatten(Zs, Zss),
    max_list(Zss, Max).


possibleMoves(display(Tiles), Tile, Move) :-
    nextMoveBounds(Tiles, Bounds),
    (NextMoveXmin, NextMoveXmax, NextMoveYmin, NextMoveYmax, NextMoveZmin, NextMoveZmax) = Bounds,
    X in NextMoveXmin..NextMoveXmax,
    Y in NextMoveYmin..NextMoveYmax,
    Z in NextMoveZmin..NextMoveZmax,
    indomain(X), indomain(Y), indomain(Z), % this effectively makes this a generate-and-test execution :(
    translate(Tile, X, Y, Z, Bounds, Move),
    isNonintersecting(Tiles, Move),
    isAdjacent(Tiles, Move),
    label([X, Y, Z]).

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
    %NextMoveZmax #= Zmax + 1,
    NextMoveZmax #= Zmax,
    Bounds = (NextMoveXmin, NextMoveXmax, NextMoveYmin, NextMoveYmax, NextMoveZmin, NextMoveZmax).

translate(Tile, X, Y, Z, Bounds, NewPos) :-
    maplist(\P^translatePoint(P, X, Y, Z, Bounds), Tile, NewPos).

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
    tileCoords(Tile, Coords),
    forall(member(Coord, Coords), #\ tuples_in([Coord], AllCoords)).

unionTileCoords(Acc, Tile, Res) :-
    tileCoords(Tile, Coords),
    union(Acc, Coords, Res).

isAdjacent(Tiles, Tile) :-
    isAdjacentOnSameLevel(Tiles, Tile),
    isAdjacentToPrecedingLevel(Tiles, Tile).

isAdjacentOnSameLevel(Tiles, Tile) :-
    boardTileCoords(Tiles, AllCoords),
    tileCoords(Tile, Coords),
    adjacentCoords(Coords, PossibleAdjacentCoords),
    intersection(PossibleAdjacentCoords, AllCoords, AdjacentCoords),
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

isAdjacentToPrecedingLevel(Tiles, Tile).

% helper rules
boardTileCoords(Tiles, AllCoords) :-
    foldl(\T^A^unionTileCoords(A, T), Tiles, [], AllCoords).

