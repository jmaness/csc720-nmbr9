:- use_module(library(clpfd)).
:- use_module(library(lambda)).
:- use_module(library(when)).

tile(Id, Points, T) :- maplist(\P^tilePoint(P, Id), Points, T).
tilePoint([Px, Py], Id, [Id, Px, Py, 0]).

tileXYCoords(Tile, Coords) :- maplist(\P^xyCoords(P), Tile, Coords).
tileXYZCoords(Tile, Coords) :- maplist(\P^xyzCoords(P), Tile, Coords).

xyCoords([_, X, Y, _], [X, Y]).
xyzCoords([_, X, Y, Z], [X, Y, Z]).

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

getXs(Tile, Xs) :- maplist(getX, Tile, Xs).
getYs(Tile, Ys) :- maplist(getY, Tile, Ys).
getZs(Tile, Zs) :- maplist(getZ, Tile, Zs).

getTileId(Tile, Id) :- maplist(getT, Tile, [Id|_]).
getTileZ(Tile, Z) :- getZs(Tile, [Z|_]).

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


possibleMoves(display(Tiles), Tile, TranslatedTile) :-
    nextMoveBounds(Tiles, Bounds),
    (NextMoveXmin, NextMoveXmax, NextMoveYmin, NextMoveYmax, NextMoveZmin, NextMoveZmax) = Bounds,
    X in NextMoveXmin..NextMoveXmax,
    Y in NextMoveYmin..NextMoveYmax,
    Z in NextMoveZmin..NextMoveZmax,
    R in 0..3,
    indomain(X), indomain(Y), indomain(Z), indomain(R), % this effectively makes this a generate-and-test execution :(
    rotate(R, Tile, RotatedTile),
    translate(RotatedTile, X, Y, Z, Bounds, TranslatedTile),
    isNonintersecting(Tiles, TranslatedTile),
    adjacent(TranslatedTile, Tiles),
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

moveScore(Tiles, Tile, Score) :-
    union(Tiles, [Tile], AllTiles),
    score(AllTiles, Score).

score(Tiles, Score) :-
    foldl(\T^A^sumTileScore(T, A), Tiles, 0, Score).

sumTileScore(Tile, Acc, TileScore) :-
    getTileId(Tile, TileId),
    TileValue is TileId mod 10,
    getTileZ(Tile, Z),
    TileScore is Acc + (TileValue * Z).
