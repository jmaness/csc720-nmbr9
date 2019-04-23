:- use_module(library(clpfd)).
:- use_module(library(lambda)).
:- use_module(library(when)).


% Adapted from "The Art of Prolog" (Sterling and Shapiro, p. 401.)
% for organizing game play
play(Heuristic) :-
    initialize(Position),
    write('Game = '),write(Position),
    display_game(Position),
    play(Heuristic, Position, _), !.

play(Heuristic, Game) :-
    display_game(Game),
    play(Heuristic, Game, _), !.

play(_, Position, Result) :-
    game_over(Position, Result), !, announce(Result).

play(Heuristic, Position, Result) :-
    draw_card(Position, Position1),
    choose_move(Heuristic, Position1, Move),
    move(Move, Position1, Position2),
    display_game(Position2),
    !, play(Heuristic, Position2, Result).

choose_move(Heuristic, Position, Move) :-
    findall(M, move(Position, M), Moves),
    evaluate_and_choose(Moves, Position, Heuristic, Move).

evaluate_and_choose(Moves, Position, Heuristic, Move) :-
    call(Heuristic, Moves, Position, Move).

announce(Result) :-
    write('Score is '),write(Result),nl.

% Initialize NMBR9 with a shuffled deck and an empty board
initialize(game(Deck, nil, [], board([]))) :-
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

draw_card(game([C | New_Deck], _, Revealed_Cards, Board), game(New_Deck, C, [C | Revealed_Cards], Board)).

% The game is over when all cards of the deck have been revealed.
game_over(game([], _, _, board(Tiles)), Result) :-
    score(Tiles, Result).

% Draw board
display_game(game(_, _, _, board([]))) :- writeln('').
display_game(game(_, card(_, Card_Value), _, Board)) :-
    Board = board(Tiles),
    x_min(Tiles, X_min),
    x_max(Tiles, X_max),
    y_min(Tiles, Y_min),
    y_max(Tiles, Y_max),
    z_min(Tiles, Z_min),
    z_max(Tiles, Z_max),
    write('Card = '), write(Card_Value), nl, nl,
    print_levels(Board, X_min, X_max, Y_min, Y_max, Z_min, Z_max),
    nl, nl,
    writeln("================================================").

print_levels(board(Tiles), X_min, X_max, Y_min, Y_max, Z, Z) :-
    level_tiles(Tiles, Z, Level_tiles),
    write('Level = '),writeln(Z),nl,nl,
    print_level(Level_tiles, X_min, X_max, Y_min, Y_max), !.

print_levels(Board, X_min, X_max, Y_min, Y_max, Z_min, Z_max) :-
    Board = board(Tiles), 
    write('Level = '),writeln(Z_max),nl,nl,
    level_tiles(Tiles, Z_max, Level_tiles),
    print_level(Level_tiles, X_min, X_max, Y_min, Y_max),
    Z is Z_max - 1,
    print_levels(Board, X_min, X_max, Y_min, Y_max, Z_min, Z).

print_level(Level_tiles, Board_x_min, Board_x_max, Y, Y) :-
    print_row(Level_tiles, Board_x_min, Board_x_max, Y),
    nl,
    writeln("------------------------------------------------"),
    !.

print_level(Level_tiles, Board_x_min, Board_x_max, Board_y_min, Board_y_max) :-
    print_row(Level_tiles, Board_x_min, Board_x_max, Board_y_max),
    nl,
    Y is Board_y_max - 1,
    print_level(Level_tiles, Board_x_min, Board_x_max, Board_y_min, Y).

print_row(Level_tiles, X, X, Y) :-
    print_cell(Level_tiles, X, Y), !.

print_row(Level_tiles, Board_x_min, Board_x_max, Y) :-
    print_cell(Level_tiles, Board_x_min, Y),
    write(" "),
    X is Board_x_min + 1,
    print_row(Level_tiles, X, Board_x_max, Y).

print_cell(Level_tiles, X, Y) :-
    (find_tile(Level_tiles, X, Y, Tile) ->
         tile_id(Tile, Tile_id),
         Value is Tile_id mod 10,
         write(Value);
     write(" ")).

find_tile([T|Ts], X, Y, Tile) :-
    tile_xy_coords(T, XY_coords),
    (memberchk([X, Y], XY_coords) ->
         Tile = T, !;
     find_tile(Ts, X, Y, Tile)).


% Greedy heuristic which selects the move that yields the
% greatest immediate score
greedy(Moves, Position, Move) :-
    maplist(\M^move_score(Position, M), Moves, Move_scores),
    foldl(\P^A^move_with_max_score(P, A), Move_scores, [], [Move, _, _]).

move_score(game(_, _, _, board(Tiles)), tileMove(Move, Adjacents), [Move, Score, Adjacents]) :-
    score([Move | Tiles], Score).

move_with_max_score([M, S, A], [], [M, S, A]) :- !.
move_with_max_score([], [M, S, A], [M, S, A]) :- !.
move_with_max_score([M1, S1, A1], [M2, S2, A2], [M, S, A]) :-
    (S1 > S2 ->
         M = M1, S = S1, A = A1 ;
     (S2 > S1 ->
          M = M2, S = S2, A = A2 ;
      (A1 >= A2 ->
           M = M1, S = S1, A = A1 ;
       M = M2, S = S2, A = A2))).



% Highest level heuristic which places a tile at the highest possible level.
% If there are more than one possible move at the highest level, use the
% greedy heuristic for the candidate moves at the highest level.
highest_level(Moves, Position, Move) :-
    maplist(\M^move_score(Position, M), Moves, Move_scores),
    foldl(\P^A^move_with_max_level(P, A), Move_scores, [], [Move, _, _]).

move_with_max_level([M, S, A], [], [M, S, A]) :- !.
move_with_max_level([], [M, S, A], [M, S, A]) :- !.
move_with_max_level([M1, S1, A1], [M2, S2, A2], [M, S, A]) :-
    tile_z(M1, Z1),
    tile_z(M2, Z2),
    (Z1 > Z2 ->
         M = M1, S = S1, A = A1 ;
     (Z2 > Z1 ->
          M = M2, S = S2, A = A2 ;
      move_with_max_score([M1, S1, A1], [M2, S2, A2], [M, S, A]))).


% Card rules
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

% Tile templates
zero([[0,0], [1,0], [2,0], [0,1], [2,1], [0,2], [2, 2], [0,3], [1,3], [2,3]]).
one([[1,0], [1,1], [1,2], [0,3], [1,3]]).
two([[0,0], [1,0], [2,0], [0,1], [1,1], [1,2], [2,2], [1,3], [2,3]]).
three([[0,0], [1,0], [2,0], [1,1], [2,1], [2,2], [0,3], [1,3], [2,3]]).
four([[1,0], [2,0], [0,1], [1,1], [2,1], [1,2], [1,3], [2,3]]).
five([[0,0], [1,0], [2,0], [2,1], [0,2], [1,2], [2,2], [0,3], [1,3], [2,3]]).
six([[0,0], [1,0], [2,0], [0,1], [1,1], [2,1], [0,2], [0,3], [1,3]]).
seven([[0,0], [0,1], [1,1], [1,2], [0,3], [1,3], [2,3]]).
eight([[0,0], [1,0], [0,1], [1,1], [1,2], [2,2], [1,3], [2,3]]).
nine([[0,0], [1,0], [0,1], [1,1], [0,2], [1,2], [2,2], [0,3], [1,3], [2,3]]).

% Tile rules
make_tile(Id, Points, tile(Id, 0, Points)).
make_tile(Id, Level, Points, tile(Id, Level, Points)).

tile_xy_coords(tile(_, _, Points), Points).
tile_xyz_coords(tile(_, Z, Points), Coords) :- maplist(\P^xyz_coords(P, Z), Points, Coords).
xyz_coords([X, Y], Z, [X, Y, Z]).

getX([X, _], X).
getY([_, Y], Y).

getXs(tile(_, _, Points), Xs) :- maplist(getX, Points, Xs).
getYs(tile(_, _, Points), Ys) :- maplist(getY, Points, Ys).

tile_id(tile(Id, _, _), Id).
tile_z(tile(_, Z, _), Z).

% Find minimum X value across a list of tuples
x_min([], 0).
x_min(Tiles, Min) :-
    maplist(\T^getXs(T), Tiles, Xs),
    flatten(Xs, Xss),
    min_list(Xss, Min).

% Find maximum X value across a list of tuples
x_max([], 0).
x_max(Tiles, Max) :-
    maplist(\T^getXs(T), Tiles, Xs),
    flatten(Xs, Xss),
    max_list(Xss, Max).

% Find minimum Y value across a list of tuples
y_min([], 0).
y_min(Tiles, Min) :-
    maplist(\T^getYs(T), Tiles, Ys),
    flatten(Ys, Yss),
    min_list(Yss, Min).

% Find maximum Y value across a list of tuples
y_max([], 0).
y_max(Tiles, Max) :-
    maplist(\T^getYs(T), Tiles, Ys),
    flatten(Ys, Yss),
    max_list(Yss, Max).

% Find minimum Z value across a list of tuples
z_min([], 0).
z_min(Tiles, Min) :-
    maplist(\T^tile_z(T), Tiles, Zs),
    min_list(Zs, Min).

% Find maximum Z value across a list of tuples
z_max([], 0).
z_max(Tiles, Max) :-
    maplist(\T^tile_z(T), Tiles, Zs),
    max_list(Zs, Max).

% Actually perform the move which updates the game state
move(Move, game(Deck, Card, Revealed_Cards, board(Tiles)), game(Deck, Card, Revealed_Cards, board([Move | Tiles]))).

% Generate moves given a game state
move(game(_, card(Card_id, Card_value), _, board([])), tileMove(Tile, 0)) :-
    card(Card_id, Card_value, Points),
    make_tile(Card_id, Points, Tile), !.

move(game(_, card(Card_id, Card_value), _, board(Tiles)), Move) :-
    card(Card_id, Card_value, Points),
    make_tile(Card_id, Points, Tile),
    move_bounds(Tiles, Bounds),
    (X_min, X_max, Y_min, Y_max, Z_min, Z_max) = Bounds,
    X in X_min..X_max,
    Y in Y_min..Y_max,
    Z in Z_min..Z_max,
    R in 0..3,
    indomain(X), indomain(Y), indomain(Z), indomain(R),
    rotate(R, Tile, Rotated_tile),
    translate(Rotated_tile, X, Y, Z, Bounds, Translated_tile),
    disjoint(Tiles, Translated_tile),
    adjacent(Translated_tile, Tiles, Move),
    label([X, Y, Z, R]).

% The move bounds are based on the x, y, z ranges of the tiles on the board plus
% the maximum x,y dimension of all possible tiles (4 units) and one additional z level
move_bounds(Tiles, Bounds) :-
    x_min(Tiles, X_min),
    x_max(Tiles, X_max),
    y_min(Tiles, Y_min),
    y_max(Tiles, Y_max),
    z_min(Tiles, Z_min),
    z_max(Tiles, Z_max),
    Next_x_min is X_min - 4,
    Next_x_max is X_max + 4,
    Next_y_min is Y_min - 4,
    Next_y_max is Y_max + 4,
    Next_z_min is Z_min,
    Next_z_max is Z_max + 1,
    Bounds = (Next_x_min, Next_x_max, Next_y_min, Next_y_max, Next_z_min, Next_z_max).


% Rotates tile T counter-clockwise N times.
rotate(0, Tile, Tile) :- !.
rotate(N, tile(Id, Z, Points), Rotated_tile) :-
    N > 0,
    N1 is N - 1,
    maplist(rotate_point, Points, Q),
    make_tile(Id, Z, Q, M),
    rotate(N1, M, Rotated_tile).

rotate_point([A,B], [C,D]) :-
    C is -1 * B,
    D is A.

% Translate a tile in three dimensions restricted by the specified bonds
translate(tile(Id, Level, Points), X, Y, Z, Bounds, tile(Id, Z1, Translated_points)) :-
    Z1 is Level + Z,
    (_, _, _, _, Z_min, Z_max) = Bounds,
    Z1 >= Z_min,
    Z1 =< Z_max,
    maplist(\P^translate_point(P, X, Y, Bounds), Points, Translated_points).

translate_point([Px, Py], X, Y, Bounds, [Px2, Py2]) :-
    (X_min, X_max, Y_min, Y_max, _, _) = Bounds,
    Px2 is Px + X,
    Py2 is Py + Y,
    Px2 >= X_min,
    Px2 =< X_max,
    Py2 >= Y_min,
    Py2 =< Y_max.

% A tile is disjoint from a set of tiles if no tile points are the same as any
% points of the set of tiles.
disjoint(Tiles, Tile) :-
    tile_z(Tile, Z),
    level_tiles(Tiles, Z, Level_tiles),
    board_tile_xy_coords(Level_tiles, LevelXY_coords),
    tile_xy_coords(Tile, TileXY_coords),
    intersection(TileXY_coords, LevelXY_coords, CommonCoords),
    length(CommonCoords, 0).

union_tile_coords(Acc, Tile, Res) :-
    tile_xyz_coords(Tile, Coords),
    union(Acc, Coords, Res).

union_tile_xy_coords(Acc, Tile, Res) :-
    tile_xy_coords(Tile, Coords),
    union(Acc, Coords, Res).

% For a valid move, a given tile must be:
%    * adjacent to one or more tiles on the same level
%    * completely supported by tiles on the preceding level
%      (i.e. all tile points are adjacent to tile points directly below)
%    * overlaps two or more tiles on the preceding level
adjacent(Tile, Tiles, Move) :-
    tile_z(Tile, Z),
    level_tiles(Tiles, Z, Level_tiles),
    adjacent_on_same_level(Tile, Level_tiles, Move),
    PrecedingLevel is Z - 1,
    level_tiles(Tiles, PrecedingLevel, PrecedingLevel_tiles),
    overlaps_preceding_level(Tile, PrecedingLevel_tiles).

adjacent_on_same_level(Tile, [], tileMove(Tile, 0)) :- !.
adjacent_on_same_level(Tile, Level_tiles, tileMove(Tile, L)) :-
    board_tile_xy_coords(Level_tiles, Level_xy_coords),
    tile_xy_coords(Tile, Tile_xy_coords),
    adjacent_coords(Tile_xy_coords, Possible_adjacent_xy_coords),
    intersection(Possible_adjacent_xy_coords, Level_xy_coords, Adjacent_xy_coords),
    length(Adjacent_xy_coords, L),
    L > 0.

adjacent_coords(Coords, Adjacent_coords) :-
    foldl(\C^A^union_adjacent_coords(A, C), Coords, [], Adjacent_coords).

union_adjacent_coords(Acc, [X, Y], Res) :-
    adjacent_points([X, Y], Points),
    union(Acc, Points, Res).

adjacent_points([X, Y], [[X1, Y], [X2, Y], [X, Y1], [X, Y2]]) :-
    X1 is X+1,
    X2 is X-1,
    Y1 is Y+1,
    Y2 is Y-1.

overlaps_preceding_level(_, []) :- !.  % A tile placed on the bottom layer does not need to overlap any other tiles.
overlaps_preceding_level(Tile, Level_tiles) :-
    tile_xy_coords(Tile, Tile_xy_coords),
    board_tile_xy_coords(Level_tiles, Level_xy_coords),
    subset(Tile_xy_coords, Level_xy_coords),
    overlaps_multiple_tiles(Tile, Level_tiles).

overlaps_multiple_tiles(Tile, Tiles) :-
    overlapped_tiles(Tile, Tiles, Overlapped_tiles),
    maplist(\T^tile_id(T), Overlapped_tiles, Ids),
    sort(Ids, Distinct_Ids),  % Removes duplicates
    length(Distinct_Ids, L),
    L > 1.

overlapped_tiles(Tile, Tiles, Covered_tiles) :-
    include(\T^(overlaps(Tile, T)), Tiles, Covered_tiles).

overlaps(Top_tile, Bottom_tile) :-
    tile_xy_coords(Top_tile, Top_tile_xy_coords),
    tile_xy_coords(Bottom_tile, Bottom_tile_xy_coords),
    intersection(Top_tile_xy_coords, Bottom_tile_xy_coords, Common_coords),
    length(Common_coords, L),
    L > 0.

board_tile_xy_coords(Tiles, XY_coords) :-
    foldl(\T^A^union_tile_xy_coords(A, T), Tiles, [], XY_coords).

level_tiles(Tiles, Z, Level_tiles) :-
    include(\T^(tile_z(T, Z)), Tiles, Level_tiles).

% Computes the total score for the given list of tiles
score(Tiles, Score) :-
    foldl(\T^A^sum_tile_score(T, A), Tiles, 0, Score).

sum_tile_score(tile(Id, Z, _), Acc, Score) :-
    Value is Id mod 10,
    Score is Acc + (Value * Z).


% Utilities for automated playing
play_n_times(N) :-
    play_n_times(0, N).

play_n_times(N, N) :- !.
play_n_times(M, N) :-
    M1 is M + 1,
    write('Game '),writeln(M1),
    play_both(),
    writeln('------------------'),
    play_n_times(M1, N).

play_both() :-
    initialize(Position),
    write('Game = '),writeln(Position),
    play_quiet(greedy, Position, _),
    play_quiet(highest_level, Position, _), !.

play_quiet(Heuristic, Position, Result) :-
    game_over(Position, Result), !, 
    write('('),write(Heuristic),write(') '),
    announce(Result).

play_quiet(Heuristic, Position, Result) :-
    draw_card(Position, Position1),
    choose_move(Heuristic, Position1, Move),
    move(Move, Position1, Position2),
    !, play_quiet(Heuristic, Position2, Result).

