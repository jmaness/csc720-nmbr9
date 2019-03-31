:- use_module(library(clpfd)).

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

display(minX, maxX, minY, maxY, minZ, maxZ, placements).

% Find minimum X value across a list of tuples
tupleXmin([[X, _, _]|Ps], Min) :-
    tupleXmin(Ps, X, Min).

tupleXmin([], Min, Min).
tupleXmin([ [X, _, _] | Ps ], Min0, Min) :-
    Min1 is min(X, Min0),
    tupleXmin(Ps, Min1, Min).

% Find maximum X value across a list of tuples
tupleXmax([[X, _, _]|Ps], Max) :-
    tupleXmax(Ps, X, Max).

tupleXmax([], Max, Max).
tupleXmax([ [X, _, _] | Ps ], Max0, Max) :-
    Max1 is max(X, Max0),
    tupleXmax(Ps, Max1, Max).

% Find minimum Y value across a list of tuples
tupleYmin([[_, Y, _]|Ps], Min) :-
    tupleYmin(Ps, Y, Min).

tupleYmin([], Min, Min).
tupleYmin([ [_, Y, _] | Ps ], Min0, Min) :-
    Min1 is min(Y, Min0),
    tupleXmin(Ps, Min1, Min).

% Find maximum Y value across a list of tuples
tupleYmax([[_, Y, _]|Ps], Max) :-
    tupleYmax(Ps, Y, Max).

tupleYmax([], Max, Max).
tupleYmax([ [_, Y, _] | Ps ], Max0, Max) :-
    Max1 is max(Y, Max0),
    tupleXmax(Ps, Max1, Max).



possibleMoves(display(Xmin, Xmax, Ymin, Ymax, Zmin, Zmax, Placements), [X, Y, Z]) :-
    X in Xmin..Xmax,
    Y in Ymin..Ymax,
    Z in Zmin..Zmax,
    #\ tuples_in([[X, Y, Z]], Placements),
    labeling([min(X), min(Y), min(Z)], [X, Y, Z]).

