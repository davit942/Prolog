% Aspects of this code were inspired by the solutions to tutorial 4 posted on moodle.

% Facts defining the different rooms in the house
location(outside).
location(porch1).
location(porch2).
location(kitchen).
location(living_room).
location(corridor).
location(bedroom).
location(master_bedroom).
location(wc).

% Facts defining how each room is connected
connected(outside, porch1).
connected(porch1, kitchen).
connected(kitchen, living_room).
connected(living_room, corridor).
connected(corridor, bedroom).
connected(living_room, porch2).
connected(corridor, master_bedroom).
connected(corridor, wc).
connected(outside, porch2).


% Rule stating that if X is connected to Y, then Y must be connected to X
bidirectional(X,Y) :- 
	connected(X,Y).
bidirectional(X,Y) :- 
	connected(Y,X).

% This is a Depth First Search that is being performed.
path(O,D,P):-
    location(O),
    location(D),
    find(O,D,[O],W), 
    reverse(W,P).

path(O, _, _) :-
    \+ location(O),
    write(O), write(' is not a valid location.'),
    fail.
    
path(_, D, _) :-
    \+ location(D),
    write(D), write(' is not a valid location.'),
    fail.

find(O,D,P,[D|P]) :- 
    bidirectional(O,D).

find(O,D,Visited,Way) :-
    bidirectional(O,X),           
    X \== D,
    \+member(X,Visited),
    find(X,D,[X|Visited],Way).

% This is a bi-directional path search that combines two paths meeting at a common place D.
bipath(O1, O2, D, P) :-
    path(O1, D, P1),
    path(O2, D, P2),
    % Removing duplicate destination from lists.
    select(D,P1,P1new),
    select(D,P2,P2new),
    % Reversing second path to return the bipath is a readable manner.
    reverse(P2new,RevP2),
    append([[P1new], [D], [RevP2]], P),
    path_length(P,L).

% predicate to find the length of the path, this will help determine shortest path.
path_length(P, L) :-
    % flatten/2 is a built in predicate that gets rid of nested lists, making counting length easier.
    flatten(P, Flat),
    length(Flat, L).


% This predicate finds the shortest path among all possible bipaths.

shortest_bipath(O1, O2, D, ShortestPath) :-
    setof([L, Path], (bipath(O1, O2, D, Path), path_length(Path, L)), Paths),
    Paths = [_|_], % requires at least one element i.e. fails if empty
    min_member([Length, ShortestPath], Paths),
    P = ShortestPath.



% Facts defining the connections between locations
connected(outside, porch1, 1).
connected(porch1, kitchen, 1).
connected(kitchen, living_room, 3).
connected(living_room, corridor, 5).
connected(corridor, bedroom, 2).
connected(living_room, porch2, 5).
connected(corridor, master_bedroom, 2).
connected(corridor, wc, 2).
connected(outside, porch2, 1).


bidirectional(X,Y,Z) :- 
	connected(X,Y,Z).
bidirectional(X,Y,Z) :- 
	connected(Y,X,Z).

% C represents Cost
path(O,D,P,C):-
    location(O),
    location(D),
    find(O,D,[O],W,C),
    reverse(W,P).

path(O, _, _, _) :-
    \+ location(O),
    write(O), write(' is not a valid location.'),
    fail.

path(_, D, _, _) :-
    \+ location(D),
    write(D), write(' is not a valid location.'),
    fail.

find(O,D,P,[D|P],C) :- 
    bidirectional(O,D,C).

find(O,D,Visited,Way,C) :-
    bidirectional(O,X,Z),           
    X \== D,
    \+member(X,Visited),
    find(X,D,[X|Visited],Way,C1),
    C is Z+C1.

% Orders the paths in terms of ascending cost.
path_asc(O, D, P, Cost) :-
    setof([C, Path], path(O,D,Path,Cost), Paths),
    member([Cost, P], Paths).

% This version of bipath also takes into account cost, will only return a P if the two costs are equal.
bipath(O1, O2, D, P, C) :-
    path_asc(O1, D, P1, C1),
    path_asc(O2, D, P2, C2),
    select(D,P1,P1new),
    select(D,P2,P2new),
    reverse(P2new,RevP2),
    append([[P1new], [D], [RevP2]], P),
    % Checking to see if costs are the same, if so set C to be one of the costs.
    C1 =:= C2,
    C is C1.
