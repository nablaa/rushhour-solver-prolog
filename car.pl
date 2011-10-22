% is the car vertical
is_vertical(car(_, Start, End)) :-
	(X0, _) = Start,
	(X1, _) = End,
	X0 = X1.

% is the car horizontal
is_horizontal(car(_, Start, End)) :-
	(_, Y0) = Start,
	(_, Y1) = End,
	Y0 = Y1.

% is the given cell inside the given car
is_within_car(car(_, Start, End), (X, Y)) :-
	is_vertical(car(_, Start, End)),
	(X0, Y0) = Start,
	(_, Y1) = End,
	X0 = X,
	Y >= Y0,
	Y =< Y1.
is_within_car(car(_, Start, End), (X, Y)) :-
	is_horizontal(car(_, Start, End)),
	(X0, Y0) = Start,
	(X1, _) = End,
	Y0 = Y,
	X >= X0,
	X =< X1.

% is the given cell inside any car
is_car_cell(Cell, [Car|_]) :-
	is_within_car(Car, Cell), !.
is_car_cell(Cell, [_|Cars]) :- is_car_cell(Cell, Cars).

% gets the car under the given cell
get_car(Cell, [Car|_], Car) :-
	is_within_car(Car, Cell), !.
get_car(Cell, [_|Cars], Car) :- get_car(Cell, Cars, Car).

% is the given cell inside the game board
is_inside_board((X, Y), board(Width, Height, _, _)) :-
	X >= 0,
	X < Width,
	Y >= 0,
	Y < Height.

% makes move and returns new board with the move applied
move(Board, Car, Dir, board(Width, Height, Exit, SortedCars)) :-
	can_car_move(Car, Dir),
	board(Width, Height, Exit, Cars) = Board,
	member(Car, Cars),
	car(Color, (X0, Y0), (X1, Y1)) = Car,
	calc_new_coordinate((X0, Y0), Dir, (X0_, Y0_)),
	calc_new_coordinate((X1, Y1), Dir, (X1_, Y1_)),
	can_move(Board, Dir, (X0_, Y0_), (X1_, Y1_)),
	NewCar = car(Color, (X0_, Y0_), (X1_, Y1_)),
	delete(Cars, Car, TempCars),
	NewCars = [NewCar|TempCars],
	sort(NewCars, SortedCars).

% is the given move legal
is_legal_move(Board, Car, Dir) :-
	can_car_move(Car, Dir),
	board(_, _, _, Cars) = Board,
	member(Car, Cars),
	car(_, (X0, Y0), (X1, Y1)) = Car,
	calc_new_coordinate((X0, Y0), Dir, (X0_, Y0_)),
	calc_new_coordinate((X1, Y1), Dir, (X1_, Y1_)),
	can_move(Board, Dir, (X0_, Y0_), (X1_, Y1_)).

% can a car in the given point be moved to the given direction
can_move(Board, Dir, Start, _) :-
	(Dir = up; Dir = left),
	is_inside_board(Start, Board),
	board(_, _, _, Cars) = Board,
	not(is_car_cell(Start, Cars)).
can_move(Board, Dir, _, End) :-
	(Dir = down; Dir = right),
	is_inside_board(End, Board),
	board(_, _, _, Cars) = Board,
	not(is_car_cell(End, Cars)).

% can the given move to the given direction
can_car_move(Car, up) :-
	is_vertical(Car).
can_car_move(Car, down) :-
	is_vertical(Car).
can_car_move(Car, left) :-
	is_horizontal(Car).
can_car_move(Car, right) :-
	is_horizontal(Car).

% calculates new coordinates for the point moving to the given direction
calc_new_coordinate((X0, Y0), up, (X1, Y1)) :-
	X1 is X0,
	Y1 is Y0 - 1.
calc_new_coordinate((X0, Y0), down, (X1, Y1)) :-
	X1 is X0,
	Y1 is Y0 + 1.
calc_new_coordinate((X0, Y0), left, (X1, Y1)) :-
	X1 is X0 - 1,
	Y1 is Y0.
calc_new_coordinate((X0, Y0), right, (X1, Y1)) :-
	X1 is X0 + 1,
	Y1 is Y0.

% is the game finished (i.e. solved); no cars are blocking the target car
finished(Board) :- heuristic2(Board, Value), Value is 1.

% prints the board
print_board(board(Width, Height, Exit, Cars)) :-
	print_board(board(Width, Height, Exit, Cars), 0).
print_board(board(_, Height, _, _), Row) :-
	Row = Height,
	nl.

print_board(board(Width, Height, Exit, Cars), Row) :-
	Row < Height,
	print_row(board(Width, Height, Exit, Cars), Row),
	R is Row + 1,
	print_board(board(Width, Height, Exit, Cars), R).

print_row(board(Width, Height, Exit, Cars), Row) :-
	print_row(board(Width, Height, Exit, Cars), Row, 0).
print_row(board(Width, _, _, _), _, Column) :-
	Column = Width,
	nl.

print_row(board(Width, Height, Exit, Cars), Row, Column) :-
	Column < Width,
	print_cell((Column, Row), Cars, Exit),
	C is Column + 1,
	print_row(board(Width, Height, Exit, Cars), Row, C).

print_cell(Cell, Cars, _) :-
	get_car(Cell, Cars, car(Color, _, _)),
	write(Color), !.
print_cell((X, Y), _, (X, Y)) :-
	put(69), !.
print_cell(_, _, _) :-
	put(46).

% get a car with the given color
get_car_from_color(Board, Color, Car) :-
	Board = board(_, _, _, Cars),
	member(Car, Cars),
	Car = car(Color, _, _).

% generates all moves for the given car
generate_moves(Board, Car, Dirs) :-
	findall(Dir, is_legal_move(Board, Car, Dir), Dirs).

% generates all moves for every car
generate_all_moves(board(_, _, _, []), []).
generate_all_moves(Board, [Move|Moves]) :-
	board(W, H, E, [Car|Cars]) = Board,
	generate_moves(Board, Car, Dirs),
	Move = (Car, Dirs),
	generate_all_moves(board(W, H, E, Cars), Moves).

% DFS solver for the puzzle. Returns path of "moves"
solve_dfs(Board, Path, Path) :- finished(Board).
solve_dfs(Board, Acc, Path) :-
	move(Board, _, _, NewBoard),
	not(member(NewBoard, Acc)),
	solve_dfs(NewBoard, [NewBoard|Acc], Path).

print_boards([]).
print_boards([Board|Boards]) :-
	print_board(Board),
	print_boards(Boards).

solve_dfs_print(board(W, H, E, Cars)) :-
	sort(Cars, SortedCars),
	write('Problem:'), nl,
	print_board(board(W, H, E, SortedCars)),
	solve_dfs(board(W, H, E, SortedCars), [], P),
	reverse(P, Path),
	write('Solution:'), nl,
	print_boards(Path),
	write('End'), nl.

% A* solver for the puzzle. Returns path of "moves"
solve_astar(Board, Path) :- astar([path(0, 0, [Board])], Path).

astar([path(_, _, [Board|Path])|_], [Board|Path]) :- finished(Board).
astar([Minimal|Rest], P) :-
	findall(X, expand(Minimal, X), NewNodes),
	merge(NewNodes, Rest, NewFringe),
	astar(NewFringe, P).

expand(path(G0, _, [Minimal|Path]), path(G, F, [New, Minimal|Path])) :-
	move(Minimal, _, _, New),
	not(member(New, Path)),
	heuristic(New, H),
	G is G0 + 1,
	F is G + H.

merge([], Ys, Ys).
merge([X|Xs], Ys, Zs) :-
	insert(X, Ys, Ws),
	merge(Xs, Ws, Zs).

insert(X, [], [X]).
insert(path(G1, F1, X), [path(G2, F2, Y)|Ys], [path(G1, F1, X), path(G2, F2, Y)|Ys]) :-
	F1 < F2, !.
insert(X, [Y|Ys], [Y|Zs]) :-
	insert(X, Ys, Zs).

solve_astar_print(board(W, H, E, Cars)) :-
	sort(Cars, SortedCars),
	write('Problem:'), nl,
	print_board(board(W, H, E, SortedCars)),
	solve_astar(board(W, H, E, SortedCars), P),
	reverse(P, Path),
	write('Solution:'), nl,
	print_boards(Path),
	write('End'), nl.

heuristic(Board, Value) :- heuristic2(Board, Value).

% heuristic1: the distance between the target car and the parking lot exit
heuristic1(board(_, _, Exit, Cars), Value) :-
	member(car(x, Start, End), Cars),
	dist(Start, Exit, D1),
	dist(End, Exit, D2),
	min(D1, D2, Value).

% heuristic2: the number of cars blocking the target car
heuristic2(board(_, _, Exit, Cars), Value) :-
	member(car(x, Start, End), Cars),
	nearest_cell(Start, End, Exit, Cell),
	cells_in_line(Cell, Exit, Line),
	count_occupied_cells(Cars, Line, Value).

% heuristic3: heuristic1 & heuristic2 combined
heuristic3(board(_, _, Exit, Cars), Value) :-
	member(car(x, Start, End), Cars),
	dist(Start, Exit, D1),
	dist(End, Exit, D2),
	nearest_cell(Start, End, Exit, Cell),
	cells_in_line(Cell, Exit, Line),
	count_occupied_cells(Cars, Line, Occupied),
	min(D1, D2, Val),
	Value is Val + Occupied.

% counts the number of occupied cells
count_occupied_cells(Cars, Cells, Value) :- count_occupied_cells(Cars, Cells, 0, Value).
count_occupied_cells(_, [], Value, Value).
count_occupied_cells(Cars, [Cell|Cells], Acc, Value) :-
	is_car_cell(Cell, Cars),
	Acc1 is Acc + 1,
	count_occupied_cells(Cars, Cells, Acc1, Value), !.
count_occupied_cells(Cars, [_|Cells], Acc, Value) :-
	count_occupied_cells(Cars, Cells, Acc, Value).

cells_in_line((X, Y), (X, Y), [(X, Y)]).
cells_in_line((X1, Y), (X2, Y), [(X1, Y)|Line]) :-
	incr_cell(X1, X2, X),
	cells_in_line((X, Y), (X2, Y), Line).

cells_in_line((X, Y1), (X, Y2), [(X, Y1)|Line]) :-
	incr_cell(Y1, Y2, Y),
	cells_in_line((X, Y), (X, Y2), Line).

incr_cell(X1, X2, X) :-
	X1 < X2,
	X is X1 + 1.
incr_cell(X1, X2, X) :-
	X1 > X2,
	X is X1 - 1.

nearest_cell(Cell1, Cell2, Exit, Cell1) :-
	dist(Cell1, Exit, D1),
	dist(Cell2, Exit, D2),
	D1 < D2, !.
nearest_cell(_, Cell2, _, Cell2).

dist((X0, Y0), (X1, Y1), D) :-
	X2 is X1 - X0,
	Y2 is Y1 - Y0,
	abs(X2, X),
	abs(Y2, Y),
	max(X, Y, D).

max(X, Y, X) :- X >= Y.
max(X, Y, Y) :- Y > X.

min(X, Y, X) :- X =< Y.
min(X, Y, Y) :- Y < X.

