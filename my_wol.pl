:- use_module(library(system)).


/* START OF TASK 3 */

test_strategy(0, _, _) :-
  !.

test_strategy(N, AStrategy, BStrategy) :-
	now(StartTime),
	test_strategy(N,AStrategy,BStrategy,Moves,Wins),
	now(EndTime),
	count_elements(Wins,'draw',R),
	format('Number of draws: ~w~n',[R]),
	count_elements(Wins,'b',R1),
	format('Number of wins for player 1 (blue): ~w~n',[R1]),
	count_elements(Wins,'r',R2),
        format('Number of wins for player 2 (red): ~w~n',[R2]),
	find_max_nonexh(Moves,R3),
        format('Longest (non-exhaustive) game: ~w~n',[R3]),
	find_min(Moves,R4),
        format('Shortest game: ~w~n',[R4]),
	average(Moves,R5),
        format('Average game length (including exhaustives): ~w~n',[R5]),
	R6 is (EndTime-StartTime)*1000/N,
	Acc is 1000/(sqrt(N)), 
	format('Average game time: ~w +/- ~w ms',[R6,Acc]).

test_strategy(0,_,_,[],[]).
test_strategy(N,AStrategy,BStrategy,[NumMoves|Moves],[Winner|Wins]) :-
	N>0,
	play(verbose,AStrategy,BStrategy, NumMoves, Winner),
	NewN is N-1,
	test_strategy(NewN,AStrategy,BStrategy, Moves,Wins).	


find_min([Min],Min).
find_min([H1,H2|T],M) :-
	(H1<H2;H1=H2),
	find_min([H1|T],M).
find_min([H1,H2|T],M) :-
	H1>H2,
	find_min([H2|T],M).


/* Assumes all 250 moves games are exhaustive (no situation in which smb just wins in last 250th move, which makes it 250 moves but non-exhaustive. Computer Scientists are supposed to be lazy, it's part of the job. */
find_max_nonexh([Max],Max).
find_max_nonexh([250|T],M) :-
	find_max_nonesh(T,M).
find_max_nonexh([H1,H2|T],M) :-
	(H1<H2;H1=H2),
	find_max_nonexh([H2|T],M).
find_max_nonexh([H1,H2|T],M) :-
        H1>H2,
	find_max_nonexh([H1|T],M).

sum_and_no([],0,0).
sum_and_no([H|T],NewS,NewN) :-
	sum_and_no(T,S,N),
	NewS is H+S,
	NewN is N+1.

average(L,A) :-
	sum_and_no(L,S,N),
	A is S/N.

count_elements([],_,0).
count_elements([E|T],E,NewR) :-
	!,
	count_elements(T,E,R),
	NewR is R+1.
count_elements([_|T], E, R) :-
	count_elements(T,E,R).


/* START OF TASK 4 */

bloodlust(Color, Board, NewBoard, Move) :-
	find_best_move(Color, Board, bloodlust, NewBoard, Move).

self_preservation(Color, Board, NewBoard, Move) :-
	find_best_move(Color, Board, self_preservation, NewBoard, Move).

land_grab(Color, Board, NewBoard, Move) :-
	find_best_move(Color, Board, land_grab, NewBoard, Move).


possible_moves(Alive,OtherPlayerAlive, PossMoves) :-
	findall([A,B,MA,MB],(member([A,B], Alive),
                             neighbour_position(A,B,[MA,MB]),
                             \+member([MA,MB],Alive),
                             \+member([MA,MB],OtherPlayerAlive)),
                 PossMoves).

/* decompose_board(+Color,+Board,-Alives,-OpponentsAlives) */
decompose_board('r', [B,R], R,B).
decompose_board('b', [B,R], B,R).

compose_board('r',Alives,OpponentAlives,[OpponentAlives,Alives]).
compose_board('b',Alives,OpponentAlives,[Alives,OpponentAlives]).

/* Returns best move for the given Strategy. */
find_maximizing_move([],_,_,_,'u','u').
find_maximizing_move([Move|Moves], Color, Board, Strategy, BestMove, BestMoveGoal) :-
	decompose_board(Color,Board,Alives,OpponentAlives),
	alter_board(Move,Alives,NewAlives),
	compose_board(Color,NewAlives,OpponentAlives,NewBoard),
	next_generation(NewBoard,NewGeneratedBoard),
	calculate_score(Color, NewGeneratedBoard,Strategy,MoveGoal),
	find_maximizing_move(Moves,Color,Board,Strategy, OldBestMove, OldBestMoveGoal),
	((OldBestMoveGoal=='u';OldBestMoveGoal<MoveGoal) ->
		(BestMove=Move, BestMoveGoal=MoveGoal) ;
                (BestMove=OldBestMove,BestMoveGoal=OldBestMoveGoal)
	).

/* Score is the number of opponent's pieces on the board. */
calculate_score(Color, Board, bloodlust, Score) :-
	decompose_board(Color,Board,_,OpponentsAlive),
	length(OpponentsAlive, OpponentNodes),
	Score is -OpponentNodes.

/* Score is the number of player's pieces on the board. */
calculate_score(Color, Board, self_preservation, Score) :-
	decompose_board(Color, Board, Alives, _),
	length(Alives, Score).

/* Score is the number of player's pieces - number of opponent's pieces. */
calculate_score(Color, Board, land_grab, Score) :-
	decompose_board(Color, Board, Alives, OpponentsAlive),
	length(OpponentsAlive, OpponentNodes),
	length(Alives, AliveNodes),
	Score is AliveNodes-OpponentNodes.

/* Given Color and Board find_best_move finds best move according to given Strategy
   and returns this Move and board configuration. */
find_best_move(Color,Board,Strategy, NewBoard, Move) :-
	decompose_board(Color,Board,Alive,OpponentsAlive),
	possible_moves(Alive, OpponentsAlive, Moves),
	find_maximizing_move(Moves, Color, Board, Strategy, Move, _),
       	alter_board(Move, Alive, NewAlive),
	compose_board(Color, NewAlive, OpponentsAlive, NewBoard).
