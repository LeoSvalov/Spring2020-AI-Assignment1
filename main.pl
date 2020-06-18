:- use_module(library(aggregate)).
:- use_module(library(dif)).
:- use_module(library(random)).
:- use_module(library(lists)).
/*_________________________________Defining of all dynamic predicates_________________________________*/
:- dynamic current/1.
:- dynamic visited/1.
:- dynamic orcs/1.
:- dynamic touchdown/1.
:- dynamic pass/4.
:- dynamic was_pass/1.
:- dynamic min_path/1.
:- dynamic min_leght/1.
:- dynamic temp_path/1.
:- dynamic rank/1.
:- dynamic auxiliary_direction/1.
:- dynamic counter/1.
:- dynamic path/1.

/*_________________________________Declaration of predicates itself_________________________________*/


path().
	/*The predicate that is used for storing solution path.*/
	
counter([]).
 	/* Used as auxiliary predicate for defining heuristic funtion in 3rd method. */
rank([-1,-1,-1]).
	/* Format - rank([X,Y,Rank of the cell]). Used as predicate for keeping track of the rank of the particullar cell(heuristic funtion). */

min_path([-1,-1,-1]).
min_leght(11111111).
temp_path([]).
	/* These predicates are needed for determing the shortest path of the generated ones in random method. 
	   'min_leght(some number)' consists minimal lenght of the path,
	   'min_path([X,Y,Condition of the cell])', temp_path has the same format, is used for swapping whether it has the shortest lenght.
	*/

was_pass(0).
	/* The predicate determines whether pass have not attemtped yet (will consists 0) or not (any other value) during the round. */

current([[]]).
	/* The predicate consists the current cell of the player, format - current([X,Y]). */

touchdown([]).
	/* The predicate consists the touchdown cell, format - touchdown([X,Y]). */

visited([-1,-1]).
	/* The predicate consists cells that have been already visited. Used in backtracking method. */

auxiliary_direction([]).
	/* Additional predicate for countion rank for heuristic function. */


/*_________________________________Defining of rules. All the stuff_________________________________*/

/* The rule that determines whether the cell is angle of the map or it is not. */
at_angle(X,Y):- 
	(compare(=,X,9),compare(=,Y,9) ->
		true,!
	;
		(compare(=,X,0),compare(=,Y,0) ->
			true,!
		;
			(compare(=,X,9),compare(=,Y,0) ->
				true,!
			;
				(compare(=,X,0),compare(=,Y,9) ->
					true,!
				;
					false,!
				)
			)
		)	
	).

/* The rule that determines whether the cell is near end of the map or it is not. */
near_border(X,Y):-
	(compare(=,X,0) ->
		true,!
	;
		(compare(=,Y,0) ->
			true,!
		;
			(compare(=,Y,9) ->
				true,!
			;
				(compare(=,X,9) ->
					true,!
				;
					false,!
				)
			)
		)
	).

/* These 3 rules draws the map of the game and also set initiate rank values of each cell for 3rd heuristic method (what rank means check out in the report).  */
draw3(J,I):- 
	(at_angle(J,I)  ->
		assert(rank([J,I,2])),!
	;
		(near_border(J,I) ->
			assert(rank([J,I,3])),!
		;
			assert(rank([J,I,4])),!
		)
	),
	coordinate(I,J,Condition),
	is(S,+(I,J)),
	(S =:= 0 -> 
		format(" S"),!
	;
		(compare(=,Condition,[free]) ->
			(compare(=,I,9) ->	
				format(" _\n"),!
			;
				format(" _"),!
			)
		;
			(compare(=,Condition,[orc]) ->	
				(compare(=,I,9) ->		
					format(" O\n"),!
				;
					format(" O"),!
				)
			;
			 	(compare(=,Condition,[human]) ->
					(compare(=,I,9) ->	
						format(" H\n"),!
					;
						format(" H"),!
					)
				;
					(compare(=,Condition,[touchdown]) ->	
						(compare(=,I,9) ->	
							format(" T\n"),
							assert(touchdown([I,J])),!
						;
							format(" T"),
							assert(touchdown([I,J])),!
						)
					;
						format("")
					)
				)
			)
		)
	).
draw2(J):-
	List= [0,1,2,3,4,5,6,7,8,9],
	foreach(member(I,List), draw3(J,I)).
draw_map :-
	List = [0,1,2,3,4,5,6,7,8,9],
	foreach(member(J,List), draw2(J)),
	format("\n").


/* _________________________________1.Random method rules._________________________________________

 Direction's values (for moves and passes):
	1 - go to the left
	2 - go up
	3 - go to the right
	4 - go down
	5 - go left-up
	6 - go right-up
	7 - go right-down
	8 - go left-down
*/

/* The rule determines whether the cell is out of the map or not. */
out_of_map(X,Y):- compare(>,X,9);compare(>,Y,9);compare(<,X,0);compare(<,Y,0).

/* The rule determines whether the cell is valid for continuing passing (it is not on orc --> fail and it is not on human --> end of the pass). */
valid_pos(Condition):- compare(=,Condition,[free]);compare(=,Condition,[touchdown]).

/* These 2 rules proceed move action for random method with given random direction, if one is failed(moved to orc or out of map), it generates another random direction and tries again. */
moving_random(Direction,X,Y):-
	(compare(=,Direction,1)  ->
		is(Xnew,X-1),
		coordinate(Xnew,Y,ConditionLeft),
		(ConditionLeft \== [orc] ->
			assert(current([Xnew,Y])),
			(ConditionLeft \== [human] ->
				assertz(path([Xnew,Y,"M"])),!
			;
				format(""),!
			),
			%% format("go to the left, [~w, ~w]\n",[Xnew,Y]),
			true,!
		)	
	;
		(compare(=,Direction,2)  ->
			is(Ynew,Y+1),
			coordinate(X,Ynew,ConditionUp),
			(ConditionUp \== [orc] ->
				assert(current([X,Ynew])),
				(ConditionUp \== [human] ->
					assertz(path([X,Ynew,"M"])),!
				;
					format(""),!
				),
				%% format("go up, [~w, ~w]\n",[X,Ynew]),
				true,!
			)	
		;	
			
			(compare(=,Direction,3)  ->
				is(Xnew,X+1),
				coordinate(Xnew,Y,ConditionRight),
				(ConditionRight \== [orc] ->
					assert(current([Xnew,Y])),
					(ConditionRight \== [human] ->
						assertz(path([Xnew,Y,"M"])),!
					;
						format(""),!
					),
					%% format("go to the right, [~w, ~w]\n",[Xnew,Y]),
					true,!
				
				)	
			;
				(compare(=,Direction,4)  ->
					is(Ynew,Y-1),
					coordinate(X,Ynew,ConditionDown),
					(ConditionDown \== [orc] ->
						assert(current([X,Ynew])),
						(ConditionDown \== [human] ->
							assertz(path([X,Ynew,"M"])),!
						;
							format(""),!
						),
						%% format("go down, [~w, ~w]\n",[X,Ynew]),	
						true,!				
					)					
				) 
			) 
		) 
	). 
moving_random(_,X,Y):- random(1,5,NewDirection),
				moving_random(NewDirection,X,Y).

/* 
   These 2 rules proceed pass action for random method and also are used in backtracking method.
   First rule (with 0 as arg) checks the validity of the cell the player is standing on, if it is ok --> call the second to continue pass.
*/
pass(Direction,X,Y,0):-
	(out_of_map(X,Y) ->
		false,!
	;
		coordinate(X,Y,Condition),
		(compare(=,Condition,[human]) ->
			(visited([X,Y]) ->
				false,!
			;
				%% format("pass is succesfull!\n"),
				retract(was_pass(_)),
				assert(was_pass(-1)),
				assert(current([X,Y])),
				assert(visited([X,Y])),
				assertz(path([X,Y,"P"])),
				true,!
			)
		;
			(valid_pos(Condition)  ->
				%% format("continueing passing...\n"),
				pass(Direction,X,Y,1)
			)
		)
	).
pass(Direction,X,Y,1):-
	(compare(=,Direction,1) ->
		is(Xnew,X-1),
		pass(Direction,Xnew,Y,0),true,!
	;
		(compare(=,Direction,2) ->
			is(Ynew,Y+1),
			pass(Direction,X,Ynew,0),true,!
		;
			(compare(=,Direction,3) ->
				is(Xnew,X+1),
				pass(Direction,Xnew,Y,0),true,!
			;
				(compare(=,Direction,4) ->
					is(Ynew,Y-1),
					pass(Direction,X,Ynew,0),true,!
				;
					(compare(=,Direction,5) ->
						is(Ynew,Y+1),is(Xnew,X-1),
						pass(Direction,Xnew,Ynew,0),true,!
					;
						(compare(=,Direction,6) ->
							is(Ynew,Y+1),is(Xnew,X+1),
							pass(Direction,Xnew,Ynew,0),true,!
						;
							(compare(=,Direction,7) ->
								is(Ynew,Y-1),is(Xnew,X+1),
								pass(Direction,Xnew,Ynew,0),true,!
							;
								(compare(=,Direction,8) ->
									is(Ynew,Y-1),is(Xnew,X-1),
									pass(Direction,Xnew,Ynew,0),true,!
								)
							)
						)
					)
				)
			)
		)
	).

/* The rule determines the availibily of the pass action (There have no been pass yet in this round and random decide to pass(var = 2), not to move(var=1)). */
pass_condition(PassOrMove,Flag):- compare(=,PassOrMove,2),compare(=,Flag,0).

/* These 2 rules are procceding the run to the touchdown point, it firstly checks is player currenty on touchdown position, if not, go to action further. */
random_algo:- 
	  retract(touchdown([X1,Y1])),
	  retract(current([X2,Y2])),
	  (X1=:=X2,Y1=:=Y2 ->
			%% format("finally, at touchdown [~w, ~w]\n",[X1,Y2]),
			assert(touchdown([X1,Y1])),
			true,!
	  ;
	  	assert(current([X2,Y2])),
	  	assert(touchdown([X1,Y1])),
	  	false
	  ).
random_algo:-
	retract(current([X,Y])),
	random(1,3, PassOrMove), /* 1 - to move, 2 - to pass*/
	retract(was_pass(Flag)),
	assert(was_pass(Flag)),
	(pass_condition(PassOrMove,Flag) ->
		random(1,9,Direction),
		pass(Direction,X,Y,0),!
	;	
		random(1,5, Direction),
		moving_random(Direction,X,Y),!
	),
	random_algo.

/* The rule is used for putting path with shortest lenght to the min_path([X,Y,Condition of cell]). */
transfer:- 
	retract(temp_path([X,Y,Z])),
	(compare(=,Z,-1) ->
		assertz(min_path([-1,-1,-1])),
		true,!
	;
		assert(min_path([X,Y,Z])),
		transfer,!	
	).

/* The rule compares the lenght of the path with minimum path lenght. */
check_minimum(Count):-
	retract(min_leght(Minimum)),
	(compare(<,Count,Minimum) ->
		assert(min_leght(Count)),
		assertz(temp_path([-1,-1,-1])),
		retractall(min_path(_)),
		transfer,!
	;
		assert(min_leght(Minimum)),!
	).

/* The rule calculates the lenght of the path and compares it with minimum using 'check_minimum'  */
printing_random_path(Count):-
	retract(path([X,Y,Z])),
	(compare(=,Z,-1) ->
		is(CountTrue,Count-1),
		check_minimum(CountTrue),
		true,!
	;	
		assert(temp_path([X,Y,Z])),
		(compare(=,Z,"P") ->
			is(NewCount,Count+1),
			printing_random_path(NewCount),!
		;
			is(NewCount,Count+1),
			printing_random_path(NewCount),!
		)
	).

/* Prints path that is stored in min_path. */
printing_min:-
	retract(min_path([X,Y,Z])),
	(compare(=,Z,-1) ->
		retract(min_leght(Minimum)),
		(compare(=,Minimum,11111111) ->
			format("All attempts have failed, sorry :(\n"),!
		;
			format("number of actions: ~w\n",[Minimum]),!
		),
		assert(min_leght(Minimum)),
		true,!
	;
		(compare(=,Z,"P") ->
			format("~w (~w ~w)\n",[Z,X,Y]),
			printing_min,!
		;
			format("(~w ~w)\n",[X,Y]),
			printing_min,!
		)
	).

/* The rule proceeds 100 times running of 'random_algo', firstly, it makes empty all needed predicates and also it does 'random_algo' with respect to the fact that some attempts may fail. */
random_search(Count):-
	(compare(=,Count,100) ->
		format(""),!
	;
		is(NewCount,Count+1),
		retractall(temp_path(_)),
		retractall(path(_)),
		retractall(current(_)),
		retractall(was_pass(_)),
		assert(was_pass(0)),
		assert(current([0,0])),
		assertz(path([0,0,"S"])),
		(random_algo ->
			assertz(path([-1,-1,-1])),
			printing_random_path(0),!
		;
			format(""),!
		),	
		random_search(NewCount),!
	).

/* The core rule of the method, it proceeds 'random_search' and prints minimum path. */
random_method:-
	random_search(0),
	printing_min.

/*_________________________________2.Backtracking method rules._________________________________________*/

/* The rule prints the path, as we find the first successful attempt we don't need to find minimum one. */
printing_path(Count):-
	retract(path([X,Y,Z])),
	(compare(=,Z,-1) ->
		is(CountTrue,Count-1),
		format("number of actions: ~w\n",[CountTrue]),
		true,!
	;	
		assert(temp_path([X,Y,Z])),
		(compare(=,Z,"P") ->
			format("~w (~w ~w)\n",[Z,X,Y]),
			is(NewCount,Count+1),
			printing_path(NewCount),!
		;
			format("(~w ~w)\n",[X,Y]),
			is(NewCount,Count+1),
			printing_path(NewCount),!
		)
	).

/* The rule determines whether we are in valid position on map or not. */
in_map(X,Y):- compare(<,X,10),compare(<,Y,10),compare(>,X,-1),compare(>,Y,-1).


/* Rules that provide all possible actions for backtracking */
move_right([Xc,Yc,_],[Xnew,Ynew,CondNew]):-
	is(Xnew,Xc+1),is(Ynew,Yc),is(CondNew,"M").
move_left([Xc,Yc,_],[Xnew,Ynew,CondNew]):-
	is(Xnew,Xc-1),is(Ynew,Yc),is(CondNew,"M").
move_up([Xc,Yc,_],[Xnew,Ynew,CondNew]):-
	is(Xnew,Xc),is(Ynew,Yc+1),is(CondNew,"M").
move_down([Xc,Yc,_],[Xnew,Ynew,CondNew]):-
	is(Xnew,Xc),is(Ynew,Yc-1),is(CondNew,"M").
	
/* The rule that lists actions, one of them will be done */
any_action([Xc,Yc,Cond],[Xnew,Ynew,CondNew]):-
	move_right([Xc,Yc,Cond],[Xnew,Ynew,CondNew]);
	move_left([Xc,Yc,Cond],[Xnew,Ynew,CondNew]);
	move_up([Xc,Yc,Cond],[Xnew,Ynew,CondNew]);
	move_down([Xc,Yc,Cond],[Xnew,Ynew,CondNew]).

/* The rule that lists all necessary conditions for futher acting */
action([Xc,Yc,Cond],[Xnew,Ynew,CondNew]):-
	any_action([Xc,Yc,Cond],[Xnew,Ynew,CondNew]),
	in_map(Xnew,Ynew),
	coordinate(Xnew,Ynew,Condition),
	Condition\==[orc].
	
/* The initial case for backtracking*/
solve(StartPosition,Solution):-
	search([],StartPosition,Solution).

/* 
	2 base cases for backtracking, 
	first - checks whether we are in touchdowm (if true --> saves path, as we are looking only ONE succesful path, not optimal/shortest).
	second - proceeds action to the availible cell that have not been visited yet.
*/
search(Path,[Xc,Yc,Cond],[[Xc,Yc,Cond]|Path]):- 
	coordinate(Xc,Yc,[touchdown]),
	save_path([[Xc,Yc,Cond]|Path],0).	
search(Path,[Xc,Yc,Cond],Solution):-
	action([Xc,Yc,Cond],[Xnew,Ynew,CondNew]),
	not(member([Xnew,Ynew,CondNew],Path)),
	search([[Xc,Yc,CondNew]|Path],[Xnew,Ynew,CondNew],Solution).

/* The rule that prints backtrack path. */
print_path(Count):-
	retract(min_path([X,Y,Z])),
	(compare(=,Z,-1) ->
		is(TrueCount,Count-1),
		format("number of actions: ~w\n",[TrueCount]),
		true,!
	;	
		(compare(=,Z,80) ->  %% 80 - ASCII number for symbol 'P' - that shows that the particular action was pass
			format("P (~w ~w)\n",[X,Y]),
			print_path(Count),!
		;
			format("(~w ~w)\n",[X,Y]),
			print_path(Count),!
		)
	).
/* 2 rules that saves the succesful backtrack path to the predicate min_path and counts lenght of the path */
save_path([],Count):- 
	%% format("count - ~w ya v ahue\n",[Count]),
	assertz(min_path([-1,-1,-1])),
	print_path(Count).
save_path([[X,Y,Z]|T],Count):- 
	%% format("~w (~w ~w)\n",[Z,X,Y]), 
	asserta(min_path([X,Y,Z])),
	is(NewCount,Count+1),
	save_path(T,NewCount).

/* Core rule that runs findnsols (analog of findall), but we are intersted in only at most 1 solution as we want first succesful one, not the optimal. */
backtracking_method:- findnsols(1,_,solve([0,0,"S"],_),_).

/*_________________________________3.Heuristic function method rules._________________________________________*/

/* The rule determines whether the position is valid for heuristic function or not. */
valid_heuristic_pos(X,Y):- 
	(out_of_map(X,Y) ->
		false,!
	;
		coordinate(X,Y,Condition),
		(compare(=,Condition,[orc]) ->
			false,!
		;
			true,!
		)
	).

/* The rule proceeds move action for backtracking method. */
moving_heuristic(Direction,X,Y):-
	(compare(=,Direction,1)  ->
		is(Xnew,X-1),
		coordinate(Xnew,Y,ConditionLeft),
		(ConditionLeft \== [orc] ->
			assert(current([Xnew,Y])),
			(ConditionLeft \== [human] ->
				assertz(path([Xnew,Y,"M"])),!
			;
				format(""),!
			),
			%% format("go to the left, [~w, ~w]\n",[Xnew,Y]),
			true,!		
		)	
	;
		(compare(=,Direction,2)  ->
			is(Ynew,Y+1),
			coordinate(X,Ynew,ConditionUp),
			(ConditionUp \== [orc] ->
				assert(current([X,Ynew])),
				(ConditionUp \== [human] ->
					assertz(path([X,Ynew,"M"])),!
				;
					format(""),!
				),
				%% format("go up, [~w, ~w]\n",[X,Ynew]),
				true,!
			)	
		;				
			(compare(=,Direction,3)  ->
				is(Xnew,X+1),
				coordinate(Xnew,Y,ConditionRight),
				(ConditionRight \== [orc] ->
					assert(current([Xnew,Y])),
					(ConditionRight \== [human] ->
						assertz(path([Xnew,Y,"M"])),!
					;
						format(""),!
					),
					%% format("go to the right, [~w, ~w]\n",[Xnew,Y]),
					true,!				
				)	
			;
				(compare(=,Direction,4)  ->
					is(Ynew,Y-1),
					coordinate(X,Ynew,ConditionDown),
					(ConditionDown \== [orc] ->
						assert(current([X,Ynew])),
						(ConditionDown \== [human] ->
							assertz(path([X,Ynew,"M"])),!
						;
							format(""),!
						),
						%% format("go down, [~w, ~w]\n",[X,Ynew]),	
						true,!				
					)					
				) 
			) 
		) 
	).

/* The rule determines the neighbour cell with maximum rank and move to this cell using 'moving_heuristic' */
compare_neighbours(Count,X,Y,MaxRank,Direction):-
	retract(auxiliary_direction([Dir])),
	(compare(=,Dir,1) ->
		is(Xtmp,X-1),
		is(Ytmp,Y),
		retract(rank([Xtmp,Ytmp,Rank])),
		(compare(>,Rank,MaxRank) ->
			is(NewDirection,Dir),
			is(NewMaxRank,Rank),!
		;
			is(NewDirection,Direction),
			is(NewMaxRank,MaxRank),
			format(""),!
		),
		is(NewRank,Rank-1),
		assert(rank([Xtmp,Ytmp,NewRank])),!
	;
		(compare(=,Dir,2) ->
			is(Xtmp,X),
			is(Ytmp,Y+1),
			retract(rank([Xtmp,Ytmp,Rank])),
			(compare(>,Rank,MaxRank) ->
				is(NewDirection,Dir),
				is(NewMaxRank,Rank),!
			;
				is(NewDirection,Direction),
				is(NewMaxRank,MaxRank),
				format(""),!
			),
			is(NewRank,Rank-1),
			assert(rank([Xtmp,Ytmp,NewRank])),!
		;
			(compare(=,Dir,3) ->
				is(Xtmp,X+1),
				is(Ytmp,Y),
				retract(rank([Xtmp,Ytmp,Rank])),
				(compare(>,Rank,MaxRank) ->
					is(NewDirection,Dir),
					is(NewMaxRank,Rank),!
				;
					is(NewDirection,Direction),
					is(NewMaxRank,MaxRank),
					format(""),!
				),
				is(NewRank,Rank-1),
				assert(rank([Xtmp,Ytmp,NewRank])),!
			;
				(compare(=,Dir,4) ->
					is(Xtmp,X),
					is(Ytmp,Y-1),
					retract(rank([Xtmp,Ytmp,Rank])),
					(compare(>,Rank,MaxRank) ->
						is(NewDirection,Dir),
						is(NewMaxRank,Rank),!
					;
						is(NewDirection,Direction),
						is(NewMaxRank,MaxRank),
						format(""),!
					),
					is(NewRank,Rank-1),
					assert(rank([Xtmp,Ytmp,NewRank])),!
				;
					format(""),!
				)
			)
		)
	),
	(compare(=,Count,1) ->
		retract(rank([X,Y,R])),
		is(NRank,R-1),
		assert(rank([X,Y,NRank])),
		moving_heuristic(NewDirection,X,Y),!
	;
		is(NewCount,Count-1),
		compare_neighbours(NewCount,X,Y,NewMaxRank,NewDirection),!
	).

/* The auxiliary rule for finding availible neighbours in 'find_rank'. */
calculate_counter(Count):-
	retract(counter([Z])),
	(compare(=,Z,-1) ->
		assert(counter([Count])),!
	;
		is(NewCount,Count+1),
		calculate_counter(NewCount),!
	).

/* The rule that finds number of availible neighbours(in map and not orc on this cell) around current cell and after that finds the maximum rank using 'compare_neighbours'.  */
find_rank(X,Y):-
	is(Xtmp1,X+1),
	is(Ytmp1,Y),
	(valid_heuristic_pos(Xtmp1,Ytmp1) -> 
		assert(counter([2])),
		assert(auxiliary_direction([3])),!
	;
		format(""),!
	),
	is(Xtmp2,X-1),
	is(Ytmp2,Y),
	(valid_heuristic_pos(Xtmp2,Ytmp2) -> 
		assert(counter([3])),
		assert(auxiliary_direction([1])),!
	;
		format(""),!
	),
	is(Xtmp3,X),
	is(Ytmp3,Y+1),
	(valid_heuristic_pos(Xtmp3,Ytmp3) -> 
		assert(counter([4])),
		assert(auxiliary_direction([2])),!
	;
		format(""),!
	),
	is(Xtmp4,X),
	is(Ytmp4,Y-1),
	(valid_heuristic_pos(Xtmp4,Ytmp4) -> 
		assert(counter([5])),
		assert(auxiliary_direction([4])),!
	;
		format(""),!
	),
	assertz(counter([-1])),
	calculate_counter(0),
	retract(counter([Count])),
 	compare_neighbours(Count,X,Y,0,-1).

/* These 2 rules are proceeding run to touchdown point, first checks whether player is on touchdown or not, second does the actions according to the maximum rank along the neighbours. */
heuristic_algo:-
	retract(touchdown([X1,Y1])),
	retract(current([X2,Y2])),
	(X1=:=X2,Y1=:=Y2 ->
		%% format("finally, at touchdown [~w, ~w]\n",[X1,Y2]),
		assert(touchdown([X1,Y1])),
		true,!
	;
		assert(current([X2,Y2])),
		assert(touchdown([X1,Y1])),
		false
	).
heuristic_algo:-
	retract(current([X,Y])),
	find_rank(X,Y),
	heuristic_algo.

/* The core rule of the method, it proceeds 'heuristic_algo' and prints the path.*/
heuristic_method:-
	retractall(path(_)),
	retractall(current(_)),
	retractall(visited(_)),
	assert(current([0,0])),
	assertz(path([0,0,"S"])),
	retractall(auxiliary_direction(_)),
	heuristic_algo,
	assertz(path([-1,-1,-1])),
	printing_path(0).

/*_________________________________4.Read input rules._________________________________________*/
/* These 4 rules are needed to read input values from 'input.pl' and convert it in the proposed format of each coordinate that is stored in 'map.pl'. */
read_touchdown:-
	retract(t(X,Y)),
	retract(coordinate(X,Y,_)),
	assert(coordinate(X,Y,[touchdown])).
read_orcs:-
	(retract(o(X,Y)) ->
		retract(coordinate(X,Y,_)),
		assert(coordinate(X,Y,[orc])),
		read_orcs,!
	;
		format(""),!
	).
read_humans:-
	(retract(h(X,Y)) ->
		retract(coordinate(X,Y,_)),
		assert(coordinate(X,Y,[human])),
		read_humans,!
	;
		format(""),!
	).
read_input:-
	read_humans,
	read_orcs,
	read_touchdown.

/*_________________________________5.Main rule._________________________________________*/
start :-
	consult("map.pl"),
	consult("input.pl"),
	read_input,
	format("Hey! Here the map:\n\n"),
	draw_map, 
	format("First, random method:\n"),
	format("Solution path:\n"),
    statistics(runtime,[T0|_]),
	random_method,
	statistics(runtime,[T1|_]),
	is(Trand,T1-T0),
	format("random method took ~3d sec. ~n",[Trand]),
	format("\nSecond, backtrack method:\n"),
	format("Solution path:\n"),
	statistics(runtime,[T2|_]),
	backtracking_method,
	statistics(runtime,[T3|_]),
	is(Tback,T3-T2),
	format("backtracking mecthod took ~3d sec. ~n",[Tback]),
	format("\nThird, heuristic function method:\n"),
	format("Solution path:\n"),
	statistics(runtime,[T4|_]),
	heuristic_method,
	statistics(runtime,[T5|_]),
	is(Theu,T5-T4),
	format("heuristic function method took ~3d sec. ~n",[Theu]),
	format("---finished---"),
	halt.