week_schedule([],_,_,[]).
week_schedule([H|T],TAs,DayMax,[Assi|T1]):-
		day_schedule(H,TAs,RemTAs,Assi),max_slots_per_day(Assi,DayMax),
		week_schedule(T,RemTAs,DayMax,T1).

day_schedule([],TAs,TAs,[]).
day_schedule([H|T],TAs,RemTAs,[Assignment|T1]):-
		slot_assignment(H,TAs,R,Assignment),day_schedule(T,R,RemTAs,T1).

max_slots_per_day(DaySched,Max):-
		flatten(DaySched,NewD), max_help(NewD,Max).
max_help([],_):-!.
max_help(L,Max):-
		L =[H|T],count_r(H,L,C),C =< Max, max_help(T,Max),!.
count_r(_,[],0).
count_r(X,[H|T],C):- X = H,count_r(X,T,C1),C is 1+C1.
count_r(X,[H|T],C):- X\=H,count_r(X,T,C).

slot_assignment(LabsNum,TAs,RemTAs,Assignment):-
		combination(LabsNum,TAs,RA),permutation(RA,R),rem(R,TAs,NL),
		asi(R,[],Assignment),help_slot(Assignment,R,NA),
		append(NA,NL,RemTAs).
	
rem([],Acc,Acc).	
rem([H|T],R,NewL):-delete(R,H,NL),rem(T,NL,NewL).

asi([],Acc,Acc).
asi([H|T],Acc,R):-H=ta(X,_), append(Acc,[X],NAcc),asi(T,NAcc,R).

help_slot([],R,R).		
help_slot([H|T],L,R):-ta_slot_assignment(L,NR,H),help_slot(T,NR,R).

combination(N,L,R):- com(L,R),length(R,N).

com([],[]):-!.
com([H|T],[H|T1]):-com(T,T1).
com([_|T],P):- com(T,P).


ta_slot_assignment(TAs,RemTAs,Name):-
		ta_slot_helper(TAs,RemTAs,[],Name).

ta_slot_helper([],Acc,Acc,_).
ta_slot_helper([H|T],R,Acc,Name):-
	H \= ta(Name,_),
	append(Acc,[H],NAcc),
	ta_slot_helper(T,R,NAcc,Name).
ta_slot_helper([H|T],NewAcc,Acc,Name):-
	H = ta(Name,X),
	X > 0,
	X1 is X-1,
	H1 = ta(Name,X1),
	append(Acc,[H1],NAcc),
	append(NAcc,T,NewAcc).
	