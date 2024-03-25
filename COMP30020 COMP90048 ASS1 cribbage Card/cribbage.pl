WeChat: cstutorcs
QQ: 749389476
Email: tutorcs@163.com


:- ensure_loaded(cards).

replace(_, _, [], []).
replace(Old, New, [Old|T], [New|Result]) :-
    replace(Old, New, T, Result).
replace(Old, New, [H|T], [H|Result]) :-
    H \= Old,
    replace(Old, New, T, Result).

%sublist(?,+)
%True when all members of Sub are members of List in the same order.
sublist([], []).
sublist([H| R1], [H| R2]) :-sublist(R1, R2).
sublist(H, [_ | R2]) :-sublist(H, R2).

hand_value(Hand, Startcard, Value) :-
    checkjack(Startcard, V1),
    countsub([Startcard|Hand], V2),
    countrank([Startcard|Hand], V3),
    Value is V1 + V2 + V3.

checkjack(Startcard,V1):-
    (Startcard = card(jack,_Suit)
    -> V1 =1
    ;V1 = 0).

convertcj(Rlist,CJRlist):-
    replace(ace,1,Rlist,Rlist1),
    replace(jack,10,Rlist1,Rlist2),
    replace(queen,10,Rlist2,Rlist3),
    replace(king,10,Rlist3,CJRlist).
%length(?, ?)
%True if Length represents the number of elements in List.
%member(?,?)
%True if Elem is a member of List. 
%findall(+, :, -)
%Create a list of the instantiations Template gets successively on backtracking
%over Goal and unify the result with Bag. Succeeds with an empty list
%if Goal has no solutions.
countsub(List, V2) :-
    findall(R, member(card(R, _), List), Rlist),
    convertcj(Rlist, CJRlist),
    maplist(convertcj, Rlist, CJRlist),
    findall(Sublist, (
        sublist(Sublist, CJRlist),
        sumlist(Sublist, Sum),Sum = 15
    ), ALLSublist),
    length(ALLSublist, Num),
    V2 is Num * 2.

convertcs(Rlist,CSRlist):-
    replace(ace,1,Rlist,Rlist1),
    replace(jack,11,Rlist1,Rlist2),
    replace(queen,12,Rlist2,Rlist3),
    replace(king,13,Rlist3,CSRlist).
%msort(+, -)
%True if Sorted can be unified with a list holding the elements of List,
%sorted to the standard order of terms
group([], []).
group([H|T], [GroupedHead|GroupedTail]) :-
    group_head(H, T, Rest, GroupedHead),
    group(Rest, GroupedTail).
group_head(H, [], [], [H]).
group_head(H, [H|T], Rest, [H|Grouped]) :-
    group_head(H, T, Rest, Grouped).

countrank(List, V3) :-
    findall(Value, member(card(Value, _), List), Values),
    convertcs(Values, CSRlist),
    msort(CSRlist, SortedRlist),
    group(SortedRlist, GroupedRlist),
    findall(Len, (
        member(Group, GroupedRlist),
        length(Group, Len),
        Len >= 2
    ), Lenlist),
    replace(3, 6, Lenlist, Lenlist1),
    replace(4, 12, Lenlist1, Lenlist2),
    sumlist(Lenlist2, V3).

%last(?, ?)
%Succeeds when Last is the last element of List. 
head([H|_],H).
% check runs(+Cardlist, -Score)
% Score is the max length of consecutive sublists of Cardlist times the
% occurrence of the sublist that has the max length in the Cardlist 
check_runs(Cardlist, Score):-
    filter_consec(Cardlist, Filterlist), 
    max_1ength(Filterlist, Max),
    max_occ(Filterlist, Max, Occ), 
    Score is Max * Occ.

maxsub(L, V4) :-
    findall(R, member(card(R, _), L), Rlist),
    convertcs(Rlist, CSRlist),
    findall(Len-Sublist, (
        sublist(Sublist, CSRlist),
        msort(Sublist, SortedSublist),
        last(SortedSublist, Max),
        head(SortedSublist, Min),
        length(SortedSublist, Len),
        MaxMinDiff is Max - Min,
        MaxMinDiff = Len 
    ), ConsecutiveLenList),
    keysort(ConsecutiveLenList, SortedConsecutiveLenList),
    last(SortedConsecutiveLenList, MaxLen-_),
    (MaxLen >= 3 
    -> V4 = MaxLen
    ; V4 = 0).

findlen(List,Len):-
    findall(S,member(card(_,S),List),SuitList),
    sort(SuitList,SortedSuitList),
    length(SortedSuitList,Len).

removelen(Hand, Startcard, V5):-
    findlen(Hand,HandLen),
    findlen([Startcard|Hand],ALLLen),
    (HandLen =:= 1
    ->(ALLLen =:= 1
      -> V5=5
      ;V5=4
      )
    ;V5=0
    ).

findexp(Hand, StartcardList, Expectation):-
    findall(Value, (
        member(Startcard, StartcardList),
        hand_value(Hand, Startcard, Value)
    ), Values),
    sum_list(Values, Sum),
    length(Values, Len),
    Expectation is Sum / Len.

select_hand(Cards, Hand, Cribcards):-
    findall(H, (sublist(H, Cards), length(H, 4)), Hlist),
    find_startcard_list(Cards, StartcardList),
    findall(Expectation-H1, (
        member(H1, Hlist),
        findexp(H1, StartcardList, Expectation)
    ), ExpectationList),
    max_member(Expectation-Hand, ExpectationList),
    findall(cards(R, S), (
        member(card(R, S), Cards),
        \+ member(card(R, S), Hand)
    ), Cribcards).