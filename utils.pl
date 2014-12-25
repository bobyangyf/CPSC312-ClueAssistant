/**
 * Clue assistant in prolog.
 *
 * *************************************
 * This file contains general list manipulation functions
 * and utils for Clue assistant.
 *
 */

:- module(utils,
          [listportion/4, removefromall/3, removeallcontaining/3, delete/3,
           reverse/2, append/3, subtract/3, member/2]).

/**
 * listportion(START, END, LIST, SEQUENCE)
 * Returns true if SEQUENCE is list of portion cutting LIST at
 * START and END, excluding end points, treating LIST as a circular list.
 */
listportion(S,E,[S|T],L) :- !, cutlistatelem(E,T,L).
listportion(S,E,L,SQ) :- rotatelist(L,R), !, listportion(S,E,R,SQ).

/**
 * cutlistatelem(E,L1,L2)
 * Returns true if L2 is the list L1 up until and excluding E in L1.
 * If E is not in L1, then L2 = L1.
 */
cutlistatelem(_,[],[]) :- !.
cutlistatelem(E,[E|_],[]) :- !.
cutlistatelem(E,[H|T1],[H|T2]) :- !, cutlistatelem(E,T1,T2).

/**
 * rotatelist(LIST1, LIST2)
 * True if LIST2 is LIST1 rotated to the left as a circular list
 */
rotatelist([],[]).
rotatelist([H|T],R) :- append(T,[H],R).

/**
 * removefromall(X, PKCs, RS)
 * True if result RS is the result of removing X from lists in PKCs
 */
removefromall(_,[],[]).
removefromall(X,[PKC|PKCs],[PKC|Rs])
   :- not(member(X,PKC)), !, removefromall(X,PKCs,Rs).
removefromall(X,[PKC|PKCs],[R|RS])
   :- member(X,PKC), !, delete(X,PKC,R), removefromall(X,PKCs,RS). 


/**
 * removeallcontaining(C, PKCs, Rs)
 * True if Rs is the result of removing all lists in PKCs that contains C,
 * where PKCS is a list of list of Cs
 */
removeallcontaining(_,[],[]).
removeallcontaining(C,[PKC|PKCs],[PKC|Rs])
   :- not(member(C,PKC)), removeallcontaining(C,PKCs,Rs).
removeallcontaining(C,[PKC|PKCs],Rs)
   :- member(C,PKC), removeallcontaining(C,PKCs,Rs).

/**
 * delete(A, L1, L2)
 * Returns true if L2 is L1 with A deleted
 */
delete(A, [A|B], B).
delete(A, [B, C|D], [B|E]) :- delete(A, [C|D], E).

/**
 * reverse(L1,L2)
 * True if L2 is the reverse of L1
 */
reverse(L1,L2) :- reverseacc(L1, [], L2).

reverseacc([], L, L).
reverseacc([H|T1],A,L) :- reverseacc(T1,[H|A],L).

/**
 * append(L1,L2,R)
 * True if the result of appending lists L1 and L2 together is R
 */
append([],X,X).
append([H|T],Y,[H|R]) :- append(T,Y,R).

/**
 * subtract(L1,L2,R)
 * True if R is the result of subtracting L2 from L1
 */
subtract([],_,[]).
subtract([H|T1],L2,R) :- member(H,L2), !, subtract(T1,L2,R).
subtract([H|T1],L2,[H|R]) :- !, subtract(T1,L2,R).

/**
 * member(E,L)
 * True if E is a member of list L
 */
member(E,[E|_]).
member(E,[_|T]) :- member(E,T).
                        

