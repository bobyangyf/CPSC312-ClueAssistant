/**
 * Clue assistant in prolog.
 *
 * *************************************
 * This file contains all facts to represent the knowledge of
 * the game state for the current user, associated procedures for
 * adding new information regarding the game and associated knowledge
 * inference predicates that attempts to infer new knowledge of the
 * game based on existing knowledge, and newly added knowledge. 
 * 
 */

:- module(knowledgeinference,
          [knowledge/3, potentialknowledge/3, question/3, culprit/1,
           parseplayercards/2, handleowned/2, handleunowned/2,
           addpotentialknowledge/3, generateculprits/0, checkplayerculprit/1,
           checklocationculprit/1, checkweaponculprit/1,
           querypossibleplayerculprits/1, querypossiblelocationculprits/1,
           querypossibleweaponculprits/1]).

/**
 * Load our definition and utilities for clue
 */
:- use_module(clueknowledge).
:- use_module(knowledgeutils).
:- use_module(utils).

/**
 * Knowledge representation scheme:
 *
 * knowledge(P,C,owned|unowned) indicates player P own|unowns card C
 * potentialknowledge(P,[Cs],owned) indicates player P owns at least one
 *   of cards in [Cs]
 * culprit(C) indicates C is not owned by any player P in the game, and thus
 *   a winning guess.
 *
 * INVARIANTS:
 *     Let {P} denote set of all players,
 *     #P = the number of unknown cards P has in hand
 *     {L} the set of locations,
 *     {W} the set of weapons,
 *     {CS} = {P} + {L} + {W} the set of all cards
 *     {K} the set of knowledge,
 *     {PK} the set of potential knowledge,
 *     {S} the set of culprits
 *
 *  1) If there exists C in {CS} st knowledge(P,C,owned|unowned), 
 *       then there does not exist PK in {PK} st PK(P',CS,owned). 
 *  2) If there exists K in {K} st K(P,C,owned), then for all P' not P in {P},
 *       K(P',C,unowned).
 *  3) If K(P,C,owned), then not K(P,C,unowned).
 *  4) For all P in {P}, let CPKs be union of CPK_i
 *       for PPK = {PK(P,CPK,owned)}, CPKs = {CPK : PK(P,CPK,owned)},
 *       then there does not exist C in CPKs st for all PK in PPK to
 *       be satisfied, |K(P,Cs,owned)| > #P
 *  5) With every new addition of knowledge(P,C,owned), #P = #P - 1
 *       For all P in {P}, #P >= 0
 *         If #P = 0, let KCs = {C in {CS} : K(P,C,owned)},
 *         then for all C' in {CS} not in KCs, K(P,C',unowned).
 *  6) For all PK(P,Cs,owned) in {PK}, |Cs| > 1. 
 *       (If |Cs|=1, then K(P,Cs,owned)).
 *  7) For all S(C) in {S}, for all P in {P}, K(P,C,unowned)
 *  8) If there exists P in {P} st S(P) in {S}, then for all P' in {P}, not S(P')
 *     If there exists L in {L} st S(L) in {S}, then for all L' in {L}, not S(L')
 *     If there exists W in {W} st S(W) in {S}, then for all W' in {W}, not S(W')
 *  9) Game ends iff |S| = 3. We win!
 *
 * question(L,P,W) indicates a question was asked with L as location,
 *   P as player, and W as weapon
 */
:- dynamic knowledge/3.
:- dynamic potentialknowledge/3.
:- dynamic question/3.
:- dynamic culprit/1.


/**
 * parseplayercards(P, Cs)
 * Adds knowledge that player P owns all cards in Cs
 */
parseplayercards(_, []).
parseplayercards(P, [C|Cs]) :- handleowned(P,C), parseplayercards(P,Cs).

/**
 * parseplayerunownedcards(P, Cs)
 * Adds knowledge that player P unowns all cards in Cs
 */
parseplayerunownedcards(_,[]).
parseplayerunownedcards(CP,[C|Cs])
   :- assertnodup(knowledge(CP,C,unowned)), parseplayerunownedcards(CP,Cs). 

/**
 * handleowned(P, C)
 * Adds new knowledge that player P owns card C, and infers all possible
 * knowledge that is able to be derived based on current knowledge.
 */
handleowned(P,C)
   :- assertnodup(knowledge(P,C,owned)),
      retract(numcardsunknownforplayer(P,N)),
      NN is N - 1, assertnodup(numcardsunknownforplayer(P,NN)),
      inferbynumforplayer(P), inferbytype(C),
      resolvepotentialknowledge(P,C,owned), 
      playersequence(P,P,Ps), handleforall(Ps,C,unowned).

/**
 * handleunowned(P, C)
 * Adds new knowledge that player P unowns card C, and infers all possible
 * knowledge that is able to be derived based on current knowledge.
 */
handleunowned(P,C)
   :- assertnodup(knowledge(P,C,unowned)), 
      resolvepotentialknowledge(P,C,unowned).

/**
 * inferbynumforplayer(P)
 * Infer new knowledge about the game state based on number of cards left
 * unknown for player P
 */
inferbynumforplayer(P)
   :- numcardsunknownforplayer(P,0), !, findall(KC,knowledge(P,KC,owned),Cs), 
      allcards(C), subtract(C,Cs,UC), parseplayerunownedcards(P,UC), !;
      !.

/**
 * inferbytype(C)
 * Infer new knowledge about the game state based on the type of card of C
 */
inferbytype(C)
   :- location(C), !, inferbylocationcards;
      player(C), !, inferbyplayercards; 
      weapon(C), !, inferbyweaponcards.

/**
 * inferbylocationcards
 * Infer new knowledge based on location cards that are known to be owned.
 * If there is all but one location card is owned, then the one remaining
 * must be the culprit and unowned by all players.
 */
inferbylocationcards
   :- alllocations(Ls), allownedlocationcards(OLs),
      subtract(Ls,OLs,Rs), length(Rs, 1), !,
      allplayers(Ps), [UC] = Rs, handleforall(Ps,UC,unowned);
      !.

/**
 * inferbyplayercards
 * Infer new knowledge based on player cards that are known to be owned.
 * If there is all but one player card is owned, then the one remaining
 * must be the culprit and unowned by all players.
 */
inferbyplayercards
   :- allplayers(Ps), allownedplayercards(OPs),
      subtract(Ps,OPs,Rs), length(Rs, 1), !,
      [UC] = Rs, handleforall(Ps,UC,unowned);
      !.

/**
 * inferbyweaponcards
 * Infer new knowledge based on weapon cards that are known to be owned.
 * If there is all but one weapon card is owned, then the one remaining
 * must be the culprit and unowned by all players.
 */
inferbyweaponcards
   :- allweapons(Ws), allownedweaponcards(OWs),
      subtract(Ws,OWs,Rs), length(Rs, 1), !,
      allplayers(Ps), [UC] = Rs, handleforall(Ps,UC,unowned);
      !.

/** 
 * addpotentialknowledge(P, Cs, owned)
 * Asserts a potential knowledge that P owned one of C in Cs,
 * resolving the potential knowledge to knowledge if applicable.
 */
addpotentialknowledge(P,Cs,owned)
   :- resolvewithknowledgeforplayer(P,Cs,NKCs), nohandoverflow(P,NKCs,VCs),
      assertpkork(P,VCs,owned).

/**
 * resolvewithknowledgeforplayer(P,Cs,Vs)
 * True if Vs = [] if Cs contains a C st knowledge(P,C,owned),
 * else if reducewithknowlegeforperson(P,Cs,Vs)
 */
resolvewithknowledgeforplayer(P,Cs,[])
   :- member(C,Cs), knowledge(P,C,owned), !.
resolvewithknowledgeforplayer(P,Cs,Vs)
   :- reducewithknowledgeforplayer(P,Cs,Vs), !.

/**
 * reducewithknowledgeforperson(P, Cs, Rs)
 * True if Rs = {C in CS : not(knowledge(P,C,unowned))}
 */
reducewithknowledgeforplayer(_, [], []).
reducewithknowledgeforplayer(P,[C|Cs],[C|NKCs])
   :- not(knowledge(P,C,unowned)), !, reducewithknowledgeforplayer(P,Cs,NKCs).
reducewithknowledgeforplayer(P,[C|Cs],NKCs)
   :- knowledge(P,C,unowned), !, reducewithknowledgeforplayer(P,Cs,NKCs).

/**
 * nohandoverflow(P, Cs, VCs)
 * True if VCs are all cards in Cs that does not cause P to exceed number of
 * cards on hand in order to satisfy all potential knowledge of P.
 */
nohandoverflow(_,[],[]).
nohandoverflow(P,[C|Cs],[C|VCs])
   :- numcardsunknownforplayer(P,N),
      findall(PKCs, potentialknowledge(P,PKCs, owned), PKs),
      nohandoverflow(P,C,N,PKs), !, nohandoverflow(P,Cs,VCs).
nohandoverflow(P,[C|Cs],VCs)
   :- numcardsunknownforplayer(P,N),
      findall(PKCs, potentialknowledge(P,PKCs, owned), PKs),
      not(nohandoverflow(P,C,N, PKs)), !, nohandoverflow(P,Cs,VCs).

/**
 * nohandoverflow(P, C, N, PKCs)
 * True if P owning C does not causes P to exceed N cards on hand in order to
 * satisfy all of PKCs. A PKCs is satisfied if P owns one of PKCs
 */
nohandoverflow(_,_,N,[]) :- N >= 0, !.
nohandoverflow(P,C,N,PKCs)
   :- N >= 0, removeallcontaining(C,PKCs,Rs), NN is N - 1,
      nohandoverflow(P, _, NN, Rs), !.

/**
 * assertpkork(P, VCs, owned)
 * Asserts a potential knowledge of P owning VCs if length of VCs is > 1,
 * otherwise infers a knowledge that P owns the card in VC
 */
assertpkork(_,[],_) :- !.
assertpkork(P,[VC],owned):- !, handleowned(P,VC).
assertpkork(P,VCs,owned) :- length(VCs, LEN), LEN > 1, !,
                            assertnodup(potentialknowledge(P,VCs,owned)).

/**
 * resolvepotentialknowledge(P, X, unowned)
 *
 * Resolve potential knowledge to modify existing knowledge
 * given knowledge that X is unowned.
 *
 * We must search through the potential knowledge for all potential
 * knowledge for person P containing card X, then remove X from the
 * potential knowledge and add new knowledge if possible.
 * If new knowledge is added, we must resolvepotentialknowledge
 * given that person P is owned by X.
 */
resolvepotentialknowledge(P,X,unowned)
   :- findall(Cs, potentialknowledge(P, Cs, owned), PKCs),
      removefromall(X,PKCs, RS), updatepotentialknowledge(P,PKCs,RS).

/**
 * resolvepotentialknowledge(P, X, owned)
 *
 * Resolve potential knowledge to modify existing knowledge
 * given knowledge that X is owned.
 *
 * We must find all potential knowledge for P with X, and remove the
 * potential knowledge.
 */
resolvepotentialknowledge(P,X,owned)
   :- findall(Cs, potentialknowledge(P,Cs,owned), PKCs),
      removeallcontaining(X,PKCs,R), reducepotentialknowledge(P,PKCs,R).

/**
 * updatepotentialknowledge(P, PKCs, RS)
 * For each PKC and R in PKCs and RS, if PKC = R, do nothing, else
 * remove the original potentialknowledge(P, PKC, owned) and assert
 * the new potentialknowledge that P owns R.
 */
updatepotentialknowledge(_,[],[]).
updatepotentialknowledge(P,[PKC|PKCs],[PKC|RS])
   :- !, updatepotentialknowledge(P,PKCs,RS).
updatepotentialknowledge(P,[PKC|PKCs],[R|RS])
   :- !, retract(potentialknowledge(P,PKC,owned)), assertpkork(P,R,owned),
         updatepotentialknowledge(P,PKCs,RS).

/**
 * reducepotentialknowledge(P, PKCs, R)
 * Retract all PK for person P that are not in R.
 */
reducepotentialknowledge(_,[],[]).
reducepotentialknowledge(P,[PKC|PKCs],[])
   :- !, retract(potentialknowledge(P,PKC,owned)),
      reducepotentialknowledge(P,PKCs,[]).
reducepotentialknowledge(P,[PKC|PKCs],[PKC|Rs])
   :- !, reducepotentialknowledge(P,PKCs,Rs). 
reducepotentialknowledge(P,[PKC|PKCs],R)
   :- !, retract(potentialknowledge(P,PKC,owned)),
      reducepotentialknowledge(P,PKCs,R).

/**
 * checkplayerculprit(P), checklocationculprit(L), checkweaponculprit(W)
 * True if P,L,W is known as a culprit of player, location, or weapon,
 * respectively.
 */
checkplayerculprit(P) :- querypossibleplayerculprits(Ps), [P] = Ps.
checklocationculprit(L) :- querypossiblelocationculprits(Ls), [L] = Ls.
checkweaponculprit(W) :- querypossibleweaponculprits(Ws), [W] = Ws.

/**
 * querypossibleplayerculprits(P), querypossiblelocationculprits(L),
 * querypossibleweaponculprits(W)
 * True if P, L, W is a list of all possible culprits for player, location,
 * or weapon, respectively
 */
querypossibleplayerculprits(P) 
   :- culprit(X), player(X), !, P = [X];
      !, allplayers(Ps), findall(K,knowledge(_,K,owned),Ks),
      intersection(Ks, Ps, KPs), subtract(Ps, KPs, P).

querypossiblelocationculprits(L)
   :- culprit(X), location(X), !, L = [X];
      !, alllocations(Ls), findall(K,knowledge(_,K,owned),Ks),
      intersection(Ks, Ls, KLs), subtract(Ls, KLs, L).

querypossibleweaponculprits(W)
   :- culprit(X), weapon(X), !, W = [X];
      !, allweapons(Ws), findall(K,knowledge(_,K,owned),Ks),
      intersection(Ks, Ws, KWs), subtract(Ws, KWs, W).

/**
 * generateculprits
 * Infers knowledge of culprits based on current game knowledge
 */
generateculprits
   :- generateplayerculprit, generatelocationculprit, generateweaponculprit.

/**
 * generateplayerculprit
 * Infers knowledge of a player culprit based on current game knowledge
 */
generateplayerculprit
   :- culprit(X), player(X), !;
      player(P), findall(Ps, knowledge(Ps,P,unowned), LPs), length(LPs,L),
      allplayers(APs), length(APs,L), not(culprit(P)), !,
      assertnodup(culprit(P));
      !.

/**
 * generatelocationculprit
 * Infers knowledge of location culprit based on current game knowledge
 */
generatelocationculprit
   :- culprit(X), location(X);
      location(L), findall(Ps, knowledge(Ps,L,unowned), LPs), length(LPs,LEN), 
      allplayers(APs), length(APs, LEN), not(culprit(L)), !,
      assertnodup(culprit(L));
      !.

/**
 * generateweaponculprit
 * Infers knowledge of weapon culprits based on current game knowledge
 */
generateweaponculprit
   :- culprit(X), weapon(X);
      weapon(W), findall(Ps, knowledge(Ps,W,unowned), LPs), length(LPs,L), 
      allplayers(APs), length(APs, L), not(culprit(W)), !,
      assertnodup(culprit(W));
      !.

