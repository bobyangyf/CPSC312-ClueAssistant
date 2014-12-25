/**
 * Clue assistant in prolog.
 *
 * *************************************
 * This file contains list manipulation functions, database
 * query and manipulation functions, and other utils for the knowledge
 * inference functions
 */

:- module(knowledgeutils,
          [assertnodup/1, playersequence/3, allplayers/1, self/1,
           allcards/1, alllocations/1, allweapons/1, allculprits/1,
           allownedcards/1, allownedcards/2, allownedplayercards/1, 
           allownedplayercards/2, allownedlocationcards/1, 
           allownedlocationcards/2, allownedweaponcards/1, 
           allownedweaponcards/2, allownedbyplayer/2, cardunownedbyplayers/2,
           allunownedbyplayer/2, allpotentialknowledgeforplayer/2]).

:- use_module(clueknowledge).
:- use_module(knowledgeinference).
:- use_module(utils).

/**
 * Asserts Z if Z is not already a fact
 */
assertnodup(Z) :- Z, !; not(Z), assert(Z), !.

/**
 * playersquence(START, END, SEQUENCE)
 * Returns true if SEQUENCE is list of players in clockwise order
 * starting from START until END, EXCLUDING START and END.
 */
playersequence(START,END,SEQUENCE)
   :- allplayers(Ps), listportion(START,END,Ps,SEQUENCE).

/**
 * allplayers(Ps)
 * True if Ps is list of all players in the game
 */
allplayers(Ps) :- findall(X,player(X),Ps).

/**
 * self(P)
 * True if P is the player using the companion
 */
self(X) :- player(X), !.

/**
 * allcards(C)
 * True if C is all the cards in the game
 */

allcards(C) :- allplayers(Ps), alllocations(Ls), allweapons(Ws),
               append(Ps,Ls,R), append(R,Ws,C).

/**
 * alllocations(Ls)
 * True if Ls is all the locations in the game
 */
alllocations(Ls) :- findall(X,location(X), Ls).

/**
 * allweapons(Ws)
 * True if Ws is all the weapons in the game
 */
allweapons(Ws) :- findall(X,weapon(X),Ws).

/**
 * allculprits(Ss)
 * True is Ss is all the culprits in the game
 */
allculprits(Ss) :- findall(X,culprit(X),Ss).

/**
 * allownedcards(OCs)
 * True if OCs is all the known owned cards
 */
allownedcards(OCs) :- findall(OC,knowledge(_,OC,owned),OCs).

/**
 * allownedcards(PK, Ocs)
 * True if OCs is all the known owned cards for PK
 */
allownedcards(PK,OCs) :- findall(OC,knowledge(PK,OC,owned),OCs).

/**
 * allownedplayercards(OPs)
 * True if OPs is all the known owned player cards
 */
allownedplayercards(OPs)
   :- allownedcards(OCs), allplayers(Ps), intersection(OCs,Ps,OPs).

/**
 * allownedlocationcards(OLs)
 * True if OLs is all the known owned location cards
 */
allownedlocationcards(OLs)
   :- allownedcards(OCs), alllocations(Ls), intersection(OCs,Ls,OLs).

/**
 * allownedweaponcards(OWs)
 * True if OWs is all the known owned weapon cards
 */
allownedweaponcards(OWs)
   :- allownedcards(OCs), allweapons(Ws), intersection(OCs,Ws,OWs).

/**
 * allownedplayercards(PK, OPs)
 * True if OPs is all the known owned player cards for PK
 */
allownedplayercards(PK,OPs)
   :- allownedcards(PK,OCs), allplayers(Ps), intersection(OCs,Ps,OPs).

/**
 * allownedlocationcards(PK, OLs)
 * True if OLs is all the known owned location cards for PK
 */
allownedlocationcards(PK,OLs)
   :- allownedcards(PK,OCs), alllocations(Ls), intersection(OCs,Ls,OLs).

/**
 * allownedweaponcards(PK, OWs)
 * True if OWs is all the known owned weapon cards for PK
 */
allownedweaponcards(PK, OWs)
   :- allownedcards(PK, OCs), allweapons(Ws), intersection(OCs,Ws,OWs).

/**
 * allownedbyplayer(P,Cs)
 * True if Cs is the set of cards in the player's hand
 */
allownedbyplayer(P,Cs) :- findall(R,knowledge(P,R,owned),Cs).

/**
 * allunownedbyplayer(P,Cs)
 * True if Cs is the set of cards not in the player's hand
 */
allunownedbyplayer(P,Cs) :- findall(R,knowledge(P,R,unowned),Cs).

/**
 * cardunownedbyplayers(Ps,C)
 * True if card C is not owned by all P in Ps based on our knowledge
 */
cardunownedbyplayers([],_).
cardunownedbyplayers([P|Ps],C) 
    :- not(knowledge(P,C,owned)), cardunownedbyplayers(Ps,C).

/**
 * allpotentialknowledgeforplayer(P,Cs)
 * True if Cs is the set of cards that we think player P potentially owns
 */
allpotentialknowledgeforplayer(P,Cs)
   :- findall(R,potentialknowledge(P,R,owned),Cs).

