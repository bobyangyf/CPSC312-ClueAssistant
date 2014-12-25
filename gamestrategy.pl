/**
 * Clue assistant in prolog.
 *
 * *************************************
 * This file contains all procedures and inference predicates
 * to optimize the strategy for clue, including which questions
 * we should ask, what card we should show, and whether we
 * should take a risk with a culprit guess.
 */

:- module(gamestrategy, [suggestquestion/3]).

/**
 * Load our definition and utilities for clue
 */
:- use_module(clueknowledge).
:- use_module(knowledgeinference).
:- use_module(knowledgeutils).
:- use_module(utils).

/**
 * suggestquestion(P,L,W)
 * True if we should suggest that person P committed the murder at location L
 * with weapon W
 */
suggestquestion(P,L,W) 
   :- player(P), location(L), weapon(W), allplayers(Ps), 
      reverse(Ps,RPs), target(RPs,T), choosesuspectplayer(T,P), 
      choosesuspectlocation(T,L), choosesuspectweapon(T,W).

/**
 * target(X,T)
 * True if T is the first person to player's right for which we don't
 * know all of T's cards
 */
target([RP|RPS],T) 
   :- numcardsunknownforplayer(RP,0), !, target(RPS, T);
      T = RP.

/**
 * choosesuspectplayer(T,P)
 * True if P should be suggested as the murderer to obtain new knowledge for
 * player T. P should be suggested if it is not owned by anyone to the
 * current player's left up to and including the target T. If we do not have
 * a player culprit, the current player must not own P. Otherwise, P can be
 * the culprit or something we own depending on the best for us and worst
 * for others
 */
choosesuspectplayer(T,P) 
   :- not(knowledge(T,P,owned)), self(S), not(knowledge(S,P,owned)),
      playersequence(S,T,SEQ), cardunownedbyplayers(SEQ,P).

/**
 * choosesuspectlocation(T,L)
 * True if L should be suggested as the murder location to obtain new
 * knowledge for player T. L should be suggested if it is not owned by anyone
 * to the current player's left up to and including the target T. If we do not
 * have a player culprit, the current player must not own L. Otherwise, L can be
 * the culprit or something we own depending on the best for us and worst
 * for others
 */
choosesuspectlocation(T,L) 
   :- not(knowledge(T,L,owned)), self(S), not(knowledge(S,L,owned)),
      playersequence(S,T,SEQ), cardunownedbyplayers(SEQ,L).

/**
 * choosesuspectweapon(T,W)
 * True if W should be suggested as the muder weapon to obtain new knowledge for
 * player T. W should be suggested if it is not owned by anyone to the
 * current player's left up to and including the target T. If we do not have
 * a player culprit, the current player must not own W. Otherwise, W can be
 * the culprit or something we own depending on the best for us and worst
 * for others
 */
choosesuspectweapon(T,W) 
   :- not(knowledge(T,W,owned)), self(S), not(knowledge(S,W,owned)),
      playersequence(S,T,SEQ), cardunownedbyplayers(SEQ,W).
       
