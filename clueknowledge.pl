/**
 * Clue assistant in prolog.
 *
 * *************************************
 * This file manages all knowledge for all players in clue, including
 * global global facts and knowledge for the game, and defines procedures
 * that deal with global knowledge inferences.
 * Global knowledge is knowledge that any player has access to.
 */

:- module(clueknowledge,
          [player/1, location/1, weapon/1, numcardsunknownforplayer/2,
           handleforall/3, handleunanswered/4, parseplayer/1,
           parselocation/1, parseweapon/1, parsenumcards/2]).

:- reexport(knowledgeinference).
:- reexport(knowledgeutils).

:- use_module(utils).

/**
 * Knowledge representation scheme
 * 
 * player(P), location(L), weapon(W) indicated the type of cards P, L, and W
 * numcardsunknownforplayer(P,N) indicates P has N unknown cards on his hand
 *
 * INVARIANTS:
 *  1) player(P) is arranged with current player first, with each subsequent
 *     player in clockwise direction.
 *  2) All cards of this Clue game are in one of and only one of
 *     player(P), location(L), or weapon(W)
 */
:- dynamic player/1.
:- dynamic location/1.
:- dynamic weapon/1.
:- dynamic numcardsunknownforplayer/2.

/**
 * handleunanswered(Ps, L, P, W)
 * Add and infer knowledge of people Ps did not respond to location L,
 * person P, weapon W
 */
handleunanswered(Ps,L,P,W)
   :- handleforall(Ps,L,unowned),
      handleforall(Ps,P,unowned), handleforall(Ps,W,unowned).

/**
 * handleforall(Ps,C,owned|unowned)
 * Asserts that all P in Ps owned|unowned C
 */
handleforall([],_,_).
handleforall([P|Ps],C,owned)
   :- handleowned(P,C), handleforall(Ps,C,owned).
handleforall([P|Ps],C,unowned)
   :- handleunowned(P,C), handleforall(Ps,C,unowned).

/**
 * parseplayer(Ps)
 * Adds knowledge that all P in Ps as a player for clue
 */
parseplayer([]).
parseplayer([P|Ps]) :- assertnodup(player(P)), parseplayer(Ps).

/**
 * parselocation(Ls)
 * Adds knowledge that all L in Ls as a location for clue
 */
parselocation([]).
parselocation([L|Ls]) :- assertnodup(location(L)), parselocation(Ls).

/**
 * parseweapon(Ws)
 * Adds knowledge that all W in Ws as a weapon for clue
 */
parseweapon([]).
parseweapon([W|Ws]) :- assertnodup(weapon(W)), parseweapon(Ws).

/**
 * parsenumcards(N, P)
 * Adds knowledge that player P has N cards
 */
parsenumcards(N,P) :- assertnodup(numcardsunknownforplayer(P,N)).

