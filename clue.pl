/**
 * *************************************
 * Clue assistant in prolog.
 * Run the program with: main.
 * *************************************
 */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**
 * The predicates and procedures defined below are for running
 * and managing the main UI. This file contains the main function
 * and the main clue algorithm. All supported features and related
 * UI procedures are defined below, with the inference functions
 * defined in their appropriate file.
 */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(clue, [main]).

/**
 * Load our definition and clauses for clue
 */
:- use_module(clueknowledge).
:- use_module(gamestrategy).

/**
 * The main start function for clue.
 */
main :-
   writeln('Welcome to Clue Assistant!'),
   writeln('We will help you keep track and win Clue!'),
   writeln('Please press y to start a new game, or q to quit.'),
   writeln('All inputs are required to be followed by a "."'),
   read(X), nl, clue(X).

/**
 * The clue assistant algorithm.
 */
clue(q) :- !.
clue(y) :- !, startsequence,
           actionsequence.
clue(IN) :- !, actionsequence(IN).

/**
 * Start sequence to set up clue game state
 */
startsequence :-
   write('Please enter the list of players in the game starting with '),
     write('yourself and then going around the circle clockwise, beginning'),
     write(' with the player to your left, in format: [p1, p2, ..]'), nl,
   read(P), nl, parseplayer(P),
   write('Please enter the list of locations in the game '),
     write('in format [l1, l2, ..]'), nl,
   read(L), nl, parselocation(L),
   write('Please enter the list of weapons in the game '),
     write('in format [w1, w2, ..]'), nl,
   read(W), nl, parseweapon(W),
   readparsenumcardsforplayer(P),
   writeln('Please enter the cards in your hand in format [c1, c2..]'),
   read(CS), nl, self(CP), parseplayercards(CP,CS).

/**
 * The recursive sequence that plays and keep track of the game
 */
actionsequence
   :- writeln('***************** CLUE ASSISTANT ****************'),
      writeln('Please enter one of the following options:'),
      writeln(' 1. for suggestion on what question you should ask.'),
      write(' 2. to input a question asked by you with its '),
        write('corresponding answer.'), nl,
      write(' 3. to input a question asked by another player with '),
        write('its corresponding answer.'), nl,
      write(' 4. to query for a list of suspected players, '),
        write('locations, and weapons.'), nl,
      write(' 5. to see the contents of the database. '), nl,
      writeln(' q. to quit.'),
      read(IN), nl, clue(IN).

/**
 * The recursive sequence that calls on the features available
 */
actionsequence(1) :- !, suggestquestion, nl, actionsequence.
actionsequence(2) :- !, parsequestion, nl, actionsequence.
actionsequence(3) :- !, parseplayersquestions, nl, actionsequence.
actionsequence(4) :- !, queryculprits, nl, actionsequence.
actionsequence(5) :- !, databasesequence, nl, actionsequence.
actionsequence(_)
   :- !, writeln('=================== Response ==================='),
      writeln('Unknown option'), nl, actionsequence.

/**
 * Manages the UI for asking the number of cards each player has
 */
readparsenumcardsforplayer([]).
readparsenumcardsforplayer([H|T])
   :- write('Please enter number of cards for: '), write(H), nl,
      read(N), nl, parsenumcards(N,H), readparsenumcardsforplayer(T).   

/**
 * Manages the UI and calls on the appropriate function to add knowledge
 * for a question and response that the current player asked
 */
parsequestion
   :- writeln('Please enter the location that you suspected:'),
      read(L), nl,
      writeln('Please enter the person that you suspected:'),
      read(P), nl,
      writeln('Please enter the weapon that you suspected:'),
      read(W), nl,
      writeln('Please enter y if a player answered your question, else n:'),
      read(YN), nl,
      assertnodup(question(L,P,W)),
      handlequestionresponse(YN, L, P, W).                   

/**
 * Manages the UI and calls on the appropriate function to add knowledge
 * for a question and response that another player asked
 */
parseplayersquestions
   :- writeln('Please enter the player who asked the question:'),
      read(PL), nl,
      writeln('Please enter the location that the player suspected:'),
      read(L), nl,
      writeln('Please enter the person that the player suspected:'),
      read(P), nl,
      writeln('Please enter the weapon that the player suspected:'),
      read(W), nl,
      writeln('Please enter y if a player answered the question, else n:'),
      read(YN), nl,
      assertnodup(question(L,P,W)),
      handleplayerquestionresponse(PL,YN,L,P,W).

/**
 * Procedure to handle question when someone showed the current
 * player a card
 */
handlequestionresponse(y, L, P, W)
   :- writeln('Please enter the player who responded to your question:'),
      read(PR), nl,
      writeln('Please enter the card he showed you:'),
      read(C), nl,
      self(CP), playersequence(CP,PR,PS), handleunanswered(PS, L, P, W),
      handleowned(PR,C),
      writeln('=================== Response ==================='),
      writeln('The new information has been added.'), nl,
      generateculprits, checkculprits.

/**
 * Procedure to handle question when no one showed the current
 * player a card
 */
handlequestionresponse(n, L, P, W)
   :- self(CP), playersequence(CP,CP,PS), handleunanswered(PS, L, P, W),
      writeln('=================== Response ==================='),
      writeln('The new information has been added.'), nl,
      generateculprits, checkculprits.

/**
 * Procedure to handle when someone showed a card to question asked
 * by another player
 */
handleplayerquestionresponse(PL, y, L, P, W)
   :- writeln('Please enter the player who responded to the question'),
      read(PR), nl,
      playersequence(PL,PR,PS), handleunanswered(PS, L, P, W),
      addpotentialknowledge(PR, [L,P,W], owned),
      writeln('=================== Response ==================='),
      writeln('The new information has been added.'), nl,
      generateculprits, checkculprits.

/**
 * Procedure to handle when no one showed a card to question asked
 * by another player
 */
handleplayerquestionresponse(PL, n, L, P, W)
   :- playersequence(PL,PL,PS), handleunanswered(PS, L, P, W),
      writeln('=================== Response ==================='),
      writeln('The new information has been added.'), nl,
      generateculprits, checkculprits.


/**
 * Checks and prints the culprit if they are certain
 *
 */
checkculprits
   :- checkplayerculprit(P), checklocationculprit(L),
      checkweaponculprit(W), !,
      writeln('******************* ATTENTION ********************'),
      writeln('We know the culprits!'),
      write('The player culprit is: '), write(P),
      write('. The location culprit is: '), write(L),
      write('. The weapon culprit is: '), write(W), nl,
      writeln('Press any key to continue:'), read(_);
      !.

/**
 * Query and prints all possible culprits for each category.
 */
queryculprits
   :- querypossibleplayerculprits(P), querypossiblelocationculprits(L),
      querypossibleweaponculprits(W),
      writeln('=================== Response ==================='),
      write('Possible player culprits are: '), write(P), nl,
      write('Possible location culprits are: '), write(L), nl,
      write('Possible weapon culprits are: '), write(W), nl.

/**
 * Prints the suggested question to ask
 */
suggestquestion
   :- suggestquestion(P,L,W), !,
      writeln('=================== Response ==================='),
      write('You should suggest that: '), write(P), nl,
      write('committed the murder at the location: '), write(L), nl,
      write('with weapon: '), write(W), nl;
      !, writeln('You already know the culprit!'), nl.

/**
 * Prints out all the features for the database options
 */
databasesequence
   :- write('***************** CLUE DATABASE *****************'), nl,
      write('Please choose one of the following options by entering a '),
        write('number:'), nl,
      write('1. to see all the player cards.'), nl,
      write('2. to see all the location cards.'), nl,
      write('3. to see all the weapon cards.'), nl,
      write('4. to view all the knowledge stored in the database'), nl,
      write('5. to return to CLUE.'), nl,
      read(IN), nl, databasesequence(IN). 

databasesequence(1) :- !, showplayercards, nl, databasesequence.
databasesequence(2) :- !, showlocationcards, nl, databasesequence.
databasesequence(3) :- !, showweaponcards, nl, databasesequence.
databasesequence(4) :- !, knowledgesequence, nl, databasesequence.
databasesequence(5) :- !.
databasesequence(_)
   :- !, writeln('=================== Response ==================='),
      writeln('Unknown option'), nl, databasesequence.

knowledgesequence
   :- write('***************************************************'), nl,
      write('Please enter the player that you want to display knowledge'),
        write(' for'), nl,
      read(P), nl,
      write('Please enter 1. to view knowledge or 2. to view potential '),
        write('knowledge'), nl,
      read(NUM), nl, showknowledgesequence(P,NUM).

showknowledgesequence(P,1) :- !, showallknowledge(P).
showknowledgesequence(P,2) :- !, showallpotentialknowledge(P).
showknowledgesequence(_,_)
   :- !, writeln('=================== Response ==================='),
      writeln('Unknown option'), nl, knowledgesequence.

/**
 * showplayercards.
 * Writes to screen all the characters cards in the current game of CLUE.
 */
showplayercards
   :- writeln('==================== Results ===================='),
      allplayers(Ps), write('The players in this game are: '), 
      nl, displayallplayers(Ps), nl. 

/**
 * showlocationcards.
 * Writes to screen all the location cards in the current game of CLUE.
 */
showlocationcards
   :- writeln('==================== Results ===================='),
      alllocations(Ls), write('The locations in this game are: '),
      nl, displayalllocations(Ls), nl. 

/**
 * showweaponcards.
 * Writes to screen all the weapon cards in the current game of CLUE.
 */
showweaponcards
   :- writeln('==================== Results ===================='),
      allweapons(Ws), write('The weapons in this game are: '),
      nl, displayallweapons(Ws), nl. 

/**
 * displayallplayers(Ps)
 * writes to screen all the players Ps
 */
displayallplayers([]).
displayallplayers([P|Ps])
   :- write(P), write(', '), displayallplayers(Ps). 

/**
 * displayalllocaitons(Ls)
 * writes to screen all the locations Ls
 */
displayalllocations([]).
displayalllocations([L|Ls])
   :- write(L), write(', '), displayalllocations(Ls).

/**
 * displayallweapons(Ws)
 * writes to screen all the weapons Ws
 */
displayallweapons([]).
displayallweapons([W|Ws])
   :- write(W), write(', '), displayallweapons(Ws).

/**
 * showallknowledge(P)
 * Prints to screen all the knowledge we have about player P
 */
showallknowledge(P) 
   :- allownedbyplayer(P,Os), allunownedbyplayer(P,Us),
      writeln('==================== Results ===================='),
      write('We know that player '), write(P), write(' owns:'), nl,
        displayknowledgeowned(Os), nl,
      write('We know that player '), write(P), write(' does not own:'), nl,
        displayknowledgeunowned(Us), nl.

/**
 * displayknowledgeowned(Ks)
 * prints to screen all the knowledge 
 */
displayknowledgeowned([]).
displayknowledgeowned([C|Cs]) 
   :- write(C), write(', '), displayknowledgeowned(Cs).  

/**
 * displayknowledgeunowned(Ks)
 * prints to screen all the knowledge 
 */
displayknowledgeunowned([]).
displayknowledgeunowned([C|Cs]) 
   :- write(C), write(', '), displayknowledgeunowned(Cs). 

/**
 * showallpotentialknowledge(P)
 * prints to screen the potential knowledge that we have about player P
 */
showallpotentialknowledge(P)
   :- allpotentialknowledgeforplayer(P,Cs),
      writeln('==================== Results ===================='),
      write('Player '), write(P), 
        write(' potentially owns one card from each of these sets:'), nl, 
      displaypotentialknowledge(Cs).

/**
 * displaypotentialknowledge(Cs)
 * prints to screen all the potential knowledge 
 */
displaypotentialknowledge([]).
displaypotentialknowledge([C|Cs]) 
   :- write(C), write(', '), displaypotentialknowledge(Cs). 
