Yifan (Bob) Yang, Frank Rui

== INSTRUCTIONS ==
1) Compile clue.pl with prolog. All other files will be automatically loaded.
2) Type main. to run clue.
(Our source files are organized into modules, but they will load automatically
and no external modules other than ones we've written are used)

== OVERVIEW OF FUNCTIONALITY ==
Clue assistant will aid a player keep track and make inferences of the
cards of a clue game, and identify the culprits when known. The players,
locations, weapons of the game are dynamic, so it will adjust to any version.
Important features include the ability for the user to input questions
he asked and the response, and the ability for the user to input questions
other people asked and who, if any player, has answered. The user can prompt
for suggestions from Clue assistant for questions to ask to infer the most
useful information and knowlege. The user will have to options to
check knowledge of database at all times, query lists of possible culprits,
look up the list of players, weapons, and locations of the game. At any point
of the game, if the clue assistant deduced the culprit, it will immediately notify
the user of the culprits.

== SPECIAL FEATURES ==
Question Suggestion:
Clue assistant features a question suggestion option to help users determine
what to ask next to aid the clue assistant the most in inferring the culprits
of the game. Our algorithm chooses a "target" player rightmost of the current player
for whom we do not already know all his hand cards for, as this will provide us with
the most knowledge gain. The algorithm then generates a player, weapon, and location
card that the "target" is not known to own, and everyone from the user, to the left,
up to the target does not own (otherwise the question will be intercepted). The idea
of using the rightmost player as the target is that we have the potential to gain
knowledge regarding all players from the user's left up to the target as not owning
the cards in the question, which helps us determine the culprit. The more players we
pass, the more we know, so we choose the right most player as the target.

Tracking All Information:
We also included features where we keep track of all questions asked by all players,
including question responses that the user does not see. We are still able to reason
and infer with this information to quickly determin the culprits, so we make use of
any information possible (see Complex Reasoning Algorithm)

Complex Reasoning Algorithm
Clue assistant uses a complex reasoning and inferring algorithm for to deduce as
much knowledge as it can.
The algorithm can reason the following sitations:
 -once one player owns a card, no other player can own the card
 -the number of cards a player owns cannot exceed the number of cards in his hand
 -if all except one card of each type (player, location, weapon) are owned, then
  that card not owned must be the culprit, and no one else can own it.
    -we can then deduce the culprit without knowing all cards in all player's hand
 -when we see other players showing other players cards, the cards must be one
  of the 3 in the question in a way such that to satisfy all the questions he
  responded to, the number of cards he owns must not exceed the nubmer of cards
  in his hand. We are then able to prune the cards that he possibly owns.
    -if we know he owns or doesn't any of these cards, we can prune the possiblilities
     down.
    -we eventually resolve the potential knowledge when there is only one possibility.

Our algorithm considers the concrete knowledges as knowledge, and possible cards a
player owns as potential knowledge. When a new knowledge is added, our algorithm
will attempt to resolve potential knowledges for all players through the reasoning
described above and deduce knowledges. If any new knowledge is generated,
the algorithm will recursively resolve more potential knowledges with the newly
generated knowledge. When adding new potential knowledges, the algorithm will use
the knowledge already given, number of cards a player has, number of cards and the 
type of card to resolve potential knowledge as much as possible at all times, to
prune away possibilities and arrive at a knowledge, which will be added recursively
as the process above.

