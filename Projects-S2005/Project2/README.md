Project 2 - Prisoner's Dilemma
============================== 

A prisoner's dilemma always involves two "game players", and each has a choice between "cooperating" and "defecting". If the two players cooperate, they each do moderately well; if they both defect, they each do moderately poorly. If one player cooperates and the other defects, then the defector does extremely well and the cooperator does extremely poorly. Before formalizing the prisoner's dilemma situation, we need to introduce some basic game theory notation.

[![](http://farm7.static.flickr.com/6147/5958316504_84bd9a0355.jpg)](http://farm7.static.flickr.com/6147/5958316504_84bd9a0355.jpg)

Crash Course in Game Theory
--------------------------- 

In game theory, we differentiate between a *game* and a *play*. A *game* refers to the set of possible choices and outcomes for the entire range of situations. A *play* refers to a specific set of choices by the players, with the associated outcome for that particular scenario. Thus, in game theory, a *two-person binary-choice* game is represented by a two-by-two matrix as above.

The two players in this case are called **A** and **B**, and the choices are called "silent" and "testify". Players **A** and **B** can play a single game by separately (and secretly) choosing either to testify or remain silent. Once each player has made a choice, he announces it to the other player; and the two then look up their respective scores in the game matrix. Each entry in the matrix is a pair of numbers indicating a score for each player, depending on their choices. Thus, in the example above, if Player **A** chooses to testify while Player **B** remains silent, then **A** goes free while **B** gets 10 years. If both players remain silent, they each get 6 months. Note, by the way, that the game matrix is a matter of public knowledge, for instance, Player **A** knows before the game even starts that if he and **B** both choose to testify, they will each get 5 years.

In an *iterated game*, the two players play repeatedly; thus after finishing one game, **A** and **B** may play another. (Admittedly, there is a little confusion in the terminology here; thus we refer to each iteration as a "play", which constitutes a single "round" of the larger, iterated game). There are a number of ways in which iterated games may be played; in the simplest situation, **A** and **B** play for some fixed number of rounds (say 200), and before each round, they are able to look at the record of all previous rounds. For instance, before playing the tenth round of their iterated game, both **A** and **B** are able to study the results of the previous nine rounds.

An Analysis of a Simple Game Matrix
----------------------------------- 

Let's examine a simple game, represented by the following game matrix:

<pre>
               ------------------------------------ 
               |                 |                |  
               |   B cooperates  |    B defects   |
               |                 |                |
------------------------------------------------- 
|              |                 |                |
| A cooperates |  A => 5, B => 5 | A => 2, B => 3 |
|              |                 |                |
-------------------------------------------------
|              |                 |                |
| A defects    |  A => 3, B => 2 | A => 1, B => 1 |
|              |                 |                |
-------------------------------------------------
</pre>

Let's examine a simple game, and let's begin by examining the situation from Player **A**'s point of view (Player **B**'s point of view is identical). Let's also transition to the terminology used in this programming exercise, i.e., "cooperate" and "defect" instead of "testify" and "remain silent":

"Suppose **B** cooperates (i.e., testifies). Then I do better by cooperating myself (I receive [xxx] instead of [xx]). On the other hand, suppose **B** defects. I still do better by cooperating (since I get two points instead of one). So no matter what **B** does, I am better off cooperating."

Player **B** will, of course, reason the same way, and both will choose to cooperate. In the terminology of game theory, both **A** and **B** have a *dominant* choice - i.e., a choice that gives a preferred outcome no matter what the other player chooses to do. The matrix shown above, by the way, does *not* represent a prisoner's dilemma situation, since when both players make their dominant choice, they also both achieve their highest personal scores. We'll see an example of a prisoner's dilemma game very shortly.

**To re-cap:** in any particular game using the matrix above, we would expect both players to cooperate; and in an iterated game, we would expect both players to cooperate repeatedly, on every round.

The Prisoner's Dilemma Game Matrix
---------------------------------- 

Now consider the following game matrix:

<pre>
               ------------------------------------ 
               |                 |                |  
               |   B cooperates  |    B defects   |
               |                 |                |
------------------------------------------------- 
|              |                 |                |
| A cooperates |  A => 3, B => 3 | A => 0, B => 5 |
|              |                 |                |
-------------------------------------------------
|              |                 |                |
| A defects    |  A => 5, B => 0 | A => 1, B => 1 |
|              |                 |                |
-------------------------------------------------
</pre>

In this case, Players **A** and **B** both have a dominant choice - namely, defection. No matter what Player **B** does, Player **A**  improves his own score by defecting, and vice versa. 

However, there is something odd about this game. It seems as though the two players would benefit by choosing the cooperate. Instead of winning only one point each, they could win three points each. So the "rational" choice of mutual defection has a puzzling self-destructive flavor.

The second matrix is an example of a prisoner's dilemma game situation. Just to formalize the situation, let CC be the number of points won by each player when they both cooperate; let DD be the number of points won when boht defect; let CD be the number of points won by the cooperating party when the other defects; and let DC be the number of points won by the defecting party when the other cooperates. Then the prisoner's dilemma situation is characterized by the following conditions:

<pre>
DC > CC > DD > CD

CC > (DC + CD) / 2
</pre>

In the second game matrix, we have:

<pre>
DC = 5, CC = 3, DD = 1, CD = 0
</pre>

so both conditions are met. In the Bunny and Clod story, by the way, you can verify that:

<pre>
DC = 0, CC = -1, DD = -10, CD = -20
</pre>

Again, these values satisfy the prisoner's dilemma conditions.

Alexrod's Tournament
-------------------- 

In the late 1970s, political scientist Robert Alexrod held a computer tournament designed to investigate the prisoner's dilemma situation (Actually, there were two tournaments. Their rules and results are described in Axelrod's book: *The Evolution of Cooperation*). Contestants in the tournament submitted computer programs that would compete in an iterated prisoner's dilemma game of approximately two hundred rounds, using the second matrix above. Each contestant's program played five iterated games against each of the other programs submitted, and after all games had been played the scores were tallied.

The contestants in Axelrod's tournament included professors of political science, mathematics, computer science, and economics. The winning program - the program with the higest average score - was submitted by Anatol Rapoport, a professor of pscyhology at the University of Toronto. In this project, we will pursue Alexrod's investigations and make up our own Scheme programs to play the iterated prisoner's dilemma game.

As part of this project, we will be running a similar tournament, but now involving a three-person prisoner's dilemma.

Before we look at the two-player program, it is worth speculating on what possible strategies might be employed in the iterated prisoner's dilemma game. Here are some examples:

**Nasty** - a program using the **Nasty** strategy simply defects on every round of every game.

**Patsy** - a program using the **Patsy** strategy cooperates on every round of every game.

**Spastic** - this program cooperates or defects on a random basis.

**Egalitarian** - this program cooperates on the first round. On all subsequent rounds, **Egalitarian** examines the history of the other player's actions, counting the total number of defections and cooperations by the other player. If the other player's defections outnumber her cooperations, **Egalitarian** will defect; otherwise this strategy will cooperate.

**Eye-for-Eye** - this program cooperates on the first round, and then on every subsequent round it mimics the other player's previous move. Thus, if the player cooperates (defects) on the nth round, then **Eye-for-Eye** will cooperate (defect) on the (n+1)th round.

All of these strategies are extremely simple. (Indeed, the first three do not even pay any attention to the other player; their responses are uninfluenced by the previous rounds of the game). Nevertheless, simplicity is not necessarily a disadvantage. Rapoport's first-prize program employed the **Eye-for-Eye** strategy, and achieved the highest average score in a field of far more complicated programs.

The Two-Player Prisoner's Dilemma Program
----------------------------------------- 
A Scheme program for an iterated prisoner's dilemma game is provided as part of the code for this project. The procedure "play-loop" pits two players (or, to be more precise, two "strategies") against one another for approximately 100 games, then prints out the average score of each player.

Player strategies are represented as procedures. Each strategy takes two inputs - its own "history" (that is, a list of all its previous "plays", where for convenience we will use "c" to represent cooperate, and "d" to represent defect) and its opponent's "history". The strategy returns either the string "c" for "cooperate" or the string "d" for "defect". (Note that we will need to use procedures appropriate for comparing strings when we analyze these results).

At the beginning of an iterated game, each history is an empty list. As the game progresses, the histories grow (via "extend-history") into lists of "c"s and "d"s, thus each history is stored from most recent to least recent. Note how each strategy must have its own history as its first input. So in "play-loop-iter", "strat0" has "history0" as its first input, and "strat1" has "history1" as its first input.

The values from the game matrix are stored in a list named "game-association-list". This list is used to calculate the scores at the end of the iterated game.

<pre>
(define *game-association-list*
 (list (list (list "c" "c") (list 3 3))
       (list (list "c" "d") (list 0 5))
       (list (list "d" "c") (list 5 0))
       (list (list "d" "d") (list 1 1)))) 
</pre>

Thus if both players cooperate, the payoff to each player is 3, if one player cooperates and the other defects, the defecting player gets a payoff of 5, the cooperating player gets a zero payoff, if both players defect, each gets a payoff of 1.

Some simple strategies are given in the code. "Nasty" and "Patsy" are particularly simple: each returns a constant value regardless of the histories. "Spastic" also ignores the histories and chooses randomly between cooperation and defection. You should study "Egalitarian" and "Eye-for-Eye" to see that their behavior is consistent with the descriptions in the previous section.

Problem 1
--------- 

To be able to test out the system, we need to complete a definition for "extract-entry". This procedure will retrieve the payoff information from the game association list. The procedure's behavior is as follows: it takes as input a play, represented as a list of choices for each strategy (i.e., a "c" or "d") and the game association list. Thus a play will in this case be a list of two entries (since there are two players), each of which is the choice of action for that player. Each entry in the game association list is a list itself, with a first element representing a list of game choices, and the second element representing a list of scores (or payoffs) for each player. Thus "extract-entry" wants to search down the game association list trying to match its first argument against the first element of each entry in the game association list, one by one. When it succeeds, it returns that whole entry.

For example, we expect the following behavior:

<pre>
(define a-play (make-play "c" "d"))

(extract-entry a-play *game-association-list*)
;Value: (("c" "d") (0 5))
</pre>

Write the procedure "extract-entry" and test it out using the above case *game-assocation-list*. Turn in a copy of your documented procedure and some test examples. You may want to use a diagram of the list structure to guide the creation of your code.

Problem 2
--------- 

Use "play-loop" to play games among the five defined strategies. Notice how a strategy's performance varies sharply depending on its opponent. For example, "Patsy" does quite well against "Eye-for-Eye" or against another "Patsy", but it loses badly against "Nasty". Pay special attention to "Eye-for-Eye". Notice how it never beats its opponent -- but it never loses badly. Create a matrix in which you show the average score for tournaments pitting all possible pairings of the five different strategies: "Nasty", "Patsy", "Eye-for-Eye", "Spastic", "Egalitarian". Describe the behavior you observe for the different strategies.

Problem 3
--------- 

Games involving "Egalitarian" tend to be slower than other games. Why is that so? Use order-of-growth notation to explain your answer.

Alyssa P. Hacker, upon seeing the code for "Egalitarian", suggested the following iterative version of the procedure:

<pre>
(define (Egalitarian my-history other-history)
  (define (majority-loop cs ds hist)
    (cond ((empty-history? hist) (if (> ds cs) "d" "c"))
          ((string=? (most-recent-play hist) "c")
           (majority-loop (+ 1 cs) ds (rest-of-plays hist)))
          (else
           (majority-loop cs (+ 1 ds) (rest-of-plays hist)))))
  (majority-loop 0 0 other-history))
</pre>

Compare this procedure with the original version. Do the orders of growth (in time) for the two procedures differ? Is the newer version faster?

Problem 4
--------- 

Write a new strategy "eye-for-two-eyes." The strategy should always cooperate unless the opponent defected on both of the previous two rounds. (Looked at another way: "eye-for-two-eyes" should cooperate if the opponent cooperated on either of the previous two rounds). Play "eye-for-two-eyes" against other strategies. Describe the behavior you observe.

Problem 5
--------- 

Write a procedure "make-eye-for-n-eyes." This procedure should take a number as input and return the appropriate "Eye-for-eye"-like strategy. For example, "(make-eye-for-n-eyes 2)" should return a strategy equivalent to "eye-for-two-eyes." Use this procedure to create a new strategy and test it against the other strategies. Describe the observed behavior.

Problem 6
--------- 

Write a procedure "make-rotating-strategy" which takes as input two strategies (say, "strat0" and "strat1"), and two integers (say "freq0" and "freq1"). "Make-rotating-strategy" should return a strategy which plays "strat0" for the first "freq0" rounds in the iterated game, then switches to "strat1" for the next "freq1" rounds, and so on. (Hint: you may find it useful to think about the "remainder" procedure in order to decide which strategy to use at each iteration). Test it against other strategies and describe the performance.

Problem 7
--------- 

Write a new strategy, "make-higher-order-spastic", which takes a list of strategies as input. It returns a new strategy that loops through this list of strategies, using the next one in the list for each play, and then starting against the beginning of the list when it has used all the strategies. Test this new strategy against other strategies and describe the performance.

Problem 8
--------- 

Write a procedure "gentle", which takes as input a strategy (say "strat"), and a number between 0 and 1 (call it "gentleness-factor"). The "gentle" procedure should return a strategy that plays the same as "strat" except: when "strat" defects, the new strategy should have a "gentleness-factor" chance of cooperating. (If "gentleness-factor" is 0, the return strategy performs exactly the same as "strat"; if "gentleness-factor" is 0.5, the returned strategy cooperates half the time that "strat" defects; if "gentleness-factor" is 1, the returned strategy performs the same as "Patsy").

Use "gentle" with a low value for "gentleness-factor" -- say 0.1 -- to create two new strategies: "slightly-gentle-Nasty" and "slightly-gentle-Eye-for-Eye".

The Three-Player Prisoner's Dilemma
----------------------------------- 

So far, all of our prisoner's dilemma examples have involved two players (and, indeed, most game-theory research on the prisoner's dilemma has focused on two-player games). But it is possible to create a prisoner's dilemma game involve three -- or even more -- players.

Strategies from the two-player game do not necessarily extend to a three-person game in a natural way. For example, what does EYE-FOR-EYE mean? Should the player defect if *either* of the opponents defected on the previous round? Or only if *both* opponents defected? And are either of these strategies nearly as effective in the three-player game as EYE-FOR-EYE is in the two-player game?

Before we analyze the three-player game more closely, we must introduce some notation for representing the payoffs. We use a notation similar to that used for the two-player game. For example, we let DCC represent the payoff to a defecting player if both opponents cooperate. Note that the first position represents the player under consideration. The second and third positions represent the opponents.

Another example: CCD represents the payoff to a cooperating player if one opponent cooperates and the other opponent defects. Since we assume a symmetric game matrix, CCD could be written as CDC. The choice is arbitrary.

Now we are ready to discuss the payoffs for the three-player game. We impose three rules (Actually, there is no universal definition for the multi-player prisoner's dilemma. The constraints used here represent one possible version of the three-player prisoner's dilemma).

1) Defection should be the dominant choice for each player. In other words, it should always be better for a player to defect, regardless of what the opponents do. This rule gives three constraints:

<pre>
DCC &gt; CCC

DDD &gt; CDD

DCD &gt; CCD
</pre>

2) A player should always be better off if more of his opponents choose to cooperate. This rule gives:

<pre>
DCC &gt; DCD &gt; DDD

CCC &gt; CCD &gt; CDD
</pre>

3) If one player's choice is fixed, the other two players should be left in a two-player prisoner's dilemma. This rule gives the following constraints:

<pre>
CCD &gt; DDD

CCC &gt; DCD

CCD &gt; (CDD + DCD)/2

CCC &gt; (CCD + DCC)/2
</pre>

We can satisfy all of these constraints with the following payoffs:

<pre>
CDD = 0, DDD = 1, CCD = 2, DCD = 3, CCC = 4, DCC = 5
</pre>

Problem 9
---------

Revise the Scheme code for the two-player game to make a three-player iterated game. The program should take three strategies as input, keep track of three histories, and print out results for three players. You need to change only three procedures: **play-loop**, **print-out-results** and **get-scores** (although you may also have to change your definition of **extract-entry** if you did not write it in a general enough manner). We would suggest that you make copies of the necessary code and rename them so that you can separate the two person version from the three person one.

You also need to change **game-association-list** as follows:

<pre>
(define *game-association-list*
 (list (list (list "c" "c" "c") (list 4 4 4))
       (list (list "c" "c" "d") (list 2 2 5))
       (list (list "c" "d" "c") (list 2 5 2))
       (list (list "d" "c" "c") (list 5 2 2))
       (list (list "c" "d" "d") (list 0 3 3))
       (list (list "d" "c" "d") (list 3 0 3))
       (list (list "d" "d" "c") (list 3 3 0))
       (list (list "d" "d" "d") (list 1 1 1))))
</pre>