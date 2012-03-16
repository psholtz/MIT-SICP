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

The Two-Player Prisoner's Dilemma Program
----------------------------------------- 