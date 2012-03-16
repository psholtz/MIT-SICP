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
             |                 |                |
A cooperates |  A => 5, B => 5 | A => 2, B => 3 |
             |                 |                |
-------------------------------------------------
             |                 |                |
A defects    |  A => 3, B => 2 | A => 1, B => 1 |
             |                 |                |
-------------------------------------------------
</pre>

Let's examine a simple game, and let's begin by examining the situation from Player **A**'s point of view (Player **B**'s point of view is identical). Let's also transition to the terminology used in this programming exercise, i.e., "cooperate" and "defect" instead of "testify" and "remain silent":

*"Suppose **B** cooperates (i.e., testifies). Then I do better by cooperating myself (I receive [xxx] instead of [xx]). On the other hand, suppose **B** defects. I still do better by cooperating (since I get two points instead of one). So no matter what **B** does, I am better off cooperating."*

Player **B** will, of course, reason the same way, and both will choose to cooperate. In the terminology of game theory, both **A** and **B** have a *dominant* choice - i.e., a choice that gives a preferred outcome no matter what the other player chooses to do. The matrix shown above, by the way, does *not* represent a prisoner's dilemma situation, since when both players make their dominant choice, they also both achieve their highest personal scores. We'll see an example of a prisoner's dilemma game very shortly.

*To re-cap:* in any particular game using the matrix above, we would expect both players to cooperate; and in an iterated game, we would expect both players to cooperate repeatedly, on every round.

