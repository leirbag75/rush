
# Overview

This is an implementation of a turn-based RPG battle system I came up
with. (Incidentally, I threw together the unit testing framework
myself too.)

The core idea is that you choose two actions per turn; for each
attack, you earn "momentum" directly proportional to the amount of
damage dealt; and if you gain a particular amount of momentum, then on
your next turn you get to choose three actions instead. This state of
getting three moves on a turn is called "rush mode" (hence the project
name). It's not implemented yet, but I was also planning to make it so
that if your opponent gets to rush mode while you're already in rush
mode, then you lose rush mode in addition to some other penalty.

My personal philosophy is that in a game, it should be easier for the
winning player to stay ahead than for the losing player to turn the
tables. Otherwise, you're punishing the winning player for playing
well. This is balanced by giving the losing player a big bonus if they
do manage to turn the tables.

For an example of a well-designed game, I would point to the Japanese
card game "daifugou." This game proceeds in rounds, where the better
you do in a given round, the better handicap you get in the next
round, making it easier for you to win again. But if you win first
place one round and then fail to win first place the next, then you
immediately fall to last place for that round, effectively bumping the
remaining players up one place in the process.

Or if I were designing Mario Kart, then rather than give the winning
player worse items and the losing player better items, I would make it
so that if a player manages to pass someone up, then they get a
boost. This rewards the losing player for playing well, without
punishing the winning player for playing well.

This battle system is a straightforward example of this philosophy;
inflicting a lot of damage means that you get more actions the next
turn, which makes it easy to inflict a lot of damage again.

# Architecture

The battle object (src/battle.lisp) is in charge of the actual rules
of the game. Any possible changes in the state of the game are
implemented as methods on battle.

Actions on the battle are not performed immediately. Instead, a
corresponding event object is pushed onto an "event-accumulator"
object. The events are then actually performed by calling
commit-changes on battle. Each event is checked for validity before
being performed (e.g., checking that the target is still alive before
inflicting damage). Each event performed is pushed onto another
event-accumulator, which can be queried later for events to show to
the player.

The battle object is currently implemented with the intention of
making the game deck-based, but I'm not sure if I'm going to stick
with this. I'm thinking of giving each character a fixed set of
attacks, like most RPG's, and adding in a "stance" system instead.

The UI is not implemented yet, but it's planned for it to handle menus
and alerting the player of events in the battle. Alerting the player
of events is to be implemented as a multimethod on the UI object and
the event object. The default method will print a textual message, so
this will be the absolute minimum that a UI object has to implement to
work. (Each event class has a predefined textual message template that
it can fill out itself, so the UI object doesn't need to worry about
that part.)

Lisp doesn't have interfaces like e.g. Java, but I grouped related
sets of generic functions into files with names of the form
"___-interface.lisp", just as an organizational tool. It also works as
a to-do list of what methods I need to implement.

Most of the tests are named test/test-(name of file in src),
although this isn't intended to be a hard-and-fast rule at all. The
testing framework is entirely in test/framework.lisp.
