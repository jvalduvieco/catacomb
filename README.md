Catacomb
========
This is a game full of rooms that can be played from the web browser. It is inspired in the old MUD games that we used to play.
For us it is an excuse to learn Erlang so don't take it too seriously, we are in the early stages of Erlang. 

This game is developed by:

* Daniel Codina [@dcodix](https://twitter.com/dcodix)
* Jordi Llonch [@jordillonch](https://twitter.com/jordillonch)
* Joan Valduvieco [@jvalduvieco](https://twitter.com/jvalduvieco)

## Screenshot

![Screenshot](https://raw.github.com/jvalduvieco/catacomb/master/apps/catacomb_core/priv/screenshot.png "Screenshot")


## Using catacomb


git clone https://github.com/jvalduvieco/catacomb.git

cd catacomb/apps/catacomb_core

./rebar get-deps

./rebar compile

Now you need to create MySQL tables and load some fixtures.

cat priv/catacomb.sql |mysql

If all goes without error just type:

erl -pa ebin deps/*/ebin -boot catacomb_core

Now you should be able to point your browser to: 

http://localhost:8081

And login as:
jordi // pass
(You can add users directly to MySQL)

Load a character, and play!!!

## Comments and help are welcome. :)