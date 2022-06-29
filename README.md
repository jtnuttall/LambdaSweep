# LambdaSweep

Basic implementation of Minesweeper in Haskell in about 750 sloc.

I had the idea to redo the older version I did in college for portfolio purposes over the course of a weekend.

* The game logic itself is arrowized and implemented using `dunai`
* Uses basic synchronization primitives to create a very basic sort of wormhole.

The presentational work could be improved significantly.

Two distinct games: one loss, then restart, then one win:

![Demo](https://user-images.githubusercontent.com/15227488/175989684-568f792e-62f6-46e8-887e-a1a765d31caf.gif)
