# block-battle
Haskell starter bot for AI Block Battle (Beta)
<br>
<br>
http://theaigames.com/competitions/ai-block-battle
<br>
<br>
Implemented as a StateT (easy to incrementally update the game
state when receiving updates from the admin script) monad with
IO as inner monad (easier to debug when developing...). A hook
called handleAction can be used to analyse the game state and
respond to an action with a list of moves.
