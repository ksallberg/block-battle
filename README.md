# block-battle
Haskell starter bot for AI Block Battle (Beta)
<br/>
<br/>
http://theaigames.com/competitions/ai-block-battle
<br/>
Let's create a starter bot (as exists for Java, C#, Scala):
http://theaigames.com/competitions/ai-block-battle/getting-started
<br/>
<br/>
Implemented as a StateT (easy to incrementally update the game
state when receiving updates from the admin script) monad with
IO as inner monad (easier to debug when developing...). A hook
called handleAction can be extended by users of this starter
bot, to analyse the parsed game state and respond to actions
with a list of moves.
