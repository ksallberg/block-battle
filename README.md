# block-battle
Haskell starter bot for AI Block Battle (Beta), parsing the
full supported set of outputs from the game engine.
<br/>
<br/>
http://theaigames.com/competitions/ai-block-battle
<br/>
http://theaigames.com/competitions/ai-block-battle/getting-started
<br/>
<br/>
Set "debug' = True" to enable debug logging
<br/>
Example usage:
```haskell
debug $ putStrLn "message"
```
Use "handleAction" as a hook for adding your AI behaviour. In this
function, use state to access the GameState type that represents
all information the game engine has provided so far.
<br/>
handleAction :: String -> Context()
To access the IO monad from the StateT monad transformer, use:
<br/>
```haskell
liftIO $ putStrLn "some IO"
xyz <- liftIO someReturningIO
```
Implemented as a StateT (easy to incrementally update the game
state when receiving updates from the admin script) monad with
IO as inner monad (easier to debug when developing...).
<br/>
<br/>
URL to the development repo: https://github.com/ksallberg/block-battle
