## alfred-footman

### A library for writing script filters in [Alfred](https://www.alfredapp.com) persistent state

Made as an exercise for myself and my own usage.


Example usage:
``` haskell
import Alfred
import Data.ByteString
import Data.Time.LocalTime

newtype MyState = MyState String

instance AlfStatable MyState where
  encodeState (MyState s) = fromString s
  decodeState = Right . MyState . toString
  defaultState = MyState []

myReturn :: AlfM MyState Return
myReturn = do
  time <- liftIO getZonedTime
  (MyState prevTime) <- get
  put (MyState $ show time)
  return $ defaultReturn
    { items = [ defaultItem { title    = "Time"
                            , subtitle = Just "Outputs the last time the scipt was run"
                            , arg      = Just $ show prevTime }]}

main = alfMain myReturn
```

* Requires [stack](http://www.haskellstack.org)
* The code is documented with haddock
* There is an test workflow included in ```app/Main.hs``` that can be built with ```./buildexample.sh```

#### Use with alfred 
The easiest way to develop a workflow is to point the script action you are making to ```~/.local/bin/<yourexe>``` and run
```stack install``` while testing, and when done copy the executable into the script folder.
