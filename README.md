## alfred-footman

### A library for writing script filters in [Alfred](https://www.alfredapp.com) persistent state

### Somewhat a WIP.


Example usage:
``` haskell
import Alfred
import Data.Time.LocalTime

myReturn :: AlfM String Return
myReturn = do
  time <- liftIO getZonedTime
  prevTime <- get
  put $ show time
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
