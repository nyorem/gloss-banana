module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.FRP.ReactiveBanana
import Reactive.Banana
import Reactive.Banana.Frameworks

fps :: Int
fps =
    30

dims :: (Int, Int)
dims =
    (800, 600)

main :: IO ()
main =
    playBanana (InWindow "Hello Banana" dims (500, 200))
                white
                fps
                reactiveMain

reactiveMain :: Event ()
             -> Behavior Float
             -> Event InputEvent
             -> MomentIO (Behavior Picture)
reactiveMain _ _ _ =
    return $ pure $ circle 20

