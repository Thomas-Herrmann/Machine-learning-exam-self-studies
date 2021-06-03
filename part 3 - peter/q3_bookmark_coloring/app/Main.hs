module Main where

import Coloring


main :: IO ()
main = do 
    print . fromPQOrdered $ fbca exampleGraph 6 100.0 10
    print . fromPQOrdered $ lfbca exampleGraph 6 100.0 0.5 10
