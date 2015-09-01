module Brainfuck.Brainfuck where

import Data.Char
import Control.Monad.RWS
import Prelude hiding ((+), (-), (>), (<), (*), init)

type Pointer = Int
type Cells   = [Int]

type State   = RWST () () (Pointer, Cells) IO ()

(+), (-) :: State
[(+), (-)] = map (modify . change) [succ, pred]
    where
        change f (p, cs) = (p, css)
            where
                css = fst a ++ (f . head $ fst b) : (snd b)
                a = splitAt p cs
                b = splitAt 1 $ snd a

(>), (<) :: State
[(>), (<)] = map (modify . fwd) [succ, pred]
    where
        fwd f (p, cs) = (f p, cs)

(*) :: State
(*) = do
    (p, cs) <- get
    liftIO $ putChar $ chr $ cs !! p

showState :: State
showState = do
    (p, cs) <- get
    liftIO $ print (p, cs)

init :: State
init = put (1, take 100 [0,0..])

runCode code = runRWST (foldr1 (>>) code) () (0, [])
