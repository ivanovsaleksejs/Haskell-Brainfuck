module Brainfuck.Brainfuck where

import Data.Char
import Data.Maybe
import Control.Monad.RWS
import System.IO
import Prelude hiding ((+), (-), (>), (<), (*), init)

type IP  = Int
type BP  = Int

data Code = Code [State]
type Cells = [Int]

type State   = RWST () () (IP, BP, Code, Cells) IO ()

(+), (-) :: State
[(+), (-)] = map (modify . change) [succ, pred]
    where
        change f (ip, p, code, cs) = (ip, p, code, css)
            where
                css = fst a ++ (f . head $ fst b) : (snd b)
                a = splitAt p cs
                b = splitAt 1 $ snd a

(>), (<) :: State
[(>), (<)] = map (modify . fwd) [succ, pred]
    where
        fwd f (ip, p, code, cs) = (ip, f p, code, cs)

(#) :: State
(#) = do
    c <- liftIO $ fmap ord getChar
    modify $ readChar c
    where
        readChar c (ip, p, code, cs) = (ip, p, code, css)
            where
                css = fst a ++ c : (snd b)
                a = splitAt p cs
                b = splitAt 1 $ snd a

(*) :: State
(*) = do
    (ip, p, code, cs) <- get
    liftIO $ putChar $ chr $ cs !! p

trFunc = fromJust . flip lookup [
        ('+',(+)),
        ('-',(-)),
        ('>',(>)),
        ('<',(<)),
        ('.',(*)),
        (',',(#))
    ]

{-showState :: State
showState = do
    (ip, p, code, cs) <- get
    liftIO $ print (ip, p, code, cs)-}

init :: State
init = do
    liftIO $ hSetBuffering stdin NoBuffering
    liftIO $ hSetBuffering stdout NoBuffering
    put (1, 1, Code [], take 100 [0,0..])

run code = runRWST (mapM_ id $ init : code) () (0, 0, Code [], [])
run' code' = runRWST (mapM_ id $ init : map trFunc code') () (0, 0, Code [], [])
