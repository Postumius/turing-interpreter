module TM
  (right,
   left,
   TM.read,
   write) where

import qualified Data.Map.Strict as D
import Data.Maybe

data Symbol = Eps | U | Symbol Char
  deriving (Show, Eq, Ord)

type Tape = ([Symbol], [Symbol])

type Move = Tape -> Tape

right :: Move
right (ls, r:[]) = (r:ls, U:[])
right (ls, r:rs) = (r:ls, rs)

left :: Move
left ([], rs) = ([], rs)
left (l:ls, rs) = (ls, l:rs)

read :: Tape -> Symbol
read (_, r:_) = r

write :: Symbol -> Tape-> Tape
write Eps t = t
write c (xs, y:ys) = (xs, c:ys)

toDict alp = D.fromList $ zip alp [0..(length alp -1)]

type State = Int

defTM :: [Symbol] -> [[(State,Symbol,Move)]] ->State ->State ->State ->
      String -> (Bool, Tape)
defTM gamma delta q0 qA qR input = let
  gDict = toDict gamma
  t0 = ([], (map Symbol input ++ [U]))
  recur qi ti
    | qi == qA = (True, ti)
    | qi == qR = (False, ti)
    | otherwise = let
        (qj, s, move) =
          delta !! qi !! (fromJust $ D.lookup (TM.read ti) gDict)
        in recur qj $ (move . write s) ti
  in recur q0 t0

fromSymbol (Symbol c) = c
fromSymbol U = 'U'

tapeToString (ls, rs) = map fromSymbol $ (reverse ls) ++ filter (/=U) rs

rightShift =
  defTM
  [  Symbol '0',          Symbol '1',           U]
  [[(1,U,right),          (2,U,right),          (4,Eps,left)],
   [(1,Symbol '0',right), (2,Symbol '0',right), (3,Symbol '0',left)],
   [(1,Symbol '1',right), (2,Symbol '1',right), (3,Symbol '1',left)],
   [(3,Eps,left),         (3,Eps,left),         (4,Eps,left)]]
  0
  4
  5
     
binInc = tapeToString . snd .
  defTM
  [  Symbol '0',          Symbol '1',           U,                    Symbol 'I',          Symbol 'O']  --gamma
                                                                                                                --delta {
  [[(1,Symbol 'O',right), (1,Symbol 'I',right)],                                                                  --q0
   [(1,Eps,right),        (1,Eps,right),        (2,Eps,left)],                                                    --q1 
   [(3,Symbol '1',left),  (2,Symbol '0',left),  (9,Eps,left),         (4,Symbol '0',left), (8,Symbol '1',left)],  --q2
   [(3,Eps,left),         (3,Eps,left),         (9,Eps,left),         (8,Symbol '1',left), (8,Symbol '0',left)],  --q3   
   [(5,U,right),          (6,U,right),          (8,Symbol '1',left)],                                             --q4
   [(5,Symbol '0',right), (6,Symbol '0',right), (7,Symbol '0',left)],                                             --q5
   [(5,Symbol '1',right), (6,Symbol '1',right), (7,Symbol '1',left)],                                             --q6
   [(7,Eps,left),         (7,Eps,left),         (8,Symbol '1',left)]]                                             --q7   }
  0    --qStart
  8    --qAccept
  9    --qReject
