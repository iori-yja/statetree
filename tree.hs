module Main where

newtype State = State (Register, [State])
              deriving (Eq, Show)
data Register = I
              | R [Int]
							deriving (Eq, Show)
type Input    = [Int]
type Output   = [Int]

--unState :: State -> (Signal, [State])
--unState (State(sig,[st])) = (sig,[st])
--
--submodstate :: State -> [State]
--submodstate = snd . unState

--genericmodule :: Modparam -> (Input, State) -> (Output, State)
--genericmodule prm

main :: IO()
main = print $ test ([0,1], State(I,[State(I,[])])) 10

test :: (Input, State) -> Int -> [Output]
test mdl k
	| k == 0 = [oup]
	| otherwise = oup:(test (([oup0+1,oup1]), st') (k-1))
   where
	   (oup@(oup0:oup1:_),st') = mod1 mdl

mod1 :: (Input, State) -> (Output, State)
mod1 ((ip:ips), State(ds, mo@(moc:[])))
    | ds == I = (0:out1, State(R[1],[st1]))
		| otherwise = (out, State(R[ip], [st1]))
		where
		(out1, st1) = mod2 (ips, moc)
		out0 = load ds
		out = (out0++out1)
		load r@(R ip) = ip

mod2 :: (Input, State) -> (Output, State)
mod2 ((ip:ips), State(ds, []))
    | ds == I = ([0], State(R[0],[]))
		| otherwise = ([1 + load ds], State(R[1 + load ds], []))
		where
		load r@(R (s:[])) = s

