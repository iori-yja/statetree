module Main where

newtype State = State (Signal, [State])
type Signal = [Int]
type Input  = [Int]
type Output = [Int]


--unState :: State -> (Signal, [State])
--unState (State(sig,[st])) = (sig,[st])
--
--submodstate :: State -> [State]
--submodstate = snd . unState

main :: IO()
main = print $ test ([0,1], State([1],[State([0],[])])) 10

test :: (Input, State) -> Int -> [Signal]
test mdl k
	| k == 0 = [oup]
	| otherwise = oup:(test (([oup0+1,oup1+1]), st') (k-1))
   where
	   (oup@(oup0:oup1:[]), st') = mod1 mdl

mod1 :: (Input, State) -> (Output, State)
mod1 ((ip:ips), State(ds, mo@(moc:[]))) = (ds++out1, State([ip], [st1]))
   where
	   (out1, st1) = mod2 (ips, moc)

mod2 :: (Input, State) -> (Output, State)
mod2 ([ip], State(ds, [])) = (ds, State([ip], []))

