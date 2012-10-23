module Main where

newtype State = State (Register, [State])
              deriving (Eq, Show)
data Register = Invalid
              | Valid [Int]
							deriving (Eq, Show)
type Input    = [Int]
type Output   = [Int]

type Module = (Input, State) -> (Output, State)

data Modparam =
	Modparam {
		 output  :: ((Input,[Int],[Output]) -> Output)
		,statev' :: ((Input,[Int],[Output]) -> Register)
		,stinitv :: [Int]
		,submod  :: [(Input, State) -> (Output, State)]
		,assign  :: [(Input -> [Int])]
		}

genericmodule :: Modparam -> (Input, State) -> (Output, State)
genericmodule p (ipv, State( regv, substate ))
		| otherwise    = (out, State(ns, substate'))
		where
		out = (output p) (ipv,reg,assign')
		ns  = (statev' p) (ipv,reg,assign')
		sbm = mas (submod p) (assign p) ipv substate
		reg = load regv
		assign' = map fst sbm
		substate' = map snd sbm
		mas :: [((Input, State) -> (Output, State))] -> [(Input -> [Int])] -> Input -> [State] -> [(Output, State)]
		mas [] _ _ _ = []
		mas m a iv s = ((head m) ((head a) iv, head s)):(mas (tail m) (tail a) iv (tail s))
		load :: Register -> [Int]
		load Invalid     = stinitv p--initial vector
		load (Valid reg) = reg

main :: IO()
main = print $ test ([0], State(Invalid,[State (Invalid,[])])) 100

test :: (Input, State) -> Int -> [Output]
test mdl k
	| k == 0 = [oup]
	| otherwise = oup:(test (([oup0+1]), st') (k-1))
   where
	   (oup@(oup0:_),st') = mod1 mdl

mod1 :: (Input, State) -> (Output, State)
mod1 = genericmodule p
  where
    p = Modparam out ns [0] [mod2] [(\_ -> [])]
    out (_,r,(a:[])) = r++a
    ns  (i,_,_) = Valid i

mod2 :: (Input, State) -> (Output, State)
mod2 = genericmodule p
  where
    p = Modparam out ns [0] [] []
    out (_,(r:[]),_) = [r+1]
    ns  (_,(r:[]),_) = Valid [r+1]
-- mod1 :: (Input, State) -> (Output, State)
-- mod1 ((ip:ips), State( dat, substate ))
--     | dat == I   = (out, State(R $ load dat,[st1]))
-- 		| otherwise = (out, State(R[ip], [st1]))
-- 		where
-- 		(out1, st1) = mod2 (ips, head substate)
-- 		out0 = load dat
-- 		out = (out0++out1)
-- 		load :: Register -> [Int]
-- 		load I        = [1] --initial vector
-- 		load r@(R reg) = reg
-- 
-- mod2 :: (Input, State) -> (Output, State)
-- mod2 ((ip:ips), State(ds, []))
--     | ds == I = ([0], State(R[0],[]))
-- 		| otherwise = ([1 + load ds], State(R[1 + load ds], []))
-- 		where
-- 		load r@(R (s:[])) = s
-- 
