module Main where

newtype State = State (Register, [State])
              deriving (Eq, Show)
data Register = Invalid
              | Valid [Int]
							deriving (Eq, Show)
type Input    = [Int]
type Output   = [Int]

type Module = (Input, State) -> (Output, State)

type Signal = (Integer, Integer) --(Body, Width)

data Modparam =
	Modparam {
		 name    :: String
		,output  :: ((Input,[Int],[Output]) -> Output)
		,statev' :: ((Input,[Int],[Output]) -> Register)
		,stinitv :: [Int]
		,submod  :: [(Input, State) -> (Output, State)]
		,assign  :: [(Input, [Int]) -> [Int]]
		}

genericmodule :: Modparam -> (Input, State) -> (Output, State)
genericmodule p (ipv, State( regv, substate ))
		| otherwise    = (out, State(ns, substate'))
		where
		out = (output p) (ipv,reg,assign')
		ns  = (statev' p) (ipv,reg,assign')
		sbm = mas (submod p) (assign p) (ipv,reg) substate
		reg = load regv
		assign' = map fst sbm
		substate' = map snd sbm
		mas :: [(Input,State) -> (Output,State)] -> [(Input,[Int]) -> Input] -> (Input,[Int]) -> [State] -> [(Output,State)]
		mas [] _ _ _ = []
		mas m a iv s = ((head m) ((head a) iv, head s)):(mas (tail m) (tail a) iv (tail s))
		load :: Register -> [Int]
		load Invalid     = stinitv p--initial vector
		load (Valid reg) = reg

main :: IO()
main = print $ run ([0], State(Invalid,[State (Invalid,[])])) 100

run :: (Input, State) -> Int -> [Output]
run mdl k
	| k == 0 = [oup]
	| otherwise = oup:(run (([oup0+1]), st') (k-1))
   where
	   (oup@(oup0:_),st') = mod1 mdl


mod1 :: (Input, State) -> (Output, State)
mod1 = genericmodule p
	where
		p = Modparam "mod1" out ns defregv [mod2] sl
		defregv = [0]           -- default register values
		sl      = [\(_,_) -> []]    -- submodule assign generate logic
		out (_,r,(a:[])) = r++a -- output logic
		ns  (i,_,_) = Valid i   -- New State logic
--       | | |
--       | | \-submodule outputs
--       | \-registers
--       \-inputs

mod2 :: (Input, State) -> (Output, State)
mod2 = genericmodule p
  where
    p = Modparam "mod2" out ns [0] [] []
    out (_,(r:[]),_) = [r+1]
    ns  (_,(r:[]),_) = Valid [r+1]

