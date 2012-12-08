module Main where
import Data.Bits as B

newtype State a = State (Register a, [State a])
              deriving (Eq, Show)
data Register a = Invalid
              | Valid [a]
							deriving (Eq, Show)
type Input a   = a
type Output a  = a

data NameTree = NameTree (String,[NameTree])
							deriving (Eq, Show)

data Modparam i o s =
	Modparam {
		 name    :: String
		,output  :: ((Input i,[s],[Output o]) -> Output o)
		,statev' :: ((Input i,[s],[Output o]) -> Register s)
		,stinitv :: [s]
--		,submod  :: [(Input, State) -> (Output, State)]
		,submod  :: [Modparam i o s]
		,assign  :: [(Input i, [s]) -> i]
		}

genericmodule :: Modparam i o s -> (Input i, State s) -> (Output o, State s)
genericmodule p (ipv, State( regv, substate )) = (out, State(ns, substate'))
		where
		out = (output p) (ipv,reg,assign')
		ns  = (statev' p) (ipv,reg,assign')
		sbm = mas (map genericmodule (submod p)) (assign p) (ipv,reg) substate
		reg = load p regv
		assign' = map fst sbm
		substate' = map snd sbm
		mas :: [(Input i,State s) -> (Output o,State s)] -> [(Input i,[s]) -> Input i] -> (Input i,[s]) -> [State s] -> [(Output o,State s)]
		mas [] _ _ _ = []
		mas m a iv s = ((head m) ((head a) iv, head s)):(mas (tail m) (tail a) iv (tail s))
		load :: Modparam i o s -> Register s -> [s]
		load p Invalid     = stinitv p--initial vector
		load _ (Valid reg) = reg

main :: IO()
main = print $ run ([0], State(Invalid,[State (Invalid,[])])) 100
--main = print $ printtree mod1

run :: (Input [Integer], State Integer) -> Integer -> [Output [Integer]]
run mdl k
	| k == 0 = [oup]
	| otherwise = oup:(run ([oup0+1], st') (k-1))
   where
	   (oup@(oup0:_),st') = genericmodule mod1 mdl

printtree :: Modparam Integer Integer Integer -> NameTree
printtree p = NameTree ((name p), (map printtree (submod p)))

mod1 :: Modparam [Integer] [Integer] Integer
mod1 = p
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

mod2 :: Modparam [Integer] [Integer] Integer
mod2 = Modparam "mod2" out ns [0] [] []
  where
    out (_,(r:_),_) = [1,r]
    ns  (_,(r:_),_) = Valid [r,20]

