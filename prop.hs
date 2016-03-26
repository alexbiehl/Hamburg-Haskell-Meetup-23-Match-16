module Prop where

import Prelude hiding (and, or, neg)

e0 :: Prop repr => repr Bool
e0 = and (or true false) true

class Prop repr where
  true :: repr Bool
  false :: repr Bool
  and :: repr Bool -> repr Bool -> repr Bool
  or :: repr Bool -> repr Bool -> repr Bool

class Relational repr where
  intLit :: repr Int
  gte :: repr Int -> repr Int -> repr Bool

tcp_1 :: Prop repr => repr Bool
tcp_1 = tcp e0

newtype Eval a = Eval { runEval :: a }

instance Prop Eval where
  true = Eval True
  false = Eval False
  and e1 e2 = Eval $ runEval e1 && runEval e2
  or e1 e2 = Eval $ runEval e1 || runEval e2

newtype Interp repr a = Interp { runInterp :: repr a }

instance Functor repr => Functor (Interp repr) where
  fmap f (Interp repr) = Interp $ fmap f repr

instance Applicative repr => Applicative (Interp repr) where
  pure = Interp . pure
  (Interp f) <*> (Interp x) = Interp (f <*> x)

instance Monad repr => Monad (Interp repr) where
  return = pure
  (Interp m) >>= f = Interp $ do
    a <- m
    runInterp (f a)

eval :: Eval a -> a
eval = runEval

-- Pretty printer
type Prec = Int
newtype Pretty a = Pretty { runPretty :: Prec -> String }

instance Prop Pretty where
  true = Pretty $ \_ -> "true"
  false = Pretty $ \_ -> "false"
  and e1 e2 = Pretty $ \p -> parenp (p > 4) (runPretty e1 4 ++ " && " ++ runPretty e2 4)
  or e1 e2 = Pretty $ \p -> parenp (p > 3) (runPretty e1 3 ++ " || " ++ runPretty e2 3)

parenp :: Bool -> String -> String
parenp True s = "(" ++ s ++ ")"
parenp _ s = s

pretty :: Pretty a -> String
pretty e = runPretty e 0

-- constant propagation

data TCP repr a = Lit a
                | Unk (repr a)

tcp :: Prop repr => TCP repr Bool -> repr Bool
tcp (Lit True) = true
tcp (Lit False) = false
tcp (Unk e) = e

instance Prop repr => Prop (TCP repr) where
  true = Lit True
  false = Lit False

  and (Lit True) e2 = e2
  and (Lit False) e2 = Lit False
  and e1 (Lit True) = e1
  and e1 (Lit False) = Lit False

  or (Lit True) _ = Lit True
  or (Lit False) e2 = e2
  or e1 (Lit True) = Lit True
  or e1 (Lit False) = e1

class PropWithNeg repr where
  neg :: repr Bool -> repr Bool

instance PropWithNeg Eval where
  neg e = Eval $ not (runEval e)

prompt :: String -> IO Bool
prompt v = do
  putStrLn $ "Bitte gib mir einen Wert für " ++ v
  read <$> getLine

class (Prop repr, PropWithNeg repr) => PropWithVars repr where
  var :: String -> repr Bool

xor :: (Prop repr, PropWithNeg repr) => repr Bool -> repr Bool -> repr Bool
xor x y = (x `or` y) `and` (neg (x `and` y))

e1 :: (Prop repr, PropWithNeg repr) => repr Bool
e1 = neg e0
