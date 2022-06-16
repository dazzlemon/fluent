module ParserM where

import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)

-- int -> pos, a -> result type, e -> error type
type ParserM t a e = StateT (Int, [t]) (Either (Int, e)) a

throw :: (Int, e) -> ParserM t a e
throw = lift . Left

chooseParserM :: ParserM t a e -> ParserM t a e -> ParserM t a e
chooseParserM p1 p2 = do
  state <- get
  let r1 = runStateT p1 state
  let r2 = runStateT p2 state
  case (r1, r2) of
    (Left l1, Left l2) -> throwFurthest l1 l2
    (Right g1, Right g2) -> returnFurthest g1 g2
    (Left l, Right g) -> returnOrThrowFurthest g l
    (Right g, Left l) -> returnOrThrowFurthest g l
  where throwFurthest (p1, e1) (p2, e2) = throw $ if p1 >= p2
          then (p1, e1)
          else (p2, e2)
        return' (a, (d, t)) = do
          put (d, t)
          return a
        returnFurthest (a1, (d1, t1)) (a2, (d2, t2)) = return' $ if d1 >= d2
          then (a1, (d1, t1))
          else (a2, (d2, t2))
        returnOrThrowFurthest (a, (d, t)) (p, e) = if d >= p
          then return' (a, (d, t))
          else throw (p, e)