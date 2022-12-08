--Norbert Jaśniewicz
import Control.Monad.Writer

extendedEuclideanPriv :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Writer (String) (Integer, Integer, Integer)
extendedEuclideanPriv a b x y r s
  | b <= 0 = do
    tell ("extendedEuclideanPriv " ++ (show (a, b, x, y, r, s)) ++ " ==> ");
    tell (show (x, y, a));
    return (x, y, a)
  | otherwise = do
    tell ("extendedEuclideanPriv " ++ (show (a, b, x, y, r, s)) ++ " ==> ");
    let c = a `mod` b;
    let q = a `div` b;
    extendedEuclideanPriv b c r s (x - q * r) (y - q * s)

extendedEuclideanPriv2:: Integer -> Integer -> Writer (String) (Integer, Integer, Integer)
extendedEuclideanPriv2 a b = do
  tell ("extendedEuclideanPriv2 " ++ (show ((abs a), (abs b))) ++ " ==> ")
  (x, y, g) <- (extendedEuclideanPriv (abs a) (abs b) 1 0 0 1);
  let x' = if a > 0 then x else -x
  let y' = if b > 0 then y else -y
  tell (" ==> restoring sign ==> " ++ show (x', y', g))
  return (x', y', g)

solveLDEWriter :: Integer -> Integer -> Integer -> Writer (String) (Maybe (Integer, Integer))
solveLDEWriter a b c = do
  tell ("solveLDE " ++ (show (a, b, c)) ++ " ==> ");
  (x, y, g) <- (extendedEuclideanPriv2 a b);
  let condition = (c `rem` g) /= 0;
  tell " ==> "
  tell (if condition then "Solution not found" else "Solution found")
  return (if condition then Nothing else Just (x * (c `div` g), y * (c `div` g)))

solveLDE :: Integer -> Integer -> Integer -> (Maybe (Integer, Integer), String)
solveLDE a b c = runWriter (solveLDEWriter a b c)

solveLDEVal :: Integer -> Integer -> Integer -> (Maybe (Integer, Integer))
solveLDEVal a b c = fst (solveLDE a b c)

solveLDELog :: Integer -> Integer -> Integer -> String
solveLDELog a b c = snd (solveLDE a b c)

inverseP :: Integer -> Integer -> (Maybe Integer)
inverseP x p = (solveLDEVal x p 1) >>= (\(x, _) -> Just (x `mod` p))

main = do
  print (solveLDE (-32) (-48) (-64))
  print (solveLDEVal (-32) (-48) (-64))
  print (solveLDELog (-32) (-48) (-64))
  print(inverseP 3 7)