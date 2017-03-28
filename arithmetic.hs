import Control.Monad
import Data.List (find,group)

--31
primes = 2 : filter isPrime [3,5..]

isPrime = ap (all.((/=0).).mod)  (flip takeWhile primes . (.join(*)) .  flip (<=))

--32
myGCD a 0 = a
myGCD a b
  | a < 0 = myGCD (-a) (-b)
  | otherwise = myGCD b (mod a b)

--33  
coprime a b = myGCD a b == 1

--34
totient 1 = 1
totient n = length $ filter (coprime n) [1..n]

--35
primeFactors 1 = []
primeFactors n = case find ((==0).mod n) primes of
                   Nothing -> []
                   Just x -> x:primeFactors (quot n x)

--36
prime_factors_mult n = map (\x -> (head x, length x)) $  group $ primeFactors n

--37
phi n = product $ map (\(p,m) -> (p-1) * p ^ (m-1)) $ prime_factors_mult n

--39
primesR low high = takeWhile (<high) $ dropWhile (<low) $ primes

--40
goldbach n = take 1 [(a,b)| a <- takeWhile (<n) primes, b <- takeWhile (<n) primes, a < b, a + b == n]

--41
goldbachList low high = do
  n <- filter even [low..high]
  res <- goldbach n
  return res

goldbachList' low high require = [(x,y)| n <- filter even [low..high] ,(x,y) <- goldbach n, (x > require && y > require)]

