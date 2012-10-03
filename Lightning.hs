
rightTriangles = [ (a,b,c) | c <- [1..], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]

quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted 


--Soultion to palindrome. I commented out the type defination just to show how it's not actually necessary
--pali :: String -> Bool
pali string = reverse string == string


--My solution to the sexy primes. The prime generation is actually kinda slow but in this case I doubt it matters
primes :: [Int]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0] 

isPrime :: Int -> Bool
isPrime x = elem x (take x primes)


sexyPrimes n = [ (x, x+6) | x <- [3..n-6], isPrime x, isPrime (x+6)] 

--The stolen solution. It's rather awesome but pretty confusing. 
sexPrimes n =[ (x , x+6) | x <- [3..n-6] , all( \k -> all((>0).mod k) [2..k-1] ) [x,x+6]]

--My solution to the triangle number words thing.
--I'd like to point out the whole map floor take sum thing. Basically
--what is going on is that triangle numbers is returning a list of doubles
--so in order to use a sexy list comprenension I actually have to waste some time
--maybe I'll find a better way to do it at some point. 

triangleNumbers = [ x | x <- [1..], y <- [1..x], x == (y / 2) * (y+1) ]

isTriangle :: String -> Bool
isTriangle word = elem sum (map floor (take sum triangleNumbers))  
  where
    sum = (foldl (\acc l -> acc + (fromEnum l - 64)) 0 word)

