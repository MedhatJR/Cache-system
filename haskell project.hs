data Item a = It Tag (Data a) Bool Int | NotPresent deriving (Show, Eq)
data Tag = T Int deriving (Show, Eq)
data Data a = D a deriving (Show, Eq)
data Output a = Out (a, Int) | NoOutput deriving (Show, Eq) 

convertBinToDec :: Integral a => a -> a
convertBinToDec 0 = 0
convertBinToDec a = 2 * convertBinToDec (div a 10) + (mod a 10)

replaceIthItem :: t -> [t] -> Int -> [t]
replaceIthItem  t (_:h1) 0 = t:h1
replaceIthItem t (h:h1) int  = h : (replaceIthItem t h1 (int-1))

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest) where (first,rest) = splitAt n list

logBase2 :: Floating a => a -> a
logBase2 n = 
             loga_b 2 n 
			 
loga_b a b
 | b == 1    = 0
 | b == a    = 1
 | otherwise = 1 + loga_b a (b / a)


getNumBits :: (Integral a, RealFloat a1) => a1 -> [Char] -> [c] -> a
getNumBits _ "fullyAssoc" _ = 0
getNumBits x "setAssoc" _  = round (logBase2 x)  
getNumBits _ "directMap" b = round(logBase2 (fromIntegral (length b)))


fillZeros :: [Char] -> Int -> [Char]
fillZeros s 0 = s 
fillZeros s n =  '0' : fillZeros s (n-1)


----convertAddress for directMap , fullyAssoc , setAssoc
convertAddress :: (Integral b1, Integral b2 ) => b1 -> b2 -> p -> (b1, b1)
convertAddress binAddress bitsNum s = ( div binAddress (10 ^ bitsNum ) , mod binAddress (10 ^ bitsNum )) 

getDataFromCache :: (Integral b, Eq a) => [Char] -> [Item a] -> [Char] -> b -> Output a

----getDataFromCache for directMap
getDataFromCache stringAddress cache "directMap" bitsNum =  if (length cache) > (index stringAddress) then compare2 (tagB stringAddress bitsNum) (cache !! (index stringAddress))  0  else NoOutput

----getDataFromCache for fullyAssoc
getDataFromCache stringAddress cache "fullyAssoc" bitsNum = compare1 (tagB stringAddress bitsNum) cache 0 

----getDataFromCache for setAssoc
getDataFromCache stringAddress cache "setAssoc" bitsNum = compare1 (tagA stringAddress) ((splitEvery  2 cache) !! (index stringAddress)) 0

compare1 tg [] h	= NoOutput							 
compare1 tg ((It (T t) (D d) v _):xs) h = if tg == t && v  then Out( d , h) else  compare1 tg xs (h+1)                        

compare2 tg (It (T t) (D d) v _) h = if tg == t && v  then Out( d , 0) else  NoOutput

index stringAddress = idx(convertAddress (read stringAddress :: Int) 2 "setAssoc") 
idx (x,xs) = xs

tagA stringAddress = tagX(convertAddress (read stringAddress :: Int) 2 "setAssoc") 
tagX (x,xs) =  x 

tagB stringAddress bitsNum = tagZ(convertAddress (read stringAddress :: Int) bitsNum "setAssoc") 
tagZ (x,xs) =  x 

