doubleUs :: (Integral a) => a -> a -> a
doubleUs x y = x+x+y+y

maybeDouble:: (Integral a) => a -> a 
maybeDouble x = if x < 100 then x+x else x

test :: (Integral a) => a -> a
test x = 
  if x == 1 then x 
  else if x == 2 then 5
  else x+x

testList :: [a] -> [Char]
testList x =
  if (mylength x) == 1 then "1 Element"
  else if (mylength x) > 1 then "Multiple Elements"
  else "Empty List"

myhead :: [a] -> a
myhead (x:xs) = x

mytail :: [a] -> [a]
mytail (x:xs) = xs

mynull :: [a] -> Bool
mynull x = (mylength x) == 0

mylast :: [a] -> a
mylast x = x!!((length x)-1)

mylength :: [a] -> Int
mylength list =
  if (null list) then 0
  else succ (mylength (mytail list) )

mytake :: Int -> [a] -> [a]
mytake x (l:xl) =
  if x == 1 then (l:[])
  else if null (l:xl) then (l:[])
  else (l:(mytake (pred x) xl))

myrepeat :: a -> [a]
myrepeat x = x:[] ++ (myrepeat x)

myreplicate :: Int -> a -> [a]
myreplicate x y = mytake x (myrepeat y)

mymaximum :: (Integral a) => [a] -> a
mymaximum (x:xs) =
  if (null xs) then x
  else if x > (myhead xs) then mymaximum (x:(mytail xs))
  else mymaximum xs

mysum :: (Integral a) => [a] -> a
mysum (x:xs) =
  if (null xs) then x
  else mysum ((x + (myhead xs)):(mytail xs))

myreverse :: [a] -> [a]
myreverse list =
  if null list then list
  else (mylast list):[] ++ myreverse(init list)

myelem :: (Eq a) => a -> [a] -> Bool
myelem x (l:xl) =
  if (x == l) then True
  else if null xl then False
  else myelem x xl
