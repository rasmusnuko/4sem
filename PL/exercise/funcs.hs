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
myhead x = x!!0

mytail :: [a] -> [a]
mytail (x:xs) = xs

mynull :: [a] -> Bool
mynull x = (mylength x) == 0

mylength :: [a] -> Int
mylength list =
  if (null list) then 0
  else succ (mylength (mytail list) )

mytake :: Int -> [a] -> [a]
mytake size [] = []
mytake size (xs:s) =
  if (mylength xs++s) > x then (xs ++ (mytake (pred size) s)
  else xs++s
