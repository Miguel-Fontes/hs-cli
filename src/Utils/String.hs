module Utils.String where

rtrim :: String -> String
rtrim xs = rtrimIter (reverse xs)
    where rtrimIter (x:xs)
            | x == ' ' = rtrimIter xs
            | otherwise = reverse (x:xs)

rpad :: Int -> String -> String
rpad x s = s ++ replicate (x - length s) ' '

lpad :: Int -> String -> String
lpad x s = replicate (x - length s) ' ' ++ s

margin :: Int -> String
margin x = replicate x ' '

identation :: Int -> String
identation x = replicate (x * 4) ' '