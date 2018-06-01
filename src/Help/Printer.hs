module Help.Printer (printHelp, usage, Layout (TwoColumns)) where

import Data.List

import Help.Command
import Utils.String

data Layout = TwoColumns (Int, Int)
larguras (TwoColumns l) = l

data Column = Column Int [String]

printHelp :: Layout -> Command -> String
printHelp l c = "\n" ++ identation 1 ++ "Uso: " ++  usage c ++ details l (grupos c)

usage :: Command -> String
usage c = cmd c ++ " " ++ (unwords (map (brackets . concat . mapGroup parseOption) (grupos c))) ++ "\n\n"
    where parseOption (FixedText x) = x
          parseOption (Single x _) = brackets x
          parseOption (Extended xs _) =  brackets (head xs)
          brackets x = "[" ++ x ++ "]"

details :: Layout -> [OptionGroup] -> String
details l = concat . foldr step []
    where step x acc = (identation 2 ++ groupName x ++ "\n" ++ concat (mapGroup (optionsDetail l) x)) : acc

optionsDetail :: Layout -> Option -> String
optionsDetail _ (FixedText x) = ""

optionsDetail (TwoColumns (a, b)) (Single x d) =
    formatLines 3 (formatColumn a x) (formatColumn b d) ++ "\n"

optionsDetail (TwoColumns (a, b)) (Extended xs d) =
    formatLines 3 (formatColumn a (unwords xs)) (formatColumn b d) ++ "\n"

breaklines :: Int -> String -> String
breaklines 0 _ = " Erro "
breaklines col s = breaklineIter col s
    where breaklineIter x s
              | length s <= col = rpad col s
              | s !! x == ' ' = take x s ++ "\n" ++ breaklines col (rpad col (drop (x + 1) s))
              | otherwise = breaklines (x-1) s

formatColumn :: Int -> String -> Column
formatColumn col s = Column col (foldr step [] textLines)
    where textLines = lines s
          step x acc
              | length x > col = (lines $ breaklines col x) ++ acc
              | otherwise = rpad col x : acc

formatLines :: Int -> Column -> Column -> String
formatLines i (Column _ []) (Column _ []) = ""

formatLines i (Column colA (x:xs)) (Column colB (y:ys)) =
    identation i ++ x ++ y ++ "\n" ++ formatLines i (Column colA xs) (Column colB ys)

formatLines i (Column colA (x:xs)) (Column colB []) =
    identation i ++ x ++ rpad colB "" ++ "\n" ++ formatLines i (Column colA xs) (Column colB [])

formatLines i (Column colA []) (Column colB (y:ys)) =
    identation i ++ rpad colA "" ++ y ++ "\n" ++ formatLines i (Column colA []) (Column colB ys)
