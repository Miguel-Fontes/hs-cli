module Help.Command where

import Data.List

data Command = Command {
    cmd :: String,
    grupos :: [OptionGroup]
}

instance Show Command where
    show (Command c o) = c ++ " " ++ show o

data OptionGroup = OptionGroup {
    groupName :: String,
    options :: [Option]
}

mapGroup :: (Option -> a) -> OptionGroup -> [a]
mapGroup f (OptionGroup _ ops) = map f ops

instance Show OptionGroup where
    show (OptionGroup d os) = d ++ " - " ++ show os

data Option =   FixedText String
              | Single String String
              | Extended [String] String

instance Show Option where
    show (FixedText s) = s
    show (Single c d) = c ++ " - " ++ d
    show (Extended xs c) = show xs ++ " - " ++ c

getKey (Single k _) = [k]
getKey (Extended k _) = k
getKey (FixedText _ ) = []