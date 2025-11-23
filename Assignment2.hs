-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE NoGeneralizedNewtypeDeriving, Safe #-}

module Assignment2 (encodeWord , encodeWords , encodeText ,
                    decodeText ,
                    decodeTextWithTree ,
                    ramify ,
                    tabulate ,
                    tree) where

import Types
import Data.List
import Data.Maybe (fromJust)

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

{- Question 1 -}

encodeWord :: Table -> String -> Code
encodeWord table [] = []
encodeWord table [c] =
    case lookup c table of
        Just code -> code
        Nothing   -> []
encodeWord table (c:cs) =
    case lookup c table of
        Just code -> code ++ shortGap ++ encodeWord table cs
        Nothing   -> encodeWord table cs

encodeWords :: Table -> [String] -> Code
encodeWords table [] = []
encodeWords table [w] = encodeWord table w
encodeWords table (w:ws) =
    encodeWord table w ++ mediumGap ++ encodeWords table ws

split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split sep xs =
    let (word, rest) = break (== sep) xs
    in case rest of
        []      -> [word]
        (_:xs') -> word : split sep xs'

encodeText :: Table -> String -> Code
encodeText table text =
    encodeWords table (split ' ' text)


{- Question 2 -}

lookupByValue :: Code -> Table -> Maybe Char
lookupByValue code table =
    lookup code (map (\(c,m) -> (m,c)) table)

letterBoundary :: Code
letterBoundary = Silence : shortGap

wordBoundary :: Code
wordBoundary = mediumGap

trimLeadingSilence :: Code -> Code
trimLeadingSilence = dropWhile (== Silence)

fixEnding :: Code -> Code
fixEnding xs =
    if null xs then xs
    else if last xs == Silence then xs
    else xs ++ [Silence]

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn gap = go
  where
    gl = length gap
    go [] = []
    go ys
      | gap `isPrefixOf` ys = go (drop gl ys)
      | otherwise =
          let (pre, rest) = breakList gap ys
          in case rest of
               [] -> [pre]
               _  -> pre : go (drop gl rest)

breakList :: Eq a => [a] -> [a] -> ([a], [a])
breakList gap xs
  | gap `isPrefixOf` xs = ([], xs)
  | otherwise =
      case xs of
        []     -> ([], [])
        y:ys   -> let (pre, rest) = breakList gap ys
                  in (y:pre, rest)

splitWords :: Code -> [Code]
splitWords = splitOn wordBoundary

splitLetters :: Code -> [Code]
splitLetters code =
    let raw = splitOn letterBoundary code
        trimmed = map trimLeadingSilence raw
    in map fixEnding trimmed

decodeLetter :: Table -> Code -> Char
decodeLetter tbl code =
    case lookupByValue code tbl of
      Just c  -> c
      Nothing -> '?'

decodeWord :: Table -> Code -> String
decodeWord tbl code =
    map (decodeLetter tbl) (splitLetters code)

decodeText :: Table -> Code -> String
decodeText tbl code =
    unwords (map (decodeWord tbl) (splitWords code))


{- Question 3 -}

data DD = Dot | Dash deriving (Eq, Show)

toDotsDashes :: Code -> [DD]
toDotsDashes [] = []
toDotsDashes (Beep:xs) =
  let (beeps, rest) = span (== Beep) (Beep:xs)
  in case rest of
       (Silence:ys) ->
          case length beeps of
            1 -> Dot  : toDotsDashes ys
            3 -> Dash : toDotsDashes ys
            _ -> toDotsDashes ys
       _ -> []
toDotsDashes (Silence:xs) = toDotsDashes xs

decodeLetterWithTree :: Tree -> Code -> Char
decodeLetterWithTree tree code =
    walk tree (toDotsDashes code)
  where
    walk (Branch (Just c) _ _) []       = c
    walk (Branch _ left right) (Dot:xs) = walk left xs
    walk (Branch _ left right) (Dash:xs)= walk right xs
    walk _ _                            = '?'

decodeWordWithTree :: Tree -> Code -> String
decodeWordWithTree tree code =
    map (decodeLetterWithTree tree) (splitLetters code)

decodeTextWithTree :: Tree -> Code -> String
decodeTextWithTree tree code =
    unwords (map (decodeWordWithTree tree) (splitWords code))


{- Question 4 -}

ramify :: Table -> Tree
ramify = foldl insertChar Empty

insertChar :: Tree -> (Char, Code) -> Tree
insertChar tree (c, code) =
    insertDD tree (toDotsDashes code)
  where
    insertDD :: Tree -> [DD] -> Tree
    insertDD Empty [] = Branch (Just c) Empty Empty
    insertDD (Branch _ l r) [] = Branch (Just c) l r
    insertDD Empty (Dot:xs) =
        Branch Nothing (insertDD Empty xs) Empty
    insertDD Empty (Dash:xs) =
        Branch Nothing Empty (insertDD Empty xs)
    insertDD (Branch v l r) (Dot:xs) =
        Branch v (insertDD l xs) r
    insertDD (Branch v l r) (Dash:xs) =
        Branch v l (insertDD r xs)


{- Question 5 -}

tabulate :: Tree -> Table
tabulate tree = go tree []
  where
    go :: Tree -> [DD] -> Table
    go Empty _ = []
    go (Branch Nothing l r) path =
        go l (path ++ [Dot]) ++ go r (path ++ [Dash])
    go (Branch (Just c) l r) path =
        [(c, ddToCode path)]
        ++ go l (path ++ [Dot])
        ++ go r (path ++ [Dash])

    ddToCode :: [DD] -> Code
    ddToCode [] = []
    ddToCode (Dot:xs)  = dit ++ ddToCode xs
    ddToCode (Dash:xs) = dah ++ ddToCode xs


{- Question 6 -}

brackets :: Bracket -> String
brackets (Round ts) = "(" ++ concat [brackets t | t <- ts] ++ ")"
brackets (Curly ts) = "{" ++ concat [brackets t | t <- ts] ++ "}"

tree :: String -> Maybe Bracket
tree xs = case parse xs of
            Just (b, "") -> Just b
            _            -> Nothing

parse :: String -> Maybe (Bracket, String)
parse ('(' : cs) = do
    (children, rest) <- parseMany cs
    case rest of
      (')' : more) -> Just (Round children, more)
      _            -> Nothing
parse ('{' : cs) = do
    (children, rest) <- parseMany cs
    case rest of
      ('}' : more) -> Just (Curly children, more)
      _            -> Nothing
parse _ = Nothing

parseMany :: String -> Maybe ([Bracket], String)
parseMany s =
    case parse s of
      Just (b, rest) -> do
        (bs, final) <- parseMany rest
        Just (b : bs, final)
      Nothing -> Just ([], s)

isWellBracketed :: String -> Bool
isWellBracketed xs = case tree xs of
                      Nothing -> False
                      Just _  -> True
