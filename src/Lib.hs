{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  ( TagTree (..),
    TagTreeF (..),
    unroll,
    roll,
    foldTree,
    buildTree,
    bottomUp,
    topDown,
    tagTree,
    flattenTree,
    parseTree,
    renderTree,
  )
where

import Text.HTML.TagSoup
import Text.StringLike (StringLike (..), fromString)

data TagTree str = TagBranch str [Attribute str] [TagTree str] | TagLeaf (Tag str) deriving (Functor, Eq, Ord, Show)

data TagTreeF str a = TagBranchF str [Attribute str] [a] | TagLeafF (Tag str) deriving (Functor, Eq, Ord, Show)

unroll :: TagTree str -> TagTreeF str (TagTree str)
unroll (TagBranch tag attrs contents) = TagBranchF tag attrs contents
unroll (TagLeaf tag) = TagLeafF tag

roll :: TagTreeF str (TagTree str) -> TagTree str
roll (TagBranchF tag attrs contents) = TagBranch tag attrs contents
roll (TagLeafF tag) = TagLeaf tag

foldTree :: (TagTreeF str a -> a) -> TagTree str -> a
foldTree f = go
  where
    go = f . fmap go . unroll

buildTree :: (a -> TagTreeF str a) -> a -> TagTree str
buildTree f = go
  where
    go = roll . fmap go . f

bottomUp, topDown :: (TagTree str -> TagTree str) -> TagTree str -> TagTree str
bottomUp f = foldTree (f . roll)
topDown f = buildTree (unroll . f)

data Pair a b = Pair a b

instance Functor (Pair a) where
  fmap f ~(Pair a b) = Pair a (f b)

sndPair (Pair _ b) = b

flattenTree :: [TagTree str] -> [Tag str]
flattenTree [] = []
flattenTree (TagLeaf tag : xs) = tag : flattenTree xs
flattenTree (TagBranch tag attrs contents : xs) = TagOpen tag attrs : flattenTree contents ++ TagClose tag : flattenTree xs

tagTree :: forall str. (StringLike str) => [Tag str] -> [TagTree str]
tagTree = sndPair . go []
  where
    go :: [str] -> [Tag str] -> Pair [Tag str] [TagTree str]
    go [] [] = Pair [] []
    go (here : _) [] = Pair [] [TagLeaf (TagWarning (fromString ("Missing closing tag: " ++ toString here)))]
    go [] (TagClose tag : xs) = (TagLeaf (TagWarning (fromString ("Unexpected closing tag: " ++ toString tag))) :) <$> go [] xs
    go (here : stack) (TagClose tag : xs)
      | tag == here = Pair xs []
      | otherwise = Pair (TagClose tag : xs) []
    go stack (TagOpen tag attrs : xs)
      | tag `notElem` voidElements =
        let (Pair rest contents) = go (tag : stack) xs
         in (TagBranch tag attrs contents :) <$> go stack rest
    go stack (misctag : xs) = (TagLeaf misctag :) <$> go stack xs

    voidElements :: [str]
    voidElements =
      fromString
        <$> [ "area",
              "base",
              "br",
              "col",
              "embed",
              "hr",
              "img",
              "input",
              "link",
              "meta",
              "source",
              "track",
              "wbr"
            ]

parseTree :: StringLike str => str -> [TagTree str]
parseTree = tagTree . parseTags

renderTree :: StringLike str => [TagTree str] -> str
renderTree = renderTags . flattenTree