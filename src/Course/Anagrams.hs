{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.Anagrams where

import Course.Core
import Course.Functor
import Course.List

{-

Functions you will need
--
\* fmap :: (a -> b) -> IO a -> IO b
\* readFile :: FilePath -> IO Str
\* lines :: Str -> [Str]
\* permutations :: [a] -> [[a]]
\* intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
\* toLower :: Char -> Char

Functions that might help
-
\* on :: (b -> b -> c) -> (a -> b) -> a -> a -> c

-}

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
anagrams ::
  Chars ->
  FilePath ->
  IO (List Chars)
anagrams str path =
  let perms = permutations str
      present w = intersectBy equalIgnoringCase w perms
   in present . lines <$> readFile path

-- Compare two strings for equality, ignoring case
equalIgnoringCase ::
  Chars ->
  Chars ->
  Bool
equalIgnoringCase = (==) `on` map toLower
