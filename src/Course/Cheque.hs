{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-

Write a function (dollars) that accepts a `String` and returns a `String`.
It will accept a numeric value as input, representing an amount of money, and convert to its transcribed English.

For example, the input "1.11" will result in a return value of "one dollar and eleven cents"

Invalid characters should be ignored, meaning that every input string has an output string.
The empty string produces "zero dollars and zero cents"

There is a `test` function below that lists more examples of input and output. There are also functions and
data structures that may assist you in deriving the result. It is not compulsory that they are used.

-}

module Course.Cheque where

import Course.Applicative
import Course.Core
import Course.Functor
import Course.List
import Course.Monad
import Course.Optional

-- $setup
-- >>> :set -XOverloadedStrings

-- The representation of the grouping of each exponent of one thousand. ["thousand", "million", ...]
illion ::
  List Chars
illion =
  let preillion ::
        List (Chars -> Chars)
      preillion =
        listh
          [ const "",
            const "un",
            const "do",
            const "tre",
            const "quattuor",
            const "quin",
            const "sex",
            const "septen",
            const "octo",
            \q -> if "n" `isPrefixOf` q then "novem" else "noven"
          ]
      postillion ::
        List Chars
      postillion =
        listh
          [ "vigintillion",
            "trigintillion",
            "quadragintillion",
            "quinquagintillion",
            "sexagintillion",
            "septuagintillion",
            "octogintillion",
            "nonagintillion",
            "centillion",
            "decicentillion",
            "viginticentillion",
            "trigintacentillion",
            "quadragintacentillion",
            "quinquagintacentillion",
            "sexagintacentillion",
            "septuagintacentillion",
            "octogintacentillion",
            "nonagintacentillion",
            "ducentillion",
            "deciducentillion",
            "vigintiducentillion",
            "trigintaducentillion",
            "quadragintaducentillion",
            "quinquagintaducentillion",
            "sexagintaducentillion",
            "septuagintaducentillion",
            "octogintaducentillion",
            "nonagintaducentillion",
            "trecentillion",
            "decitrecentillion",
            "vigintitrecentillion",
            "trigintatrecentillion",
            "quadragintatrecentillion",
            "quinquagintatrecentillion",
            "sexagintatrecentillion",
            "septuagintatrecentillion",
            "octogintatrecentillion",
            "nonagintatrecentillion",
            "quadringentillion",
            "deciquadringentillion",
            "vigintiquadringentillion",
            "trigintaquadringentillion",
            "quadragintaquadringentillion",
            "quinquagintaquadringentillion",
            "sexagintaquadringentillion",
            "septuagintaquadringentillion",
            "octogintaquadringentillion",
            "nonagintaquadringentillion",
            "quingentillion",
            "deciquingentillion",
            "vigintiquingentillion",
            "trigintaquingentillion",
            "quadragintaquingentillion",
            "quinquagintaquingentillion",
            "sexagintaquingentillion",
            "septuagintaquingentillion",
            "octogintaquingentillion",
            "nonagintaquingentillion",
            "sescentillion",
            "decisescentillion",
            "vigintisescentillion",
            "trigintasescentillion",
            "quadragintasescentillion",
            "quinquagintasescentillion",
            "sexagintasescentillion",
            "septuagintasescentillion",
            "octogintasescentillion",
            "nonagintasescentillion",
            "septingentillion",
            "deciseptingentillion",
            "vigintiseptingentillion",
            "trigintaseptingentillion",
            "quadragintaseptingentillion",
            "quinquagintaseptingentillion",
            "sexagintaseptingentillion",
            "septuagintaseptingentillion",
            "octogintaseptingentillion",
            "nonagintaseptingentillion",
            "octingentillion",
            "decioctingentillion",
            "vigintioctingentillion",
            "trigintaoctingentillion",
            "quadragintaoctingentillion",
            "quinquagintaoctingentillion",
            "sexagintaoctingentillion",
            "septuagintaoctingentillion",
            "octogintaoctingentillion",
            "nonagintaoctingentillion",
            "nongentillion",
            "decinongentillion",
            "vigintinongentillion",
            "trigintanongentillion",
            "quadragintanongentillion",
            "quinquagintanongentillion",
            "sexagintanongentillion",
            "septuagintanongentillion",
            "octogintanongentillion",
            "nonagintanongentillion"
          ]
   in listh
        [ "",
          "thousand",
          "million",
          "billion",
          "trillion",
          "quadrillion",
          "quintillion",
          "sextillion",
          "septillion",
          "octillion",
          "nonillion",
          "decillion",
          "undecillion",
          "duodecillion",
          "tredecillion",
          "quattuordecillion",
          "quindecillion",
          "sexdecillion",
          "septendecillion",
          "octodecillion",
          "novemdecillion"
        ]
        ++ lift2 ((++) =<<) preillion postillion

-- A data type representing the digits zero to nine.
data Digit
  = Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  deriving (Eq, Ord, Show)

-- A data type representing one, two or three digits, which may be useful for grouping.
data Digit3
  = D1 Digit
  | D2 Digit Digit
  | D3 Digit Digit Digit
  deriving (Eq, Show)

data Dollars a = Dollars (List a) (List a)
  deriving (Show)

(<$~>) :: (List a -> List b) -> Dollars a -> Dollars b
(<$~>) f (Dollars whole cents) = Dollars (f whole) (f cents)

infixl 4 <$~>

bimap :: (List a -> List b) -> (List a -> List b) -> Dollars a -> Dollars b
bimap f g (Dollars whole cents) = Dollars (f whole) (g cents)

showDigit ::
  Digit ->
  Chars
showDigit Zero =
  "zero"
showDigit One =
  "one"
showDigit Two =
  "two"
showDigit Three =
  "three"
showDigit Four =
  "four"
showDigit Five =
  "five"
showDigit Six =
  "six"
showDigit Seven =
  "seven"
showDigit Eight =
  "eight"
showDigit Nine =
  "nine"

-- Possibly convert a character to a digit.
fromChar ::
  Char ->
  Optional Digit
fromChar '0' =
  Full Zero
fromChar '1' =
  Full One
fromChar '2' =
  Full Two
fromChar '3' =
  Full Three
fromChar '4' =
  Full Four
fromChar '5' =
  Full Five
fromChar '6' =
  Full Six
fromChar '7' =
  Full Seven
fromChar '8' =
  Full Eight
fromChar '9' =
  Full Nine
fromChar _ =
  Empty

readDigits :: Chars -> Dollars Digit
readDigits str =
  let zero = Dollars Nil Nil
      folder (digits, _) '.' = (digits, True)
      folder (digits, inCents) c = (optional (add digits inCents) digits (fromChar c), inCents)
      add (Dollars whole cents) False d = Dollars (d :. whole) cents
      add (Dollars whole cents) True d = Dollars whole (d :. cents)
   in bimap reverse (take 2 . reverse) $ fst $ foldLeft folder (zero, False) str

addDigit :: Digit -> Digit3 -> Optional Digit3
addDigit d (D1 a) = Full $ D2 d a
addDigit d (D2 a b) = Full $ D3 d a b
addDigit _d (D3 {}) = Empty

groupDigits :: Dollars Digit -> Dollars Digit3
groupDigits dollars' = group <$~> bimap emptyToZero (rightPad 2) dollars'
  where
    group = foldRight folder Nil
    emptyToZero Nil = pure Zero
    emptyToZero l = l
    rightPad n l = l ++ replicate (n - length l) Zero
    folder d Nil = pure (D1 d)
    folder d (d3 :. digits) = optional (:. digits) (D1 d :. d3 :. digits) (addDigit d d3)

trimLeftZero :: Digit3 -> Digit3
trimLeftZero (D2 Zero d) = D1 d
trimLeftZero (D3 Zero Zero d) = D1 d
trimLeftZero (D3 Zero d1 d2) = D2 d1 d2
trimLeftZero d = d

trimLeftZeros :: Dollars Digit3 -> Dollars Digit3
trimLeftZeros dollars' = map trimLeftZero <$~> dollars'

showTen :: Digit -> Chars
showTen Zero = error "Shoult not happen"
showTen One = error "Should not happen"
showTen Two = "twenty"
showTen Three = "thirty"
showTen Four = "forty"
showTen Five = "fifty"
showTen Six = "sixty"
showTen Seven = "seventy"
showTen Eight = "eighty"
showTen Nine = "ninety"

showOneTen :: Digit -> Chars
showOneTen Zero = "ten"
showOneTen One = "eleven"
showOneTen Two = "twelve"
showOneTen Three = "thirteen"
showOneTen Four = "fourteen"
showOneTen Five = "fifteen"
showOneTen Six = "sixteen"
showOneTen Seven = "seventeen"
showOneTen Eight = "eighteen"
showOneTen Nine = "nineteen"

showDigit3 :: Digit3 -> Chars
showDigit3 (D1 d) = showDigit d
showDigit3 (D2 Zero d) = showDigit d
showDigit3 (D2 One d) = showOneTen d
showDigit3 (D2 d Zero) = showTen d
showDigit3 (D2 d1 d2) = showTen d1 ++ "-" ++ showDigit d2
showDigit3 (D3 d Zero Zero) = showDigit d ++ " hundred"
showDigit3 (D3 d1 d2 d3) = showDigit d1 ++ " hundred and " ++ showDigit3 (D2 d2 d3)

joinBy :: List a -> List (List a) -> List a
joinBy _sep Nil = Nil
joinBy _sep (h :. Nil) = h
joinBy sep (h :. t) = h ++ sep ++ joinBy sep t

humanize :: Dollars Digit3 -> Dollars Char
humanize dollars' = humanize' <$~> dollars'
  where
    humanize' digits =
      let strDigits = map showDigit3 digits
          withIllion = zipWith addIllion (reverse strDigits) illion
       in joinBy " " $ flatten $ reverse withIllion
    addIllion strDigit "" = pure strDigit
    addIllion "zero" _ = Nil
    addIllion strDigit illion' = strDigit :. illion' :. Nil

withSuffixPlural :: Chars -> Chars -> Chars
withSuffixPlural suffix word@"one" = word ++ " " ++ suffix
withSuffixPlural suffix word = word ++ " " ++ suffix ++ "s"

-- | Take a numeric value and produce its English output.
--
-- >>> dollars "0"
-- "zero dollars and zero cents"
--
-- >>> dollars "1"
-- "one dollar and zero cents"
--
-- >>> dollars "0.1"
-- "zero dollars and ten cents"
--
-- >>> dollars "1."
-- "one dollar and zero cents"
--
-- >>> dollars "0."
-- "zero dollars and zero cents"
--
-- >>> dollars "0.0"
-- "zero dollars and zero cents"
--
-- >>> dollars ".34"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "0.3456789"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "1.0"
-- "one dollar and zero cents"
--
-- >>> dollars "1.01"
-- "one dollar and one cent"
--
-- >>> dollars "a1a"
-- "one dollar and zero cents"
--
-- >>> dollars "a1a.a0.7b"
-- "one dollar and seven cents"
--
-- >>> dollars "100"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.0"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00000"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "1000456.13"
-- "one million four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "1001456.13"
-- "one million one thousand four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "16000000456.13"
-- "sixteen billion four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "100.45"
-- "one hundred dollars and forty-five cents"
--
-- >>> dollars "100.07"
-- "one hundred dollars and seven cents"
--
-- >>> dollars "9abc9def9ghi.jkl9mno"
-- "nine hundred and ninety-nine dollars and ninety cents"
--
-- >>> dollars "12345.67"
-- "twelve thousand three hundred and forty-five dollars and sixty-seven cents"
--
-- >>> dollars "456789123456789012345678901234567890123456789012345678901234567890.12"
-- "four hundred and fifty-six vigintillion seven hundred and eighty-nine novemdecillion one hundred and twenty-three octodecillion four hundred and fifty-six septendecillion seven hundred and eighty-nine sexdecillion twelve quindecillion three hundred and forty-five quattuordecillion six hundred and seventy-eight tredecillion nine hundred and one duodecillion two hundred and thirty-four undecillion five hundred and sixty-seven decillion eight hundred and ninety nonillion one hundred and twenty-three octillion four hundred and fifty-six septillion seven hundred and eighty-nine sextillion twelve quintillion three hundred and forty-five quadrillion six hundred and seventy-eight trillion nine hundred and one billion two hundred and thirty-four million five hundred and sixty-seven thousand eight hundred and ninety dollars and twelve cents"
dollars ::
  Chars ->
  Chars
dollars str =
  let parse = trimLeftZeros . groupDigits . readDigits
      amount = parse str
      (Dollars whole cents) = bimap (withSuffixPlural "dollar") (withSuffixPlural "cent") $ humanize amount
   in whole ++ " and " ++ cents
