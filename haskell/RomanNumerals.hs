import Control.Applicative
import Data.List

numerals = [("M", 1000), ("CM", 900), ("D", 500), ("CD", 400), ("C", 100),
            ("XC", 90), ("L", 50), ("XL", 40), ("X", 10), ("IX", 9), ("V", 5),
            ("IV", 4), ("I", 1)]

unsafeRomanNumerals :: Int -> String
unsafeRomanNumerals n = case (find (\(_, x) -> x <= n) numerals) of
                        Just(l, num) -> l ++ unsafeRomanNumerals (n - num)
                        Nothing -> ""

safeRomanNumerals :: Int -> Either String String
safeRomanNumerals n
        | n <= 0        = Left "Numbers of 0 or below not supported"
        | n >= 5000     = Left "Numbers of 5000 and over not supported"
        | otherwise     = Right $ unsafeRomanNumerals n

romanNumerals :: Int -> Either String String
romanNumerals n
        | n >= 5000         = Left "Numbers of 5000 and over not supported"
        | n >= 1000         = (++) <$> Right "M" <*> romanNumerals (n - 1000)
        | n >= 900          = (++) <$> Right "CM" <*> romanNumerals (n - 900)
        | n >= 500          = (++) <$> Right "D" <*> romanNumerals (n - 500)
        | n >= 400          = (++) <$> Right "CD" <*> romanNumerals (n - 400)
        | n >= 100          = (++) <$> Right "C" <*> romanNumerals (n - 100)
        | n >= 90           = (++) <$> Right "XC" <*> romanNumerals (n - 90)
        | n >= 50           = (++) <$> Right "L" <*> romanNumerals (n - 50)
        | n >= 40           = (++) <$> Right "XL" <*> romanNumerals (n - 40)
        | n >= 10           = (++) <$> Right "X" <*> romanNumerals (n - 10)
        | n == 9             = Right "IX"
        | n >= 5            = (++) <$> Right "V" <*> (romanNumerals (n - 5))
        | n == 4            = Right "IV"
        | n >= 1 && n <= 3  = Right (replicate n 'I')
        | n <= 0            = Left "Numbers of 0 or below not supported"

romanNumerals' :: Int -> String
romanNumerals' n
        | n >= 5000         = "N"
        | n >= 1000         = "M" ++ romanNumerals' (n - 1000)
        | n >= 900          = "CM" ++ romanNumerals' (n - 900)
        | n >= 500          = "D" ++ romanNumerals' (n - 500)
        | n >= 400          = "CD" ++ romanNumerals' (n - 400)
        | n >= 100          = "C" ++ romanNumerals' (n - 100)
        | n >= 90           = "XC" ++ romanNumerals' (n - 90)
        | n >= 50           = "L" ++ romanNumerals' (n - 50)
        | n >= 40           = "XL" ++ romanNumerals' (n - 40)
        | n >= 10           = "X" ++ romanNumerals' (n - 10)
        | n == 9            = "IX"
        | n >= 5            = "V" ++ romanNumerals' (n - 5)
        | n == 4            = "IV"
        | n >= 1 && n <= 3  = replicate n 'I'
        | otherwise         = "N"
