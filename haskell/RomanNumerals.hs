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

unsafeRomanNumerals' :: Int -> String
unsafeRomanNumerals' n = fst $ foldl
                                (\ (accStr, accNum) (str, num)
                                   -> let div = accNum `quot` num
                                      in
                                        if div == 0 then
                                            (accStr, accNum)
                                        else
                                            ((accStr ++ (concat $ replicate div str)), (accNum - (div * num))))
                                ("", n)
                                numerals