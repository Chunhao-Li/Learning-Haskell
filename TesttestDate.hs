module TesttestDate where
type Date = (Int, Int, Int) 

{-|
  The function 'showDate' converts a 'Date' triple into a string for that date.
  Make sure that days ending in 1 have a suffix "st", days ending in 2 have a suffix of "nd",
  and days ending in 3 have a suffix of "rd".  Otherwise the day has a suffix of "th".

  Examples:

  >>> showDate (1,1,2001)
  "1st January, 2001"

  >>> showDate (2,2,2002)
  "2nd February, 2002"

  >>> showDate (3,3,2003)
  "3rd March, 2003"

  >>> showDate (4,4,2004)
  "4th April, 2004"

  >>> showDate (15,3,1999)
  "15th March, 1999"
-}


showDate :: Date -> String
showDate (d,m,y) = showday d ++ " " ++ showmonth m  ++ ", " ++ show y

showday:: Int -> String
showday d
        | d `mod` 10 == 1 && d /= 11 = show d ++ "st"
        | d `mod` 10 == 2 && d /= 12 = show d ++ "nd"
        | d `mod` 10 == 3 && d /= 13 = show d ++ "rd"
        | otherwise = show d ++ "th"




showmonth :: Int -> String
showmonth m = case m of
              1 -> "January"
              2 -> "February"
              3 -> "March"
              4 -> "April"
              5 -> "May"
              6 -> "June"
              7 -> "July"
              8 -> "August"
              9 -> "September"
              10 -> "October"
              11 -> "November"
              12 -> "December"
              _ -> error "beyond the range"

