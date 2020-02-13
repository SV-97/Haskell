data WeekDay
    = So
    | Mo
    | Di
    | Mi
    | Do
    | Fr
    | Sa
    deriving (Show, Enum)
  
  data Month
    = Jan
    | Feb
    | Mar
    | Apr
    | May
    | June
    | July
    | Aug
    | Sep
    | Oct
    | Nov
    | Dec
    deriving (Show, Eq, Enum)
  
  (|>) = flip ($)
  
  type Day = Integer
  
  type Year = Integer
  
  data Date =
    Date Day Month Year
  
  isLeapYear year =
    (year `mod` 4 == 0 && year `mod` 100 /= 0) || year `mod` 400 == 0
  
  monthDays month = sum $ map rawDays [Jan .. month]
    where
      rawDays Jan  = 0
      rawDays Feb  = 31
      rawDays Mar  = 28
      rawDays Apr  = 31
      rawDays May  = 30
      rawDays June = 31
      rawDays July = 30
      rawDays Aug  = 31
      rawDays Sep  = 31
      rawDays Oct  = 30
      rawDays Nov  = 31
      rawDays Dec  = 30
  
  daysUntil :: Date -> Integer
  daysUntil (Date day month year) =
    (year - 1900) |> (* 365) |> (+ ((year - 1900) `div` 4)) |>
    (if isLeapYear year && month `elem` [Jan, Feb]
       then (-1 +)
       else id) |>
    (+ monthDays month) |>
    (+ day)
  
  weekDayOf :: Date -> WeekDay
  weekDayOf date = (days `mod` 7) |> fromIntegral |> toEnum
    where
      days = daysUntil date
  
  main = do
    let day = 23
    let month = Apr
    let year = 1916
    print $ weekDayOf $ Date day month year
  