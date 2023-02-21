module Model.Interval exposing (Interval, compare, full, length, oneYear, open, view, withDurationMonths, withDurationYears, startI, endI)

import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class, style)
import Model.Date as Date exposing (Date, Month)
import Model.Util exposing (chainCompare)


type Interval
    = Interval { start : Date, end : Maybe Date }


--converts the year into string
yearToString : Maybe Date -> String
yearToString date =
    case date of
      Just d -> String.fromInt (Date.year d)
      Nothing -> "Present"


--converts the month into string
monthToString : Maybe Date -> String
monthToString date =
    case date of
      Just d -> Date.monthToStringM (Date.month d)
      Nothing -> ""


--get the end date
endI : Interval -> Maybe Date
endI (Interval i) =
    i.end


--get the start date
startI : Interval -> Date
startI (Interval i) =
    i.start


--get the start month
startIMonth : Interval -> Maybe Month
startIMonth (Interval i) =
    Date.month i.start


--get the start year
startIYear : Interval -> Int
startIYear (Interval i) =
    Date.year i.start


--get the end month
endIMonth : Interval -> String
endIMonth (Interval i) =
    monthToString i.end


--get the end year
endIYear : Interval -> String
endIYear (Interval i) =
    yearToString i.end


--this function compares the start date from the two intervals
compareStart : Interval -> Interval -> Order
compareStart (Interval intA) (Interval intB) =
     Date.compare intA.start intB.start


--this function compares the end date from the two intervals
compareEnd : Interval -> Interval -> Order
compareEnd (Interval intA) (Interval intB) =
     case (intA.end, intB.end) of
        (Just endA, Just endB) -> Date.compare endA endB        
        (Nothing, Nothing) -> EQ
        (Nothing, Just endB) -> GT
        (Just endA, Nothing) -> LT


{-| Create an `Interval` from 2 `Date`s. If the second date is before the first the date, the function will return
`Nothing`. When possible, use the `withDurationMonths` or `withDurationYears` functions.
-}
full : Date -> Date -> Maybe Interval
full start end =
    if Date.compare start end == GT then
        Nothing

    else
        Just <| Interval { start = start, end = Just end }


{-| Create an `Interval` from a start year, start month, and a duration in months.
The start year and month are explicitly required because the duration in months is only specified if the start date
also includes a month.
This function, (assuming positive inputs) by definition, can always return a valid `Interval`.
-}
withDurationMonths : Int -> Month -> Int -> Interval
withDurationMonths startYear startMonth duration =
    let
        start =
            Date.full startYear startMonth

        end =
            Date.offsetMonths duration start
    in
        Interval { start = start, end = Just end }


{-| Create an `Interval` from a start `Date`, and a duration in years. This function, (assuming positive inputs)
by definition, can always return a valid `Interval`.
-}
withDurationYears : Date -> Int -> Interval
withDurationYears start duration =
    let
        end =
            Date.offsetMonths (duration * 12) start
    in
    Interval { start = start, end = Just end }


{-| Create an open `Interval` from a start `Date`. Usually used for creating ongoing events.
-}
open : Date -> Interval
open start =
    Interval { start = start, end = Nothing }


{-| Convenience function to create an `Interval` that represents one year.
-}
oneYear : Int -> Interval
oneYear year =
    withDurationYears (Date.onlyYear year) 1


{-| The length of an Interval, in (years, months)
-}
length : Interval -> Maybe ( Int, Int )
length (Interval interval) =
    interval.end
        |> Maybe.andThen (Date.monthsBetween interval.start)
        |> Maybe.map (\totalMonths -> ( totalMonths // 12, modBy 12 totalMonths ))


{-| Compares two intervals.

Intervals are first compared compare by the `start` field.
If the `start` field is equal, the they are compare by the `end` fields:

  - If both are missing (`Nothing`), the intervals are considered equal
  - If both are present (`Just`), the longer interval is considered greater
  - If only one interval is open (its `end` field is `Nothing`) then it will be considered greater

```
    import Model.Date as Date

    Model.Interval.compare (oneYear 2019) (oneYear 2020) --> LT
    Model.Interval.compare (oneYear 2019) (withDurationYears (Date.onlyYear 2020) 2) --> LT
    Model.Interval.compare (withDurationMonths 2019 Date.Jan 2) (withDurationMonths 2019 Date.Jan 2) --> EQ
    Model.Interval.compare (withDurationMonths 2019 Date.Feb 2) (withDurationMonths 2019 Date.Jan 2) --> GT
    Model.Interval.compare (withDurationMonths 2019 Date.Jan 2) (open (Date.onlyYear 2019)) --> LT
```

-}
compare : Interval -> Interval -> Order
compare (Interval intA) (Interval intB) =
    --EQ
    --Debug.todo "Implement Model.Interval.compare"
    let
       startCompareResult = compareStart (Interval intA) (Interval intB)
    in
       let
           endCompareResult = compareEnd (Interval intA) (Interval intB)
       in
           chainCompare endCompareResult startCompareResult



--converts two integers (years and months) to string
yearMonthToString : Int -> Int -> String
yearMonthToString years months =
  ((String.fromInt years) ++ " years, " ++ (String.fromInt months) ++ " months")



view : Interval -> Html msg
view interval =
    --div [] []
    --Debug.todo "Implement Model.Interval.view"
    let
      intervalLength = 
        length interval
        |> Maybe.map (\( years, months ) -> yearMonthToString years months)
        |> Maybe.withDefault ""      
    in 
      div [class "interval"]
        [ div [class "interval-start"] [text (String.fromInt (startIYear interval) ++ (Date.monthToStringM (startIMonth interval)) )],
          div [class "interval-end"] [text ((endIYear interval) ++ (endIMonth interval))],
          div [class "interval-length"] [text (intervalLength)] 
        ]
