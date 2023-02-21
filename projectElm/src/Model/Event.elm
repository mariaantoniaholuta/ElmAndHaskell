module Model.Event exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Model.Event.Category exposing (EventCategory(..))
import Model.Interval as Interval exposing (Interval)
import Model.Date as Date exposing (Date)


type alias Event =
    { title : String
    , interval : Interval
    , description : Html Never
    , category : EventCategory
    , url : Maybe String
    , tags : List String
    , important : Bool
    }


categoryView : EventCategory -> Html Never
categoryView category =
    case category of
        Academic ->
            text "Academic"

        Work ->
            text "Work"

        Project ->
            text "Project"

        Award ->
            text "Award"


orderBy : (a -> Interval) -> (Interval -> Interval -> Order) -> a -> a -> Order
orderBy accessor orderFunc a b =
        orderFunc (accessor a) (accessor b)

sortByWith : (a -> Interval) -> (Interval -> Interval -> Order) -> List a -> List a
sortByWith accessor sortFunc list =
    List.sortWith (orderBy accessor sortFunc) list


sortByInterval : List Event -> List Event
sortByInterval events =
    --events
    --Debug.todo "Implement Event.sortByInterval"
    let
        compareEvents event1 event2 =
           Interval.compare event1.interval event2.interval
    in
        List.sortWith compareEvents events 
    --or using orderBy and sortByWith
    --sortByWith .interval Interval.compare events

--transforms category type data into string
eventCategoryToString : EventCategory -> String
eventCategoryToString category = 
    case category of
      Academic -> "Academic"
      Work -> "Work"
      Project -> "Project"
      Award -> "Award"


urlToString : Maybe String -> String
urlToString url = 
    case url of
      Just u -> u
      Nothing -> "null"


boolToString : Bool -> String
boolToString b = 
    case b of
      True -> "True"
      False -> "False"


--checks and transforms class if important field is true
classImportant : Bool -> String -> String
classImportant b str = 
    case b of
      True -> "event-important"
      False -> str


--makes interval values into a string
intervalToString : Interval -> String
intervalToString i = 
    "Start: " ++ String.fromInt(Date.year (Interval.startI i)) ++ (Date.monthToStringM (Date.month (Interval.startI i))) 


view : Event -> Html Never
view event =
    div [] [
    --Debug.todo "Implement the Model.Event.view function"
      p [class "event", class (classImportant event.important "")]
        [ p [class "event-title", class (classImportant event.important "")] [text event.title],
          p [class "event-description", class (classImportant event.important "")] [event.description],
          p [class "event-category", class (classImportant event.important "")] [ text (eventCategoryToString event.category)],
          p [class "event-url", class (classImportant event.important "")] [text (urlToString event.url)],
          p [] [text (boolToString event.important)],
          p [class "event-interval"] [text (intervalToString event.interval)]
        ]
    ]
