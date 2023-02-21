module Model.Event.Category exposing (EventCategory(..), SelectedEventCategories, allSelected, eventCategories, isEventCategorySelected, set, view)

import Html exposing (Html, div, input, text)
import Html.Attributes exposing (checked, class, style, type_)
import Html.Events exposing (onCheck)


type EventCategory
    = Academic
    | Work
    | Project
    | Award


eventCategories =
    [ Academic, Work, Project, Award ]


{-| Type used to represent the state of the selected event categories
-}
type SelectedEventCategories  
      --= TODOCompleteThisType 
   = SelectedEventCategories {academicSelected : Bool, workSelected: Bool, projectSelected: Bool, awardSelected: Bool}

{-| Returns an instance of `SelectedEventCategories` with all categories selected

    isEventCategorySelected Academic allSelected --> True

-}
allSelected : SelectedEventCategories
allSelected = 
     --TODOCompleteThisType
    --Debug.todo "Implement Model.Event.Category.allSelected"
    SelectedEventCategories {academicSelected = True, workSelected = True, projectSelected = True, awardSelected = True}

{-| Returns an instance of `SelectedEventCategories` with no categories selected

-- isEventCategorySelected Academic noneSelected --> False

-}
noneSelected : SelectedEventCategories
noneSelected = 
     --TODOCompleteThisType
    --Debug.todo "Implement Model.Event.Category.noneSelected"
     SelectedEventCategories {academicSelected = False, workSelected = False, projectSelected = False, awardSelected = False}

{-| Given a the current state and a `category` it returns whether the `category` is selected.

    isEventCategorySelected Academic allSelected --> True

-}
isEventCategorySelected : EventCategory -> SelectedEventCategories -> Bool
isEventCategorySelected category current = 
    -- False
    --Debug.todo "Implement Model.Event.Category.isEventCategorySelected"
    let
        (SelectedEventCategories c) = current
    in 
        case category of
           Academic -> c.academicSelected
           Work -> c.workSelected
           Project -> c.projectSelected
           _ -> c.awardSelected

{-| Given an `category`, a boolean `value` and the current state, it sets the given `category` in `current` to `value`.

    allSelected |> set Academic False |> isEventCategorySelected Academic --> False

    allSelected |> set Academic False |> isEventCategorySelected Work --> True

-}
set : EventCategory -> Bool -> SelectedEventCategories -> SelectedEventCategories
set category value current =
    if(current == allSelected) then 
       case category of
          Academic -> SelectedEventCategories {academicSelected = value, workSelected = True, projectSelected = True, awardSelected = True}
          Work -> SelectedEventCategories {academicSelected = True, workSelected = value, projectSelected = True, awardSelected = True}
          Project -> SelectedEventCategories {academicSelected = True, workSelected = True, projectSelected = value, awardSelected = True}
          Award -> SelectedEventCategories {academicSelected = True, workSelected = True, projectSelected = True, awardSelected = value}
    else
        case category of
          Academic -> SelectedEventCategories {academicSelected = value, workSelected = value, projectSelected = False, awardSelected = False}
          Work -> SelectedEventCategories {academicSelected = False, workSelected = value, projectSelected = False, awardSelected = False}
          Project -> SelectedEventCategories {academicSelected = False, workSelected = False, projectSelected = value, awardSelected = False}
          Award -> SelectedEventCategories {academicSelected = False, workSelected = False, projectSelected = False, awardSelected = value}  

   
-- Debug.todo "Implement Model.Event.Category.set"
    


checkbox : String -> Bool -> EventCategory -> Html ( EventCategory, Bool )
checkbox name state category =
    div [ style "display" "inline", class "category-checkbox" ]
        [ input [ type_ "checkbox", onCheck (\c -> ( category, c )), checked state ] []
        , text name
        ]


view : SelectedEventCategories -> Html ( EventCategory, Bool )
view model =
   let
      (SelectedEventCategories selected) = model
   in
     div [] 
      [ checkbox "Academic" selected.academicSelected Academic, 
        checkbox "Work" selected.workSelected Work,
        checkbox "Project" selected.projectSelected Project,
        checkbox "Award" selected.awardSelected Award
      ] 
     --Debug.todo "Implement the Model.Event.Category.view function"
