module Model.PersonalDetails exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, classList, id, href)


type alias DetailWithName =
    { name : String
    , detail : String
    }


type alias PersonalDetails =
    { name : String
    , contacts : List DetailWithName
    , intro : String
    , socials : List DetailWithName
    }

--those functions transforms the data into strings
detailWithNameToString : DetailWithName -> String
detailWithNameToString details = 
   details.name ++ " " ++ details.detail


detailWithNameLinkToString : DetailWithName -> String
detailWithNameLinkToString details = 
    details.detail


detailWithNameNameToString : DetailWithName -> String
detailWithNameNameToString details = 
    "Socials: " ++ details.name


--this function process every element and makes it's link
renderListLink : List String -> Html msg
renderListLink lst =
    lst
       |> List.map (\l -> li [] [ a [class "social-link", href l] [ text l ]])
       |> ul []


--this function process every element and makes it's name
renderListName : List String -> Html msg
renderListName lst =
    lst
       |> List.map (\l -> div [][ text l ])
       |> ul []


--detail represents {name, contacts, intro, socials} from type PersonalDetails
view : PersonalDetails -> Html msg
view detail =
    --div [] []
    --Debug.todo "Implement the Model.PersonalDetails.view function"
    let
      contacts = 
        detail.contacts
        |> List.map detailWithNameToString
        |> List.intersperse " "
        |> List.map text
      socialsLink = 
        detail.socials
        |> List.map detailWithNameLinkToString 
      socialsName = 
        detail.socials
        |> List.map detailWithNameNameToString 
    in  
      div []
        [ h1 [ id "name" ] [ text "Holuta Maria" ],
          --h1 [ id "name" ] [ text detail.name ],
          em [ id "intro" ] [ text detail.intro ],
          --div [ class "contact-detail" ] contacts,
          div [ class "contact-detail" ] [ text "email holuta.maria@gmail.com" ],
          div [] [ renderListName (socialsName) ],
          div [] [ renderListLink (socialsLink) ]
        ]
