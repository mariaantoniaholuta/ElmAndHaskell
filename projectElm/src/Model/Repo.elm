module Model.Repo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href)
import Json.Decode as De
import Http


type alias Repo =
    { name : String
    , description : Maybe String
    , url : String
    , pushedAt : String
    , stars : Int
    }


getUrl : Repo -> String
getUrl repo =
  repo.url

--converts description to string
descriptionToString : Maybe String -> String
descriptionToString description = 
    case description of
      Just d -> d
      Nothing -> ""

view : Repo -> Html msg
view repo =
    --div [] []
    --Debug.todo "Implement Model.Repo.view"
    div [class "repo"]
        [ div [class "repo-name"] [text (repo.name)],
          div [class "repo-description"] [text <| (Maybe.withDefault repo.name repo.description)],
          div [class "repo-url"] [a [href repo.url] [text (repo.pushedAt)]],
          div [class "repo-stars"] [text (String.fromInt repo.stars)]
        ]


sortByStars : List Repo -> List Repo
sortByStars repos =
    --[]
    --Debug.todo "Implement Model.Repo.sortByStars"
    List.sortBy .stars repos


{-| Deserializes a JSON object to a `Repo`.
Field mapping (JSON -> Elm):

  - name -> name
  - description -> description
  - html\_url -> url
  - pushed\_at -> pushedAt
  - stargazers\_count -> stars

-}
decodeRepo : De.Decoder Repo
decodeRepo =
    --De.fail "Not implemented"
    --Debug.todo "Implement Model.Repo.decodeRepo"
      De.map5 Repo
        (De.field "name" De.string)
        (De.maybe (De.field "description" De.string))
        (De.field "html_url" De.string)
        (De.field "pushed_at" De.string) 
        (De.field "stargazers_count" De.int) 



