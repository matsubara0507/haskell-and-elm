module Generated.TodoAPI exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String


type alias Todo =
    { todoId : Int
    , title : String
    , done : Bool
    }

decodeTodo : Decoder Todo
decodeTodo =
    decode Todo
        |> required "todoId" int
        |> required "title" string
        |> required "done" bool

getTodos : Http.Request (List (Todo))
getTodos =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ "http://localhost:8080"
                , "todos"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeTodo)
        , timeout =
            Nothing
        , withCredentials =
            False
        }