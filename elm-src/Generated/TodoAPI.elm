module Generated.TodoAPI exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String


type alias Todo =
    { id : Int
    , title : String
    , done : Bool
    }

decodeTodo : Decoder Todo
decodeTodo =
    decode Todo
        |> required "id" int
        |> required "title" string
        |> required "done" bool

encodeTodo : Todo -> Json.Encode.Value
encodeTodo x =
    Json.Encode.object
        [ ( "id", Json.Encode.int x.id )
        , ( "title", Json.Encode.string x.title )
        , ( "done", Json.Encode.bool x.done )
        ]

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

postTodos : Todo -> Http.Request (Todo)
postTodos body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ "http://localhost:8080"
                , "todos"
                ]
        , body =
            Http.jsonBody (encodeTodo body)
        , expect =
            Http.expectJson decodeTodo
        , timeout =
            Nothing
        , withCredentials =
            False
        }

putTodosById : Int -> Todo -> Http.Request (())
putTodosById capture_id body =
    Http.request
        { method =
            "PUT"
        , headers =
            []
        , url =
            String.join "/"
                [ "http://localhost:8080"
                , "todos"
                , capture_id |> toString |> Http.encodeUri
                ]
        , body =
            Http.jsonBody (encodeTodo body)
        , expect =
            Http.expectStringResponse
                (\{ body } ->
                    if String.isEmpty body then
                        Ok ()
                    else
                        Err "Expected the response body to be empty"
                )
        , timeout =
            Nothing
        , withCredentials =
            False
        }

deleteTodosById : Int -> Http.Request (())
deleteTodosById capture_id =
    Http.request
        { method =
            "DELETE"
        , headers =
            []
        , url =
            String.join "/"
                [ "http://localhost:8080"
                , "todos"
                , capture_id |> toString |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectStringResponse
                (\{ body } ->
                    if String.isEmpty body then
                        Ok ()
                    else
                        Err "Expected the response body to be empty"
                )
        , timeout =
            Nothing
        , withCredentials =
            False
        }