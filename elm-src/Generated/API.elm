module Generated.API exposing (Todo, deleteApiTodosById, getApiTodos, jsonDecTodo, jsonEncTodo, postApiTodos, putApiTodosById)

-- The following module comes from bartavelle/json-helpers

import Dict exposing (Dict)
import Http
import Json.Decode
import Json.Encode exposing (Value)
import Json.Helpers exposing (..)
import Set
import String
import Url.Builder


type alias Todo =
    { id : Int
    , title : String
    , done : Bool
    }


jsonDecTodo : Json.Decode.Decoder Todo
jsonDecTodo =
    Json.Decode.succeed (\pid ptitle pdone -> { id = pid, title = ptitle, done = pdone })
        |> required "id" Json.Decode.int
        |> required "title" Json.Decode.string
        |> required "done" Json.Decode.bool


jsonEncTodo : Todo -> Value
jsonEncTodo val =
    Json.Encode.object
        [ ( "id", Json.Encode.int val.id )
        , ( "title", Json.Encode.string val.title )
        , ( "done", Json.Encode.bool val.done )
        ]


getApiTodos : (Result Http.Error (List Todo) -> msg) -> Cmd msg
getApiTodos toMsg =
    let
        params =
            List.filterMap identity
                (List.concat
                    []
                )
    in
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin ""
                [ "api"
                , "todos"
                ]
                params
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson toMsg (Json.Decode.list jsonDecTodo)
        , timeout =
            Nothing
        , tracker =
            Nothing
        }


postApiTodos : Todo -> (Result Http.Error Todo -> msg) -> Cmd msg
postApiTodos body toMsg =
    let
        params =
            List.filterMap identity
                (List.concat
                    []
                )
    in
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin ""
                [ "api"
                , "todos"
                ]
                params
        , body =
            Http.jsonBody (jsonEncTodo body)
        , expect =
            Http.expectJson toMsg jsonDecTodo
        , timeout =
            Nothing
        , tracker =
            Nothing
        }


putApiTodosById : Int -> Todo -> (Result Http.Error () -> msg) -> Cmd msg
putApiTodosById capture_id body toMsg =
    let
        params =
            List.filterMap identity
                (List.concat
                    []
                )
    in
    Http.request
        { method =
            "PUT"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin ""
                [ "api"
                , "todos"
                , capture_id |> String.fromInt
                ]
                params
        , body =
            Http.jsonBody (jsonEncTodo body)
        , expect =
            Http.expectString
                (\x ->
                    case x of
                        Err e ->
                            toMsg (Err e)

                        Ok _ ->
                            toMsg (Ok ())
                )
        , timeout =
            Nothing
        , tracker =
            Nothing
        }


deleteApiTodosById : Int -> (Result Http.Error () -> msg) -> Cmd msg
deleteApiTodosById capture_id toMsg =
    let
        params =
            List.filterMap identity
                (List.concat
                    []
                )
    in
    Http.request
        { method =
            "DELETE"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin ""
                [ "api"
                , "todos"
                , capture_id |> String.fromInt
                ]
                params
        , body =
            Http.emptyBody
        , expect =
            Http.expectString
                (\x ->
                    case x of
                        Err e ->
                            toMsg (Err e)

                        Ok _ ->
                            toMsg (Ok ())
                )
        , timeout =
            Nothing
        , tracker =
            Nothing
        }
