module Main exposing (..)

import Generated.TodoAPI as API exposing (getTodos)

import Html as Html exposing (..)
import Html.Attributes exposing (style, type_, checked)
import Html.Events exposing (onClick)
import Http
import RemoteData exposing (RemoteData(..))

import Data.Composition exposing (..)

main : Program Never Model Msg
main =
  Html.program
    { init = init model
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Model =
  { todos : RemoteData String (List API.Todo) }

type Msg
  = FetchTodos (Result Http.Error (List API.Todo))

model : Model
model = { todos = NotAsked }

init : Model -> (Model, Cmd Msg)
init model =  (model, fetchTodos)

view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text "ToDo List !!" ]
    , viewToDos model
    ]

viewToDos : Model -> Html Msg
viewToDos model =
  case model.todos of
    NotAsked -> text "Please Push Button."
    Loading -> text "Loading..."
    Failure err -> text ("Error: " ++ toString err)
    Success todos ->
      List.map viewTodo todos
      |> (::) (tr [] [ th [] [ text "ToDo" ], th [] [ text "Done" ] ])
      |> table []

viewTodo : API.Todo -> Html Msg
viewTodo todo =
  tr [] [ td [] [ text todo.title ]
        , td [] [ input [ type_ "checkbox", checked todo.done ] [] ] ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    FetchTodos (Ok todos) -> ({ model | todos = Success todos }, Cmd.none)
    FetchTodos (Err _) -> ({ model | todos = Failure "Something went wrong.." }, Cmd.none)

fetchTodos : Cmd Msg
fetchTodos =
  Http.send FetchTodos getTodos

baseUrl : String
baseUrl = "localhost:8000"

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
