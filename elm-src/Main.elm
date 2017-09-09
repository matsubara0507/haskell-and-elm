module Main exposing (..)

import Generated.TodoAPI as API exposing (getTodos)

import Html as Html exposing (..)
import Html.Attributes exposing (style)
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
  | PushButton

model : Model
model = { todos = NotAsked }

init : Model -> (Model, Cmd Msg)
init model =  (model, Cmd.none)

view : Model -> Html Msg
view model =
  div []
    [ h2 [] [ text "ToDo !!" ]
    , button [ onClick PushButton ] [ text "Get ToDo!" ]
    , br [] []
    , viewToDos model
    ]

viewToDos : Model -> Html msg
viewToDos model =
  case model.todos of
    NotAsked -> text "Please Push Button."
    Loading -> text "Loading..."
    Failure err -> text ("Error: " ++ toString err)
    Success todos -> ul [] $ List.map viewTodo todos

viewTodo : API.Todo -> Html msg
viewTodo todo = li [ colorWithDone todo ] [ text todo.title ]

colorWithDone : API.Todo -> Attribute msg
colorWithDone todo =
  style [ ("color", if todo.done then "blue" else "red") ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    FetchTodos (Ok todos) -> ({ model | todos = Success todos }, Cmd.none)
    FetchTodos (Err _) -> ({ model | todos = Failure "Something went wrong.." }, Cmd.none)
    PushButton -> (model, fetchTodos)

fetchTodos : Cmd Msg
fetchTodos =
  Http.send FetchTodos getTodos

baseUrl : String
baseUrl = "localhost:8000"

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
