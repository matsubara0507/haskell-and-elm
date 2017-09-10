module Main exposing (..)

import Generated.TodoAPI as API exposing (Todo)

import Html as Html exposing (..)
import Html.Attributes exposing (class, style, type_, checked, placeholder, attribute)
import Html.Events exposing (onCheck, onClick, onInput)
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
  { todos : RemoteData String (List API.Todo)
  , title : String
  }

type Msg
  = FetchTodos (Result Http.Error (List API.Todo))
  | Reload
  | ChangeTodo Todo
  | Title String
  | Push Button

type Button = Post String
            | Delete Int

model : Model
model = { todos = NotAsked, title = "" }

init : Model -> (Model, Cmd Msg)
init model =  (model, fetchTodos)

view : Model -> Html Msg
view model =
  div [ class "container" ]
    [ h1 [] [ text "ToDo List !!" ]
    , viewToDos model
    , viewPost model
    ]

viewToDos : Model -> Html Msg
viewToDos model =
  case model.todos of
    NotAsked -> text "Please Push Button."
    Loading -> text "Loading..."
    Failure err -> text ("Error: " ++ toString err)
    Success todos ->
      div
        [ class "border-bottom" ]
        [ ul [] $ List.map viewTodo todos ]

viewTodo : API.Todo -> Html Msg
viewTodo todo =
  li
    [ class "Box-row" ]
    [ label
        [ class "float-left py-2 pl-3" ]
        [ input
            [ type_ "checkbox"
            , checked todo.done
            , onCheck (\b -> ChangeTodo $ { todo | done = b })
            ] []
        ]
    , div
        [ class "float-left col-9 p-2 lh-condensed" ]
        [ div [ class "h4" ] [ text todo.title ] ]
    , button
        [ class "btn-link", onClick . Push $ Delete todo.todoId ]
        [ i
            [ class "fa fa-trash-o"
            , attribute "aria-hidden" "true"
            , style [("color", "#cb2431")]
            ] []
        ]
    ]

viewPost : Model -> Html Msg
viewPost model =
  div []
    [ input [ class "form-control m-3"
            , type_ "text"
            , placeholder "Todo Title"
            , onInput Title
            ] []
    , span [] [ button [ class "btn"
                       , onClick . Push $ Post model.title
                       ]
                       [ text "Add Todo" ]
              ]
    ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    FetchTodos (Ok todos) -> ({ model | todos = Success todos }, Cmd.none)
    FetchTodos (Err _) -> ({ model | todos = Failure "Something went wrong.." }, Cmd.none)
    ChangeTodo todo -> (model, changeTodo todo)
    Title title -> ({ model | title = title }, Cmd.none)
    Push (Post title) -> (model, addTodo $ Todo 0 title False)
    Push (Delete todoId) -> (model, removeTodo todoId)
    Reload -> (model, fetchTodos)

fetchTodos : Cmd Msg
fetchTodos =
  Http.send FetchTodos API.getTodos

changeTodo : Todo -> Cmd Msg
changeTodo todo =
  Http.send (const Reload) $ API.putTodosById todo.todoId todo

addTodo : Todo -> Cmd Msg
addTodo todo =
  Http.send (const Reload) $ API.postTodos todo

removeTodo : Int -> Cmd Msg
removeTodo todoId =
  Http.send (const Reload) $ API.deleteTodosById todoId

baseUrl : String
baseUrl = "localhost:8000"

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
