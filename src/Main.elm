module Main exposing (main)

import Browser
import Html exposing (Html, button, div, img, input, text)
import Html.Attributes exposing (placeholder, src, style, type_, value)
import Html.Events exposing (onClick, onInput)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


type State
    = Done
    | InProgress
    | Stalled


type alias Todo =
    { id : Int
    , description : String
    , state : State
    }


type alias TodoList =
    List Todo


type alias Model =
    { todos : TodoList
    , newTodo : String
    }


init : Model
init =
    { todos = []
    , newTodo = ""
    }


type Msg
    = AddTodo
    | DeleteTodo Int
    | ChangeNewTodo String


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddTodo ->
            { model | todos = addTodo model.todos model.newTodo }

        DeleteTodo id ->
            { model | todos = deleteTodo model.todos id }

        ChangeNewTodo newTodo ->
            { model | newTodo = newTodo }


view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "text", placeholder "Type a new todo", value model.newTodo, onInput ChangeNewTodo ] []
        , button [ onClick AddTodo ] [ text "Add Todo" ]
        ]


createNewTodo : Int -> String -> Todo
createNewTodo id description =
    Todo id description Stalled


nextId : TodoList -> Int
nextId todoList =
    List.length todoList + 1


addTodo : TodoList -> String -> TodoList
addTodo prevTodoList description =
    prevTodoList ++ [ createNewTodo (nextId prevTodoList) description ]


deleteTodo : TodoList -> Int -> TodoList
deleteTodo oldList id =
    List.filter (\todo -> todo.id /= id) oldList
