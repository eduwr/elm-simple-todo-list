module Main exposing (main)

import Browser
import Html exposing (Html, div, img)
import Html.Attributes exposing (src, style)


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
    }


init : Model
init =
    { todos = []
    }


type Msg
    = AddTodo String
    | DeleteTodo Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddTodo description ->
            { model | todos = addTodo model.todos description }

        DeleteTodo id ->
            { model | todos = deleteTodo model.todos id }


view : Model -> Html Msg
view model =
    div []
        []


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
