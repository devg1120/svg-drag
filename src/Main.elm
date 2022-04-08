module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import Svg as S
import Svg.Attributes as SA
import Html.Attributes exposing (class,id)



-- model ----------------------------------------------------------------------


main =
    Browser.document { init = init, update = update, view = view, subscriptions = subscriptions }

daycell_width =  40

task_bar_height = 18

task_bar_vertical_gap = 8

type alias Model =
    { tasks : List Task
    }


type alias Task =
    { id : TaskId
    , dependsOn : List TaskId
    , color : String
    , start : Int
    , length : Int
    }


type alias TaskId =
    String


type alias DrawableTask =
    { id : TaskId, col : Col, row : Row, len : Int, color : String }


type alias Col =
    Int


type alias Row =
    Int


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model initTask, Cmd.none )


initTask : List Task
initTask =
    let
        t1 =
            Task "task1" [] "grey" 1 2

        t2 =
            Task "task2" [ "task1" ] "fuchsia" 3 2

        t3 =
            Task "task3" [ "task1" ] "orange" 3 2

        t4 =
            Task "task4" [ "task3" ] "navy" 5 2

        t5 =
            Task "task5" [ "task2", "task4" ] "lightskyblue" 8 3
    in
    [ t1, t5, t4, t2, t3 ]



-- subscription ---------------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- update ---------------------------------------------------------------------


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- view frame -----------------------------------------------------------------


type alias Document msg =
    { title : String
    , body : List (Html msg)
    }


view : Model -> Document Msg
view model =
    { title = "DIJIT"
    , body = [ viewGantt model.tasks ]
    }



-- view gantt -----------------------------------------------------------------


viewGantt : List Task -> Html Msg
viewGantt tasks = 
    let
        cell n = 
             Html.div [ class  "cell"] [Html.text (String.fromInt n )]
        days = 
             List.range 1 31
        cells =
                List.map cell days
    in
    Html.div[] [
     Html.div [ class  "gantt-header"] 
     {--
       [
         Html.div [ class  "cell"] [Html.text "1"]
        ,Html.div [ class  "cell"] [Html.text "2"]
        ,Html.div [ class  "cell"] [Html.text "3"]
        ,Html.div [ class  "cell"] [Html.text "4"]
        ,Html.div [ class  "cell"] [Html.text "5"]
        ,Html.div [ class  "cell"] [Html.text "6"]
        ,Html.div [ class  "cell"] [Html.text "7"]
        ,Html.div [ class  "cell"] [Html.text "8"]
        ,Html.div [ class  "cell"] [Html.text "9"]
        ,Html.div [ class  "cell"] [Html.text "10"]
        ,Html.div [ class  "cell"] [Html.text "11"]
        ,Html.div [ class  "cell"] [Html.text "12"]
        ,Html.div [ class  "cell"] [Html.text "13"]
        ,Html.div [ class  "cell"] [Html.text "14"]
        ,Html.div [ class  "cell"] [Html.text "15"]
        ,Html.div [ class  "cell"] [Html.text "16"]
        ,Html.div [ class  "cell"] [Html.text "17"]
        ,Html.div [ class  "cell"] [Html.text "18"]
        ,Html.div [ class  "cell"] [Html.text "19"]
        ,Html.div [ class  "cell"] [Html.text "20"]
        ,Html.div [ class  "cell"] [Html.text "21"]
        ,Html.div [ class  "cell"] [Html.text "22"]
        ,Html.div [ class  "cell"] [Html.text "23"]
       ]
      --}
      (cells)
    ,
     Html.div [ ]
        [ S.svg [ SA.viewBox "0 0 100% 100%", SA.width "100%", SA.height "100%" ] <|
            List.map viewTask <|
                toDrawableListOfTasks tasks
        ]
   ]

viewTask : DrawableTask -> S.Svg Msg
viewTask task =
    let
        x =
            String.fromInt ((((-)task.col 1) * daycell_width) )

        y =
            String.fromInt ((task.row * (task_bar_height + task_bar_vertical_gap)) + task_bar_vertical_gap)
        
        w =
            String.fromInt ((task.len * daycell_width) )

        h =
            String.fromInt task_bar_height
    in
    S.rect [ SA.x x, SA.y y, SA.width w, SA.height h, SA.fill task.color ] []



-- transform list of tasks to a drawable list of tasks ------------------------


toDrawableListOfTasks : List Task -> List DrawableTask
toDrawableListOfTasks tasks =
    order initTemp tasks


{-| This stores the partial list of gantt tasks that can already be drawn
-}
type alias Temp =
    Dict TaskId DrawableTask


initTemp =
    Dict.empty


order : Temp -> List Task -> List DrawableTask
order temp todo =
    case List.partition (allDependenciesMet temp) todo of
        ( [], [] ) ->
            -- We are done and can return the list
            Dict.values temp
                |> List.sortBy .row

        ( [], _ ) ->
            Debug.todo "An invalid list of tasks was passed"

        ( drawableTasks, nextTodo ) ->
            let
                nextTemp =
                    List.indexedMap (toDrawable temp) drawableTasks
                        |> List.map (\t -> ( t.id, t ))
                        |> Dict.fromList
                        |> Dict.union temp
            in
            order nextTemp nextTodo


{-| Returns true if all tasks that need to be rendered above this one exist in temp
-}
allDependenciesMet : Temp -> Task -> Bool
allDependenciesMet temp task =
    List.all (\id -> Dict.member id temp) task.dependsOn


toDrawable : Temp -> Int -> Task -> DrawableTask
toDrawable temp index task =
    { id = task.id
    --, col = getColumn temp task
    , col = task.start
    , row = Dict.size temp + index
    , len = task.length
    , color = task.color
    }


getColumn : Temp -> Task -> Col
getColumn temp task =
    List.filterMap
        (\id -> Dict.get id temp |> Maybe.map (\p -> p.col + 1))
        task.dependsOn
        |> List.maximum
        |> Maybe.withDefault 0

