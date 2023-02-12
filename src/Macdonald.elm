module Macdonald exposing (..)

import Browser
import Html exposing (Html)
import Html.Events as Events
import Html.Attributes as Attrs
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import SingleSlider
import Random
import Time
import Permutation

type alias Model = {lambda: List Int
                   ,newParts: List Int
                   ,q: Float
                   ,t: Float
                   ,qSlider: SingleSlider.SingleSlider Msg
                   ,tSlider: SingleSlider.SingleSlider Msg
                   }

type Msg = QChanged Float
    | TChanged Float
    | Elapsed Time.Posix
    | QGenerated (List Bool)
    | TGenerated (List Bool)
    | PermGenerated Permutation.Permutation

main = Browser.element {init = init
                       ,update = update
                       ,view = view
                       ,subscriptions = subscriptions
                       }
k=100
w = 2000
unit = round (w/(sqrt (toFloat k))/15)
       
init : () -> (Model, Cmd Msg)
init _ = (Model (List.reverse <| List.range 1 15) [] 1.01 1.01 initQSlider initTSlider,
              Cmd.none)

minFormatter = \value -> String.fromFloat value         
initQSlider = SingleSlider.init
                    { min = 1.001
                    , max = 6
                    , value = 1.01
                    , step = 0.001
                    , onChange = QChanged
                    }
            |> SingleSlider.withMinFormatter minFormatter

initTSlider = SingleSlider.init
                    { min = 1.001
                    , max = 6
                    , value = 1.01
                    , step = 0.001
                    , onChange = TChanged
                    }
            |> SingleSlider.withMinFormatter minFormatter
         
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        QChanged q ->
            let
                qSlider =
                    SingleSlider.update q model.qSlider
            in
                ({model|q = q, qSlider = qSlider}, Cmd.none)
        TChanged t ->
            let
                tSlider =
                    SingleSlider.update t model.tSlider
            in
                ({model|t = t, tSlider = tSlider}, Cmd.none)
        Elapsed time ->
            (model, Random.generate QGenerated (qCoin model))
        QGenerated rBoolList ->
            let
                rBoolTable = List.foldr
                             (\l (blist, rest) ->
                                  ((List.take l rest)::blist
                                  ,(List.drop l rest)
                                  )
                             )
                             ([],rBoolList) model.lambda
                stay = Debug.log "" <|
                        List.map (\bl -> List.all (\b -> b) bl) (Tuple.first rBoolTable)
                newLambda = List.map2 (\l s -> if s then
                                                   l
                                               else
                                                   0
                                  ) model.lambda stay |> List.filter (\l -> l > 0)
                r = (List.sum model.lambda) - (List.sum newLambda)
            in
                ({model|lambda = newLambda}, Random.generate PermGenerated (Permutation.random r))
        TGenerated rBoolList ->
            let
                rBoolTable = List.foldr
                             (\l (blist, rest) ->
                                  ((List.take l rest)::blist
                                  ,(List.drop l rest)
                                  )
                             )
                             ([],rBoolList) model.newParts
                chosen = List.all (\na -> na)  <|
                         List.map (\bl -> not<| List.all (\b -> b) bl) (Tuple.first rBoolTable)
                newLambda = if chosen then
                                List.reverse <| List.sort <| (model.lambda ++ model.newParts)
                            else
                                model.lambda
            in
                if chosen then
                    ({model|lambda = newLambda, newParts = []}, Cmd.none)
                else
                    (model, Random.generate PermGenerated (Permutation.random (List.sum model.newParts)))
        PermGenerated perm ->
            let
                newParts = Permutation.perm2Partition perm
            in
                ({model|newParts = newParts}
                ,Random.generate TGenerated (tCoin model)
                )
                
qCoin: Model -> Random.Generator (List Bool)
qCoin model =
    Random.list (List.sum model.lambda)
        (Random.weighted (1/model.q, True)
             [((1.0-1/model.q), False)]
        )
tCoin: Model -> Random.Generator (List Bool)
tCoin model =
    Random.list (List.sum model.newParts)
        (Random.weighted (1/model.t, True)
             [((1.0-1/model.t), False)]
        )

        
view : Model -> Html Msg
view model =
    Html.div [][Html.br[][]
               ,Html.text "q"
               ,SingleSlider.view model.qSlider
               ,Html.text "t"
               ,SingleSlider.view model.tSlider
               ,svg [width (String.fromFloat w)
                    ,height "1000"
                    ]
                    [partitionView model.lambda 
                    ]
               ]
partitionView : List Int -> Svg Msg
partitionView lambda =
    g [transform "translate(50,50)"
      ]<| List.indexedMap rowView lambda
        
rowView : Int -> Int -> Svg Msg
rowView i l =
    rect [x "0"
         ,y (String.fromInt (unit*i))
         ,width (String.fromInt (unit*l))
         ,height (String.fromInt unit)
         ,fill "gray"
         ,stroke "black"
         ][]
        
        
subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 100 Elapsed
