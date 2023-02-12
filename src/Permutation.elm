module Permutation exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes as Attrs
import Random.List
import Random

main = Browser.element {init = init
                       ,view = view
                       ,update = update
                       ,subscriptions = subscriptions
                       }

type alias Permutation = List Int

type alias Model = {perm: Permutation}
type Msg = PermGeneraed Permutation
    
init : () -> (Model, Cmd Msg)
init _ = (Model [], Random.generate PermGeneraed (random 1000))
    
random : Int -> Random.Generator Permutation
random size =
    Random.List.shuffle <| List.range 1 size

-- cycleLengths : Permutation -> List Int

orbit : Permutation -> Int -> List Int -> List Int
orbit perm x orb =
    if List.member x orb then
        orb
    else
        let
            y = Maybe.withDefault 0 <| List.head (List.drop (x-1) perm)
        in
            orbit perm y (orb++[x])

cycleDecomposition : Permutation -> List (List Int)
cycleDecomposition perm =
    List.foldr (\x orbits -> if List.member x (List.concat orbits) then
                                 orbits
                             else
                                 orbits++[orbit perm x []]
               ) [] (List.range 1 (List.length perm))

perm2Partition perm =
    List.reverse <| List.sort <| List.map List.length <| cycleDecomposition perm
        
perm2String: Permutation -> String
perm2String perm =
    "(" ++ (String.join "," <| List.map String.fromInt perm) ++ ")"

                
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        PermGeneraed perm ->
            ({model|perm = perm}, Cmd.none)

view : Model -> Html Msg
view model =
    Html.div[]([Html.text <| perm2String model.perm
               ,Html.br [][]
               ]++
                   (List.map (\cycle ->
                                  Html.text <| perm2String cycle
                             )
                        <| cycleDecomposition model.perm
                   )++
                   [Html.br[][]
                   ,Html.text <| String.join "," <| List.map String.fromInt <| perm2Partition model.perm
                   ]
              )
                        

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
