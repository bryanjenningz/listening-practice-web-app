module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (class)
import List.Extra as List
import Material.Tab as Tab exposing (Tab)
import Material.TabBar as TabBar


type alias Model =
    { selectedTab : Int }


init : () -> ( Model, Cmd Msg )
init () =
    ( { selectedTab = 0 }, Cmd.none )


type Msg
    = TabClicked Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TabClicked selectedTab ->
            ( { model | selectedTab = selectedTab }, Cmd.none )


type alias TabData =
    { text : String
    , icon : String
    }


firstTab : TabData
firstTab =
    { text = "Find", icon = "search" }


remainingTabs : List TabData
remainingTabs =
    [ { text = "Listen", icon = "headphones" }
    , { text = "Review", icon = "grading" }
    , { text = "Profile", icon = "person" }
    ]


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div [ class "fixed w-full" ] [ viewTabs model ]
        , Html.div [ class "pt-20 px-3" ]
            [ (firstTab :: remainingTabs)
                |> List.getAt model.selectedTab
                |> Maybe.withDefault firstTab
                |> .text
                |> Html.text
            ]
        ]


viewTabs : Model -> Html Msg
viewTabs model =
    TabBar.tabBar (TabBar.config |> TabBar.setStacked True)
        (viewTab model 0 firstTab)
        (List.indexedMap (\i tab -> viewTab model (i + 1) tab) remainingTabs)


viewTab : Model -> Int -> TabData -> Tab Msg
viewTab model index tab =
    Tab.tab
        (Tab.config
            |> Tab.setActive (model.selectedTab == index)
            |> Tab.setOnClick (TabClicked index)
        )
        { label = tab.text, icon = Just (Tab.icon tab.icon) }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
