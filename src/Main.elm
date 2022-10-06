module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (class)
import List.Extra as List
import Material.Tab as Tab
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


type alias Tab =
    { text : String
    , icon : String
    }


firstTab : Tab
firstTab =
    { text = "Find", icon = "search" }


remainingTabs : List Tab
remainingTabs =
    [ { text = "Listen", icon = "headphones" }
    , { text = "Review", icon = "grading" }
    , { text = "Profile", icon = "person" }
    ]


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div [ class "fixed w-full" ] [ viewBar model ]
        , Html.div [ class "pt-20 px-3" ]
            [ (firstTab :: remainingTabs)
                |> List.getAt model.selectedTab
                |> Maybe.withDefault firstTab
                |> .text
                |> Html.text
            ]
        ]


viewBar : Model -> Html Msg
viewBar model =
    TabBar.tabBar (TabBar.config |> TabBar.setStacked True)
        (Tab.tab
            (Tab.config
                |> Tab.setActive (model.selectedTab == 0)
                |> Tab.setOnClick (TabClicked 0)
            )
            { label = firstTab.text, icon = Just (Tab.icon firstTab.icon) }
        )
        (List.indexedMap
            (\index tab ->
                Tab.tab
                    (Tab.config
                        |> Tab.setActive (model.selectedTab == index + 1)
                        |> Tab.setOnClick (TabClicked (index + 1))
                    )
                    { label = tab.text, icon = Just (Tab.icon tab.icon) }
            )
            remainingTabs
        )


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
