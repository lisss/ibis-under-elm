module ItemList exposing (Model, Msg(..), initialModel, main, modalSub, sendEmail, update, view)

import BasicAuth exposing (buildAuthorizationHeader)
import Bootstrap.Badge as Badge
import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Modal as Modal
import Bootstrap.Navbar as Navbar
import Bootstrap.Popover as Popover
import Bootstrap.Utilities.Display as Display
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Utilities.Spacing as Spacing
import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.String as HS
import Html.String.Attributes as HSA
import Http
import Json.Encode as E


type alias Item =
    { id : Int
    , isInCart : Bool
    , band : String
    , album : String
    , year : Int
    , imageUrl : String
    , price : Maybe Int
    }


type alias Items =
    Dict Int Item


type alias Email =
    { from : String
    , to : String
    , subject : String
    , html : String
    }


type Modal
    = ItemDetails
    | Checkout


type alias Model =
    { items : Items
    , modalVisibility :
        { itemDetails : Modal.Visibility
        , checkout : Modal.Visibility
        }
    , cart : Items
    , navbarState : Navbar.State
    , popoverState : Popover.State
    }


type Msg
    = CloseModal Modal
    | ShowModal Modal
    | AnimateModal Modal Modal.Visibility
    | AddToCart Item
    | RemoveFromCart Item
    | NavbarMsg Navbar.State
    | PopoverMsg Popover.State
    | SendEmail Email
    | GotText (Result Http.Error String)


initialModel : Model
initialModel =
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg
    in
    { items =
        Dict.fromList
            [ ( 1
              , { id = 1
                , isInCart = False
                , band = "Gris"
                , album = "Il était une forêt..."
                , year = 2007
                , imageUrl = "src/img/no-photo.svg"
                , price = Just 50
                }
              )
            , ( 2
              , { id = 2
                , isInCart = False
                , band = "Metallica"
                , album = "Garage Inc."
                , year = 1998
                , imageUrl = "src/img/no-photo.svg"
                , price = Just 100
                }
              )
            , ( 3
              , { id = 3
                , isInCart = False
                , band = "Disturbed"
                , album = "Indestructible"
                , year = 2008
                , imageUrl = "src/img/no-photo.svg"
                , price = Nothing
                }
              )
            , ( 4
              , { id = 4
                , isInCart = False
                , band = "October Tide"
                , album = "Rain Without End"
                , year = 1997
                , imageUrl = "src/img/no-photo.svg"
                , price = Just 70
                }
              )
            ]
    , modalVisibility =
        { itemDetails = Modal.hidden
        , checkout = Modal.hidden
        }
    , cart = Dict.empty
    , navbarState = navbarState
    , popoverState = Popover.initialState
    }


cartMessage : Item -> Msg
cartMessage item =
    if item.isInCart then
        RemoveFromCart item

    else
        AddToCart item


getCartButtonIcon : Bool -> Maybe String -> Html Msg
getCartButtonIcon isInCart className =
    img
        [ src <|
            "src/img/cart-"
                ++ (if isInCart then
                        "remove"

                    else
                        "add"
                   )
                ++ ".svg"
        , class <| Maybe.withDefault "" className
        ]
        []


getCartButtonText : Bool -> String
getCartButtonText isInCart =
    if isInCart then
        "Remove from cart"

    else
        "Add to cart"



-- popover : Model -> Html Msg -> Html Msg
-- popover model btn =
--     Popover.config
--         btn
--         |> Popover.right
--         |> Popover.titleH4 [] [ text "Username help" ]
--         |> Popover.content []
--             [ text "Your username must not contain numbers..." ]
--         |> Popover.view model.popoverState


card : Item -> Html Msg -> Html Msg
card item cartButton =
    div [ class "card-wrapper" ]
        [ Card.config []
            |> Card.header [] [ img [ src item.imageUrl, width 200, height 200 ] [] ]
            |> Card.block []
                [ Block.text [] [ div [] [ text item.album ], div [] [ text item.band ] ]
                , Block.custom <|
                    div [ class "item-card-buttons" ]
                        [ Button.button
                            [ Button.small
                            , Button.primary
                            , Button.attrs [ onClick <| ShowModal ItemDetails ]
                            ]
                            [ text "Details" ]
                        , cartButton
                        ]
                ]
            |> Card.view
        ]


modalItemDetails : Item -> Modal.Visibility -> Html Msg
modalItemDetails item visibility =
    Modal.config
        (CloseModal
            ItemDetails
        )
        |> Modal.withAnimation (AnimateModal ItemDetails)
        |> Modal.large
        |> Modal.h4 [] [ text item.album ]
        |> Modal.body []
            [ img [ src item.imageUrl, width 400, height 400 ] []
            , div []
                [ div
                    []
                    [ div [] [ text item.album ]
                    , div [] [ text item.band ]
                    , div [] [ text <| String.fromInt <| item.year ]
                    ]
                ]
            ]
        |> Modal.footer []
            [ Button.button
                [ Button.primary
                , Button.attrs
                    [ onClick <| cartMessage item, class "cart-button" ]
                ]
                [ getCartButtonIcon item.isInCart <| Just "icon-white"
                , span [ class "cart-button-text" ]
                    [ text <| getCartButtonText item.isInCart
                    ]
                ]
            ]
        |> Modal.view visibility


modalCheckout : Model -> Html Msg
modalCheckout model =
    let
        items =
            Dict.values model.cart
    in
    Modal.config
        (CloseModal Checkout)
        |> Modal.withAnimation (AnimateModal Checkout)
        |> Modal.hideOnBackdropClick (List.length items <= 0)
        |> Modal.large
        |> Modal.h4 [] [ text "Checkout" ]
        |> Modal.body [ class "checkout-body" ]
            [ if List.length items > 0 then
                div []
                    [ div [ class "checkout-text" ] [ text "Items selected" ]
                    , items
                        |> List.map
                            (\it ->
                                ListGroup.li [ ListGroup.attrs [ class "checkout-item" ] ]
                                    [ div [ class "checkout-item-details" ]
                                        [ div [] [ text it.album ], div [] [ text <| "(" ++ it.band ++ ")" ] ]
                                    , Button.button
                                        [ Button.attrs [ onClick <| RemoveFromCart it ]
                                        ]
                                        [ img [ src "src/img/minus.svg" ] [] ]
                                    ]
                            )
                        |> ListGroup.ul
                    ]

              else
                div [ class "checkout-text" ] [ text "Your cart is empty" ]
            ]
        |> Modal.footer []
            [ Form.form
                [ class "checkout-form"
                , onSubmit <|
                    SendEmail <|
                        { from = "liss@test.com"
                        , to = "alicedeadbride@gmail.com"
                        , subject = "Your order"
                        , html = HS.toString 4 <| emailOrder items
                        }
                ]
                [ div []
                    [ Form.group []
                        [ Form.label
                            [ for "name"
                            , class "email-label"
                            , class <|
                                if List.length items <= 0 then
                                    "email-label-disabled"

                                else
                                    ""
                            ]
                            [ text "Your name" ]
                        , Input.text [ Input.id "name", Input.disabled <| List.length items <= 0 ]
                        ]
                    , Form.group []
                        [ Form.label
                            [ for "email"
                            , class "email-label"
                            , class <|
                                if List.length items <= 0 then
                                    "email-label-disabled"

                                else
                                    ""
                            ]
                            [ text "Your email" ]
                        , Input.email [ Input.id "email", Input.disabled <| List.length items <= 0 ]
                        ]
                    ]
                ]
            , Button.button
                [ Button.primary
                , Button.disabled <| List.length items <= 0
                ]
                [ span [ class "cart-button-text" ]
                    [ text "Proceed to checkout"
                    ]
                ]
            ]
        |> Modal.view model.modalVisibility.checkout


emailOrder : List Item -> HS.Html Msg
emailOrder items =
    HS.div
        [ HSA.style "display" "flex"
        , HSA.style "flex-direction" "column"
        ]
        [ HS.div
            []
            [ HS.div
                [ HSA.style "padding" "1rem"
                , HSA.style "font-size" "1.5rem"
                ]
                [ HS.text "A new order" ]
            , HS.div [] <|
                List.map
                    (\i ->
                        HS.div [ HSA.style "padding" ".5rem 1rem" ]
                            [ HS.text <| i.band ++ " - " ++ i.album
                            ]
                    )
                    items
            ]
        ]


emailConfirmation : List Item -> String -> HS.Html Msg
emailConfirmation items name =
    HS.div
        [ HSA.style "display" "flex"
        , HSA.style "flex-direction" "column"
        ]
        [ HS.div
            []
            [ HS.div
                [ HSA.style "padding" "1rem"
                , HSA.style "font-size" "1.5rem"
                ]
                [ HS.text <| name ++ ", your order has been received :)" ]
            , HS.div [] <|
                List.map
                    (\i ->
                        HS.div [ HSA.style "padding" ".5rem 1rem" ]
                            [ HS.text <| i.band ++ " - " ++ i.album
                            ]
                    )
                    items
            , HS.div
                [ HSA.style "padding" "1rem" ]
                [ HS.text "I'll contact you soon" ]
            ]
        ]


getTotalPrice : List Item -> Int
getTotalPrice =
    List.foldl (\a b -> Maybe.withDefault 0 a.price + b) 0


emailConfirmation1 : List Item -> String -> Html Msg
emailConfirmation1 items name =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        ]
        [ div
            []
            [ div
                [ style "padding" "1rem"
                , style "font-size" "1.5rem"
                ]
                [ text <| name ++ ", your order has been received :)" ]
            , div [] <|
                List.map
                    (\i ->
                        div [ style "padding" ".5rem 1rem" ]
                            [ text <| i.band ++ " - " ++ i.album ++ " (" ++ (Maybe.withDefault "free" <| Maybe.map String.fromInt i.price) ++ " UAH)"
                            ]
                    )
                    items
            , div
                [ style "padding" "1rem", style "font-weight" "700" ]
                [ span [] [ text "Total price:" ]
                , span [ style "margin-left" ".5rem" ] [ text <| (String.fromInt <| getTotalPrice items) ++ " UAH" ]
                ]
            , div
                [ style "padding" "1rem", style "font-style" "italic" ]
                [ div [] [ text "I'll contact you soon" ], div [] [ text "Liss" ] ]
            ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ Navbar.config NavbarMsg
            |> Navbar.lightCustomClass "ibis-navbar"
            |> Navbar.collapseSmall
            |> Navbar.brand
                [ href "#" ]
                [ text "Ibis"
                ]
            |> Navbar.customItems
                [ Navbar.formItem []
                    [ Input.text [ Input.attrs [ placeholder "Type to search..." ] ]
                    , Button.button
                        [ Button.outlinePrimary
                        , Button.attrs [ Spacing.ml2Sm ]
                        ]
                        [ text "Search" ]
                    , let
                        numItems =
                            List.length <| Dict.keys model.cart
                      in
                      div [ class "my-cart", onClick <| ShowModal Checkout ]
                        [ Badge.pillDark [ class "my-cart-number" ] [ text <| String.fromInt <| numItems ]
                        , img
                            [ src <|
                                "src/img/cart-"
                                    ++ (if numItems > 0 then
                                            "non-empty"

                                        else
                                            "empty"
                                       )
                                    ++ ".svg"
                            , class "icon"
                            ]
                            []
                        ]
                    , modalCheckout model
                    ]
                ]
            |> Navbar.view model.navbarState
        , Grid.container [ class "main-container" ]
            [ Grid.row [ Row.leftXs ] <|
                List.map
                    (\it ->
                        Grid.col [ Col.xs3 ]
                            [ card it <|
                                Button.button
                                    [ Button.light
                                    , Button.attrs <|
                                        [ onClick <| cartMessage it
                                        , title <| getCartButtonText it.isInCart
                                        ]

                                    -- ++ Popover.onHover model.popoverState PopoverMsg
                                    ]
                                    [ getCartButtonIcon it.isInCart Nothing
                                    ]
                            , modalItemDetails it model.modalVisibility.itemDetails
                            ]
                    )
                <|
                    Dict.values
                        model.items
            ]
        , emailConfirmation1
            (Dict.values
                model.items
            )
            "Liss"
        ]


updateItemInCart : Int -> Bool -> Items -> Items
updateItemInCart id isInCart items =
    Dict.update id (Maybe.map (\old -> { old | isInCart = isInCart })) items


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CloseModal modal ->
            let
                { modalVisibility } =
                    model
            in
            case modal of
                ItemDetails ->
                    ( { model | modalVisibility = { modalVisibility | itemDetails = Modal.hidden } }, Cmd.none )

                Checkout ->
                    ( { model | modalVisibility = { modalVisibility | checkout = Modal.hidden } }, Cmd.none )

        ShowModal modal ->
            let
                { modalVisibility } =
                    model
            in
            case modal of
                ItemDetails ->
                    ( { model | modalVisibility = { modalVisibility | itemDetails = Modal.shown } }, Cmd.none )

                Checkout ->
                    ( { model | modalVisibility = { modalVisibility | checkout = Modal.shown } }, Cmd.none )

        AnimateModal modal visibility ->
            let
                { modalVisibility } =
                    model
            in
            case modal of
                ItemDetails ->
                    ( { model | modalVisibility = { modalVisibility | itemDetails = visibility } }, Cmd.none )

                Checkout ->
                    ( { model | modalVisibility = { modalVisibility | checkout = visibility } }, Cmd.none )

        AddToCart item ->
            ( { model
                | cart = Dict.insert item.id item model.cart
                , items = updateItemInCart item.id True model.items
              }
            , Cmd.none
            )

        RemoveFromCart item ->
            ( { model
                | cart = Dict.remove item.id model.cart
                , items = updateItemInCart item.id False model.items
              }
            , Cmd.none
            )

        NavbarMsg state ->
            ( { model | navbarState = state }, Cmd.none )

        PopoverMsg state ->
            ( { model | popoverState = state }, Cmd.none )

        SendEmail email ->
            let
                d =
                    Debug.log ">>>>" email
            in
            ( model, sendEmail email )

        -- ( model, Cmd.none )
        GotText e ->
            let
                d =
                    Debug.log ">>>>ddd" e
            in
            ( model, Cmd.none )


modalSub : Model -> Sub Msg
modalSub model =
    Sub.batch
        [ Modal.subscriptions model.modalVisibility.checkout <| AnimateModal Checkout
        , Modal.subscriptions model.modalVisibility.itemDetails <| AnimateModal ItemDetails
        , Navbar.subscriptions model.navbarState NavbarMsg
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = modalSub
        }


jsonEncOrder : Email -> Http.Body
jsonEncOrder { from, to, subject, html } =
    E.object
        [ ( "from", E.string from )
        , ( "to", E.string to )
        , ( "subject", E.string subject )
        , ( "html", E.string html )
        ]
        |> Http.jsonBody


expectJson : (Result Http.Error String -> msg) -> Http.Expect msg
expectJson toMsg =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata body ->
                    Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ metadata body ->
                    Ok body


sendEmail : Email -> Cmd Msg
sendEmail email =
    Http.request
        { method = "POST"
        , url = "http://localhost:8000/email"
        , headers = []
        , body = jsonEncOrder email
        , expect =
            expectJson
                (\r ->
                    case r of
                        Ok l ->
                            GotText (Ok l)

                        Err e ->
                            GotText (Err e)
                )
        , timeout = Nothing
        , tracker = Nothing
        }
