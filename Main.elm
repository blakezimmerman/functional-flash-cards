import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main =
  Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL

type alias Card =
  { frontCard : String
  , backCard : String
  }

type alias Model =
  { currentCard : Card
  , cardList: List Card
  }

model : Model
model =
  { currentCard = Card "" ""
  , cardList = []
  }


-- UPDATE

type Msg
  = FrontCard String
  | BackCard String
  | AddCard

update : Msg -> Model -> Model
update msg model =
  case msg of
    FrontCard front ->
      { model | currentCard = { frontCard = front, backCard = model.currentCard.backCard } }

    BackCard back ->
      { model | currentCard = { frontCard = model.currentCard.frontCard, backCard = back } }

    AddCard ->
      { model | cardList = model.currentCard :: model.cardList }

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "Front of Card", onInput FrontCard ] []
    , input [ placeholder "Back of Card", onInput BackCard ] []
    , button [ onClick AddCard ] [ text "Add Card" ]
    , div []
      [ renderCardList model.cardList
      ]
    ]


--viewCardList : Maybe List Card -> Html Msg
renderCardList : List Card -> Html Msg
renderCardList cardList =
  ul []
    (List.map (\card -> li [] [ text (toString card) ]) cardList)
