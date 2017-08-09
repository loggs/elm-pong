port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Time
import Keyboard.Extra
import AnimationFrame
import Char
import Random
import Svg
import Random.Pcg as Rand
import Html.Events exposing (onClick)

main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

---- Ports
port sendGameBoard : String -> Cmd msg

--port updateComputerDirection : (String -> Msg) -> Sub Msg

-- MODEL


type Direction
    = Left
    | Right
    | Still


type Person
    = Player
    | Computer

type Difficulty
     = Easy 
     | Medium 
     | Hard 
     | NeuralNet

type alias Model =
    { ballSpeed : Float
    , playerSpeed : Float
    , computerSpeed : Float
    , playerX : Float
    , playerY : Float
    , computerX : Float
    , computerY : Float
    , playerDirection : Direction
    , computerDirection : Direction
    , ballX : Float
    , ballY : Float
    , ballDirectionX : Float
    , ballDirectionY : Float
    , keyboardModel : Keyboard.Extra.State
    , playerScore : Int
    , computerScore : Int
    , computerDifficulty : Difficulty
    , gameAreaString : String
    }


initModel : Model
initModel =
    { ballSpeed = 2.5
    , playerSpeed = 4
    , computerSpeed = 7
    , playerX = 40
    , playerY = 420
    , computerX = 40
    , computerY = 10
    , playerDirection = Still
    , computerDirection = Still
    , ballX = 250
    , ballY = 250
    , ballDirectionX = -1
    , ballDirectionY = 1
    , playerScore = 0
    , computerScore = 0
    , keyboardModel = Keyboard.Extra.initialState
    , computerDifficulty = Easy
    , gameAreaString = ""
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )



-- UPDATE


type Msg
    = KeyboardExtraMsg Keyboard.Extra.Msg
    | Step Time.Time
    | NewBallDirectionX Int
    | NewBallDirectionY Int
    | IncreaseBallSpeed Time.Time
    | ChangeDifficulty Difficulty


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyboardExtraMsg keyMsg ->
            onUserInput keyMsg model

        Step time ->
            onFrame time model

        NewBallDirectionX newDirection ->
            ( { model | ballDirectionX = updateBallDirection newDirection model.ballDirectionX }, Cmd.none )

        NewBallDirectionY newDirection ->
            ( { model | ballDirectionY = updateBallDirection newDirection model.ballDirectionY }, Cmd.none )

        IncreaseBallSpeed _->
            ( {model | ballSpeed = model.ballSpeed + 0.1}, sendGameBoard model.gameAreaString)

        ChangeDifficulty newDifficulty -> 
            ({model | computerDifficulty = newDifficulty, ballSpeed = 2.5, playerScore = 0, computerScore = 0, ballX = 250, ballY = 250}, Cmd.none)



{- Step time -> onFrame time game -}


updateBallDirection : Int -> Float -> Float
updateBallDirection zeroOrOne modelDirection =
    case zeroOrOne of
        1 ->
            modelDirection

        _ ->
            -1 * modelDirection


onUserInput : Keyboard.Extra.Msg -> Model -> ( Model, Cmd Msg )
onUserInput keyMsg model =
    let
        keyboardModel =
            Keyboard.Extra.update keyMsg model.keyboardModel

        playerDir =
            if (Keyboard.Extra.arrows keyboardModel).x > 0 then
                Right
            else if (Keyboard.Extra.arrows keyboardModel).x < 0 then
                Left
            else
                Still

        computerDir =
            if (Keyboard.Extra.wasd keyboardModel).x > 0 then
                Right
            else if (Keyboard.Extra.wasd keyboardModel).x < 0 then
                Left
            else
                Still
    in
        ( { model
            | keyboardModel = keyboardModel
            , playerDirection = playerDir
            , computerDirection = computerDir
          }
        , Cmd.none
        )


onFrame : Time.Time -> Model -> ( Model, Cmd Msg )
onFrame time model =
    let
        ( newPositionX, newPositionY, updateScoreComp, updateScorePlayer, updateBallDirectionX, updateBallDirectionY, newBallSpeed ) =
            checkGoalScored model

        newComputerDirection =
            if model.ballX + (model.ballDirectionX * model.ballSpeed) < model.computerX + 40 then
                Left
            else if model.ballX + (model.ballDirectionX * model.ballSpeed) > model.computerX && 
                model.ballX + (model.ballDirectionX * model.ballSpeed) < model.computerX + 60
                then Still
            else
                Right
    in
        ( { model
            | playerX = updatePlayer model Player
            , computerX = updatePlayer model Computer
            , ballX = newPositionX
            , ballY = newPositionY
            , ballDirectionX = updateBallDirectionX
            , ballDirectionY = updateBallDirectionY
            , computerDirection = newComputerDirection
            , computerScore = model.computerScore + updateScoreComp
            , playerScore = model.playerScore + updateScorePlayer
            , ballSpeed = newBallSpeed
            , gameAreaString = toString <| gameArea model
          }
        , Cmd.none
        )


checkGoalScored : Model -> ( Float, Float, Int, Int, Float, Float, Float )
checkGoalScored model =
    let
        firstSeed =
            Rand.initialSeed <| round (model.ballX * 1243433.3123443243)

        ( randX, newSeed ) =
            Rand.step (Rand.int 0 100) firstSeed

        ( randY, _ ) =
            Rand.step (Rand.int 0 100) newSeed

        ( newBallDirX, newBallDirY ) =
            checkCollision model
    in
        if (model.ballY + model.ballDirectionY) <= -40 then
            ( 250, 250, 0, 1, negOneOrOne randX, negOneOrOne randY, 2.5 )
        else if (model.ballY + model.ballDirectionY) >= 470 then
            ( 250, 250, 1, 0, negOneOrOne randX, negOneOrOne randY, 2.5 )
        else
            ( model.ballX + (model.ballDirectionX * model.ballSpeed), model.ballY + (model.ballDirectionY * model.ballSpeed), 0, 0, newBallDirX, newBallDirY, model.ballSpeed)


updatePlayer : Model -> Person -> Float
updatePlayer model person =
    let
        difficultyChange =
            case model.computerDifficulty of
                Easy -> -0.07
                Medium -> 0.5
                Hard -> 1
                NeuralNet -> 1
        ( playerSpeed, playerPosition, direction ) =
            if person == Player then
                ( model.playerSpeed, model.playerX, model.playerDirection )
            else
                ( model.ballSpeed + difficultyChange, model.computerX, model.computerDirection )
    in
        checkBoundaries playerPosition playerSpeed direction


checkBoundaries : Float -> Float -> Direction -> Float
checkBoundaries position change dir =
    let
        ( maxValue, operator, comparison ) =
            if dir == Left then
                ( 0, (-), (>) )
            else if dir == Right then
                ( 400, (+), (<) )
            else
                ( -100, (+), (/=) )

        withChange =
            operator position change
    in
        if comparison maxValue withChange then
            position
        else
            withChange


checkCollision : Model -> ( Float, Float )
checkCollision model =
    if
        (model.ballX + (model.ballDirectionX * model.ballSpeed))
            <= 0
            || (model.ballX + (model.ballDirectionX * model.ballSpeed))
            >= 480
    then
        ( model.ballDirectionX * -1, model.ballDirectionY )
    else if
        (model.ballY + (model.ballDirectionY * model.ballSpeed))
            >= 2
            && (model.ballY + (model.ballDirectionY * model.ballSpeed))
            <= 12
            && (model.ballX + (model.ballDirectionX * model.ballSpeed))
            >= model.computerX
            - 15
            && (model.ballX + (model.ballDirectionX * model.ballSpeed))
            <= (model.computerX + 100)
    then
        if
            (model.computerDirection == Right && (model.ballDirectionX * model.ballSpeed) < 0)
                || (model.computerDirection == Left && (model.ballDirectionX * model.ballSpeed) > 0)
        then
            ( model.ballDirectionX * -1, model.ballDirectionY * -1 )
        else
            ( model.ballDirectionX, model.ballDirectionY * -1 )
    else if
        (model.ballY + (model.ballDirectionY * model.ballSpeed) + 15)
            >= 435
            && (model.ballY + (model.ballDirectionY * model.ballSpeed) + 15)
            <= 445
            && (model.ballX + (model.ballDirectionX * model.ballSpeed))
            >= model.playerX
            - 15
            && (model.ballX + (model.ballDirectionX * model.ballSpeed))
            <= (model.playerX + 100)
    then
        if
            (model.playerDirection == Right && (model.ballDirectionX * model.ballSpeed) < 0)
                || (model.playerDirection == Left && (model.ballDirectionX * model.ballSpeed) > 0)
        then
            ( model.ballDirectionX * -1, model.ballDirectionY * -1 )
        else
            ( model.ballDirectionX, model.ballDirectionY * -1 )
    else
        ( model.ballDirectionX, model.ballDirectionY )


negOneOrOne : Int -> Float
negOneOrOne randInt =
    if randInt > 50 then
        1.0
    else
        -1.0



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map KeyboardExtraMsg Keyboard.Extra.subscriptions
        , AnimationFrame.times (\time -> Step time)
        , Time.every Time.second IncreaseBallSpeed
        ]



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "container"
        , style
            [ ( "width", "100%" )
            , ( "height", "100%" )
            ]
        ]
        [ heading
        , div [ class "row" ]
            [ score_ model.playerScore Player
            , gameArea model
            , score_ model.computerScore Computer
            , button_ Easy
            , button_ Medium
            , button_ Hard
            , button_ NeuralNet
            ]
        ]


gameArea : Model -> Html Msg
gameArea model =
    div [ class "col-sm-6 text-center", style [ ( "background-color", "white" ) ] ]
        [ div
            [ class "text-center"
            , style
                [ ( "width", "500px" )
                , ( "height", "500px" )
                , ( "text-align", "center" )
                , ( "display", "inline-block" )
                , ( "border-color", "black" )
                , ( "border-width", "3px" )
                , ( "border-style", "solid" )
                , ( "background-color", "black" )
                , ( "color", "white" )
                ]
            ]
            [ paddle_ model Computer
            , ball_ model
            , paddle_ model Player
            ]
        ]


heading : Html Msg
heading =
    div [ class "row" ]
        [ div
            [ class "col-sm-12"
            , style
                [ ( "font-family", "Faster One" )
                , ( "text-align", "center" )
                , ( "color", "black" )
                , ( "font-size", "300%" )
                ]
            ]
            [ text "PONG" ]
        ]


paddle_ : Model -> Person -> Html Msg
paddle_ model person =
    let
        ( positionX, positionY ) =
            case person of
                Player ->
                    ( model.playerX, model.playerY )

                Computer ->
                    ( model.computerX, model.computerY )
    in
        div
            [ style
                [ ( "background-color", "white" )
                , ( "width", "100px" )
                , ( "height", "25px" )
                , ( "position", "relative" )
                , ( "left", (positionX |> toString) ++ "px" )
                , ( "top", (positionY |> toString) ++ "px" )
                ]
            ]
            []


ball_ : Model -> Html Msg
ball_ model =
    div
        [ style
            [ ( "background-color", "white" )
            , ( "width", "15px" )
            , ( "height", "15px" )
            , ( "position", "relative" )
            , ( "left", (model.ballX |> toString) ++ "px" )
            , ( "top", (model.ballY |> toString) ++ "px" )
            ]
        ]
        []


score_ : Int -> Person -> Html Msg
score_ scoreValue person =
    let
        ( scorePlacement, classAddition ) =
            case person of
                Player ->
                    ( "50px", "text-right" )

                Computer ->
                    ( "845px", "text-left" )
    in
        div
            [ class ("col-sm-3 " ++ classAddition)
            , style
                [ ( "background-color", "white" )
                , ( "color", "black" )

                --, ( "text-align", "center" )
                , ( "font-family", "Monofett" )
                , ( "font-size", "400%" )
                ]
            ]
            [ text <| toString person
            , br [] []
            , text <| toString scoreValue
            ]

button_ : Difficulty -> Html Msg
button_ buttonText =
    button [ onClick <| ChangeDifficulty buttonText ] [text <| toString buttonText]
