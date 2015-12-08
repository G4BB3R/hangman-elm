import Graphics.Collage    exposing (..)
import Graphics.Element    exposing (..)
import Graphics.Input      exposing (..)
import Color               exposing (red)
import Text                exposing (fromString)
import String              exposing (concat, fromChar)
import Set                 exposing (Set)
import Signal              exposing (..)
import Window              exposing (dimensions)
import Keyboard            
import Task                exposing (Task, succeed)
import Char                exposing (toCode)
import Random              exposing (..)
import Array               exposing (Array, get, fromList, length)
import Maybe               exposing (withDefault)
import Time                exposing (Time, every, second, minute)
import Debug               exposing (log)

type GameScene   = GSMenu | GSCredits | GSPlaying
type Action      = ActNop | ActKey Int | ActScene GameScene | ActUpdateSeed Int | ActStart 
type alias Model = { state       : GameScene -- Game Scene
                   , vida        : Int       -- How many tries you still have
                   , tip         : String    -- Tip to discover the word
                   , palavra     : String    -- The word
                   , letras      : Set Int   -- The right letters
                   , letras_err  : Set Int   -- The wrong letters
                   , win         : Bool
                   , random_seed : Seed
                   }

initialModel : Model
initialModel = Model GSMenu 0 "" "" (Set.fromList []) (Set.fromList []) False (initialSeed 41)

database : Array (String, String)
database = [ ("lisp",       "Programação alienígena")
           , ("redbull",    "Marca de bebida")
           , ("safadao",    "Deus da musica")
           , ("nariz",      "Tem um na sua fuça")
           , ("pernambuco", "Se for bom, descubra")
           , ("school",     "A place")
           , ("indigo",     "The best app in the world")
           , ("elm",        "The best front-end programming language")
           , ("pineapple",  "A fruit")
           , ("esperanto",  "An idiom")
           , ("flamingo",   "A bird")
           , ("pacman",     "Classical Game")
           ] |> Array.fromList

palavraToSet : String -> Set Int
palavraToSet str =
    str
    |> String.toList
    |> List.map toCode
    |> Set.fromList

palavraToStr : Set Int -> String -> String
palavraToStr allowed_letters palavra =
    palavra
    |> String.toList
    |> List.map (\x ->
             if x == ' ' then "    "
             else (if Set.member (Char.toCode x) allowed_letters then
                 String.fromList [x]
             else "_") ++ " ")
    |> String.concat

update : Action -> Model -> Model
update action model =
    case action of
        ActNop ->
            model
            
        ActKey key ->
            if model.state /= GSPlaying || model.vida == 0 || model.win then model
            else
                let
                    valid_key  = String.contains (key |> Char.fromCode |> String.fromChar) model.palavra
                    letras     = if     valid_key then Set.insert key model.letras     else model.letras
                    letras_err = if not valid_key then Set.insert key model.letras_err else model.letras_err
                    win        = letras == palavraToSet model.palavra
                    vida       = model.vida - if not valid_key && not (Set.member key model.letras_err) then 1 else 0
                in
                    { model | letras     = letras
                            , letras_err = letras_err
                            , vida       = vida
                            , win        = win
                            }

        ActScene scene  -> { model | state = scene }
        ActUpdateSeed t -> { model | random_seed = initialSeed t}
        ActStart        ->
            let
                (indice, random_seed) = generate (Random.int 1 (Array.length database)) model.random_seed
                (palavra, tip) = Array.get (indice - 1) database |> Maybe.withDefault ("?", "?")
            in
                { model | state       = GSPlaying
                        , vida        = 5
                        , tip         = tip
                        , palavra     = palavra
                        , letras      = Set.fromList []
                        , letras_err  = Set.fromList []
                        , win         = False
                        , random_seed = random_seed
                        }

titulo = "Hangman 1.0"
        |> Text.fromString
        |> Text.height 20.0
        |> text
        |> moveY 200

view_menu : (Int, Int) -> Model -> Element
view_menu (w', h') model =
    collage w' h' [ titulo,
        
        button (Signal.message mailbox.address ActStart) "Play"
        |> toForm,
        
        button (Signal.message mailbox.address (ActScene GSCredits)) "Credits"
        |> toForm
        |> moveY -50
        
    ]
    |> color Color.blue

view_credits : (Int, Int) -> Model -> Element
view_credits (w', h') model =
    collage w' h' [ titulo,
        
        moveY  20 <| text <| fromString <| "Coded with Elm for learning purposes",
        moveY   0 <| text <| fromString <| "You can see the source code at github.com/G4BB3R/hangman-elm",
        moveY -40 <| text <| fromString <| "If you want to know more about Elm, access elm-lang.org",
        
        button (Signal.message mailbox.address (ActScene GSMenu)) "Back"
        |> toForm
        |> moveY -250
        
    ]
    |> color Color.blue

view_playing : (Int, Int) -> Model -> Element
view_playing (w', h') model =
    collage w' h' [ titulo,
        
        palavraToStr model.letras model.palavra
        |> Text.fromString
        |> Text.height 16.0
        |> text,
        
        model.vida
        |> toString >> (++) "Vida: "
        |> Text.fromString
        |> Text.height 16.0
        |> text
        |> moveX -200,
        
        model.letras_err
        |> Set.map Char.fromCode
        |> Set.toList
        |> List.intersperse ' '
        |> String.fromList
        |> (\str -> if str == "" then "" else "Wrong: " ++ str)
        |> Text.fromString
        |> Text.height 12.0
        |> text
        |> moveY -250,
        
        "Tip: " ++ model.tip
        |> Text.fromString
        |> Text.height 12.0
        |> text
        |> moveY 50,
        
        (if model.win then "VITORIA!" else if model.vida == 0 then "PERDEU!" else "")
        |> Text.fromString
        |> Text.height 20.0
        |> Text.color (if model.win then Color.green else if model.vida == 0 then Color.red else Color.green)
        |> text
        |> moveY -50,
        
        button (Signal.message mailbox.address (ActScene GSMenu)) "Back"
        |> toForm        
        |> move (-250, -250)
    ]
    |> color Color.blue

view : (Int, Int) -> Model -> Element
view size model =
    (case model.state of
        GSMenu    -> view_menu
        GSCredits -> view_credits
        GSPlaying -> view_playing) size model

port teclado : Signal (Task x ())
port teclado =
    Keyboard.presses
    |> Signal.map 
           (\key ->
               if key >= 97 && key <= 122 then
                   Signal.send mailbox.address (ActKey key)
               else
                   Task.succeed ())

mailbox : Signal.Mailbox Action
mailbox = Signal.mailbox ActNop

port do_random_seed : Signal (Task a ())
port do_random_seed =
    (Time.every Time.minute)
    |> Signal.map (\t -> Signal.send mailbox.address (ActUpdateSeed (round t)))

main : Signal Element
main =
    Signal.foldp update initialModel mailbox.signal
    |> Signal.map2 view Window.dimensions
