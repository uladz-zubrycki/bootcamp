namespace Poker

type GameType =
  | TexasGame
  | OmahaGame
  | FiveCardGame

type CardRank =
  | Ace
  | King
  | Queen
  | Jack
  | Ten
  | Nine
  | Eight
  | Seven
  | Six
  | Five
  | Four
  | Three
  | Two

type CardSuit =
  | Spades
  | Hearts
  | Diamonds
  | Clubs

type Card = { Rank: CardRank; Suit: CardSuit }

type Board =
  | Board of Card list
  static member Create cards =
    if cards |> List.length <> 5
    then Error(sprintf "Board is supposed to have 5 cards, but got %d" cards.Length)
    else Ok(Board cards)

type Hand =
  | Hand of Card list
  static member Create size cards =
    if cards |> List.length <> size
    then Error(sprintf "Hand is supposed to have %d cards, but got %d" size cards.Length)
    else Ok(Hand cards)

type TexasGame = { Board: Board; Hands: Hand list }
type OmahaGame = { Board: Board; Hands: Hand list }
type FiveCardGame = { Hands: Hand list }

type Game =
  | Texas of TexasGame
  | Omaha of OmahaGame
  | FiveCard of FiveCardGame
