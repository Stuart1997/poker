import CardValue.CardValue
import Suit.Suit

object Poker extends App
{
  Dealing.Deal
}

//Shows high card with a flush - fix
//Shows 3 of a kind with a full house - fix
//Map each hand to a numerical value, change a variable to this value when the
//  hand is discovered so it only shows the highest one rather than 2
object CardValue extends Enumeration
{
  type CardValue = Value
  val Ace = Value(0)
  val Two = Value(1)
  val Three = Value(2)
  val Four = Value(3)
  val Five = Value(4)
  val Six = Value(5)
  val Seven = Value(6)
  val Eight = Value(7)
  val Nine = Value(8)
  val Ten = Value(9)
  val Jack = Value(10)
  val Queen = Value(11)
  val King = Value(12)
}

object Suit extends Enumeration
{
  type Suit = Value
  val Hearts, Clubs, Spades, Diamonds = Value
}

case class Card(value: CardValue, suit: Suit)

object Dealing
{
  def Deal: Unit =
  {

    val random = scala.util.Random

    //Put this in a function
    val firstCard, secondCard, thirdCard, fourthCard, fifthCard =
      Card(CardValue(random.nextInt(12)), Suit(random.nextInt(3)))
    val fullHand = List(firstCard, secondCard, thirdCard, fourthCard, fifthCard)
    println("Hand dealt: " + fullHand(0).value + " of " + fullHand(0).suit.toString + " / " + fullHand(1).value + " of " + fullHand(1).suit.toString)
    println("More cards: " + fullHand(2).value + " of " + fullHand(2).suit.toString + " / " + fullHand(3).value + " of " + fullHand(3).suit.toString + " / " +
                             fullHand(4).value + " of " + fullHand(4).suit.toString)
    println()

    def threeOfAKind = handChecker.nOfAKind(3)_
    def fourOfAKind = handChecker.nOfAKind(4)_
    threeOfAKind(fullHand)
    fourOfAKind(fullHand)
    handChecker.Pairs(fullHand)
    handChecker.Flush(fullHand)
    Straight(fullHand)

    def Straight(hand: List[Card]): Boolean =
    {
      //Gets the hand, stores the values into a list and sorts them
      val handValuesList: List[CardValue] = hand.map(card => card.value).sorted

      println("Straight checker: " + handValuesList)

      //true

      //Make a function for this
      //if (handValuesList.head == CardValue(_) && handValuesList(1) == CardValue(_+1))
      if (handValuesList.last == CardValue.King)
      {
        //handValuesList.head = CardValue.Nine
        true
      }

      /*handValuesList match {
        case CardValue(x) :: CardValue(x+1) => ""
      }*/

      /*if (handValuesList.head = (handValuesList(1) + 1)) {
        println("")
        true
      }
      else {false}*/
    }
  }
}

