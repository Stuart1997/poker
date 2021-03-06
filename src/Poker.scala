import CardValue.CardValue
import Suit.Suit

object Poker extends App
{
  Dealing.Deal
}

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

sealed trait HandResult
case class RoyalFlush(value:CardValue, suit:Suit) extends HandResult
case class StraightFlush(value:CardValue, suit:Suit) extends HandResult
case class FourOfAKind(value:CardValue) extends HandResult
case class FullHouse(value:CardValue) extends HandResult
case class Flush(suit:Suit) extends HandResult
case class Straight(value:CardValue) extends HandResult
case class ThreeOfAKind(value:CardValue) extends HandResult
case class TwoPairs(value:CardValue) extends HandResult
case class OnePair(value:CardValue) extends HandResult
case class HighCard(value:CardValue) extends HandResult

object Dealing
{
  def Deal: Unit =
  {
    val random = scala.util.Random
    val firstCard, secondCard, thirdCard, fourthCard, fifthCard = Card(CardValue(random.nextInt(13)), Suit(random.nextInt(4)))
    val fullHandTwo =   List(firstCard, secondCard)
    val fullHandThree = List(firstCard, secondCard, thirdCard)
    val fullHandFour =  List(firstCard, secondCard, thirdCard, fourthCard)
    val fullHandFive =  List(firstCard, secondCard, thirdCard, fourthCard, fifthCard)

    def printHand (Counter:Int, Hand:List[Card])
    {
      var i = 0
      var handDealt = ""
      print("Hand dealt: ")
      for (i <- 0 to Counter)
      { print(s"${Hand(i).value} of ${Hand(i).suit.toString} / ") }
      println()
    }

    printHand(1, fullHandTwo)                     //Outputs the hand according to the stage in the draw
    println(Scoring.handScore(fullHandTwo)._2)    //Prints the highest value hand (._2 = hand name, _.1 = score value)
    scala.io.StdIn.readLine()                     //User must press enter to proceed

    printHand(2, fullHandThree)
    println(Scoring.handScore(fullHandThree)._2)
    scala.io.StdIn.readLine()

    printHand(3, fullHandFour)
    println(Scoring.handScore(fullHandFour)._2)
    scala.io.StdIn.readLine()

    printHand(4, fullHandFive)
    println(Scoring.handScore(fullHandFive)._2)

    //Pre-scoring method calling
    /*handChecker.royalFlush(fullHandFive)
    handChecker.straightFlush(fullHandFive)
    handChecker.fourOfAKind(fullHandFive)
    handChecker.fullHouse(fullHandFive)
    handChecker.flush(fullHandFive)
    handChecker.straight(fullHandFive)
    handChecker.threeOfAKind(fullHandFive)
    handChecker.twoPairs(fullHandFive)
    handChecker.onePair(fullHandFive)
    handChecker.highCard(fullHandFive)*/

    //Royal Flush
    /*val firstCard = Card(CardValue(0), Suit(0))
    val secondCard = Card(CardValue(12), Suit(0))
    val thirdCard = Card(CardValue(11), Suit(0))
    val fourthCard = Card(CardValue(10), Suit(0))
    val fifthCard = Card(CardValue(9), Suit(0))*/

    //Straight Flush
    /*val firstCard = Card(CardValue(9), Suit(0))
    val secondCard = Card(CardValue(8), Suit(0))
    val thirdCard = Card(CardValue(7), Suit(0))
    val fourthCard = Card(CardValue(6), Suit(0))
    val fifthCard = Card(CardValue(5), Suit(0))*/
  }
}
