
import CardValue.CardValue
import Suit.Suit

object handChecker {
  def nOfAKind(n: Int)(hand: List[Card]): Boolean = {
    val handValuesList: List[CardValue] = hand.map(card => card.value).sorted
    //println("Step 1 - " + valueList)        //Prints a list of all the cards in the hand
    val groupMap = handValuesList.groupBy(identity)
    //println("Step 2 - " + groupMap)         //Prints a map of (card value -> no. of those card values) - ignores suit
    val listDuplicates: List[Int] = groupMap.toList.map{case (k:CardValue, ls:List[CardValue]) => ls.length}
    //println("Step 3 - " + listDuplicates)   //Prints only the no. of card values, e.g. 1,2,1,1

    if (listDuplicates contains n)
    {
      println(s"$n of a kind")
      true
    }
    else {false}
  }

  def Pairs(hand: List[Card]): Boolean = {
    //Gets the hand, stores the values into a list and sorts them
    val handValuesList: List[CardValue] = hand.map(card => card.value).sorted

    //Groups the values from the hand by unique values, if there's a duplicate it'll make it 5 -> 2 (two 5s)
    val groupMap = handValuesList.groupBy(identity)

    //If the list contains values more than 1, display how many of those there are - i.e. number of pairs
    //Don't forget that this would remove hierarchy as a boolean, returning an int would be better
    val listDuplicates: List[Int] = groupMap.toList.map{case (k:CardValue, ls:List[CardValue]) => ls.length}

    if (listDuplicates.length == 4)
    {
      println("One pair")
      true
    }
    else if ((listDuplicates.length == 3) && (listDuplicates contains 2))
    {
      println("Two pairs")
      true
    }
    else if ((listDuplicates contains 3) && (listDuplicates contains 2))
    {
      println("Full house")
      true
    }
    else if (listDuplicates.length == 5)
      {
        val handValuesList: List[CardValue] = hand.map(card => card.value).sorted
        println("High Card " + handValuesList.last)
        true
      }
    else {false}
  }

  def Flush(hand: List[Card]): Boolean = {
    val suitList: List[Suit] = hand.map(card => card.suit).sorted
    val groupMap = suitList.groupBy(identity)
    val listDuplicates: List[Suit] = groupMap.toList.map{case (k:Suit.Suit, ls:List[Suit]) => ls.head}
    //val listDuplicates: List[Suit] = List(Suit.Spades)

    if (listDuplicates.length == 1)
    {
      println("Flush (" + listDuplicates(0) + ")")
      true
    }
    else {false}
  }
}
