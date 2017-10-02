import CardValue.CardValue
import CardValue._
import Suit.Suit

object handChecker {
  var handResult = "None"

  //Gets a list of cards, maps them, if there's just the one suit then == 1
  def allCardsSameSuit(hand:List[Card]):Boolean ={
    val suitList: List[Suit] = hand.map(card => card.suit).sorted
    val howManySuits = suitList.groupBy(identity).size
    howManySuits == 1
  }

  //Gets a list of cards, checks to see if the first two are Ace & 10, if yes, shifts Ace to the end
  def isHandAStraight(hand:List[CardValue]):Boolean = hand match {
    case CardValue.Ace :: Ten :: (xs:List[CardValue]) => isHandAStraightInt(List(Ten.id) ++ xs.map(_.id) ++ List(13)) //Ace is high might be a Royal Flush
    case xs:List[CardValue] => isHandAStraightInt(xs.map(_.id))
    case _ => false
  }

  //Gets the list of cards, checks 2 at a time, if the second is +1 more than the first, do the next pair
  def isHandAStraightInt(hand:List[Int]):Boolean = hand.sliding(2).forall(x => x.head + 1 == x.last)

  def royalFlush(hand: List[Card]): Boolean = {
    if (allCardsSameSuit(hand))
    {
      //Gets the hand, stores the values into a list and sorts them
      val sortedHandValuesList: List[CardValue] = hand.map(card => card.value).sorted
      //List(CardValue.Seven, CardValue.Queen, CardValue.Seven, CardValue.Six, CardValue.Three)

      //Creates a list based on the Value (eg. 1,2,3 for Ace Two Three)
      val enumIdList: List[Int] = sortedHandValuesList.map(x => x.id)

      //Sliding(2) takes two values. forall is used to compare them. If all fit Predicate then true.
      val isItAStraight: Boolean = isHandAStraight(sortedHandValuesList)

      if (isItAStraight && sortedHandValuesList.startsWith(List(Ace, Ten)))
      {
        handResult = s"Royal Flush (${hand.head.suit})"
        true
      }
      else {false}
    }
    else {false}
  }

  def straightFlush(hand: List[Card]): Boolean = {
    if (allCardsSameSuit(hand))
    {
      //Gets the hand, stores the values into a list and sorts them
      val sortedHandValuesList: List[CardValue] = hand.map(card => card.value).sorted
      //List(CardValue.Seven, CardValue.Queen, CardValue.Seven, CardValue.Six, CardValue.Three)

      //Creates a list based on the Value (eg. 1,2,3 for Ace Two Three)
      val enumIdList: List[Int] = sortedHandValuesList.map(x => x.id)

      //Sliding(2) takes two values. forall is used to compare them. If all fit Predicate then true.
      val isItAStraight: Boolean = isHandAStraight(sortedHandValuesList)

      if (isItAStraight)
      {
        handResult = s"Straight Flush (${hand.head.suit}: ${sortedHandValuesList.last} to ${sortedHandValuesList.head})"
        true
      }
      else {false}
    }
    else {false}
  }

  def nOfAKind(n: Int)(hand: List[Card]): Boolean = {
    val handValuesList: List[CardValue] = hand.map(card => card.value).sorted
    //println("Step 1 - " + handValuesList)         //Prints a list of all the cards in the hand
    val groupMap = handValuesList.groupBy(identity)
    //println("Step 2 - " + groupMap)               //Prints a map of (card value -> no. of those card values) - ignores suit
    val listDuplicates: List[Int] = groupMap.toList.map{case (k:CardValue, ls:List[CardValue]) => ls.length}
    //println("Step 3 - " + listDuplicates)         //Prints only the no. of card values, e.g. 1,2,1,1

    if (listDuplicates contains n)
    {
      handResult = s"$n of a kind"
      true
    }
    else {false}
  }

  def threeOfAKind = nOfAKind(3)_
  def fourOfAKind = nOfAKind(4)_

  def fullHouse(hand: List[Card]): Boolean = {
    val handValuesList: List[CardValue] = hand.map(card => card.value).sorted
    //println("Step 1 - " + valueList)        //Prints a list of all the cards in the hand
    val groupMap = handValuesList.groupBy(identity)
    //println("Step 2 - " + groupMap)         //Prints a map of (card value -> no. of those card values) - ignores suit
    val listDuplicates: List[Int] = groupMap.toList.map { case (k: CardValue, ls: List[CardValue]) => ls.length }
    //println("Step 3 - " + listDuplicates)   //Prints only the no. of card values, e.g. 1,2,1,1

    if ((listDuplicates contains 3) && (listDuplicates contains 2))
    {
      handResult = "Full house"
      true
    }
    else {false}
  }

  def flush(hand: List[Card]): Boolean = {
    val suitList: List[Suit] = hand.map(card => card.suit).sorted
    val groupMap = suitList.groupBy(identity)
    val listDuplicates: List[Suit] = groupMap.toList.map{case (k:Suit.Suit, ls:List[Suit]) => ls.head}
    //val listDuplicates: List[Suit] = List(Suit.Spades)

    if (listDuplicates.length == 1)
    {
      handResult = s"Flush (${listDuplicates.head})"
      true
    }
    else {false}
  }

  def straight(hand: List[Card]): Boolean = {
    //Gets the hand, stores the values into a list and sorts them
    val handValuesList: List[CardValue] = hand.map(card => card.value).sorted
    //List(CardValue.Ace, CardValue.Two, CardValue.Three, CardValue.Four, CardValue.Five)

    //Creates a list based on the Value (eg. 1,2,3 for Ace Two Three)
    val valuedList: List[Int] = handValuesList.map(x => x.id)

    //Sliding(2) takes two values. forall is used to compare them. If all fit Predicate then true.
    val isItAStraight: Boolean = valuedList.sliding(2).forall(x => x.head + 1 == x.last)
    if (isItAStraight)
    {
      handResult = s"Straight: ${handValuesList.last} to ${handValuesList.head}"
      true
    }
    else {false}
  }

  def twoPairs(hand: List[Card]): Boolean = {
    //Gets the hand, stores the values into a list and sorts them
    val handValuesList: List[CardValue] = hand.map(card => card.value).sorted

    //Groups the values from the hand by unique values, if there's a duplicate it'll make it 5 -> 2 (two 5s)
    val groupMap = handValuesList.groupBy(identity)

    //If the list contains values more than 1, display how many of those there are - i.e. number of pairs
    //Don't forget that this would remove hierarchy as a boolean, returning an int would be better
    val listDuplicates: List[Int] = groupMap.toList.map{case (k:CardValue, ls:List[CardValue]) => ls.length}

    val listOfPairs = listDuplicates.filter(x => x == 2)

     if (listOfPairs.length == 2)
       {
         handResult = "Two pairs"
         true
       }
    else {false}
  }

  def onePair(hand: List[Card]): Boolean = {
    //Gets the hand, stores the values into a list and sorts them
    val handValuesList: List[CardValue] = hand.map(card => card.value).sorted

    //Groups the values from the hand by unique values, if there's a duplicate it'll make it 5 -> 2 (two 5s)
    val groupMap = handValuesList.groupBy(identity)

    //If the list contains values more than 1, display how many of those there are - i.e. number of pairs
    //Don't forget that this would remove hierarchy as a boolean, returning an int would be better
    val listDuplicates: List[Int] = groupMap.toList.map{case (k:CardValue, ls:List[CardValue]) => ls.length}

    val listOfPairs = listDuplicates.filter(x => x == 2)

    if (listOfPairs.length == 1)
    {
      handResult = "One pair"
      true
    }
    else {false}
  }

  def highCard(hand: List[Card]): Boolean = {
    //Gets the hand, stores the values into a list and sorts them
    val handValuesList: List[CardValue] = hand.map(card => card.value).sorted

    //Groups the values from the hand by unique values, if there's a duplicate it'll make it 5 -> 2 (two 5s)
    val groupMap = handValuesList.groupBy(identity)

    //If the list contains values more than 1, display how many of those there are - i.e. number of pairs
    val listDuplicates: List[Int] = groupMap.toList.map{case (k:CardValue, ls:List[CardValue]) => ls.length}

    if (listDuplicates.length == handValuesList.length)
    {
      if (handValuesList.startsWith(List(Ace)))
      {
        handResult = s"High Card ${handValuesList.head}"
      }
      else
      {
        handResult = s"High Card ${handValuesList.last}"
      }
      true
    }
    else {false}
  }
}

object Scoring {
  def handScore(hand:List[Card]): (Int, String) =
  {
    val listOfFunctions = List((handChecker.royalFlush _, 10, "Royal Flush"), (handChecker.straightFlush _, 9, "Straight Flush"),
      (handChecker.fourOfAKind, 8, "Four of a Kind"), (handChecker.fullHouse _, 7, "Full House"),
      (handChecker.flush _, 6, "Flush"), (handChecker.straight _, 5, "Straight"),
      (handChecker.threeOfAKind, 4, "Three of a Kind"), (handChecker.twoPairs _, 3, "Two pairs"),
      (handChecker.onePair _, 2, "One pair"), (handChecker.highCard _, 1, "High card"))

    val scores = listOfFunctions.map{case (f,v,s) => (f(hand), v, s)}.filter{case (t, _, _) => t}
    val allScores = scores.map{case (b,v,s) => (v,s)}
    val highScore = allScores.max
    highScore
  }
}
