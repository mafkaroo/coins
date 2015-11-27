package services

import java.math.BigDecimal

import models.Coin

class CoinChange {

  val coins = List(new Coin(200), new Coin(100), new Coin(50), new Coin(20), new Coin(10), new Coin(5), new Coin(2), new Coin(1))
  val higherCurrencyCode = '£'
  val lowerCurrencyCode = 'p'
  val separator = '.'

  val hundred = new BigDecimal(100)

  def parseStringToCurrencyInt(s: String): Int = {
    val bd = parseStringToCurrency(s)
    bd.multiply(hundred).intValue()
  }

  //TODO need better exception handling
  def parseStringToCurrency(s: String): BigDecimal = {
    if (validCurrency(s)) {
      val hasPound = s.head == higherCurrencyCode
      val hasPence = s.last == lowerCurrencyCode

      if (s.contains(separator)) {
        val almostnumber = removeCurrencyCode(removeCurrencyCode(s, higherCurrencyCode), lowerCurrencyCode)
        new BigDecimal(almostnumber)
      } else if (hasPound && hasPence) {
        throw new IllegalArgumentException("Don't understand this")
      } else if (hasPound) {
        new BigDecimal(removeCurrencyCode(s, higherCurrencyCode))
      } else {
        new BigDecimal(removeCurrencyCode(s, lowerCurrencyCode)).divide(hundred)
      }
    } else new BigDecimal(0)
  }

  //should be a better way to do this with reg ex
  //note this is case sensitive, need to validate with requirements
  def validCurrency(s: String): Boolean = {
    val withoutpound = if (s.head == higherCurrencyCode) s.tail.reverse else s.reverse
    val withoutpence = if (withoutpound.head == lowerCurrencyCode) withoutpound.tail else withoutpound
    val count = s.count(c => c == separator)
    if (count < 2) {
      withoutpence.forall(c => c == separator || Character.isDigit(c))
    } else false
  }

  def removeCurrencyCode(s: String, code: Character): String = s.replace(code.toString, "")

  def calculateChange(amount: String): Map[Coin, Int] = {
    val parsedAmount = parseStringToCurrencyInt(amount)
    calculateChange(parsedAmount)
  }

  def calculateChange(amount: Int): Map[Coin, Int] = {

    val empty = Map[Coin, Int]()
    if (amount > 0) {
      val emptyAndRemainder: (Map[Coin, Int], Int) = (empty, amount)
      val (result, _) = coins.foldLeft(emptyAndRemainder)(calculateCount)
      result
    } else empty
  }

  //TODO don't need to iterate over every coin if the remainder is 0, ie the change is £2
  private def calculateCount(e: (Map[Coin, Int], Int), coin: Coin): (Map[Coin, Int], Int) = {
    val (denominations: Map[Coin, Int], amount: Int) = e
    if (amount > 0) {
      val count = amount / coin.amount
      if (count > 0) {
        val remainder = amount - (coin.amount * count)
        (denominations.updated(coin, count), remainder)
      } else {
        e
      }
    } else {
      e
    }
  }
}