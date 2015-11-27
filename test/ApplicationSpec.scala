import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._

import play.api.test._
import play.api.test.Helpers._

import services.CoinChange
import java.math.BigDecimal

/**
 * Add your spec here.
 * You can mock out a whole application including requests, plugins etc.
 * For more information, consult the wiki.
 */
@RunWith(classOf[JUnitRunner])
class ApplicationSpec extends Specification {

  "Application" should {

    "calculate correct change" in {
      val change = new CoinChange
      val x = change.calculateChange(0)
      x.isEmpty must beTrue
      val y = change.calculateChange(-1)
      y.isEmpty must beTrue
      val result1 = change.calculateChange(85).map(e => e._1.amount -> e._2)
      result1 must equalTo(Map((50, 1),(20, 1),(10, 1),(5, 1)))

      val result2 = change.calculateChange(123).map(e => e._1.amount -> e._2)
      result2 must equalTo(Map((100, 1),(20, 1),(2, 1),(1, 1)))

      val result3 = change.calculateChange(1234).map(e => e._1.amount -> e._2)
      result3 must equalTo(Map((200, 6),(20, 1),(10, 1),(2, 2)))
    }
    
    "parse some currency" in {
      val change = new CoinChange
      change.parseStringToCurrency("1234") must equalTo(new BigDecimal("12.34"))
      change.parseStringToCurrency("1234p") must equalTo(new BigDecimal("12.34"))
      change.parseStringToCurrency("£1234") must equalTo(new BigDecimal("1234"))
      change.parseStringToCurrency("£12.34") must equalTo(new BigDecimal("12.34"))
      change.parseStringToCurrency("£12.34p") must equalTo(new BigDecimal("12.34"))
      change.parseStringToCurrency("12.34p") must equalTo(new BigDecimal("12.34"))
      
      
    }
    
    "parse currency to int" in {
      val change = new CoinChange
      //432 (432), 213p (213), £16.23p (1623), £14 (1400), £54.04 (5404), £23.33333 (2333), 001.41p (141).
      change.parseStringToCurrencyInt("432") must equalTo(432)
      change.parseStringToCurrencyInt("213p") must equalTo(213)
      change.parseStringToCurrencyInt("£16.23p") must equalTo(1623)
      change.parseStringToCurrencyInt("£16.23P") must equalTo(0) //currently case sensitive
      change.parseStringToCurrencyInt("£14") must equalTo(1400)
      change.parseStringToCurrencyInt("£54.04") must equalTo(5404)
      change.parseStringToCurrencyInt("£23.333333") must equalTo(2333)
      change.parseStringToCurrencyInt("001.41p") must equalTo(141)
    }
    
    "parse input" in {
      val change = new CoinChange

      change.validCurrency("£1234.02p") must beTrue
      change.validCurrency("£1234") must beTrue
      change.validCurrency("$1234") must beFalse
      change.validCurrency("1234") must beTrue
      change.validCurrency("1234p") must beTrue
      change.validCurrency("12 34") must beFalse
      change.validCurrency("nonesense") must beFalse
      change.validCurrency("1234") must beTrue
      change.validCurrency("£1234p") must beTrue
      change.validCurrency("p1234£") must beFalse
      change.validCurrency("£12.34.02p") must beFalse
    }
  }
}
