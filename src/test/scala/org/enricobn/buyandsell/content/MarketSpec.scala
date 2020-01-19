package org.enricobn.buyandsell.content

import org.enricobn.SpecWithShell

class MarketSpec extends SpecWithShell {

  "adding" should "be fine" in {
    val market = Market(Map(), Map())
      .addDefaultPrice("silver", 1000)
      .goodChanged("silver", 3)

    market.get("silver").price should be (BigDecimal(333.33))
  }

}
