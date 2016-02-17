case class Coffee(price: Double = 25)


case class CreditCard(id: Int)


case class Charge(cc: CreditCard, amount: Double) {
  def combine(other: Charge): Charge = {
    if (cc == other.cc)
      Charge(cc, amount + other.amount)
    else
      throw new RuntimeException("Can not combinen charges to different credit cards")
  }
}


class Cafe {
  def buyCoffee(cc: CreditCard): (Coffee, Charge) = {
    val cup = Coffee()
    val charge = Charge(cc, cup.price)
    (cup, charge)
  }


  def buyCoffees(cc: CreditCard, n: Int): (List[Coffee], Charge) = {
    val purchases = List.fill(n)(buyCoffee(cc))
    val (coffees, charges) = purchases.unzip
    (coffees, charges.reduce(_ combine _))
  }


  def coalescePerCreditcard(charges: List[Charge]): List[Charge] = {
    charges.groupBy(_.cc).values.map(_.reduce(_ combine _)).toList
  }
}


val cafe = new Cafe()
val myCc = new CreditCard(666)

cafe.buyCoffee(myCc)
cafe.buyCoffees(myCc, 4)


val otherCc = new CreditCard(777)
val inputMultipleCards = List.fill(3)(cafe.buyCoffee(myCc)._2) ::: List.fill(2)(cafe.buyCoffee(otherCc)._2)

cafe.coalescePerCreditcard(inputMultipleCards)

"afdafaf"