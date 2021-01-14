// This Cafe is intended to 'serve' as an introduction to functional programming in Scala
// Note Credit card is not implemented as it is not needed to meet the learning objective.
class Cafe {
  /*
  Note there are no side effects here. The credit card is simply passed along as a
  Charge object. No state is mutated which makes this a Pure Function. Input, Output
   */
  def buyCoffee(cc: CreditCard):(Coffee, Charge) = {
    val cup = new Coffee()
    (cup, Charge(cc, cup.price))
  }

  /*
  Calls buyCoffee n times and returns a list of Coffees with n charges reduced to
  a single credit card charge.
   */
  def buyCoffees(cc:CreditCard, n:Int):(List[Coffee],Charge) = {
    /*
    List.fill(n)(x) creates a list with n copies of x. Remember x can be a method.
     */
    val purchases: List[(Coffee,Charge)] = List.fill(n)(buyCoffee(cc))
    /*
    'unzip' splits a list of pairs into a pair of lists
     */
    val (coffees, charges) = purchases.unzip
    /*
     charges.reduce reduces the entire list into a single charge using combine
     to combine two charges at a time
     */
    (coffees,charges.reduce((c1,c2)=>c1.combine(c2)))
  }

  def coalesce(charges: List[Charge]): List[Charge] =
    charges.groupBy(_.cc).values.map(_.reduce(_ combine _)).toList
}

// Note that a 'case' class does not need to be instantiated with 'new'
case class Charge(cc: CreditCard, amount: Double){
  def combine(other:Charge): Charge ={
    if(cc == other.cc)
      Charge(cc,amount + other.amount)
    else
      throw new Exception("Cant combine charges to different cards")
  }
}
