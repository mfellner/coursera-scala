import org.coursera.frp._

class BankAccountClassic(b: Int) {
  var balance = b

  def deposit(amount: Int) = {
    balance = balance + amount
  }

  def withdraw(amount: Int) = {
    balance = balance - amount
  }
}

class BankAccountReactive(b: Int) {
  val balance = Var(b)

  def deposit(amount: Int) = {
    val b = balance()
    balance() = b + amount
  }

  def withdraw(amount: Int) = {
    val b = balance()
    balance() = b - amount
  }
}

val account1 = new BankAccountClassic(10)

account1.deposit(5)
account1.withdraw(10)
account1.balance

val account2 = new BankAccountReactive(10)

account2.deposit(5)
account2.withdraw(10)
account2.balance()
