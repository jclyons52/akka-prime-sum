package primenumbers.sum

import akka.actor.{Actor, ActorSystem, Props}
import akka.routing.{ActorRefRoutee, RoundRobinRoutingLogic, Router}

object PrimeSum extends App {
  case class FindPrimeSum(limit: Int)
  case class AddToSum(n: Int)

  class PrimeSumActor extends Actor {
    private var number = 0
    private var responseCount = 0
    private var limit = 0

    private val router = {
      val routees = Vector.fill(4) {
        val r = context.actorOf(Props[PrimeCheckActor])
        context watch r
        ActorRefRoutee(r)
      }
      Router(RoundRobinRoutingLogic(), routees)
    }

    def receive = {
      case FindPrimeSum(n) => requestPrimeChecks(n)

      case AddToSum(n) =>
        addToTotal(n)
        incrementResponseCount()
        printIfFinished()

    }

    private def printIfFinished() = {
      if(responseCount == limit) {
        println(number)
        context.system.terminate()
      }
    }

    private def incrementResponseCount() = responseCount += 1

    private def addToTotal(n: Int) = number += n

    private def requestPrimeChecks(n: Int) = {
      limit = n
      (1 to n).foreach(i => {
        router.route(CheckPrime(i), sender())
      })

    }
  }

  case class CheckPrime(n: Int)

  class PrimeCheckActor extends Actor {
    def receive = {
      case CheckPrime(n) => runPrimeCheck(n)
    }

    private def runPrimeCheck(n: Int): Unit = {
      context.parent ! AddToSum(getAmountToAdd(n))
    }

    private def getAmountToAdd(n: Int): Int = {
      if (n < 2) return 0
      if (n == 2) return 2
      if (n % 2 == 0) return 0

      var i = 3
      while ({ i * i <= n }) {
        if (n % i == 0)return 0
        i += 2
      }
      n
    }
  }

  val system = ActorSystem("PrimeSumSystem")
  val actor = system.actorOf(Props[PrimeSumActor], "PrimeSumActor1")

  actor ! FindPrimeSum(1000)
}