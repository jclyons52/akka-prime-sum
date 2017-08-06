package primenumbers.sum

import akka.actor.{Actor, ActorSystem, Props}
import akka.routing.{ActorRefRoutee, RoundRobinRoutingLogic, Router}

object PrimeSum extends App {
  case class FindPrimeSum(limit: Int)
  case class CheckPrime(n: Int)
  case class AddToSum(n: Int)
  case class CreateChildren(n: Int)

  class PrimeSumActor extends Actor {
    private var number = 0
    private var responseCount = 0
    private var limit = 0

    var router = {
      val routees = Vector.fill(4) {
        val r = context.actorOf(Props[PrimeCheckActor])
        context watch r
        ActorRefRoutee(r)
      }
      Router(RoundRobinRoutingLogic(), routees)
    }

    def receive = {
      case FindPrimeSum(n) => requestPrimeChecks(n)

      case AddToSum(n) => {
        addToTotal(n)
        incrementResponseCount()
        printIfFinished()
      }
    }

    private def printIfFinished() = {
      if(responseCount == limit) {
        println(number)
        context.system.terminate()
      }
    }

    private def incrementResponseCount() = {
      responseCount += 1
    }

    private def addToTotal(n: Int) = {
      number += n
    }

    private def requestPrimeChecks(n: Int) = {
      this.limit = n
      (1 to n).foreach(i => {
        router.route(CheckPrime(i), sender())
      })

    }
  }

  class PrimeCheckActor extends Actor {
    def receive = {
      case CheckPrime(n) => isPrime(n)
    }

    def isPrime(n: Int): Unit = {
      if (n < 2) {
        context.parent ! AddToSum(0)
        return
      }
      if (n == 2) {
        context.parent ! AddToSum(n)
        return
      }
      if (n % 2 == 0) {
        context.parent ! AddToSum(0)
        return
      }

      var i = 3
      while ({ i * i <= n }) {
        if (n % i == 0){
          context.parent ! AddToSum(0)
          return
        }
        i += 2
      }
      context.parent ! AddToSum(n)
    }
  }

  val system = ActorSystem("PrimeSumSystem")
  val actor = system.actorOf(Props[PrimeSumActor], "PrimeSumActor1")

  actor ! FindPrimeSum(10000)

  //  system.terminate()
}