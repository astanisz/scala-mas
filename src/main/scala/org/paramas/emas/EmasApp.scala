package org.paramas.emas

import org.paramas.mas.util.{Logger, Reaper}
import akka.actor.{Props, ActorSystem}
import scala.concurrent.duration._
import akka.event.Logging
import org.paramas.emas.stat.{MonitoredEmasLogic, Statistics}
import scala.concurrent.ExecutionContext.Implicits.global
import org.paramas.mas.{Logic, RootEnvironment}
import org.paramas.emas.config.AppConfig
import org.paramas.mas.async.AsyncEnvironment
import org.paramas.mas.sync.SyncEnvironment

object Async extends EmasApp {

  def main(args: Array[String]) {
    run("concurrent", AsyncEnvironment.props, 10 seconds)
  }
}

object Sync extends EmasApp {

  def main(args: Array[String]) {
    run("hybrid", SyncEnvironment.props, 10 seconds)
  }
}

class EmasApp {

  def run(name: String, islandsProps: (Logic) => Props, duration: FiniteDuration) {
    implicit val system = ActorSystem(name)
    implicit val settings = AppConfig(system)
    implicit val stats = Statistics()

    val log = Logging(system, getClass)
    Logger(frequency = 1 second) {
      time =>
        val (fitness, reproductions) = stats()
        log info (s"fitness $time $fitness")
        log info (s"reproductions $time $reproductions")
    }

    val root = system.actorOf(RootEnvironment.props(islandsProps(MonitoredEmasLogic(new EmasLogic))), "root")
    for (
      _ <- Reaper.terminateAfter(root, duration);
      _ <- stats.updatesDone) {
      system.shutdown()
    }
  }
}