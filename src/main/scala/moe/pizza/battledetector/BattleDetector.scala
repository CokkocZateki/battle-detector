package moe.pizza.battledetector

import moe.pizza.zkapi.RedisQTypes.Killmail
import moe.pizza.zkapi.ZKBAPI
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import org.slf4j.LoggerFactory
import scala.collection.mutable
import moe.pizza.eveapi.SyncableFuture

import scala.util.{Failure, Success}

/**
  * Created by andi on 09/03/16.
  */
class BattleDetector(pilotthreshold: Long = 20, timewindow: Int = 1) {

  val log = LoggerFactory.getLogger(getClass)
  val lastHourStorage = mutable.Map[Long, mutable.Buffer[TimeAndKillmail]]()

  case class TimeAndKillmail(time: DateTime, km: Killmail)

  val zkbdtformat = DateTimeFormat.forPattern("yyyy.MM.dd HH:mm:ss")

  /***
    * Called by the websocket handler, parses a killmail
    * @param k
    */
  def addKm(k: Killmail): Unit = {
    val time = zkbdtformat.parseDateTime(k.killTime)
    if (time.isAfter(DateTime.now().minusHours(timewindow))) {
      // the killmail is in our interesting section
      lastHourStorage
        .get(
          k.solarSystem.id).getOrElse {
            val buf = mutable.Buffer[TimeAndKillmail]()
            lastHourStorage.put(k.solarSystem.id, buf)
            buf
         }
        .append(
          TimeAndKillmail(time, k)
        )
      log.info(s"added relevant looking killmail with id ${k.killID}")
    }
  }

  /***
    * Remove all killmails that are too old from our buckets
    */
  def clean(): Unit = {
    val threshhold = DateTime.now().minusHours(timewindow)
    lastHourStorage.transform { (system, buffer) =>
      buffer.filter(_.time.isAfter(threshhold))
    }
  }

  def analyse(): Unit = {
    lastHourStorage.foreach { case (system, buffer) =>
      // dedupe on charactername + shiptype
      println(buffer.flatMap(k => Option(k.km.attackers)).flatten)
      println(buffer.flatMap(k => Option(k.km.attackers)).flatten.map(_.character))
      println(buffer.flatMap(k => Option(k.km.attackers)).flatten.map(_.shipType))
      val uniqueattackers = buffer.flatMap(k => Option(k.km.attackers)).flatten.map(a => Option(a.character).map(_.name).getOrElse("unknown")).toSet
      val uniquevictims = buffer.map(_.km.victim).map(a => Option(a.character).map(_.name).getOrElse("unknown")).toSet
      // join together attackers and victims
      val uniquepilots = uniqueattackers.union(uniquevictims)

      // detect unique battles based on attackers
      uniqueattackers.map { name =>
          buffer.map(_.km).filter(_.attackers.flatMap(a => Option(a.character).map(_.name)).contains(name))
      }.foldRight(List.empty[mutable.Buffer[Killmail]]) { (kmgroup, accumulators) =>
        val names = kmgroup.flatMap(_.attackers.flatMap(a => Option(a.character).map(_.name)))
        accumulators.find(_.contains{k: Killmail => k.attackers.map(_.character.name).union(names).length > 0}) match {
          case Some(s) =>
            s ++= kmgroup
            accumulators
          case None => accumulators :+ kmgroup
        }
      }.foreach { battle =>
        val battleuniques = battle.flatMap(_.attackers).flatMap(a => Option(a.character).map(_.name)).toSet
          .union(
            battle.map(_.victim).map(_.character.name).toSet
          )

        println(s"detected a battle in ${system} with ${battle.length} killmails and ${battleuniques.size} unique pilots")
      }



    }
  }


}
