package moe.pizza.battledetector

import moe.pizza.zkapi.zkillboard.Killmail
import org.joda.time.DateTime
import org.slf4j.LoggerFactory
import scala.collection.mutable

/**
  * Created by andi on 09/03/16.
  */
class BattleDetector(pilotthreshold: Long = 20, timewindow: Int = 1) {

  val log = LoggerFactory.getLogger(getClass)
  val lastHourStorage = mutable.Map[Long, mutable.Buffer[TimeAndKillmail]]()

  case class TimeAndKillmail(time: DateTime, km: Killmail)

  /***
    * Called by the websocket handler, parses a killmail
    * @param k
    */
  def addKm(k: Killmail): Unit = {
    val time = DateTime.parse(k.killTime)
    if (time.isAfter(DateTime.now().minusHours(timewindow))) {
      // the killmail is in our interesting section
      lastHourStorage
        .getOrElse(
          k.solarSystemID,
          lastHourStorage.put(k.solarSystemID, mutable.Buffer()).get
        )
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
      val uniqueattackers = buffer.flatMap(_.km.attackers).map(a => (a.characterName, a.shipTypeID)).toSet
      val uniquevictims = buffer.map(_.km.victim).map(v => (v.characterName, v.shipTypeID)).toSet
      // join together attackers and victims
      val uniquepilots = uniqueattackers.union(uniquevictims)

      // detect unique battles based on attackers
      uniqueattackers.map { case (name, shiptype) =>
          buffer.map(_.km).filter(_.attackers.map(_.characterName).contains(name))
      }.foldRight(List.empty[mutable.Buffer[Killmail]]) { (kmgroup, accumulators) =>
        val names = kmgroup.flatMap(_.attackers.map(_.allianceName))
        accumulators.find(_.contains{k: Killmail => k.attackers.map(_.characterName).union(names).length > 0}) match {
          case Some(s) =>
            s ++= kmgroup
            accumulators
          case None => accumulators :+ kmgroup
        }
      }.foreach { battle =>
        println(s"detected a battle in ${system} with ${battle.length} killmails and ${uniquepilots} unique pilots")
      }



    }
  }

}
