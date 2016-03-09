package moe.pizza.battledetector

import moe.pizza.zkapi.WebsocketFeed
import moe.pizza.zkapi.zkillboard.Killmail
import org.slf4j.LoggerFactory

/**
  * Created by andi on 09/03/16.
  */
object Main extends App {

  val logger = LoggerFactory.getLogger("main")
  val ws = WebsocketFeed.createClient("ws://ws.eve-kill.net/kills", {s => println(s)}) //"wss://api.pizza.moe/stream/killmails/", {s: Killmail => logger.info(s.toString)})



}
