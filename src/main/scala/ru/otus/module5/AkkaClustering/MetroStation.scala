package ru.otus.module5.AkkaClustering

import akka.actor.Props
import akka.cluster.sharding.{ClusterSharding, ClusterShardingSettings, ShardRegion}
import com.typesafe.config.ConfigFactory
import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem}
import akka.cluster.sharding.external.ExternalShardAllocationStrategy.ShardRegion

import scala.util.Random
import java.util.{Date, UUID}

case class TroykaCard(id: String, isAllowed: Boolean)
case class EntryAttemp(troykaCard: TroykaCard, date: Date)
case class EntryRejected(reason: String)
case object EnryAccepted


class Turnstile(validator: ActorRef) extends Actor with ActorLogging {
  override def receive: Receive = {
    case o: TroykaCard =>
      log.info("validator")
      validator ! EntryAttemp(o, new Date)
    case EnryAccepted => log.info("Green")
    case EntryRejected(reason) => log.info(s"Red $reason")
  }
}

class TroykaCovidPassValidator extends Actor with ActorLogging {
  override def preStart(): Unit = {
    super.preStart()
    log.info("start checking")
  }

  override  def receive: Receive = {
    case EntryAttemp(card @ TroykaCard(_, isAllowed), _) =>
      log.info(s"validating $card")
      if (isAllowed) sender() ! EnryAccepted
      else sender() ! EntryRejected(s"not your day, sorry")
  }
}

object TurnstileSettings {
  val numberOfShards = 3
  val numberOfEntitites = 30

  val extractEntityId: ShardRegion.ExtractEntityId = {
    case attemp @ EntryAttemp(TroykaCard(cardId, _), _) =>
      val entryId = cardId.hashCode % numberOfEntitites
      println(s"!!!! extract entry id for card # ${attemp.troykaCard.id} to entry ID ${entryId}")
      (entryId.toString, attemp)
  }

  val extractShardId: ShardRegion.ExtractShardId = {
    case EntryAttemp(TroykaCard(cardId, _), _) =>
      val shardId = cardId.hashCode % numberOfShards
      println(s"!!!! extract shard id for card # $cardId to entity Id $shardId")
      shardId.toString
  }
}

class MetroStation(port: Int, amountOfTurnstile: Int) extends App {
  val config = ConfigFactory.parseString(
    s"""
       akka.remote.artery.canonical.port = $port
       |""".stripMargin).withFallback(ConfigFactory.load("clusterShardingExample.conf"))

  val system = ActorSystem("DemoCluster", config)

  val validatorShardRegionRef: ActorRef =
    ClusterSharding(system).start(
      typeName = "TroykaCovidPassValidator",
      entityProps = Props[TroykaCovidPassValidator],
      settings = ClusterShardingSettings(system),
      extractEntityId = TurnstileSettings.extractEntityId,
      extractShardId = TurnstileSettings.extractShardId)

  val turnstiles: Seq[ActorRef] = (1 to amountOfTurnstile)
    .map{x=>
      println(s"Before starting actor of turnstiles # $x")
      system.actorOf(Props(new Turnstile(validatorShardRegionRef)))
    }

  Thread.sleep(30000)
  for (_ <- 1 to 1000) {
    val randomTurnstileIndex = Random.nextInt(amountOfTurnstile)
    val randomTurnstile = turnstiles(randomTurnstileIndex)

    randomTurnstile ! TroykaCard(UUID.randomUUID().toString, Random.nextBoolean())
    Thread.sleep(200)
  }
}

object  ChistyePrudy extends  MetroStation(2551, 10)
object  Lubanka extends  MetroStation(2561, 5)
object  OkhotnuRad extends  MetroStation(2571, 15)