package fr.unice.modalis.cosmic.rest

import akka.actor.{Actor, ActorRefFactory, ActorSystem, Props}
import akka.io.IO
import akka.pattern.ask
import akka.util.Timeout
import spray.can.Http

import scala.concurrent.duration._
/**
  * Created by Cyril Cecchinel - I3S Laboratory on 24/06/2016.
  */
trait RunServer{
  val serverPort = 7000
  implicit val system = ActorSystem("on-spray-can")

  val service = system.actorOf(Props[RemoteDEPOSITActor], "collector-service")
  implicit val timeout = Timeout(5.seconds)


  IO(Http) ? Http.Bind(service, interface = "0.0.0.0", port = serverPort)
}

class RemoteDEPOSITActor extends Actor with PolicyRouting {


  implicit val system = context.system

  override def receive: Receive = runRoute(policyRouting)

  override implicit def actorRefFactory: ActorRefFactory = context
}