package fr.unice.modalis.cosmic.rest

import fr.unice.modalis.cosmic.deposit.core._
import fr.unice.modalis.cosmic.runtime.{RepositoriesManager, Repository}
import spray.http.MediaTypes._
import spray.json.DefaultJsonProtocol._
import spray.json._
import spray.routing.HttpService
/**
  * Created by Cyril Cecchinel - I3S Laboratory on 24/06/2016.
  */
trait PolicyRouting extends HttpService{


  def convertSensor(s:Sensor[_<:DataType]): Map[String, String] = {
    s match {
      case x:EventSensor[_] => Map("name" -> x.url, "type" -> x.dataType.getSimpleName)
      case x:PeriodicSensor[_] => Map("name" -> x.url, "type" -> x.dataType.getSimpleName, "period" -> x.wishedPeriod.toString)
    }
  }

  case class UpdatePeriod(period:Int)


  object UpdatePeriodProtocol extends DefaultJsonProtocol {
    implicit val updateFormat = jsonFormat1(UpdatePeriod)
  }


  val policyRouting = {
    var repo: Repository = null

    pathPrefix("repo" / Segment) { repoName =>
      get {
        repo = RepositoriesManager.getRepository(repoName).get
        pathEndOrSingleSlash {
          respondWithMediaType(`application/json`) {
            complete(repo.getPolicies.values.map {
              _.name
            }.toArray.toJson.toString())
          }
        }
      } ~ pathPrefix("policy" / Segment) { policy =>
        var _policy: Policy = null
        pathEndOrSingleSlash {
          get {
            _policy = repo.getPolicies.find(_._2.name equals policy).get._2
            complete(_policy.toString)
          }
        } ~ pathPrefix("deploy") {
          pathEndOrSingleSlash {
            get {
              _policy.generate()
              complete(s"OK")
            }
          }
        } ~ pathPrefix("sensors") {
          pathEndOrSingleSlash {
            get {
              respondWithMediaType(`application/json`) {
                complete(_policy.sensors.map {
                  _.url
                }.toArray.toJson.toString())
              }
            }
          }
        } ~ pathPrefix("sensors" / Segment) { sensor =>
          var _sensor:Sensor[_<:DataType] = _policy.findSensorByName(sensor).get
          pathEndOrSingleSlash {
            get {
              respondWithMediaType(`application/json`) {
                complete(convertSensor(_sensor).toJson.toString())
              }
            } ~ put {
              import UpdatePeriodProtocol._
              import spray.httpx.SprayJsonSupport.sprayJsonUnmarshaller

              _sensor match {
                case x:EventSensor[_] => complete("event")
                case x:PeriodicSensor[_] =>
                  entity(as[UpdatePeriod]) { update =>
                    x.wishedPeriod = update.period
                    complete(convertSensor(_sensor).toJson.toString())
                  }
              }
            }
          }
        }
      }
    }
  }
}
