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


  implicit object SensorFormat extends JsonWriter[Sensor[_<:DataType]] {
    override def write(obj: Sensor[_ <: DataType]): JsValue = obj match {
      case x:EventSensor[_] => JsObject("name" -> JsString(x.url), "type" -> JsString(x.dataType.getSimpleName))
      case x:PeriodicSensor[_] => JsObject("name" -> JsString(x.url), "type" -> JsString(x.dataType.getSimpleName), "period" -> JsNumber(x.wishedPeriod))
    }
  }

  implicit object CollectorFormat extends JsonWriter[Collector[_<:DataType]]{
    override def write(obj: Collector[_ <: DataType]): JsValue = JsObject("name" -> JsString(obj.name), "type" -> JsString(obj.dataType.getSimpleName))
  }

  implicit object PolicyFormat extends JsonWriter[Policy] {
    override def write(obj: Policy): JsValue = JsObject("name" -> JsString(obj.name),
      "sensors" -> JsArray(obj.sensors.map{SensorFormat.write}.toVector),
      "collectors" -> JsArray(obj.collectors.collect{case x:Collector[_] => x}.map{CollectorFormat.write}.toVector)
    )
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
            respondWithMediaType(`application/json`) {
              complete(PolicyFormat.write(_policy).toString())
            }
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
                complete(SensorFormat.write(_sensor).toString())
              }
            } ~ put {
              import UpdatePeriodProtocol._
              import spray.httpx.SprayJsonSupport.sprayJsonUnmarshaller

              _sensor match {
                case x:EventSensor[_] => complete("event")
                case x:PeriodicSensor[_] =>
                  entity(as[UpdatePeriod]) { update =>
                    x.wishedPeriod = update.period
                    complete(SensorFormat.write(_sensor).toString())
                  }
              }
            }
          }
        }
      }
    }
  }
}
