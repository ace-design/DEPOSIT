package fr.unice.modalis.cosmic.simulator.smartparking

import java.io.File

import com.github.tototoshi.csv.CSVWriter
import fr.unice.modalis.cosmic.deployment.utils.TopologyModelBuilder

/**
  * Created by Cyril Cecchinel - I3S Laboratory on 25/03/2016.
  */
object Benchmark extends App{

  val ratio_bridge = 10
  var valuesRun = List[(Int,List[(Int,Long)])]()

  for (exec <- 1 to 30) {
    println(s"*** Execution #$exec ***")

    var xpResults = List[(Int,Long)]()

    for (i <- 1000 to 20000 by 1000){
      println(s"#Sensors $i")
      val model = TopologyParkingHelper("SmartParking", 1 to i, 1 to (i*ratio_bridge / 100))
      scala.xml.XML.save("assets/configurations/demo_smartparking.xml", model)
      val t1 = System.currentTimeMillis()
      TopologyModelBuilder.loadFromSpineFM("assets/configurations/demo_smartparking.xml")
      val t2 = System.currentTimeMillis()
      xpResults = (i, t2 - t1) :: xpResults
      new File("assets/configurations/demo_smartparking.xml").delete()
    }
    xpResults = xpResults.reverse
    valuesRun = (exec, xpResults) :: valuesRun

  }

  valuesRun = valuesRun.reverse
  val f = new File(s"out/benchmark/result_xp.csv")
  val writer = CSVWriter.open(f)
  valuesRun.foreach(v => writer.writeRow(v._2.map(_._2)))
  writer.close()

}
