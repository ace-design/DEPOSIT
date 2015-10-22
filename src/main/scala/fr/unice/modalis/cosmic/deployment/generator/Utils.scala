package fr.unice.modalis.cosmic.deployment.generator

import java.io.PrintWriter

import scala.io.Source

/**
 * Created by Cyril Cecchinel - I3S Laboratory on 05/10/15.
 */
/**
 * Utils functions for action conversion
 */
object Utils {

  // Sensor registry location
  val SENSOR_MAPPING = "assets/sensors/mapping.csv"

  /**
   * Read csv file function
   * @param file File to parse
   * @return Iterator of array strings
   */
  def csvReader(file:String):Iterator[Array[String]] =
  {
    Source.fromFile(file).getLines().drop(1).map(_.split(","))
  }

  /**
   * Find sensor assignement
   * @param name Sensor name
   * @return Assignement (or exception if not found)
   */
  def lookupSensorAssignment(name:String):String = {
    csvReader(SENSOR_MAPPING).find(a => a(0).equals(name)) match {
      case Some(s) => s(1)
      case None => throw new Exception("Sensor " + name + " not found")
    }
  }

  /**
  * Compute the gcd
  * @param a a
  * @param b b
  * @return The greatest common divisor between a and b
    */
  def gcd(a: Int, b: Int): Int = {
    if (b == 0) a.abs else gcd(b, a % b)
  }

  /**
   * Compute the lcd
   * @param a a
   * @param b b
   * @return The least commin divisor between a and b
   */
  def lcm(a: Int, b: Int) = {
    (a * b).abs / gcd(a, b)
  }

  /**
   * Compute lcd over a number list
   * @param args Int list
   * @return The greatest common divisor between each number
   */
  def lcmm(args: List[Int]): Int =
    args match {
      case Nil => throw new Exception("The lcd computation needs two operands")
      case a :: Nil => a
      case a :: b :: Nil => lcm(a, b)
      case a :: b :: tail => lcmm(lcm(a, b) :: tail)
    }


  /**
   * Write text to a file located in the out/ directory
   * @param name Name of the file (will be completed with the current timestap)
   * @param text Text
   */
  def writefile(name:String, text:String):Unit = {
    val file = new PrintWriter("out/"+ name + System.currentTimeMillis())
    file.println(text)
    file.close()
  }
}
