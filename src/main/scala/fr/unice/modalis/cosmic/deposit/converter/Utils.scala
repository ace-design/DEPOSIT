package fr.unice.modalis.cosmic.deposit.converter

import java.io.PrintWriter

/**
 * Writing Util methods
 * Created by Cyril Cecchinel - I3S Laboratory on 26/12/14.
 */
object Utils {
  def writefile(name:String, text:String):Unit = {
    val file = new PrintWriter("out/"+ name + System.currentTimeMillis())
    file.println(text)
    file.close()
  }
}
