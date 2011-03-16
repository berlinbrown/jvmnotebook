/*********************************************************************
 *********************************************************************/

package octane.gui.adder

import scala.swing._
import scala.swing.event._
import java.util.Date
import java.awt.Color
import java.awt.{Image, Point}

object addermain extends GUIApplication {

  def resourceFromClassloader(path: String): java.net.URL =
    this.getClass.getResource(path)
  
  def resourceFromUserDirectory(path: String): java.io.File =
    new java.io.File(System.getProperty("user.dir"), path)

  def main(args: Array[String]) = run {

    val comp = new Component {
      size  = (500, 400) 
      preferredSize = (500, 400) 
      override def paintComponent(g: java.awt.Graphics) {
        super.paintComponent(g)

        // Paint the background with white.
        g.setColor(Color.WHITE)
        g.fillRect(0, 0, 500, 400)

        // Paint the foreground with red.
        g.setColor(Color.RED)
        g.fillRect(0, 0, 30, 30)
      }   
    } // End of Component

    val frame = new MainFrame {
      title = "Genetic Adder"
      location = new Point(300, 300)
      size  = (500, 400)                 
      contents = comp  
    } // End of Frame   

    frame.pack()
    frame.visible = true
  } // End of Main
 
} // End of Object

//////////////////////////////
// End of File
//////////////////////////////
