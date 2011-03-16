package com.berlin.wstock.job

import scala.{ Array => ScalaArray }
import java.sql._
import javax.sql.DataSource
import org.apache.derby.drda.NetworkServerControl
import java.util.Properties
import java.io.BufferedReader
import java.io.InputStreamReader

object JobMain {	
	
	def waitForStart() : Unit = {
		
		// Use NetworkServerControl.ping() to wait for
		// NetworkServer to come up.  We could have used
		// NetworkServerControl to start the server but the property is
		// easier.
		val server = new NetworkServerControl

		System.out.println("Testing if Network Server is up and running...");		
		for (i <- 0 until(10)) {
			
			try {
				Thread.sleep(2000)
				server.ping
				println("Ping-Try #" + i)
			} catch {
				case e: Exception => {
					println("Ping-Try<Err> #" + i + " " +e.toString())
					if (i == 9) {				
						println("Giving up trying to connect to Network Server!")
						throw e;
					} // End of if //
				} // End Case Exception //
			}
			
		} // End of the For //				
		System.out.println("Derby Network Server now running");

	}

	def waitForExit() : Unit = { 	
		println("Clients can continue to connect: ")
 		val in = new BufferedReader(new InputStreamReader(System.in))
 		println("Press [Enter] to stop Server")
 		in.readLine
	}

	
	def startWithProperty() : Unit = {
		
		println("Starting Network Server")
		System.setProperty("derby.drda.startNetworkServer", "true")

		Class.forName("org.apache.derby.jdbc.EmbeddedDriver").newInstance();		
	}
	
	def main(args : ScalaArray[String]) : Unit = {
		println("INFO: running job with DB server")
		startWithProperty
		waitForStart
		waitForExit
		println("INFO: exiting job with DB server")
	}
  
} // End of the Application //
