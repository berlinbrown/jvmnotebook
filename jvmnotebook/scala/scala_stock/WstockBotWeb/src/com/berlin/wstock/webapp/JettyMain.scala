package com.berlin.wstock.webapp

import org.mortbay.jetty.Connector
import org.mortbay.jetty.Server
import org.mortbay.jetty.bio.SocketConnector
import org.mortbay.jetty.webapp.WebAppContext

object MainJetty {

  def main(args : Array[String]) : Unit = {
	  
	  	val server = new Server
		val connector = new SocketConnector						
		connector.setPort(6680)
		connector.setMaxIdleTime(1000 * 60 * 60)
		connector.setSoLingerTime(-1)
				
		var connectorArr: Array[Connector] = new Array[Connector](1)
		connectorArr(0) = connector
		server.setConnectors(connectorArr)
		
		val webAppContext = new WebAppContext
		webAppContext.setServer(server)	
		//webAppContext.setContext("/")
		webAppContext.setWar("./webapp/wstockbot")			
		server.addHandler(webAppContext)

		try {
			println("INFO: Starting Jetty")
			server.start
			System.in.read
			println("INFO: Stopping Jetty")            
			server.stop
			//server.join	
		} catch {
			case e: Exception => {
				e.printStackTrace
				System.exit(100)	  			 
			}
		} // End of try catch //
		
  } // End of the Method //
  
} // End of the Application //
