package com.berlin.wstock.job.ping

import java.lang.reflect.InvocationTargetException
import java.lang.reflect.Method
import java.sql.Connection
import java.sql.DriverManager
import java.sql.ResultSet
import java.sql.SQLException
import java.sql.Statement
import java.util.Properties
import javax.sql.DataSource


object SimpleClientApp {
		
	/**
	 * The database is located in the same directory where this program is being
	 * run. Alternately one can specify the absolute path of the database location
	 */
	val DBNAME ="NSSampleDB"
	
	/**
	 * Derby network server port ; default is 1527
	 */
	val NETWORKSERVER_PORT = 1527
	
	val DERBY_CLIENT_DRIVER = "org.apache.derby.jdbc.ClientDriver"
	val DERBY_CLIENT_DS     = "org.apache.derby.jdbc.ClientDataSource"

	/**
	 * This URL is used to connect to Derby Network Server using the DriverManager.
	 * Notice that the properties may be established via the URL syntax
	 */
	val CS_NS_DBURL= "jdbc:derby:net://localhost:"+NETWORKSERVER_PORT+"/"+DBNAME+";retrieveMessagesFromServerOnGetMessage=true;deferPrepares=true;";
	
   /**
    * URL for the Derby client JDBC driver.
    */
	val DERBY_CLIENT_URL= "jdbc:derby://localhost:"+ NETWORKSERVER_PORT+"/"+DBNAME+";create=true";
	
	val url            = DERBY_CLIENT_URL
    val jdbcDriver     = DERBY_CLIENT_DRIVER
    val jdbcDataSource = DERBY_CLIENT_DS
	
    /////////////////////////////////////////////////////////////////
    
    def getClientDriverManagerConnection() : Connection = {
		
		// See Derby documentation for description of properties that may be set
		//  in the context of the network server.
		val properties = new java.util.Properties();

		// The user and password properties are a must, required by JCC
		properties.setProperty("user","derbyuser")
		properties.setProperty("password","pass")

		// Get database connection  via DriverManager api
		val conn = DriverManager.getConnection(url, properties) 
		return conn
	}

    def startPingClient() : Unit = {
    	
    	val conn = getClientDriverManagerConnection
    	println(conn)    
    	
    	val ds = getClientDataSource("user", "hello", "hello")
    	println(ds)
    }
	
    def getMethod[T](c:java.lang.Class[_], o:AnyRef, fieldName:String, array:Array[Class[T]]) = {
    	
    	val method = c.getMethod(fieldName, array:_*)
    	method
    }
    
    def getClientDataSource(database:String, user:String, password:String) : DataSource = {
    	
		val nsDataSourceClass = Class.forName(jdbcDataSource)
		val ds:DataSource = (nsDataSourceClass.newInstance()).asInstanceOf[DataSource]

		// can also include Derby URL attributes along with the database name
		val methodParams:Array[Class[Object]] = new Array[Class[Object]](1)		
		var args:Array[String] = new Array[String](1)
		args(0) = database
		/*
		val methodParams:Array[Class[_]] = new Array[Class[_]](1)	
		methodParams(0) = 
		val dbname = nsDataSource.getMethod("setDatabaseName", methodParams)		

		var args:Array[java.lang.Object] = new Array[java.lang.Object](1)
		args(0) = database				
		dbname.invoke(ds, args)
		*/
		
		val methodDbName = getMethod[Object](nsDataSourceClass, ds, "setDatabaseName", methodParams)
		methodDbName.invoke(ds, args)
		
		/*
		if (user != null) {
			val setuser = nsDataSource.getMethod("setUser", methodParams)
			args = new Array[java.lang.Object](1)
			args(0) = user
			
			setuser.invoke(ds, args)
		}
		
		if (password != null) {
			val setpw = nsDataSource.getMethod("setPassword", methodParams)
			args = new Array[java.lang.Object](1)
			args(0) = password			
			setpw.invoke(ds, args)
		}
		
		// host on which network server is running
		val servername = nsDataSource.getMethod("setServerName", methodParams)
		
		args = new Array[java.lang.Object](1)
		args(0) = "localhost"		
		servername.invoke(ds, args)

		// port on which Network Server is listening
		val methodParamsPort:java.lang.Class[_] = classOf[java.lang.Integer]	
		val portnumber = nsDataSource.getMethod("setPortNumber", methodParams)
		
		args = new Array[java.lang.Object](1)
		args(0) = new Integer(1527)				
		portnumber.invoke(ds, args)
			*/
		/* Return DS */
		ds
	}
  
	def main(args : Array[String]) : Unit = {
		  
		println("Running ping client")
		Class.forName(jdbcDriver).newInstance()
		startPingClient
	}
  
} // End of the Class //
