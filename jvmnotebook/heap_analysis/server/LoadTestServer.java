/*
 * Created on 12/22/2008
 * -------------------------- COPYRIGHT_AND_LICENSE --
 * Botlist contains an open source suite of software applications for
 * social bookmarking and collecting online news content for use on the web.
 * Multiple web front-ends exist for Django, Rails, and J2EE.
 * Users and remote agents are allowed to submit interesting articles.
 *
 * Copyright (c) 2007, Botnode.com (Berlin Brown)
 * http://www.opensource.org/licenses/bsd-license.php
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 *	    * Redistributions of source code must retain the above copyright notice,
 *	    this list of conditions and the following disclaimer.
 *	    * Redistributions in binary form must reproduce the above copyright notice,
 *	    this list of conditions and the following disclaimer in the documentation
 *	    and/or other materials provided with the distribution.
 *	    * Neither the name of the Newspiritcompany.com (Berlin Brown) nor
 *	    the names of its contributors may be used to endorse or promote
 *	    products derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * -------------------------- END_COPYRIGHT_AND_LICENSE --
 */
package server;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;

/**
 * Simple Test Server
 */
public class LoadTestServer {
	public static final int DEFAULT_PORT = 9999;

	private int port = 9999;
	private ServerSocket server;

	public LoadTestServer(int port) {
		this.port = port;
	}
	public void runServer() {
		try {
			server = new ServerSocket(this.port);
			System.out.println("server bound to port=" + this.port);
		} catch (IOException e) {
			System.out.println("Could not listen on port=" + this.port);
			System.exit(-1);
		}
		try {
			while (true) {
				LoadTestServerThread clientThread;
				try {
					// server.accept returns a client connection
					System.out.println("waiting for client connection...");
					Socket clientSocket = server.accept();
					if  (clientSocket == null) {
						System.out.println("ERROR: invalid socket connection, closing.");
						return;
					} else {
						System.out.println("connection made=" + clientSocket);
					}
					clientThread = new LoadTestServerThread(clientSocket);
					Thread t = new Thread(clientThread);
					t.start();
				} catch (IOException e) {
					System.out.println("Accept failed: " + this.port);
					System.exit(-1);
				}
			} // End of While
		} finally {
			try {
				System.out.println("Closing server connection");
				server.close();
			} catch (IOException e1) { }
		}
	}

	public static void main(String[] args) {
		System.out.println("-- Running Server");
		LoadTestServer server = new LoadTestServer(DEFAULT_PORT);
		server.runServer();
		System.out.println("-- Done");

	}
}
//End of File
