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

import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.net.Socket;
import java.util.Date;

/**
 * Simple Server Client Thread Handler
 */
public class LoadTestServerThread implements Runnable {

    public static final int MAX_NO_ROWS = 50;
    
    private final SimpleReportBean reportData [] = new SimpleReportBean[MAX_NO_ROWS];

    private static final Runtime runtime = Runtime.getRuntime();
	private Socket client;
	private boolean running = false;
	private DataInputStream in;
	private PrintStream out;

	public LoadTestServerThread(Socket client) {
		this.client = client;
		try {
			System.out.println("communicating with server=" + client);
			in = new DataInputStream(client.getInputStream());
			out = new PrintStream(client.getOutputStream());
		} catch (IOException e) {
			try {
				client.close();
			} catch (IOException e2) { ; }
			System.err.println("Exception while opening socket streams: " + e);
			return;
		}
	}
    
    /**
     * Build the report output
     */
    public String reportOutput_StringBuffer() {
        for (int i = 0; i < reportData.length; i++) {
            reportData[i] = new SimpleReportBean();
            
        }   
        return null;
    }
    public String reportOutput_UseString() {
        String data = "";
        
        data += "<table border='1'>\n";
        for (int i = 0; i < reportData.length; i++) {
            reportData[i] = new SimpleReportBean();
            data += "<tr>\n";

            data += "<td>";
            data += reportData[i].getSomeNo();
            data += "</td>\n";

            data += "<td>";
            data += reportData[i].getMailingAddr1();
            data += "</td>\n";

            data += "</tr>\n";
        }
        data += "</table>\n";
        return data;
    }

	/**
	 * @see java.lang.Runnable#run()
	 */
	public void run() {
		running = true;
		String line;
		try {
			BufferedReader bufReader = new BufferedReader(new InputStreamReader(in));
			while(running) {
				// read in a line from the client
				line = bufReader.readLine();
				if (line == null)
					break;
				// and write out the reversed line
				System.out.println("[server/" + line.length() + "]" + line);
				if (line.length() == 0)
					break;
			}
			// Write a html response back
			StringBuffer buf = new StringBuffer();
			buf.append("HTTP/1.1 200 Ok\r\n");
			buf.append("Server: Apache-Test\r\n");
			buf.append("Connection: close\r\n");
			buf.append("Content-Type: text/html\r\n");
			buf.append("\r\n");

			buf.append("<html>");
			buf.append("<body>");
			buf.append("" + new Date() + " / " + this.client + "\n<br />\n\n");
            buf.append(reportOutput_UseString());
			buf.append("</body>");
			buf.append("</html>");
			out.println(buf);


            this.printMemoryStats("incoming request");
		} catch (IOException e) {
			e.printStackTrace();
		} finally {
			try {
				if (out != null) out.close();
				if (in != null) in.close();
				client.close();
			} catch (IOException e2) {;}
			System.out.println("[server] closing connection");
		} // End of Try - Catch - Finally

	}
    
    public void printMemoryStats(final String header) {
		
		System.out.println("************************");
		System.out.println("* Memory Stats");
		System.out.println("## " + header);
		System.out.println("************************");

		final double mb_size = 1024.0 * 1024.0;

		final long freeMemory = runtime.freeMemory();
		final long maxMemory = runtime.maxMemory();
		final long totalMemory = runtime.totalMemory();

		final double freeMemory_mb= (double)freeMemory / mb_size;
		final double maxMemory_mb = (double)maxMemory / mb_size;
		final double totalMemory_mb = (double)totalMemory / mb_size;
		
		System.out.println(" Free Memory = " + freeMemory);
		System.out.println(" Max Memory = " + maxMemory);
		System.out.println(" Total Memory = " + totalMemory);

		System.out.println(" Free Memory(mb) = " + freeMemory_mb);
		System.out.println(" Max Memory(mb) = " + maxMemory_mb);
		System.out.println(" Total Memory(mb) = " + totalMemory_mb);
	}

}
// End of File