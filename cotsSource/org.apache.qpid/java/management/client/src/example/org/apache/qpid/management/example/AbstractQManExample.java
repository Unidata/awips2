/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *
 */
package org.apache.qpid.management.example;

import java.io.IOException;
import java.net.URI;

import org.apache.muse.ws.addressing.EndpointReference;

/**
 * Common interface for all QMan related examples.
 * 
 * @author Andrea Gazzarini
 */
public abstract class AbstractQManExample
{
	final static String LINE_SEPARATOR = System.getProperty("line.separator","\n");
	protected final static String PREFIX = "qman";

	/**
	 * Prints out the expected command line of this sample and after that exits.
	 */
	static void printUsageAndExit(String reason)
	{
		StringBuilder builder = new StringBuilder();
		builder.append("WARNING! Unable to run this sample : ")
			.append(reason)
			.append(LINE_SEPARATOR)
			.append("-------------------------------------------------------------")
			.append(LINE_SEPARATOR)			
			.append("Expected command line args for this sample are :")
			.append(LINE_SEPARATOR)
			.append(LINE_SEPARATOR)
			.append("1) host : ip or host name where QMan is running.")
			.append(LINE_SEPARATOR)
			.append("2) port : port number where QMan is running.")
			.append(LINE_SEPARATOR)
			.append("------------------------------------------------------------");
		System.out.println(builder);
		System.exit(1);
	}

	/**
	 * Prints out a description of this example.
	 */
	abstract void printOutExampleDescription();

	/**
	 * Executes this example.
	 * Note that this is just a template method used to avoid code duplication 
	 * (printOutExampleDescription() line) so in order to see how the example 
	 * works you should have a look at the concrete implementation of 
	 * executeExample(String host, int port).
	 * 
	 * @param host the host where QMan is running.
	 * @param port the port where QMan is running.
	 */
	void execute(String [] arguments)
	{
		if (arguments.length != 2){
			printUsageAndExit("invalid command line was given.");
		}

		try 
		{
			// 1) Parses command line arguments...
			String host = arguments[0];
			int port = Integer.parseInt(arguments[1]);
			
			printOutExampleDescription();
			
			waitForUserInput("Type enter to proceed...");
			
			executeExample(host, port);
			
		} catch(NumberFormatException exception)
		{
			printUsageAndExit("port number must be a number.");
		} catch(Exception exception)
		{
			System.out.println("-----------------------EXAMPLE FAILURE-----------");
			System.out.println("Not well-defined exception was detected while");
			System.out.println("running the example.");
			exception.printStackTrace(System.out);
			System.out.println("--------------------------------------------------------");						
		}
	}		
	
	protected void waitForUserInput(String message) throws IOException {
		System.out.println(message);
		System.in.read();
	}
	
	/**
	 * Each concrete implementor must define here how the example works.
	 * So, on top of that, user who wants to see how to use a specific feature 
	 * should have a look at the concrete implementation of this method..
	 * 
	 * @param host the host where QMan is running.
	 * @param port the port where QMan is running.
	 * @throws Exception when the example fails (not at application level).
	 */
	void executeExample(String host, int port) throws Exception{};
	
	/**
	 * Returns the endpoint reference of the adapter service.
	 * 
	 * @param host ip or host name where the service is running.
	 * @param port the port number of the server where the service is running.
	 * @return the endpoint reference of the adapter service.
	 */
	EndpointReference getAdapterEndpointReference(String host, int port)
	{
		URI address = URI.create(
				"http://"+
				host+
				":"+
				port+
				"/qman/services/adapter");
		return new EndpointReference(address);		
	}	
}