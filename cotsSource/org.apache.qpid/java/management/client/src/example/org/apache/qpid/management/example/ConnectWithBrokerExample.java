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

import javax.xml.namespace.QName;

import org.apache.muse.core.proxy.ProxyHandler;
import org.apache.muse.core.proxy.ReflectionProxyHandler;
import org.apache.muse.ws.addressing.EndpointReference;
import org.apache.muse.ws.resource.remote.WsResourceClient;

/**
 * This example shows how to connect QMan with a broker using the adapter interface.
 * As you can see the interface is very simple and requests you the following parameters :
 * 
 * <ul>
 * 	<li>hostname : the host (hostname or ip) where the broker is running;</li>
 * 	<li>port : the port number of the running broker;</li>
 * 	<li>virtual host  : the name of the virtual host;</li>
 * 	<li>username : the username that will be used for estabilshing connection.</li>
 * 	<li>password : the password that will be used for estabilshing connection.</li>
 * 	<li>initial pool capacity : the initial size of broker connection pool. </li>
 * 	<li>maximum pool capacity :  the max allowed size of broker connection pool.</li>
 * 	<li>maximum wait timeout  :  the max wait timeout for retrieving connections.</li>
 * </ul>
 * 
 * @author Andrea Gazzarini
 */
public class ConnectWithBrokerExample extends AbstractQManExample
{
	
	/**
	 * Executes the connect example.
	 * Although not specified explicitly, the input array MUST contains in the following order :
	 * 
	 * <ul>
	 * 	<li>host : the hostname where QMan is running;</li>
	 * 	<li>port : the port number where QMan is running;</li>
	 * 	<li>qpid host : the host  name where QPid is running;</li>
	 * 	<li>qpid port : the port number where Qpid is running;</li>
	 * 	<li>virtual host : the virtual host name;</li>
	 * 	<li>username : the username that will be used for estabilshing connection.</li>
	 * 	<li>password : the password that will be used for estabilshing connection.</li>
	 * 	<li>initial pool capacity : the initial size of broker connection pool. </li>
	 * 	<li>maximum pool capacity :  the max allowed size of broker connection pool.</li>
	 * 	<li>maximum wait timeout  :  the max wait timeout for retrieving connections.</li>
	 * </ul>
	 * 
	 * Note that this example differs from the others (and therefore is overriding the execute() method) because 
	 * in this case we are not using the "standard" WSRF interface but instead an additional custom 
	 * "operation" on a WS-Resource.
	 * 
	 * @param arguments the commadn line arguments.
	 */
	void execute(String [] arguments)
	{
		if (arguments.length != 10){
			printUsageAndExit("invalid command line was given.");
		}

		try 
		{
			// 1) Parses command line arguments...
			String host = arguments[0];
			int port = Integer.parseInt(arguments[1]);
			String qpidHost = arguments[2];
			int qpidPort = Integer.parseInt(arguments[3]);
			String virtualHost = arguments[4];
			String username = arguments[5];
			String password = arguments[6];
			int initPoolCapacity = Integer.parseInt(arguments[7]);
			int maxPoolCapacity = Integer.parseInt(arguments[8]);
			long maxWaitTimeout = Long.parseLong(arguments[9]);
			
			printOutExampleDescription();
			
			waitForUserInput("Type enter to proceed...");
			
			executeExample(
					host, 
					port,
					qpidHost,
					qpidPort,
					virtualHost,
					username,
					password,
					initPoolCapacity,
					maxPoolCapacity,
					maxWaitTimeout);
			
		} catch(NumberFormatException exception)
		{
			printUsageAndExit("Unable to run the example. Please ensure that all numeric values are correctly supplied.");
		} catch(Exception exception)
		{
			System.out.println("-----------------------EXAMPLE FAILURE-----------");
			System.out.println("Not well-defined exception was detected while");
			System.out.println("running the example.");
			exception.printStackTrace(System.out);
			System.out.println("--------------------------------------------------------");						
		}
	}			
	
	/** 
	 * Connects QMan with a broker.
	 * 
	 *@param host the hostname where QMan is running;
	 *@param port the port number where QMan is running;
	 *@param qpidHost the host  name where QPid is running;
	 *@param qpidPort the port number where Qpid is running;
	 *@param virtualHost the virtual host name;
	 *@param username the username that will be used for estabilshing connection.
	 *@param password the password that will be used for estabilshing connection.
	 *@param initPoolCapacity the initial size of broker connection pool. 
	 *@param maxPoolCapacity the max allowed size of broker connection pool.
	 *@param maxWaitTimeout the max wait timeout for retrieving connections.
	 *
	 * @throws Exception when the example fails (not at application level).
	 */
	void executeExample(String host, int port, String qpidHost, int qpidPort, String virtualHost, String username, String password, int initPoolCapacity, int maxPoolCapacity, long maxWaitTimeout) throws Exception
	{		
		// 1) Creates an endpoint reference of the adapter service...
		EndpointReference adapterEndpointReference = getAdapterEndpointReference(host, port);
		WsResourceClient adapterClient = new WsResourceClient(adapterEndpointReference);
		adapterClient.setTrace(true);
		
		// 2) Creates the Adapter service client...
		adapterClient.invoke(
				getProxyHandler(), 
				new Object[]{
					qpidHost,
					qpidPort,
					username,
					password,
					virtualHost,
					initPoolCapacity,
					maxPoolCapacity,
					maxWaitTimeout});
	}
	
	/**
	 * Prints out a description of this example.
	 */
	void printOutExampleDescription()
	{
		System.out.println("                 "+getClass().getSimpleName()+" ");
		System.out.println("-------------------------------------------------------------------");
		System.out.println();
		System.out.println("This example shows how to connect QMan with a broker using");
		System.out.println("the adapter interface.");
		System.out.println();
	}

	/**
	 * A proxy handler is a module needed in order to make a capability 
	 * service invocation.
	 * It contains logic to serialize and deserialize request, response, input and 
	 * output parameters during a web service invocation.
	 * 
	 * @return a proxy handler.
	 */
	private ProxyHandler getProxyHandler()
	{
        ProxyHandler handler = new ReflectionProxyHandler();
        handler.setAction("http://amqp.apache.org/qpid/management/qman/Connect");
        handler.setRequestName(new QName("http://amqp.apache.org/qpid/management/qman", "Connect", PREFIX));
        handler.setRequestParameterNames(new QName[]{
        		new QName("http://amqp.apache.org/qpid/management/qman", "host", PREFIX),
                new QName("http://amqp.apache.org/qpid/management/qman", "port", PREFIX),
                new QName("http://amqp.apache.org/qpid/management/qman", "username", PREFIX),
                new QName("http://amqp.apache.org/qpid/management/qman", "password", PREFIX),
                new QName("http://amqp.apache.org/qpid/management/qman", "virtualHost", PREFIX),
                new QName("http://amqp.apache.org/qpid/management/qman", "initialPoolCapacity", PREFIX),
                new QName("http://amqp.apache.org/qpid/management/qman", "maxPoolCapacity", PREFIX),
                new QName("http://amqp.apache.org/qpid/management/qman", "maxWaitTimeout", PREFIX)});
        handler.setResponseName(new QName("http://amqp.apache.org/qpid/management/qman", "ConnectResponse", PREFIX));
        handler.setReturnType(null);
        return handler;
	}
	
	public static void main(String[] arguments)
	{
		new ConnectWithBrokerExample().execute(arguments);
	}
	
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
			.append("3) qpid host : port number where Qpid is running.")
			.append(LINE_SEPARATOR)
			.append("4) qpid port : port number where Qpid is running.")
			.append(LINE_SEPARATOR)
			.append("5) virtual host : virtual host name.")
			.append(LINE_SEPARATOR)
			.append("6) username : port number where QMan is running.")
			.append(LINE_SEPARATOR)
			.append("7) password : port number where QMan is running.")
			.append(LINE_SEPARATOR)
			.append("8) initial pool capacity : port number where QMan is running.")
			.append(LINE_SEPARATOR)
			.append("9) max pool capacity : port number where QMan is running.")
			.append(LINE_SEPARATOR)
			.append("10) max wait timeout : port number where QMan is running.")
			.append(LINE_SEPARATOR)
			
			.append("------------------------------------------------------------");
		System.out.println(builder);
		System.exit(1);
	}	
}