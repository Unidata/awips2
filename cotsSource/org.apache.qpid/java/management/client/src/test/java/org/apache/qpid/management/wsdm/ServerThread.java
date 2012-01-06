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
package org.apache.qpid.management.wsdm;

import java.io.File;

import org.apache.qpid.management.Names;
import org.mortbay.component.LifeCycle.Listener;
import org.mortbay.jetty.Connector;
import org.mortbay.jetty.Server;
import org.mortbay.jetty.nio.SelectChannelConnector;
import org.mortbay.jetty.webapp.WebAppContext;
import org.mortbay.start.Monitor;

/**
 * Web Server startup thread.
 * It is used on adapter test case in order to start the embedded 
 * web server as a separated thread.
 * 
 * @author Andrea Gazzarini
 */
class ServerThread extends Thread
{	
	private final Listener _lifecycleListener;
	private Server _server;

	private SelectChannelConnector _connector;
	
	/**
	 * Builds a new server thread with the given lifecycle listener.
	 * 
	 * @param listener the lifecycle listener.
	 */
	ServerThread(Listener listener)
	{
		this._lifecycleListener = listener;
	}
	
	/**
	 * Starts the server.
	 */
	@Override
	public void run()
	{
		try 
		{		
			Monitor.monitor();        
			_server = new Server();
    		_server.setStopAtShutdown(true);
    		
            _connector=new SelectChannelConnector();
            _connector.setPort(Integer.parseInt(System.getProperty(Names.ADAPTER_PORT_PROPERTY_NAME)));
            _connector.setHost(System.getProperty(Names.ADAPTER_HOST_PROPERTY_NAME));
            
            
            _server.setConnectors(new Connector[]{_connector});
            
            WebAppContext webapp = new WebAppContext();
            webapp.setContextPath("/qman");
            
            // Additional web application descriptor containing test components.
            webapp.setDefaultsDescriptor("/org/apache/qpid/management/wsdm/web.xml");

            String webApplicationPath = System.getProperty("qman.war");
            File rootFolderPath = (webApplicationPath != null) ? new File(webApplicationPath) : new File(".");
            
            webapp.setWar(rootFolderPath.toURI().toURL().toExternalForm());
            webapp.addLifeCycleListener(_lifecycleListener);
            _server.setHandler(webapp);
            _server.start();
            System.setProperty(Names.ADAPTER_PORT_PROPERTY_NAME,String.valueOf( _connector.getLocalPort()));            
            _server.join();
            
		} catch(Exception exception)
		{
			throw new RuntimeException(exception);
		}
	}
	
	/**
	 * Shutdown the server.
	 * 
	 * @throws Exception when a problem is encountered during shutdown.
	 */
	void shutdown() throws Exception
	{
		_server.stop();
	}
	
	/**
	 * Returns the port number where the server is running.
	 * 
	 * @return the port number where the server is running.
	 */
	int getPort()
	{
		return _connector.getLocalPort();
	}
	
}
