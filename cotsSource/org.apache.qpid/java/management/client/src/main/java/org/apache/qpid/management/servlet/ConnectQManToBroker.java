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
package org.apache.qpid.management.servlet;

import java.util.UUID;
import java.util.Map.Entry;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.muse.core.platform.mini.MiniServlet;
import org.apache.qpid.management.Messages;
import org.apache.qpid.management.Names;
import org.apache.qpid.management.configuration.BrokerConnectionData;
import org.apache.qpid.management.configuration.Configuration;
import org.apache.qpid.management.domain.services.QMan;
import org.apache.qpid.transport.util.Logger;

/**
 * When QMan is started and a configuration file is given 
 * (via system property) with initial broker connection data(s), 
 * this servlet simply sends connect command(s) to QMan in order
 * to estabilish the connection(s) to the requested broker(s).
 * 
 * @author Andrea Gazzarini
 */
public class ConnectQManToBroker extends MiniServlet 
{	
	private static final long serialVersionUID = 6149614872902682208L;
    private final static Logger LOGGER = Logger.get(ConnectQManToBroker.class);
			
	/**
	 * Send one or more initial "connect" command(s) to QMan in order 
	 * to estabilish a connection with broker found on the configuration file..
	 * Note that this is done only if that configuration file is given (via system 
	 * property) and it is valid. 
	 */
	public void init()
	{
        Configuration configuration = Configuration.getInstance();
        if (configuration.hasOneOrMoreBrokersDefined())
        {
        	QMan qman = (QMan)getServletContext().getAttribute(Names.APPLICATION_NAME);
        	
        	LOGGER.info(Messages.QMAN_000003_CREATING_MANAGEMENT_CLIENTS);
            for (Entry<UUID, BrokerConnectionData> entry : Configuration.getInstance().getConnectionInfos())
            {
            	qman.createManagementClient(entry.getKey(), entry.getValue());
            }
		} else 
		{
			LOGGER.info(Messages.QMAN_000022_NO_BROKER_CONFIGURED);
		}
	}
	
	/**
	 * This is a startup module only so an override of the default servlet 
	 * behaviour must be done in order to prevent incoming http 
	 * requests processing.
	 * 
	 * @param request the http request.
	 * @param response the http response.
	 * @throws ServletException each time this method is called.
	 */
	@Override
	public void service(HttpServletRequest request, HttpServletResponse response) throws ServletException
	{
		throw new ServletException();
	}
}