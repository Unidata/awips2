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
package org.apache.qpid.management.web.action;

import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.qpid.management.Names;
import org.apache.qpid.management.configuration.BrokerAlreadyConnectedException;
import org.apache.qpid.management.configuration.BrokerConnectionData;
import org.apache.qpid.management.configuration.BrokerConnectionException;
import org.apache.qpid.management.domain.services.ManagementClient;
import org.apache.qpid.management.domain.services.QMan;

/**
 * This controller is responsible to :
 * 
 * <ul>
 * 	<li> prepare data for the page that is showing all connected brokers.</li>.
 * 	</li> connect QMan with a broker on demand.
 * </ul>
 * 
 * @author Andrea Gazzarini
 */
public class BrokersManagementAction extends HttpServlet
{
	private static final long serialVersionUID = -2411413147821629363L;
	 
	/**
	 * Retrieves all connected brokers (their connection data) and prepare the model that
	 * is then forwarded to the appropriate view page.
	 * 
	 * @param request the http request.
	 * @param response the http response.
	 * @throws ServletException in case of failure while forwarding to the view component.
	 * @throws IOException in case of failure while forwarding to the view component.
	 */
	@SuppressWarnings("unchecked")
	@Override
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException
	{
		try 
		{	
			QMan qman = (QMan)getServletContext().getAttribute(Names.APPLICATION_NAME);
			List<ManagementClient> managementClients = qman.getManagementClients();
			
			List<BrokerConnectionData> brokers = new ArrayList<BrokerConnectionData>(managementClients.size());
			
			if (!managementClients.isEmpty())
			{
				for (ManagementClient managementClient : managementClients)
				{
					brokers.add(managementClient.getBrokerConnectionData());
				}
				request.setAttribute("model", brokers);		
			}
			
			RequestDispatcher dispatcher = request.getRequestDispatcher("/brokers_management.jsp");
			dispatcher.forward(request,response);
		} catch(Exception exception)
		{
			request.setAttribute("errorMessage","Unable to detect the exact cause Please look at the reported stack trace below.");
			request.setAttribute("exception",exception);
			RequestDispatcher dispatcher = request.getRequestDispatcher("/error_page.jsp");
			dispatcher.forward(request,response);			
		}
	}
	
	/**
	 * Connects QMan with a new broker and forwards to 
	 * the brokers list view page.
	 * 
	 * @param request the http request.
	 * @param response the http response.
	 * @throws ServletException in case of failure while forwarding to the view component.
	 * @throws IOException in case of failure while forwarding to the view component.
	 */
	@SuppressWarnings("unchecked")
	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException
	{
		try 
		{	
			QMan qman = (QMan)getServletContext().getAttribute(Names.APPLICATION_NAME);
			
			String host = request.getParameter("host");
			String portString = request.getParameter("port");
			String virtualHost = request.getParameter("virtualHost");
			String username = request.getParameter("username");
			String password = request.getParameter("password");
			
			String initialCapacityString = request.getParameter("initialCapacity");
			String maxCapacityString = request.getParameter("maxCapacity");
			String maxWaitTimeoutString = request.getParameter("maxWaitTimeout");
			
			List<String> errors = new LinkedList<String>();
			int port = 0;
			int initialPoolCapacity = 0;
			int maxPoolCapacity = 0;
			long maxWaitTimeout = 0;
			
			if(host== null || host.trim().length()==0)
			{
				errors.add("Invalid value for \"host\" attribute. Must be not null.");				
			}

			if(virtualHost == null || virtualHost.trim().length()==0)
			{
				errors.add("Invalid value for \"virtualHost\" attribute. Must be not null.");				
			}
			
			try
			{
				port = Integer.parseInt(portString);
			} catch(Exception exception) 
			{
				errors.add("Invalid value for \"port\" attribute. Must be not null and must be a number.");
			}
			
			try
			{
				initialPoolCapacity = Integer.parseInt(initialCapacityString);
			} catch(Exception exception) 
			{
				errors.add("Invalid value for \"Initial Pool Capacity\" attribute. Must be not null and must be a number.");
			}
			
			try
			{
				maxPoolCapacity = Integer.parseInt(maxCapacityString);
			} catch(Exception exception) 
			{
				errors.add("Invalid value for \"Max Pool Capacity\" attribute. Must be not null and must be a number.");
			}

			try
			{
				maxWaitTimeout = Long.parseLong(maxWaitTimeoutString);
			} catch(Exception exception) 
			{
				errors.add("Invalid value for \"Max Wait Timeout\" attribute. Must be not null and must be a number.");
			}
			
			request.setAttribute("errors", errors);
			
			if (errors.isEmpty()) 
			{
				qman.addBroker(
						host, 
						port, 
						username, 
						password, 
						virtualHost, 
						initialPoolCapacity, 
						maxPoolCapacity, 
						maxWaitTimeout);
			}
			doGet(request, response);
		}catch(BrokerAlreadyConnectedException exception)
		{
			request.setAttribute("errorMessage","Supplied data refers to an already connected broker...");
			RequestDispatcher dispatcher = request.getRequestDispatcher("/brokers_management.jsp");
			dispatcher.forward(request,response);			
		}
		catch(BrokerConnectionException exception)
		{
			request.setAttribute("errorMessage","Unable to connect with the requested broker...");
			RequestDispatcher dispatcher = request.getRequestDispatcher("/brokers_management.jsp");
			dispatcher.forward(request,response);			
		} catch(Exception exception)
		{
			request.setAttribute("errorMessage","Unable to detect the exact cause Please look at the reported stack trace below.");
			request.setAttribute("exception",exception);
			RequestDispatcher dispatcher = request.getRequestDispatcher("/error_page.jsp");
			dispatcher.forward(request,response);			
		}
	}	
}