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
import java.lang.management.ManagementFactory;
import java.util.List;
import java.util.Set;

import javax.management.MBeanServer;
import javax.management.MalformedObjectNameException;
import javax.management.ObjectName;
import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.qpid.management.Names;
import org.apache.qpid.management.domain.services.ManagementClient;
import org.apache.qpid.management.domain.services.QMan;

/**
 * This controller retrieves from QMan all the registered resources and organize
 * that data in a model that is then forwarded to the appropriate view page.
 * 
 * TODO : In the corresponding view page only one broker is displayed.
 * A query should be made on QMan mbean in order to retrieve all connected broker and therefore
 * a model for each of them should be created. 
 * In the corresponding weg page there should be a "tab" for each broker. Each tab should show only
 * the objects belonging to that broker.
 * 
 * @author Andrea Gazzarini
 */
public class ResourcesManagementAction extends HttpServlet
{
	private static final long serialVersionUID = -2411413147821629363L;
	 
	@SuppressWarnings("unchecked")
	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException
	{
		try 
		{	
			QMan qman = (QMan)getServletContext().getAttribute(Names.APPLICATION_NAME);
			List<ManagementClient> managementClient = qman.getManagementClients();
			
			if (!managementClient.isEmpty())
			{
				BrokerModel model = new BrokerModel();
				model.setId(managementClient.toString());
				
				MBeanServer mxServer = ManagementFactory.getPlatformMBeanServer();
				Set<ObjectName> objectNames = mxServer.queryNames(new ObjectName("Q-MAN:*"), null);
				for (ObjectName objectName : objectNames)
				{
					model.addObject(objectName);
				}
	
				request.setAttribute("model", model);		
			}
			
			RequestDispatcher dispatcher = request.getRequestDispatcher("/resources_management.jsp");
			dispatcher.forward(request,response);
		} catch(MalformedObjectNameException exception)
		{
			request.setAttribute("errorMessage","Unable to detect the exact cause Please look at the reported stack trace below.");
			request.setAttribute("exception",exception);
			RequestDispatcher dispatcher = request.getRequestDispatcher("/error_page.jsp");
			dispatcher.forward(request,response);			
		}
	}
}
