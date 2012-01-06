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

import java.io.File;
import java.io.IOException;
import java.lang.management.ManagementFactory;
import java.lang.management.OperatingSystemMXBean;
import java.lang.management.RuntimeMXBean;
import java.util.Date;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.qpid.management.Messages;
import org.apache.qpid.management.Names;
import org.apache.qpid.transport.util.Logger;

/**
 * This action is the controller responsible to prepare data for the home 
 * page (System Overview) of QMan admin console.
 * 
 * @author Andrea Gazzarini
 */
public class ConsoleAction extends HttpServlet
{
	private static final long serialVersionUID = -2411413147821629363L;
	
	private static final Logger LOGGER = Logger.get(ConsoleAction.class);
	
	private Date _startDate;
	
	/**
	 * Initializes this controller.
	 * Simply it computes the start date of the application.
	 */
	@Override
	public void init()
	{
		_startDate = new Date();
	}
	 
	/**
	 *  Prepares data for System Overview admin console page and forward that data to that page.
	 *  
	 * @throws ServletException when this controller is not able to forward to the appropriate view page.
	 * @throws IOException when this controller is not able to forward to the appropriate view page.
	 */
	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException
	{
		ConsoleModel model = new ConsoleModel();
		model.setVersion("1.0");
		model.setVersionName("Sofia");
		model.setStartDate(_startDate);
		model.setHost(System.getProperty(Names.ADAPTER_HOST_PROPERTY_NAME, "localhost"));
		model.setPort(Integer.parseInt(System.getProperty(Names.ADAPTER_PORT_PROPERTY_NAME, "8080")));

		try 
		{
			OperatingSystemMXBean operatingSystem = ManagementFactory.getOperatingSystemMXBean();
			model.setOsName(operatingSystem.getName());
			model.setProcessors(operatingSystem.getAvailableProcessors());
			model.setOsVersion(operatingSystem.getVersion());
			model.setArchName(operatingSystem.getArch());
		} catch(Exception exception)
		{
			LOGGER.warn(exception,Messages.QMAN_300006_OS_MBEAN_FAILURE);
			model.setOsName("N.A.");
			model.setProcessors(null);
			model.setOsVersion("N.A.");
			model.setArchName("N.A.");			
		}
		
		try 
		{
			RuntimeMXBean runtime = ManagementFactory.getRuntimeMXBean();
			
			String bootClasspath = runtime.getBootClassPath();
			model.setBootClasspath(bootClasspath.split(File.pathSeparator));
			
			String classpath = runtime.getClassPath();
			model.setClasspath(classpath.split(File.pathSeparator));
			
			model.setInputArguments(runtime.getInputArguments().toArray(new String[]{}));
		} catch(Exception exception)
		{
			LOGGER.warn(exception,Messages.QMAN_300007_RUNTIME_MBEAN_FAILURE);
		}
		
		request.setAttribute("model", model);
		
		RequestDispatcher dispatcher = request.getRequestDispatcher("/console.jsp");
		dispatcher.forward(request,response);
	}
}
