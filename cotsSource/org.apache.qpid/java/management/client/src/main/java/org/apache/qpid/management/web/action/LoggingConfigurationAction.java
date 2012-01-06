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

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.qpid.qman.debug.WsdlDebugger;
import org.apache.qpid.qman.debug.XmlDebugger;

/**
 * Logging configuration controller.
 * Accepts input parameters from admin console and configure the underlying
 * logging subsystem at runtime.
 * 
 * @author Andrea Gazzarini
 */
public class LoggingConfigurationAction extends HttpServlet
{
	private static final long serialVersionUID = 633352305870632824L;

	private final static String WSDL_DEBUG_ENABLED_PARAM = "wsdlDebugEnabled";
	private final static String SOAP_DEBUG_ENABLED_PARAM = "soapDebugEnabled";
	private final static String WEB_SERVER_LOG_LEVEL_PARAM = "webServerLogLevel";
	private final static String QMAN_LOG_LEVEL_PARAM = "qmanLogLevel";
	
	private final static String WEB_SERVER_PACKAGE = "org.mortbay";
	private final static String QMAN_PACKAGE = "org.qpid.apache.management";
	
	/**
	 * Retrieves current logging configuration and forward those data to the logging configuration view page.
	 * In this way that page will be able to display the current logging settings.
	 * 
	 * @param request the http request.
	 * @param response the http response.
	 * @throws ServletException when this controller is not able to forward to the appropriate view page.
	 * @throws IOException when this controller is not able to forward to the appropriate view page.
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException
	{
		Level messageDebuggerLogLevel = Logger.getLogger(XmlDebugger.class).getEffectiveLevel();
		Level wsdlDebuggerLogLevel = Logger.getLogger(WsdlDebugger.class).getEffectiveLevel();		
		Level webServerLogLevel = Logger.getLogger(WEB_SERVER_PACKAGE).getEffectiveLevel();		
		Level qmanLogLevel = Logger.getLogger(QMAN_PACKAGE).getEffectiveLevel();		
		
		request.setAttribute(WSDL_DEBUG_ENABLED_PARAM,wsdlDebuggerLogLevel.equals(Level.DEBUG));
		request.setAttribute(SOAP_DEBUG_ENABLED_PARAM,messageDebuggerLogLevel.equals(Level.DEBUG));
		request.setAttribute(WEB_SERVER_LOG_LEVEL_PARAM,webServerLogLevel);
		request.setAttribute(QMAN_LOG_LEVEL_PARAM,qmanLogLevel);		
		
		RequestDispatcher dispatcher = request.getRequestDispatcher("/logging_configuration.jsp");
		dispatcher.forward(request, response);
	}
	
	/**
	 * Accepts user data coming from admin console and use it for configure the underlying logging 
	 * subsystem.
	 * 
	 * @param request the http request.
	 * @param response the http response.
	 * @throws ServletException when this controller is not able to forward to the appropriate view page.
	 * @throws IOException when this controller is not able to forward to the appropriate view page.
	 */
	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException
	{
		String wsdlDebugEnabled = request.getParameter(WSDL_DEBUG_ENABLED_PARAM);
		String soapDebugEnabled = request.getParameter(SOAP_DEBUG_ENABLED_PARAM);
		
		String qmanLevel = request.getParameter(QMAN_LOG_LEVEL_PARAM);
		String serverLevel = request.getParameter(WEB_SERVER_LOG_LEVEL_PARAM);
		
		Logger.getLogger(WEB_SERVER_PACKAGE).setLevel(Level.toLevel(serverLevel));
		Logger.getLogger(QMAN_PACKAGE).setLevel(Level.toLevel(qmanLevel));
		
		Logger.getLogger(WsdlDebugger.class).setLevel(
				"on".equals(wsdlDebugEnabled)
					? Level.DEBUG
					: Level.INFO);

		Logger.getLogger(XmlDebugger.class).setLevel(
				"on".equals(soapDebugEnabled)
					? Level.DEBUG
					: Level.INFO);
		
		doGet(request, response);
	}
}
	