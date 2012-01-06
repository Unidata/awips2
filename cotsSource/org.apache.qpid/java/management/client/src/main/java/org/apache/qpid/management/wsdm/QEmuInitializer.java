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

import java.lang.management.ManagementFactory;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.qpid.management.Messages;
import org.apache.qpid.management.Names;
import org.apache.qpid.transport.util.Logger;

/**
 * QPid Emulator Initializer.
 * This component is basically responsible to create and initialize 
 * an emulator module used for simulate object instances creation.
 * 
 * @author Andrea Gazzarini
 */
public class QEmuInitializer extends HttpServlet 
{
	private static final long serialVersionUID = 6149614872902682208L;
	private final static Logger LOGGER = Logger.get(QEmuInitializer.class);
	
	/**
	 * QEmu initialization method.
	 * 
	 * @throws ServletException when the module cannot be initialized.
	 */
	public void init() throws ServletException 
	{
		try 
		{
    		ManagementFactory.getPlatformMBeanServer().registerMBean(
    				new QEmu(), 
    				Names.QPID_EMULATOR_OBJECT_NAME);
		} catch(Exception exception)
		{
			LOGGER.warn(
					exception,
					Messages.QMAN_300005_QEMU_INITIALIZATION_FAILURE);
			throw new ServletException(exception);
		}
	}
	
	/**
	 * This is a startup module only so an override of the 
	 * default servlet behaviour must be done in order to 
	 * prevent incoming http requests processing.
	 * 
	 * @param request the http request.
	 * @param response the http response.
	 * @throws ServletException each time this method is called.
	 */
	@Override
	public void service(HttpServletRequest request,HttpServletResponse response) throws ServletException
	{		
		throw new ServletException();
	}

	/**
	 * Unregister QPid emulator.
	 */
	public void destroy() 
	{
		try 
		{
			ManagementFactory.getPlatformMBeanServer().unregisterMBean(
					Names.QPID_EMULATOR_OBJECT_NAME);
		} catch (Exception exception) 
		{
		}
	}
}