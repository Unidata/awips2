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
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Map;
import java.util.Properties;
import java.util.Map.Entry;

import javax.management.MBeanAttributeInfo;
import javax.management.MBeanInfo;
import javax.management.MBeanServer;
import javax.management.MalformedObjectNameException;
import javax.management.ObjectName;
import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * This controller is responsible to provide a jmx perspective of a specific resource.
 * That means that this controller is querying the Platform MBean server in order to 
 * get metadata for the requested mbean.
 * 
 * After that metadata will be forwarded to the appropriate view page and therefore 
 * will be shown on the Admin console.
 * 
 * @author Andrea Gazzarini
 */
public class JmxPerspectiveAction extends HttpServlet
{
	private static final long serialVersionUID = -2411413147821629363L;

	/**
	 * Adapter interface for converting objects on html strings.
	 * 
	 * @author Andrea Gazzarini.
	 */
	interface JavaToHtmlAdapter 
	{
		/**
		 * Returns an HTML string representation of the given object.
		 * 
		 * @param javaObject the object that needs to be converted.
		 * @return an html string containing value of the given object.
		 */
		String toHtml(Object javaObject);
	}
	
	/**
	 * Adapter implementation for Map (and subclasses).
	 */
	private JavaToHtmlAdapter mapAdapter = new JavaToHtmlAdapter()
	{
		@SuppressWarnings("unchecked")
		public String toHtml(Object javaObject)
		{
			Map<String,Object> value = (Map<String, Object>) javaObject;

			// Sanity check : if the map is empty or null there's no need to 
			// do any convertion
			if (value == null || value.isEmpty())
			{
				return "(empty)";
			}

			StringBuilder builder = new StringBuilder("<ul>");
			for (Entry<String, Object> entry : value.entrySet())
			{
				builder
					.append("<li>")
					.append(entry.getKey())
					.append(" = ")
					.append(entry.getValue());
			}
			builder.append("</ul>");
			return builder.toString();
		}
	};

	private Map<String, JavaToHtmlAdapter> _adapters = new HashMap<String, JavaToHtmlAdapter>();
	
	@Override
	public void init() throws ServletException
	{
		_adapters.put(Map.class.getName(), mapAdapter);
		_adapters.put(HashMap.class.getName(),mapAdapter);
		_adapters.put(Properties.class.getName(),mapAdapter);
		_adapters.put(Hashtable.class.getName(),mapAdapter);
	}
	
	@SuppressWarnings("unchecked")
	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException
	{
		String resourceId = null;
		try 
		{
			resourceId = request.getParameter("resourceId");
			
			ObjectName objectName = new ObjectName(resourceId);
			String [] keyProperties = objectName.getKeyPropertyListString().split(",");
			
			MBeanServer server = ManagementFactory.getPlatformMBeanServer();
			
			MBeanInfo metadata = server.getMBeanInfo(objectName);
			
			Map<String, String> attributes = getAttributes(server, objectName,metadata.getAttributes());
			
			request.setAttribute("resourceId", objectName);
			request.setAttribute("metadata",metadata);
			request.setAttribute("nameAttributes",keyProperties);
			request.setAttribute("attributes",attributes);
			
			RequestDispatcher dispatcher = request.getRequestDispatcher("/jmx_perspective.jsp");
			dispatcher.forward(request,response);
		} catch(MalformedObjectNameException exception)
		{
			request.setAttribute("errorMessage","Malformed Resource ID : supplied value is "+resourceId);
			request.setAttribute("exception",exception);
			RequestDispatcher dispatcher = request.getRequestDispatcher("/error_page.jsp");
			dispatcher.forward(request,response);			
			
		}
		catch(Exception exception)
		{
			request.setAttribute("errorMessage","Unable to detect the exact cause Please look at the reported stack trace below.");
			request.setAttribute("exception",exception);
			RequestDispatcher dispatcher = request.getRequestDispatcher("/error_page.jsp");
			dispatcher.forward(request,response);			
		}
	}

	/**
	 * Starting from an mbean metadata, this method retrieves all the attributes 
	 * from the corresponding MBean Server.
	 * 
	 * @param server the mbean server where the target mbean is registered.
	 * @param name the name of the target mbean.
	 * @param metadata the metadata of mbean.
	 * @return a map containing all attributes of the given mbean.
	 * @throws Exception when it's not possible to retrieve attributes.
	 */
	private Map<String, String> getAttributes(MBeanServer server, ObjectName name, MBeanAttributeInfo [] metadata) throws Exception
	{
		Map<String,String> result = new HashMap<String, String>(metadata.length);
		for (MBeanAttributeInfo attribute : metadata)
		{
			Object value = server.getAttribute(name, attribute.getName());
			result.put(attribute.getName(),getAdaptedValue(attribute.getType(), value));
		}
		return result;
	}
	
	/**
	 * Converts the given attribute value in a html string format.
	 *  
	 * @param type the java type of the given attribute value.
	 * @param value the attribute value.
	 * @return a html string format of the given value.
	 */
	private String getAdaptedValue(String type, Object value) 
	{
		JavaToHtmlAdapter adapter = _adapters.get(type);
		return (adapter != null) ? adapter.toHtml(value) : String.valueOf(value);
	}
}