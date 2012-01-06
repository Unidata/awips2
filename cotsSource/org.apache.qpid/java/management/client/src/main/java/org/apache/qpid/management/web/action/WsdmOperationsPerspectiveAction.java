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
import java.net.URI;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Map;
import java.util.Properties;
import java.util.Map.Entry;

import javax.management.MBeanAttributeInfo;
import javax.management.MBeanInfo;
import javax.management.MBeanServer;
import javax.management.ObjectName;
import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.xml.namespace.QName;

import org.apache.muse.core.proxy.ProxyHandler;
import org.apache.muse.core.proxy.ReflectionProxyHandler;
import org.apache.muse.ws.addressing.EndpointReference;
import org.apache.qpid.management.Names;
import org.w3c.dom.Element;

/**
 * This controller is responsible to retirve operations metadata from a WS-Resource.
 * That metadat will be forwarded and used by the corresponding view page. 
 * 
 * TODO : This is not really showing WS metadata. Insted JMX metadata is used here.
 * 
 * @author Andrea Gazzarini
 *
 */
public class WsdmOperationsPerspectiveAction extends HttpServlet
{
	private static final long serialVersionUID = -2411413147821629363L;
	
	private ProxyHandler proxyHandler;
	
	interface JavaToHtmlAdapter 
	{
		String toHtml(Object javaObject);
	}
	
	private URI resourceUri;
	
	private JavaToHtmlAdapter mapAdapter = new JavaToHtmlAdapter()
	{
		@SuppressWarnings("unchecked")
		public String toHtml(Object javaObject)
		{
			Map<String,Object> value = (Map<String, Object>) javaObject;

			if (value == null || value.isEmpty())
			{
				return "(empty)";
			}
			
			StringBuilder builder = new StringBuilder();
			builder.append("<ul>");
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
	
	private Map<String, JavaToHtmlAdapter> adapters = new HashMap<String, JavaToHtmlAdapter>();
	
	@Override
	public void init() throws ServletException
	{
		adapters.put(Map.class.getName(), mapAdapter);
		adapters.put(HashMap.class.getName(),mapAdapter);
		adapters.put(Properties.class.getName(),mapAdapter);
		adapters.put(Hashtable.class.getName(),mapAdapter);
		
		proxyHandler  = new ReflectionProxyHandler();
		proxyHandler.setAction("http://schemas.xmlsoap.org/ws/2004/09/mex/GetMetadata");
		proxyHandler.setRequestName(new QName("http://schemas.xmlsoap.org/ws/2004/09/mex", "GetMetadata", Names.PREFIX));
		proxyHandler.setRequestParameterNames(new QName[]{new QName("http://schemas.xmlsoap.org/ws/2004/09/mex", "Dialect", Names.PREFIX)});
		proxyHandler.setResponseName(new QName("http://schemas.xmlsoap.org/ws/2004/09/mex", "Metadata", Names.PREFIX));
		proxyHandler.setReturnType(Element[].class);
	}
	
	@SuppressWarnings("unchecked")
	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException
	{
		try 
		{
			String resourceId = request.getParameter("resourceId");
			ObjectName objectName = new ObjectName(resourceId);
			String wsdmResourceId = objectName.getKeyProperty(Names.OBJECT_ID);
			
			EndpointReference resourceEndpointReference = new EndpointReference(getURI(request));
			resourceEndpointReference.addParameter(
					Names.RESOURCE_ID_QNAME, 
					wsdmResourceId);
			
//			WsResourceClient resourceClient = new WsResourceClient(resourceEndpointReference);
//			Element wsdl = ((Element[])resourceClient.invoke(proxyHandler,WSDL_DIALECT))[0];
//			Element rmd = ((Element[])resourceClient.invoke(proxyHandler,RMD_DIALECT))[0];
						
			String [] keyProperties = objectName.getKeyPropertyListString().split(",");
			MBeanServer server = ManagementFactory.getPlatformMBeanServer();
			
			MBeanInfo metadata = server.getMBeanInfo(objectName);
			
			Map<String, String> attributes = getAttributes(server, objectName,metadata.getAttributes());
			
			request.setAttribute("resourceId", resourceId);
			request.setAttribute("metadata",metadata);
			request.setAttribute("nameAttributes",keyProperties);
			request.setAttribute("attributes",attributes);
			
			RequestDispatcher dispatcher = request.getRequestDispatcher("/wsdm_operations_perspective.jsp");
			dispatcher.forward(request,response);
		} catch(Exception exception)
		{
			request.setAttribute("errorMessage","Unable to detect the exact cause Please look at the reported stack trace below.");
			request.setAttribute("exception",exception);
			RequestDispatcher dispatcher = request.getRequestDispatcher("/error_page.jsp");
			dispatcher.forward(request,response);			
		}
	}

	private URI getURI(HttpServletRequest request)
	{
		if (resourceUri == null)
		{
			StringBuilder builder = new StringBuilder();
			builder
				.append(request.getProtocol())
				.append("//")
				.append(request.getServerName())
				.append(":")
				.append(request.getServerPort())
				.append("/qman/services/QManWsResource");
			resourceUri = URI.create(builder.toString());
		}
		return resourceUri;
	}

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
	
	private String getAdaptedValue(String type, Object value) 
	{
		JavaToHtmlAdapter adapter = adapters.get(type);
		return (adapter != null) ? adapter.toHtml(value) : String.valueOf(value);
	}
}