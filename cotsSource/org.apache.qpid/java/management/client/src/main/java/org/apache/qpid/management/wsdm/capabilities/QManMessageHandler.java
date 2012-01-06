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
package org.apache.qpid.management.wsdm.capabilities;

import java.lang.reflect.Method;

import javax.xml.namespace.QName;

import org.apache.muse.core.routing.ReflectionMessageHandler;
import org.apache.muse.core.serializer.Serializer;
import org.apache.muse.core.serializer.SerializerRegistry;
import org.apache.muse.util.xml.XmlUtils;
import org.apache.muse.ws.addressing.soap.SoapFault;
import org.apache.qpid.management.wsdm.muse.serializer.ByteArraySerializer;
import org.w3c.dom.Element;

/**
 * JMXConnectionListener_example custom implementation of Muse message handler to properly deal with 
 * byte arrays.
 * 
 * @author Andrea Gazzarini
 */
public class QManMessageHandler extends ReflectionMessageHandler
{

	/**
	 * Builds a new message handler with the given arguments.
	 * 
	 * @param actionURI the action URI.
	 * @param requestQName the qname of the incoming request.
	 * @param returnValueName the qname of the result value.
	 */
	public QManMessageHandler(String actionURI, QName requestQName,
			QName returnValueName)
	{
		super(actionURI, requestQName, returnValueName);
	}

	/**
	 * Transforms the given xml element in the corresponding 
	 * object representation.
	 * 
	 * @throws SoapFaul when unmarshal operation fails.
	 */
	@SuppressWarnings("unchecked")
	public Object[] fromXML(Element xml) throws SoapFault
	{
		Method method = getMethod();

		if (xml == null )
		{
			return EMPTY_REQUEST;
		}

		Class[] parameters = method.getParameterTypes();
		Object[] objects = new Object[parameters.length];

		Element[] elements = XmlUtils.getAllElements(xml);

		if (parameters.length == 1 && elements.length == 0)
		{
			elements = new Element[]{ xml };
		}

		if (elements.length != parameters.length)
		{
			throw new SoapFault("IncorrectParams");
		}

		SerializerRegistry registry = SerializerRegistry.getInstance();

		for (int i = 0; i < elements.length; ++i)
		{
			Class clazz = parameters[i];
			if (clazz == byte[].class)
			{
				objects[i] = new ByteArraySerializer().fromXML(elements[i]);
			} else
			{
				Serializer ser = registry.getSerializer(parameters[i]);
				objects[i] = ser.fromXML(elements[i]);
			}
		}
		return objects;
	}
}