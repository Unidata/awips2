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
package org.apache.qpid.management.wsdm.muse.serializer;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import javax.xml.XMLConstants;
import javax.xml.namespace.QName;

import org.apache.muse.core.serializer.Serializer;
import org.apache.muse.core.serializer.SerializerRegistry;
import org.apache.muse.util.xml.XmlUtils;
import org.apache.muse.ws.addressing.soap.SoapFault;
import org.apache.qpid.management.Names;
import org.w3c.dom.Element;

/**
 * Implementation of Muse Serializer for Map type.
 *  
 *  <amspam>
 *  	<entry>
 *  		<key>
 *  		<value>
 *  	</entry>
 *  </amsasm>
 *  
 * @author Andrea Gazzarini
 */
public class MapSerializer implements Serializer 
{
	
	ByteArraySerializer _byteArraySerializer = new ByteArraySerializer();
	Serializer _objectSerializer = SerializerRegistry.getInstance().getSerializer(Object.class);
	Serializer _stringSerializer = SerializerRegistry.getInstance().getSerializer(String.class);
	
	/**
	 * Return a map representation of the given xml element.
	 * 
	 * @param xml the element to unmarshal.
	 * @throws SoapFault when the unmarshalling fails.
	 */	
	public Object fromXML(Element xml) throws SoapFault 
	{
		Map<Object,Object> result = new HashMap<Object,Object>();

		if (xml != null)
		{
			Element[] children = XmlUtils.getAllElements(xml);
			Serializer objectDeserializer = SerializerRegistry.getInstance().getSerializer(Object.class);
	
			for (Element entry : children) 
			{
				Element[] keysAndValues = XmlUtils.getAllElements(entry);
				Object key = null;
				Object value = null;
				for (Element element : keysAndValues) 
				{
					if (Names.KEY.equals(element.getLocalName()))
					{
						key = _stringSerializer.fromXML(element);
					} else if (Names.VALUE.equals(element.getLocalName()))
					{
						value = objectDeserializer.fromXML(element);
					}
				}
				result.put(key, value);
			}
		}
		return result;
	}

	/**
	 * Returns the java type associated to this class.
	 * 
	 * @return the java type associated to this class.
	 */
	public Class<?> getSerializableType() 
	{
		return Map.class;
	}

	/**
	 * Return an xml representation of the given Map with the given name.
	 * 
	 * @param object the Map to marshal.
	 * @param qname the qualified (xml) name of the object to use in xml representation.
	 * @return the xml representation of the given Map.
	 * @throws SoapFault when the marshalling fails.
	 */
	public Element toXML(Object obj, QName qname) throws SoapFault 
	{
		
		Map<?, ?> data = (Map<?, ?>) obj;

		QName entryQName = new QName(qname.getNamespaceURI(),Names.ENTRY,qname.getPrefix());
		QName keyQName = new QName(qname.getNamespaceURI(),Names.KEY,qname.getPrefix());
		QName valueQName = new QName(qname.getNamespaceURI(),Names.VALUE,qname.getPrefix());
		
		Element root = XmlUtils.createElement(qname);
		root.setAttribute("xmlns:xsi", XMLConstants.W3C_XML_SCHEMA_INSTANCE_NS_URI);
		for (Entry<?, ?> mapEntry: data.entrySet()) 
		{
			Element entry = XmlUtils.createElement(entryQName);									
			entry.appendChild(_stringSerializer.toXML(mapEntry.getKey(), keyQName));
			if (mapEntry.getValue().getClass() == byte[].class) {				
				entry.appendChild(_byteArraySerializer.toXML(mapEntry.getValue(), valueQName));
			} else {
				entry.appendChild(_objectSerializer.toXML(mapEntry.getValue(), valueQName));
			}
			root.appendChild(entry);
		}
		return root;
	}
}