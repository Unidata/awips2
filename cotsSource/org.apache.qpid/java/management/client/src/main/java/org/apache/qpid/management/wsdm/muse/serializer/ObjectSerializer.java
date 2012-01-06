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

import java.net.URI;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import javax.xml.XMLConstants;
import javax.xml.namespace.QName;

import org.apache.muse.core.serializer.Serializer;
import org.apache.muse.core.serializer.SerializerRegistry;
import org.apache.muse.ws.addressing.soap.SoapFault;
import org.apache.qpid.management.Names;
import org.w3c.dom.Attr;
import org.w3c.dom.Element;

/**
 * Generic Serializer for objects.
 * It is a general-purpose serializer used for encoding Object values.  
 */
public class ObjectSerializer implements Serializer 
{
	/**
	 * Mapping between xsd and java types.
	 */
	private Map<String, Class<?>> xml2Java = new HashMap<String, Class<?>>();
	{
		xml2Java.put("xsd:long", Long.class);
		xml2Java.put("xsd:boolean",Boolean.class);
		xml2Java.put("xsd:double",Double.class);
		xml2Java.put("xsd:float",Float.class);
		xml2Java.put("xsd:integer",Integer.class);
		xml2Java.put("xsd:int",Integer.class);
		xml2Java.put("xsd:short",Short.class);
		xml2Java.put("xsd:string",String.class);
		xml2Java.put("xsd:anyURI",URI.class);
		xml2Java.put("xsd:dateTime",Date.class);
		xml2Java.put("xsd:QName",QName.class);
		xml2Java.put("xsd:element",Element.class);
		xml2Java.put("xsd:base64Binary",byte[].class);
		xml2Java.put("qman:arrayOfLong",Long[].class);
		xml2Java.put("qman:arrayOfBoolean",Boolean[].class);
		xml2Java.put("qman:arrayOfDouble",Double[].class);
		xml2Java.put("qman:arrayOfFloat",Float[].class);
		xml2Java.put("qman:arrayOfInteger",Integer[].class);
		xml2Java.put("qman:arrayOfShort",Short[].class);
		xml2Java.put("qman:arrayOfString",String[].class);
		xml2Java.put("qman:arrayOfURI",URI[].class);
		xml2Java.put("qman:arrayOfDate",Date[].class);
		xml2Java.put("qman:uuid",UUID.class);
		xml2Java.put("qman:map",Map.class);		
		xml2Java.put("qman:map",HashMap.class);		
	}
	
	private Map<Class<?>, String> java2Xml = new HashMap<Class<?>, String>();
	{		
		java2Xml.put(UUID.class,"qman:uuid");
		java2Xml.put(Long.class,"xsd:long");
		java2Xml.put(long.class,"xsd:long");
		java2Xml.put(Boolean.class,"xsd:boolean");
		java2Xml.put(boolean.class,"xsd:boolean");
		java2Xml.put(Double.class,"xsd:double");
		java2Xml.put(double.class,"xsd:double");
		java2Xml.put(Float.class,"xsd:float");
		java2Xml.put(float.class,"xsd:float");
		java2Xml.put(Integer.class,"xsd:integer");
		java2Xml.put(int.class,"xsd:integer");
		java2Xml.put(Short.class,"xsd:short");
		java2Xml.put(short.class,"xsd:short");
		java2Xml.put(String.class,"xsd:string");
		java2Xml.put(URI.class,"xsd:anyURI");
		java2Xml.put(Date.class,"xsd:dateTime");
		java2Xml.put(QName.class,"xsd:QName");
		java2Xml.put(Element.class,"xsd:element");
		java2Xml.put(byte[].class,"xsd:base64Binary");
		java2Xml.put(Long[].class,"qman:arrayOfLong");	
		java2Xml.put(long[].class,"qman:arrayOfLong");
		java2Xml.put(Boolean[].class,"qman:arrayOfBoolean");
		java2Xml.put(boolean[].class,"qman:arrayOfBoolean");
		java2Xml.put(Double[].class,"qman:arrayOfDouble");
		java2Xml.put(double[].class,"qman:arrayOfDouble");
		java2Xml.put(Float[].class,"qman:arrayOfFloat");
		java2Xml.put(float[].class,"qman:arrayOfFloat");
		java2Xml.put(Integer[].class,"qman:arrayOfInteger");
		java2Xml.put(int[].class,"qman:arrayOfInteger");
		java2Xml.put(Short[].class,"qman:arrayOfShort");
		java2Xml.put(short[].class,"qman:arrayOfShort");
		java2Xml.put(String[].class,"qman:arrayOfString");
		java2Xml.put(URI[].class,"qman:arrayOfURI");
		java2Xml.put(Date[].class,"qman:arrayOfDate");
		java2Xml.put(Map.class,"qman:map");
		java2Xml.put(HashMap.class,"qman:map");
	}
	
	/**
	 * Converts the incoming element into the appropriate Java type.
	 * The method will fail if :
	 * 
	 * <br>1) The element has no xsi:type attribute;
	 * <br>2) The xsi:type attribute has no corresponding java type on this serializer mappings.
	 * 
	 * @param elementData the xml element containing data to be unmarshalled.l
	 * @return the java object as result of xml element unmarshalling.
	 * @throws SoapFault when the marshalling fails.
	 */
	public Object fromXML(Element elementData) throws SoapFault
	{
		Attr typeAttribute = elementData.getAttributeNodeNS(
				XMLConstants.W3C_XML_SCHEMA_INSTANCE_NS_URI, 
				Names.TYPE);
		
		if (typeAttribute == null)
		{
			throw new SoapFault(
				"No type attribute was found for the current element. " +
				"If you are using this serializer, in order to unmarshal the" +
				" opportune type the xsi:type must be specified.");
		}
		
		Class<?> clazz = xml2Java.get(typeAttribute.getValue());
		
		if (clazz == null)
		{
			throw new SoapFault(
					String.format(
							"No corresponding class was found on this serializer mappings for xsi:type %s.",
							typeAttribute));
		}
		
		if (clazz == byte[].class) {
			return new ByteArraySerializer().fromXML(elementData);
		}
		
		return SerializerRegistry.getInstance().getSerializer(clazz).fromXML(elementData);
	}

	/**
	 * As this serializer is supposed to deal with generic object types, this method returns Object.class.
	 * 
	 * @return Object.class
	 */
	public Class<?> getSerializableType() 
	{
		return Object.class;
	}

	/**
	 * Converts the given object (with the given qname) in XML format.
	 * This method fails if there's no corresponding xml type for the given runtime type of the input object.
	 * 
	 * @param obj the object to be marshalled.
	 * @param qname the qualified name that will be used in encoding.
	 */
	public Element toXML(Object obj, QName qname) throws SoapFault 
	{
		Class<?> clazz = obj.getClass();		

		Element result = null;
		
		if (clazz == byte[].class) {
			result = new ByteArraySerializer().toXML(obj,qname);
		} 
		else {
			result = SerializerRegistry.getInstance().getSerializer(clazz).toXML(obj,qname);
		}
		result.setAttribute(Names.XSI_TYPE, java2Xml.get(clazz));
		return result;
	}
	
	/**
	 * Returns the xml type associated with the given class. 
	 * 
	 * @param clazz the class.
	 * @return the xml type associated with the given class.
	 */
	public String getXmlType(Class<?> clazz) 
	{
		return java2Xml.get(clazz);
	}
}