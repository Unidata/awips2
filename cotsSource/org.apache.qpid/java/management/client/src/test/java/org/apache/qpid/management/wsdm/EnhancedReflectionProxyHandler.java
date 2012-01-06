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

import javax.xml.namespace.QName;

import org.apache.muse.core.proxy.ReflectionProxyHandler;
import org.apache.muse.util.xml.XmlUtils;
import org.apache.muse.ws.addressing.soap.SoapFault;
import org.apache.qpid.management.wsdm.muse.serializer.ByteArraySerializer;
import org.w3c.dom.Element;

/**
 * Custom implementation of Muse ReflectionProxyHandler 
 * that uses a base64 serializer for byte arrays.
 * Note that this proxy handler is only needed for tests because it provides
 * client side Base64 serializer capability.
 * In a concrete scenario we don't mind what instrument the client is using in order to 
 * propertly serialize byte arrays.
 * 
 * @author Andrea Gazzarini
 */
public class EnhancedReflectionProxyHandler extends ReflectionProxyHandler
{
	@Override
	 protected Element serialize(Object obj, QName qname) throws SoapFault
	 {
	     if (obj == null)
	     {
	 		return XmlUtils.createElement(qname);
	     }
	     
	     if (obj.getClass() == byte[].class)
	     {
	    	 return new ByteArraySerializer().toXML(obj, qname);
	     } else 
	     {
	    	 return super.serialize(obj, qname);
	     }
	 }
	
	@SuppressWarnings("unchecked")
	@Override
	protected Object deserialize(Element xml, Class theClass) throws SoapFault
	{
	     if (theClass == byte[].class)
	     {
	    	 return new ByteArraySerializer().fromXML(xml);
	     } else 
	     {
	    	 return super.deserialize(xml, theClass);
	     }
	}
}