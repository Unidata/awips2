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
package org.apache.qpid.management.wsdm.common;

import javax.xml.namespace.QName;

import org.apache.muse.util.xml.XmlUtils;
import org.apache.muse.ws.addressing.EndpointReference;
import org.apache.qpid.management.Names;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * This is the exception encapsulating the fault that will be thrown in case of 
 * broker connection failure.
 * 
 * @author Andrea Gazzarini
 */
public class UnableToConnectWithBrokerFault extends QManFault 
{
	private static final long serialVersionUID = 5977379710882983474L;

	private String _host;
	private int _port;
	private String _username;
	private String _virtualHostName;
	
	/**
	 * Builds a new exception with the given endpoint reference and connection data.
	 * 
	 * @param host the requested qpid host.
	 * @param port the requested qpid port.
	 * @param username the username used for estabilishing connection.
	 * @param virtualHostName the name of the target virtual host..
	 */
	public UnableToConnectWithBrokerFault(
			EndpointReference endpointReference, 
			String host,
			int port,
			String username,
			String virtualHostName,
			String message) 
	{
		super(
				endpointReference,
				new QName(
						Names.NAMESPACE_URI,
						"UnableToConnectFault",
						Names.PREFIX),
				String.format("Unable to connect with the requested broker. Underlying exception message was %s",message));
		this._host = host;
		this._port = port;
		this._username = username;
		this._virtualHostName = virtualHostName;
	}
	
	@Override
	public Element getDetail()
	{
		Element detail = super.getDetail();
		Document owner = detail.getOwnerDocument();
		detail.appendChild(XmlUtils.createElement(owner, Names.HOST_QNAME,_host));
		detail.appendChild(XmlUtils.createElement(owner, Names.PORT_QNAME,_port));
		detail.appendChild(XmlUtils.createElement(owner, Names.USERNAME_QNAME,_username));
		detail.appendChild(XmlUtils.createElement(owner, Names.VIRTUAL_HOST_QNAME,_virtualHostName));
		return detail;
	}
}