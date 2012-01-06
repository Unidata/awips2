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
 * method invocation failure.
 * 
 * @author Andrea Gazzarini
 */
public class MethodInvocationFault extends QManFault 
{
	private static final long serialVersionUID = 5977379710882983474L;

	private String _message;
	private long _returnCode;
	
	/**
	 * Builds a new exception with the given endpoint reference and method invocation exception.
	 * This constructor will be used when the invocation thrown the MethodInvocationException.
	 * 
	 * @param endpointReference the endpoint reference.
	 * @param methodName the name of the method.
	 * @param message the explanatio message.
	 * @param returnCode the a mnemonic code associated with the failure.
	 */
	public MethodInvocationFault(
			EndpointReference endpointReference, 
			String methodName,
			String message,
			long returnCode) 
	{
		super(
				endpointReference,
				new QName(
						Names.NAMESPACE_URI,
						"OperationInvocationFault",
						Names.PREFIX),
				String.format("OPERATION \"%s\" FAILURE. See detail section for further details.",methodName));
		this._message = message;
		this._returnCode = returnCode;
	}
	
	@Override
	public Element getDetail()
	{
		Element detail = super.getDetail();
		Document owner = detail.getOwnerDocument();
		detail.appendChild(XmlUtils.createElement(owner, Names.QMAN_STATUS_TEXT_NAME,_message));
		detail.appendChild(XmlUtils.createElement(owner, Names.QMAN_STATUS_CODE_NAME,_returnCode));
		return detail;
	}
}