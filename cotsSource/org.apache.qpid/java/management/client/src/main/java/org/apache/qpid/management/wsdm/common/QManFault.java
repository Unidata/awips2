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

import org.apache.muse.ws.addressing.EndpointReference;
import org.apache.muse.ws.resource.basefaults.BaseFault;
import org.apache.qpid.management.Names;

/**
 * Base class for QMan thrown faults.
 * This should be thrown to denote a situation where a not-well cause could be determined.
 */
public class QManFault extends BaseFault 
{	
	private static final long serialVersionUID = 5977379710882983474L;
	private final static QName SERVICE = new QName(
			Names.NAMESPACE_URI,
			"QMan",
			Names.PREFIX);
	
	private final static QName EXCEPTION_QNAME = new QName(
			Names.NAMESPACE_URI,
			"QManFault",
			Names.PREFIX);
	
	/**
	 * Builds a new exception with the given endpoint reference and the exception cause.
	 * 
	 * @param endpointReference the endpoint reference.
	 * @param cause the exception cause.
	 */
	public QManFault(EndpointReference endpointReference, Exception cause) 
	{
		super(EXCEPTION_QNAME,cause.getMessage());
		setCode(SERVICE);
		setOriginReference(endpointReference);
	}
	
	/**
	 * Builds a new exception with the given endpoint reference, qname and detail message.
	 * 
	 * @param endpointReference the endpoint reference.
	 * @param qname the qname of this exception.
	 * @param message the detail message of this exception.
	 */
	public QManFault(EndpointReference endpointReference, QName qname, String message) 
	{
		super(qname,message);
		setOriginReference(endpointReference);
		setCode(SERVICE);		
	}
}