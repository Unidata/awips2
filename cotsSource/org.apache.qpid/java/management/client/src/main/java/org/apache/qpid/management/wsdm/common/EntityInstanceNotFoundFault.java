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

import javax.management.ObjectName;
import javax.xml.namespace.QName;

import org.apache.muse.ws.addressing.EndpointReference;
import org.apache.qpid.management.Names;

/**
 * Thrown when some operation has been requested on an entity and the entity hasn't been found 
 * on the managed domain.
 * 
 * @author Andrea Gazzarini
 */
public class EntityInstanceNotFoundFault extends QManFault 
{
	private static final long serialVersionUID = -3772811863214553615L;

	private final static QName EXCEPTION_QNAME = new QName(
			Names.NAMESPACE_URI,
			"EntityInstanceNotFoundFault",
			Names.PREFIX);
	
	/**
	 * Builds a new exception with the given endpoint reference and the object name of the entity
	 * that wasn't found.
	 * 
	 * @param endpointReference the origin endpoint reference of this exception.
	 * @param targetEntityName the object name of the not found entity.
	 */
	public EntityInstanceNotFoundFault(EndpointReference endpointReference, ObjectName targetEntityName) 
	{
		super(
				endpointReference,
				EXCEPTION_QNAME, 
				targetEntityName.getCanonicalName());
	}
}