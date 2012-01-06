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

import org.apache.muse.core.routing.ResourceIdFactory;
import org.apache.qpid.management.Names;

/**
 * ResourceIdFactory implementation that is using an objectName as 
 * resource identifier.
 * This is done in order to make a relationship between an MBean (which is part of the
 * JMX core domain model) and a WS-Resource (the same entity as is represented on WS-DM adapter side).
 * 
 * @author Andrea Gazzarini
 */
public class ObjectNameIdFactory implements ResourceIdFactory 
{
	/**
	 * Returns the name of the identifier element.
	 * 
	 *  @return the name of the identifier element.
	 */
	public QName getIdentifierName() 
	{
		return Names.RESOURCE_ID_QNAME;
    }

	/**
	 * Returns the object name used as a resource identifier.
	 * Developer note : this factory is highly coupled with ThreadSessionManager stuff because 
	 * the object name that will be used as identifier is supposed to be in the thread session.
	 * 
	 * @return the object name used as a resource identifier.
	 */
	public String getNextIdentifier() 
	{
		ObjectName objectName = ThreadSessionManager.getInstance().getSession().getObjectName();
		return objectName.getKeyProperty(Names.OBJECT_ID);
	}
}