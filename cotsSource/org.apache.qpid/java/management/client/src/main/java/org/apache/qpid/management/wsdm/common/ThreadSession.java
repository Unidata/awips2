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

import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * Thread-scoped session.
 * 
 * @author Andrea Gazzarini
 */
public class ThreadSession 
{	
	private ObjectName _objectName;
	private Document _wsdl;
	private Element [] _wsrmdProperties;
	
	/**
	 * Empty constructor.
	 */
	ThreadSession() 
	{
	}

	/**
	 * Gets the object name associated with this thread session.
	 * 
	 * @return the object name associated with this thread session.
	 */
	public ObjectName getObjectName() 
	{
		return _objectName;
	}

	/**
	 * Sets the object name on this thread session.
	 * 
	 * @param the object name of this thread session..
	 */
	public void setObjectName(ObjectName objectName) 
	{
		this._objectName = objectName;
	}

	/**
	 * Sets the WSDL document on this thread session.
	 * 
	 * @param the WSDL document of this thread session..
	 */
	public void setWsdlDocument(Document wsdlDoc) 
	{
		this._wsdl = wsdlDoc;
	}
	
	/**
	 * Gets the WSDL document associated with this thread session.
	 * 
	 * @return the WSDL document associated with this thread session.
	 */
	public Document getWsdlDocument() 
	{
		return _wsdl;
	}

	/**
	 * Gets the RDM elements associated with this thread session.
	 * 
	 * @return the RDM elements associated with this thread session.
	 */
	public Element[] getResourceMetadataDescriptor() 
	{
		return _wsrmdProperties;
	}
	
	/**
	 * Sets the WSDL elements on this thread session.
	 * 
	 * @param the WSDL elements of this thread session..
	 */
	public void setResourceMetadataDescriptor(Element[] rmd) 
	{
		this._wsrmdProperties = rmd;
	}	
}