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

import javax.management.ObjectName;

/**
 * Thrown when the artifacts related to a specific resource cannot be built.
 * 
 * @author Andrea Gazzarini
 */
public class ArtifactsNotAvailableException extends Exception 
{
	private static final long serialVersionUID = -9010460152421791926L;
	
	private final WsArtifacts _artifacts;
	private final ObjectName _objectName;
	
	/**
	 * Builds a new exception with the given arguments.
	 * 
	 * @param artifacts the artifacts built.
	 * @param cause the exception cause.
	 * @param objectName the object name of the corresponding JMX entity.
	 */
	public ArtifactsNotAvailableException(
			WsArtifacts artifacts,
			Throwable cause, 
			ObjectName objectName) 
	{
		super(cause);
		this._artifacts = artifacts;
		this._objectName = objectName;
	}
	
	/**
	 * Returns a message that indicates which artifacts were built.
	 * 
	 * @return a message that indicates which artifacts were built.
	 */
	@Override
	public String getMessage() 
	{
		StringBuilder builder = new StringBuilder();
		if (_artifacts == null) 
		{
			return super.getMessage();
		}
		
		builder.append("Built artifacts for ")
			.append(_objectName)
			.append(" : ")
			.append( (_artifacts.getWsdl() != null) ? "WSDL," : "")
			.append( (_artifacts.getCapabilityClass() != null) ? "Capability Class," : "")
			.append( (_artifacts.getResourceMetadataDescriptor() != null) ? "Resource Metadata Descriptor," : "");		
		return builder.toString();
	}
}
