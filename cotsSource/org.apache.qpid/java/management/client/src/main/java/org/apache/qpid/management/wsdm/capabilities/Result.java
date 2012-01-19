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

import java.util.*;

/**
 * Data Transfer Object that encapsulates the result of a method invocation.
 * This is the object that will be marshalled in XML and will contain the result of a method 
 * invocation (status code & text).
 * 
 * @author Andrea Gazzarini
 */
public final class Result
{
	private final Map<String,Object> _outputParameters;
	
	/**
	 * Builds a new result DTO with the given parameters.
	 * 
	 * @param statusCode the return code.
	 * @param statusText the status message.
	 * @param outputParameters the output parameters.
	 */
	public Result(Map<String, Object> outputParameters)
	{
		this._outputParameters = outputParameters;
	}
		
	/**
	 * Returns the output parameterss.
	 * 
	 * @return the output parameterss.
	 */
	public Map<String, Object> getOutputParameters()
	{
		return _outputParameters;
	}
}