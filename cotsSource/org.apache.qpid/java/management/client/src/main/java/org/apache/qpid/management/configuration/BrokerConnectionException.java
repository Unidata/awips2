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

package org.apache.qpid.management.configuration;

/**
 * Thrown when a connection to a broker cannot be estabilished.
 * 
 * @author Andrea Gazzarini
 */
public class BrokerConnectionException extends Exception 
{
	private static final long serialVersionUID = 8170112238862494025L;

	/**
	 * Builds a new exception with the given cause.
	 * 
	 * @param cause the exception cause.
	 */
	BrokerConnectionException(Throwable cause) 
	{
		super(cause);
	}
}
