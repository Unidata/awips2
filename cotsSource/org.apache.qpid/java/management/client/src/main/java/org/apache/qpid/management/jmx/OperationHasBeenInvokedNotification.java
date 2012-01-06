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
package org.apache.qpid.management.jmx;

import javax.management.Notification;

import org.apache.qpid.management.Names;
import org.apache.qpid.management.domain.handler.impl.InvocationResult;
import org.apache.qpid.management.domain.services.SequenceNumberGenerator;

/**
 * Q-Man JMX method invocation notification.
 * This kind of notification is sent to interested listener by Q-Man when 
 * a method has been invoked (Method invocation request)
 * 
 * @author Andrea Gazzarini
 */
public class OperationHasBeenInvokedNotification extends Notification
{
	private static final long serialVersionUID = -7755773156742412161L;
	public static final String NOTIFICATION_TYPE = "org.apache.qpid.management.operation.invoked";
	
	private final String _operationName;
	private final Object [] _parameters;
	private final String [] _signature;
	private final Exception _exception;
	private final InvocationResult _result;
	
	/**
	 * Builds a new notification with the given parameters.
	 * 
	 * @param type the notification type.
	 * @param operationName the operation name.
	 * @param params the operation parameters.
	 * @param signature the operation signature.
	 * @param exception the exception raised by the invocation.
	 */
	public OperationHasBeenInvokedNotification(
			String operationName, 
			Object[] parameters, 
			String [] signature, 
			Exception exception) 
	{
		super(
				NOTIFICATION_TYPE,
				Names.APPLICATION_NAME,
				SequenceNumberGenerator.getNextSequenceNumber());
		
		this._operationName= operationName;
		this._parameters = parameters;
		this._signature = signature;		
		this._result = null;
		this._exception = exception;
	}

	/**
	 * Builds a new notification with the given parameters.
	 * 
	 * @param type the notification type.
	 * @param operationName the operation name.
	 * @param params the operation parameters.
	 * @param signature the operation signature.
	 * @param objectName the target mbean object name.
	 * @param result the invocation result.
	 */
	public OperationHasBeenInvokedNotification(
			String operationName, 
			Object[] parameters, 
			String [] signature,
			InvocationResult result) 
	{
		super(
				NOTIFICATION_TYPE,
				Names.APPLICATION_NAME,
				SequenceNumberGenerator.getNextSequenceNumber());
		
		this._operationName= operationName;
		this._parameters = parameters;
		this._signature = signature;
		this._result = result;
		this._exception = null;
	}
	
	/**
	 * Returns the exception raised by this notification 
	 * referred operation.
	 * 
	 * @return the exception raised by this notification referred operation.
	 */
	public Exception getException()
	{
		return _exception;
	}
	
	/**
	 * Returns the exception raised by this notification 
	 * referred operation.
	 * 
	 * @return the exception raised by this notification referred operation.
	 */
	public InvocationResult getResult()
	{
		return _result;
	}
	
	/**
	 * Returns the operation name.
	 * 
	 * @return the operation name.
	 */
	public String getOperationName()
	{
		return _operationName;
	}
	
	/**
	 * Returns the parameters used in method invocation.
	 * 
	 * @return the parameters used in method invocation.
	 */
	public Object [] getParameters()
	{
		return _parameters;
	}

	/**
	 * Returns the signature of the invoked operation.
	 * 
	 * @return the signature of the invoked operation.
	 */
	public String [] getSignature()
	{
		return _signature;
	}
	
	/**
	 * Returns a string representation of this notification.
	 * 
	 * @return a string representation of this notification.
	 */
	@Override
	public String toString()
	{
		return new StringBuilder()
			.append(getType())
			.append(':')
			.append(_operationName)
			.toString();
	}
}