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

import java.lang.management.ManagementFactory;

import javax.management.Attribute;
import javax.management.AttributeNotFoundException;
import javax.management.InstanceNotFoundException;
import javax.management.MBeanException;
import javax.management.MBeanServer;
import javax.management.ObjectName;

import org.apache.muse.ws.resource.impl.AbstractWsResourceCapability;
import org.apache.qpid.management.Messages;
import org.apache.qpid.management.domain.handler.impl.InvocationResult;
import org.apache.qpid.management.domain.services.MethodInvocationException;
import org.apache.qpid.management.wsdm.common.EntityInstanceNotFoundFault;
import org.apache.qpid.management.wsdm.common.MethodInvocationFault;
import org.apache.qpid.management.wsdm.common.NoSuchAttributeFault;
import org.apache.qpid.management.wsdm.common.QManFault;
import org.apache.qpid.transport.util.Logger;

/**
 * Abstract capability used for centralize common 
 * behaviour of the QMan resource(s) related capabilities.
 * 
 * @author Andrea Gazzarini
 */
public abstract class MBeanCapability extends AbstractWsResourceCapability 
{
	private static final Logger LOGGER = Logger.get(MBeanCapability.class);

	protected final MBeanServer _mxServer;
	protected ObjectName _objectName;
	
	/**
	 * Builds a new capability related to the given object name.
	 * 
	 * @param objectName the name of the target object of this capability.
	 */
	public MBeanCapability() 
	{
		_mxServer = ManagementFactory.getPlatformMBeanServer();
	}

	/**
	 * Injects on this capability the object name of the target mbean.
	 * 
	 * @param objectName the object name of the target mbean.
	 */
	void setResourceObjectName(ObjectName objectName) 
	{
		this._objectName = objectName;
	}

	/**
	 * Returns the attribute value of a QMan managed object instance.
	 * 
	 * @param attributeName the name of the attribute to be requested.
	 * @return the value for the requested attribute.
	 * @throws NoSuchAttributeFault when the requested attribute cannot be found 
	 * 			on the given entity instance.
	 * @throws EntityInstanceNotFoundFault when the requested entity instance cannot 
	 * 			be found.
	 * @throws QManFault in case of internal system failure.
	 */
	Object getAttribute(String attributeName) throws NoSuchAttributeFault, EntityInstanceNotFoundFault, QManFault 
	{
		try 
		{
			return _mxServer.getAttribute(_objectName, attributeName);
		} catch (AttributeNotFoundException exception) 
		{
			throw new NoSuchAttributeFault(
					getWsResource().getEndpointReference(), 
					attributeName);
		} catch (InstanceNotFoundException exception) 
		{
			throw new EntityInstanceNotFoundFault(
					getWsResource().getEndpointReference(), 
					_objectName);
		} catch (Exception exception) 
		{
			LOGGER.error(
					Messages.QMAN_100035_GET_ATTRIBUTE_FAILURE,
					attributeName,
					_objectName);			
			throw new QManFault(
					getWsResource().getEndpointReference(),
					exception);
		}
	}

	/**
	 * Sets the value for the given attribute on this MBean (proxy).
	 * 
	 * @param objectName
	 *            the object name of the target instance (excluding the domain
	 *            name).
	 * @param attributeName
	 *            the name of the attribute to be requested.
	 * @param value
	 *            the value for the requested attribute.
	 * @throws NoSuchAttributeFault
	 *             when the requested attribute cannot be found on the given
	 *             entity instance.
	 * @throws EntityInstanceNotFoundFault
	 *             when the requested entity instance cannot be found.
	 * @throws QManFault
	 *             in case of internal system failure.
	 */
	void setAttribute(String attributeName, Object value) throws NoSuchAttributeFault, EntityInstanceNotFoundFault, QManFault 
	{
		try 
		{
			_mxServer.setAttribute(_objectName, new Attribute(attributeName,value));
		} catch (AttributeNotFoundException exception) 
		{
			throw new NoSuchAttributeFault(
					getWsResource().getEndpointReference(), 
					attributeName);
		} catch (InstanceNotFoundException exception) 
		{
			throw new EntityInstanceNotFoundFault(
					getWsResource().getEndpointReference(), 
					_objectName);
		} catch (Exception exception) 
		{
			LOGGER.error(
					Messages.QMAN_100036_SET_ATTRIBUTE_FAILURE,
					attributeName,
					_objectName);			
			throw new QManFault(
					getWsResource().getEndpointReference(),
					exception);
		}
	}

	/**
	 * Invokes the requested operation on target JMX resource.
	 * 
	 * @param operationName the name of the operation to be invoked.
	 * @param params parameters used for operation invocation.
	 * @param signature the operation / method signature.
	 * @throws EntityInstanceNotFoundFault 
	 * 		when the target MBean doesn't exist on Management server.
	 * @throws MethodInvocationFault 
	 * 		when the invocation of the requested operation raises an exception.
	 * @throws QManFault 
	 * 		in case of not-well known failure.
	 */
	Result invoke(String operationName, Object [] params, String [] signature) throws EntityInstanceNotFoundFault, MethodInvocationFault,QManFault 
	{
		try
		{
			InvocationResult output =  (InvocationResult) _mxServer
				.invoke(
						_objectName, 
						operationName, 
						params, 
						signature);
			
		return new Result(output.getOutputSection());

		} catch (InstanceNotFoundException exception)
		{
			throw new EntityInstanceNotFoundFault(
					getWsResource().getEndpointReference(), 
					_objectName);
		} catch (MBeanException exception)
		{
			if (exception.getTargetException() instanceof MethodInvocationException)
			{
				MethodInvocationException failure = (MethodInvocationException) exception.getTargetException();
				throw new MethodInvocationFault(
						getWsResource().getEndpointReference(),
						operationName,
						failure.getStatusText(),
						failure.getReturnCode());				
			} else {
				LOGGER.error(
						Messages.QMAN_100037_INVOKE_OPERATION_FAILURE,
						operationName,
						_objectName);			
				throw new QManFault(
						getWsResource().getEndpointReference(),
						exception);							
			}
		}catch(Exception exception)
		{
			LOGGER.error(
					Messages.QMAN_100037_INVOKE_OPERATION_FAILURE,
					operationName,
					_objectName);			
			throw new QManFault(
					getWsResource().getEndpointReference(),
					exception);			
		}
	}
}