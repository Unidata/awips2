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
package org.apache.qpid.management.domain.model;

import java.lang.management.ManagementFactory;
import java.util.Set;
import java.util.UUID;

import javax.management.MBeanException;
import javax.management.MBeanServer;
import javax.management.MalformedObjectNameException;
import javax.management.ObjectName;

import org.apache.qpid.management.Messages;
import org.apache.qpid.management.Names;
import org.apache.qpid.management.domain.model.QpidClass.QManManagedObject;
import org.apache.qpid.management.domain.model.QpidEvent.QManManagedEvent;
import org.apache.qpid.management.domain.model.type.Binary;
import org.apache.qpid.management.domain.services.QMan;
import org.apache.qpid.transport.util.Logger;

/**
 * A simple facade used to perform operations on Mbean server.
 */
public class JmxService
{
    private final static Logger LOGGER = Logger.get(JmxService.class);
    MBeanServer _mxServer = ManagementFactory.getPlatformMBeanServer();
    
    /**
     * Registers QMan with the MBeanServer.
     * After that QMan management interface will be JMX-exposed. 
     * 
     * @param qman QMan
     * @throws MBeanException when some error occurs during registration.
     */
    public void registerQManService(QMan qman) throws MBeanException 
    {
    	if (!_mxServer.isRegistered(Names.QMAN_OBJECT_NAME))
    	{
    		try {
				_mxServer.registerMBean(qman, Names.QMAN_OBJECT_NAME);
			} catch (Exception exception) {
				throw new MBeanException(exception);
			}
    	}
    }
    
    /**
     * Registers an event instance with MBean server.
     * 
     * @param eventInstance the mben event instance
     * @param brokerId the broker identifier.
     * @param packageName the package name.
     * @param eventClassName the event class name.
     * @return the object name used for registration.
     */
	ObjectName registerEventInstance(
			QManManagedEvent eventInstance,
			UUID brokerId, 
			String packageName, 
			String eventClassName) 
	{
        ObjectName name = createEventName(brokerId, packageName, eventClassName);
        if (!_mxServer.isRegistered(name)) 
        {
            try
            {
                _mxServer.registerMBean(eventInstance, name);

                LOGGER.debug(
                		Messages.QMAN_200010_EVENT_MBEAN_REGISTERED,
                        brokerId,
                        packageName,
                        eventClassName,
                        name);
            }  catch (Exception exception)
            {
                throw new RuntimeException(exception);
            } 
        }
        return name;
}
    
    /**
     * Registers a pre-existing object instance as an MBean with the MBean
     * server. 
     * 
     * @param instance the object instance.
     * @param brokerId the broker identifier.
     * @param packageName the name of the package containing this instance.
     * @param className the name of the owner class of this instance.
     * @param objectId the object instance identifier.
     *  @return the object name used for registration.
     */
    ObjectName registerObjectInstance(
            QManManagedObject instance,
            UUID brokerId,
            String packageName, 
            String className, 
            Binary objectId)
    {    
            ObjectName name = createObjectName(brokerId, packageName, className, objectId);
            if (!_mxServer.isRegistered(name)) 
            {
                try
                {
                    _mxServer.registerMBean(instance, name);

                    LOGGER.debug(
                            Messages.QMAN_200011_OBJECT_MBEAN_REGISTERED, 
                            brokerId,
                            packageName,
                            className,
                            objectId,
                            name);
                }  catch (Exception exception)
                {
                    throw new RuntimeException(exception);
                } 
            }
            return name;
    }

    /**
     * Removes / unregisters a managed object instance from the MBean Server.
     * 
     * @param brokerId the broker identifier.
     * @param packageName the name of the package containing this instance.
     * @param className the name of the owner class of this instance.
     * @param objectId the object instance identifier.
     * @return obejctName the obejct name used for deregistration.
     */
    ObjectName unregisterObjectInstance(
            UUID brokerId,
            String packageName, 
            String className, 
            Binary objectId)
    {    
            ObjectName name = createObjectName(brokerId, packageName, className, objectId);
            if (_mxServer.isRegistered(name)) 
            {
                try
                {
                    _mxServer.unregisterMBean(name);

                    LOGGER.debug(
                            Messages.QMAN_200012_OBJECT_MBEAN_UNREGISTERED, 
                            brokerId,
                            packageName,
                            className,
                            objectId,
                            name);
                }  catch (Exception exception)
                {
                    LOGGER.error(exception,Messages.QMAN_100013_MBEAN_REGISTRATION_FAILURE,name);
                } 
            }
            return name;
    }    
    
    /**
     * Removes (unregister) all events from MBean Server.
     */
    void unregisterEvents()
    {
    	for (ObjectName name : getEventMBeans()) 
    	{
    		try 
    		{
    			_mxServer.unregisterMBean(name);
    		} catch(Exception ignore)
    		{
    		}
		}
    }
    
    @SuppressWarnings("unchecked")
	Set<ObjectName> getEventMBeans()
    {
    	return _mxServer.queryNames(createEventSearchName(),null);
    }
    
    /**
     * Removes (unregister) all object instances from MBean Server.
     */
    @SuppressWarnings("unchecked")
	void unregisterObjectInstances()
    {
    	Set<ObjectName> names = _mxServer.queryNames(createObjectInstanceSearchName(),null);
    	for (ObjectName name : names) 
    	{
    		try 
    		{
    			_mxServer.unregisterMBean(name);
    		} catch(Exception ignore)
    		{
    		}
		}
    }    
    
    /**
     * Factory method for ObjectNames.
     * 
     * @param brokerId the broker identifier.
     * @param packageName the name of the package containing this instance.
     * @param className the name of the owner class of this instance.
     * @param objectId the object instance identifier.
     * @return the object name built according to the given parameters.
     */
    private ObjectName createObjectName(UUID brokerId, String packageName, String className, Binary objectId) 
    {
        String asString = new StringBuilder()
            .append(Names.DOMAIN_NAME)
            .append(':')
            .append(Names.BROKER_ID)
            .append('=')
            .append(brokerId)
            .append(",type=Object,")
            .append(Names.PACKAGE)
            .append('=')
            .append(packageName)
            .append(',')
            .append(Names.CLASS)
            .append('=')
            .append(className)
            .append(',')
            .append(Names.OBJECT_ID)
            .append('=')
            .append(objectId)
            .toString();
        try
        {
            return new ObjectName(asString);
        } catch (MalformedObjectNameException exception)
        {
            throw new RuntimeException(exception);
        } 
    }
        
    /**
     * Creates an object name that will be used for searching all registered events.
     * 
     * @return the object name that will be used for searching all registered events.
     */
    ObjectName createEventSearchName() 
    {
        String asString = new StringBuilder()
            .append(Names.DOMAIN_NAME)
            .append(':')
            .append('*')
            .append(",type=Event")
            .toString();
        try
        {
            return new ObjectName(asString);
        } catch (MalformedObjectNameException exception)
        {
            throw new RuntimeException(exception);
        } 
    }        
    
    /**
     * Creates an object name that will be used for searching all registered events.
     * 
     * @return the object name that will be used for searching all registered events.
     */
    ObjectName createClassDefinitionSearchName() 
    {
        String asString = new StringBuilder()
            .append(Names.DOMAIN_NAME)
            .append(":Category=Schema,*")
            .toString();
        try
        {
            return new ObjectName(asString);
        } catch (MalformedObjectNameException exception)
        {
            throw new RuntimeException(exception);
        } 
    }           
    
    /**
     * Creates an object name that will be used for searching all registered object instances.
     * 
     * @return the object name that will be used for searching all registered object instances.
     */
    private ObjectName createObjectInstanceSearchName() 
    {
        String asString = new StringBuilder()
            .append(Names.DOMAIN_NAME)
            .append(':')
            .append('*')
            .append(",type=Object")
            .toString();
        try
        {
            return new ObjectName(asString);
        } catch (MalformedObjectNameException exception)
        {
            throw new RuntimeException(exception);
        }         
    }
    
    /**
     * Factory method for ObjectNames.
     * 
     * @param brokerId the broker identifier.
     * @param packageName the name of the package containing this instance.
     * @param className the name of the owner class of this instance.
     * @return the object name built according to the given parameters.
     */
    private ObjectName createEventName(UUID brokerId, String packageName, String className) 
    {
        String asString = new StringBuilder()
            .append(Names.DOMAIN_NAME)
            .append(':')
            .append(Names.BROKER_ID)
            .append('=')
            .append(brokerId)
            .append(",type=Event,")
            .append(Names.PACKAGE)
            .append('=')
            .append(packageName)
            .append(',')
            .append(Names.CLASS)
            .append('=')
            .append(className)
            .append(',')
            .append(Names.OBJECT_ID)
            .append('=')
            .append(UUID.randomUUID())
            .toString();
        try
        {
            return new ObjectName(asString);
        } catch (MalformedObjectNameException exception)
        {
            throw new RuntimeException(exception);
        } 
    }        

    ObjectName createEntityDefinitionName(String packageName, String className, String type) 
    {
		String asString = new StringBuilder()
		.append(Names.DOMAIN_NAME)
		.append(':')
		.append("Category=Schema,Type=")
		.append(type)
		.append(",package=")
		.append(packageName)
		.append(",name=")
		.append(className)
		.toString();
        try
        {
            return new ObjectName(asString);
        } catch (MalformedObjectNameException exception)
        {
            throw new RuntimeException(exception);
        } 		
    }
    
	public void registerEntityDefinition(ObjectName name, QpidEntity entity,String packageName, String className)
	{
		try 
		{
			if (!_mxServer.isRegistered(name))
				_mxServer.registerMBean(entity, name);
				_mxServer.addNotificationListener(name, Names.QMAN_OBJECT_NAME, null, null);
		} catch(Exception exception)  
		{
			throw new RuntimeException(exception);
		}
	}

	@SuppressWarnings("unchecked")
	public void unregisterClassDefinitions()
	{
		Set<ObjectName> names = _mxServer.queryNames(createClassDefinitionSearchName(),null);
    	for (ObjectName name : names) 
    	{
    		try 
    		{
    			_mxServer.unregisterMBean(name);
    		} catch(Exception ignore)
    		{
    		}
		}
		
	}        
}
