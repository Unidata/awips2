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

import java.util.HashMap;
import java.util.Hashtable;
import java.util.Map;

import javax.management.MBeanInfo;
import javax.management.MBeanServer;
import javax.management.ObjectName;

import org.apache.muse.core.Environment;
import org.apache.muse.core.Resource;
import org.apache.qpid.management.Messages;
import org.apache.qpid.management.Names;
import org.apache.qpid.transport.util.Logger;

/**
 * Manager for all WS-* related artifacts.
 * Basically it is a factory ehnanced with a _cache mechanism so each created resource 
 * (WSDL, capability class, descriptor) is created and its reference is returned when requested 
 * again.
 * 
 * @author Andrea Gazzarini
 */
class WsArtifactsFactory 
{
	private final static Logger LOGGER = Logger.get(WsArtifactsFactory.class);
	
	private final MBeanServer _mxServer;
	private final Environment _environment;
	private Map<ObjectName, WsArtifacts> _cache; 

	/**
	 * Builds a new factory with the given environment and mbean server.
	 * 
	 * @param environment the builder environment.
	 * @param mxServer the management server.
	 */
	public WsArtifactsFactory(Environment environment, MBeanServer mxServer) 
	{
		this._environment = environment;
		this._mxServer = mxServer;
		this._cache = new HashMap<ObjectName, WsArtifacts>();
	}
	
	/**
	 * Returns the WS artifacts corresponding with the given resource.
	 * 
	 * @param resource the WS resource.
	 * @param objectName the resource identifier (name).
	 * @return the WS artifacts corresponding with the given resource.
	 * @throws ArtifactsNotAvailableException when some problem occurs during artifacts generation.
	 */
	@SuppressWarnings("unchecked")
	WsArtifacts getArtifactsFor(Resource resource, ObjectName objectName) throws ArtifactsNotAvailableException
	{
		WsArtifacts result = null;
		try 
		{
			Hashtable<String, String> keyProperties = objectName.getKeyPropertyList();
			keyProperties.remove(Names.NAME_ATTRIBUTE);
			keyProperties.remove(Names.OBJECT_ID);
			
			ObjectName searchKey = ObjectName.getInstance(objectName.getDomain(),keyProperties);
			
			LOGGER.debug(
					Messages.QMAN_200041_INCOMING_OBJECT_NAME_AND_DERIVED_KEY,
					objectName,
					searchKey);
						
			result = _cache.get(searchKey);
			if (result == null)
			{
				MBeanInfo metadata = _mxServer.getMBeanInfo(objectName);
				
				WSDMArtifactsDirector director = new WSDMArtifactsDirector(objectName,metadata);
				director.setEnvironment(_environment);
				director.setResource(resource);
				director.direct();
				
				result = new WsArtifacts(
						director.getCapabilityClass(),
						director.getResourceMetadataDescriptor(),
						director.getWsdl());
				
				_cache.put(searchKey, result);

				LOGGER.debug(
						Messages.QMAN_200040_WS_ARTIFACTS_CACHED,
						searchKey);
			}
			
			return result;
		} catch(Exception exception)
		{
			throw new ArtifactsNotAvailableException(
					result,
					exception,
					objectName);
		}
	}
	
	/**
	 * Utility method for create concrete instance of the given capability class.
	 * 
	 * @param capabilityClass the capability class.
	 * @param objectName the object name that will act as the target for this capability invocations.
	 * @return an initialized instance of the given capability class.
	 * @throws InstantiationException when the class cannot be instantiated.
	 * @throws IllegalAccessException when this method does not have access to 
	 * 				the definition of the capability class.
	 */
	MBeanCapability createCapability(Class<MBeanCapability> capabilityClass, ObjectName objectName) 
		throws InstantiationException, IllegalAccessException
	{
		MBeanCapability capability = capabilityClass.newInstance();
		capability.setResourceObjectName(objectName);
		return capability;
	}
}