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

import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.apache.qpid.management.domain.handler.impl.IMethodInvocationListener;
import org.apache.qpid.management.domain.handler.impl.MethodOrEventDataTransferObject;
import org.apache.qpid.management.domain.model.type.Binary;

/**
 * Broker domain model.
 * This is the local representation of a remote broker domain model.
 * 
 * @author Andrea Gazzarini
 */
public class DomainModel
{
    private final UUID _id;
    
    /** Here the known packages of the remote broker are stored. */
    Map<String,QpidPackage> _packages = new HashMap<String, QpidPackage>();

    private Date _lastRefreshDate = new Date();
    
    private IMethodInvocationListener _methodInvocationListener;
    
    /**
     * Builds a new domain model with the given broker identifier.
     * 
     * @param brokerId the broker identifier.
     */
    public DomainModel(UUID brokerId)
    {
        this._id = brokerId;
    }

    /**
     * Returns the identifier of the broker associated with this domain model.
     * 
     * @return the identifier of the broker associated with this domain model.
     */
    public UUID getBrokerId()
    {
        return _id;
    }
    
    /**
     * Adds the specified schema to this domain model.
     * 
     * @param packageName the package name.
     * @param className the class name.
     * @param classHash the class schema hash.
     * @param properties the class properties.
     * @param statistics the class statistics.
     * @param methods the class methods.
     * @throws UnableToBuildFeatureException 
     */
    public void addSchema(
            String packageName, 
            String className, 
            Binary classHash,
            List<Map<String, Object>> properties,
            List<Map<String, Object>> statistics,
            List<MethodOrEventDataTransferObject> methods) throws UnableToBuildFeatureException
    {
        QpidPackage qpidPackage = getPackageByName(packageName);
        qpidPackage.addClassDefinition(className,classHash,properties,statistics,methods);
    }
    
    /**
     * Updates the last refresh date.
     */
    public void updateLastRefreshDate() 
    {
    	this._lastRefreshDate = new Date();
    }
    
    /**
     * Returns the last refresh date.
     * 
     * @return the last refresh date.
     */
    public Date getLastRefreshDate()
    {
    	return _lastRefreshDate;
    }
    
    /**
     * Adds the specified schema to this domain model.
     * 
     * @param packageName the package name.
     * @param className the class name.
     * @param classHash the class schema hash.
     * @param properties the class properties.
     * @param statistics the class statistics.
     * @param methods the class methods.
     * @throws UnableToBuildFeatureException 
     */
    public void addEventSchema(
            String packageName, 
            String className, 
            Binary classHash,
            List<Map<String, Object>> arguments) throws UnableToBuildFeatureException
    {
        QpidPackage qpidPackage = getPackageByName(packageName);
        qpidPackage.addEventDefinition(className,classHash,arguments);
    }

    /**
     * Gets the package with the specified name.
     * Note that if the package doesn't exist a new one will be created and returned.
     * 
     * @param packageName the name of the package.
     * @return the package.
     */
    QpidPackage getPackageByName (String packageName)
    {
        QpidPackage qpidPackage = _packages.get(packageName);
        if (qpidPackage == null) 
        {
            qpidPackage = new QpidPackage(packageName,this);
            _packages.put(packageName, qpidPackage);
        } 
        return qpidPackage;
    }

    /**
     * Returns true if a package with the specified name already exists on this domain model.
     * 
     * @param packageName the name of the package.
     * @return true if the package exists, false otherwise.
     */
    boolean containsPackage (String packageName)
    {
        return _packages.containsKey(packageName);
    }

    /**
     * Adds the given instrumentation data (raw format) to this domain model.
     * Note that this data is belonging to a specific object instance.
     * 
     * @param packageName the name of the ower package.
     * @param className the name of the owner class.
     * @param classHash the schema hash for this class.
     * @param objectId the object instance identifier.
     * @param rawData the instrumentation data.
     */
    public void addInstrumentationRawData (String packageName, String className,Binary classHash, Binary objectId, byte[] rawData)
    {
        QpidPackage qpidPackage = getPackageByName(packageName);
        qpidPackage.setObjectInstanceInstrumentationRawData(className,classHash,objectId,rawData);
    }
    
    public void addEventRawData (
    		String packageName, 
    		String eventName, 
    		Binary eventHash, 
    		byte[] rawData,
    		long currentTimestamp, 
    		int severity)
    {
        QpidPackage qpidPackage = getPackageByName(packageName);
        qpidPackage.setEventInstanceRawData(eventName,eventHash,rawData,currentTimestamp,severity);
    }
    
    /**
     * Adds the given configuration data (raw format) to this domain model.
     * Note that this data is belonging to a specific object instance.
     * 
     * @param packageName the name of the ower package.
     * @param className the name of the owner class.
     * @param classHash the schema hash for this class.
     * @param objectId the object instance identifier.
     * @param rawData the configuration data.
     */
    public void addConfigurationRawData (String packageName, String className, Binary classHash,Binary objectId, byte[] rawData)
    {
        QpidPackage qpidPackage = getPackageByName(packageName);
        qpidPackage.setObjectInstanceConfigurationRawData(className,classHash,objectId,rawData);
    }

    /**
     * Removes the object instance associated to the given parameters.
     * 
     * @param packageName the owner package.
     * @param className the class definition of the object instance.
     * @param classHash the class hash
     * @param objectId the object identifier.
     */
    public void removeObjectInstance (String packageName, String className, Binary classHash, Binary objectId)
    {
        QpidPackage qpidPackage = getPackageByName(packageName);
        qpidPackage.removeObjectInstance(className, classHash, objectId);
    }

    /**
     * Releases all the resources kept by domain model entitiies.
     */
    public void releaseResources()
    {
        for (QpidPackage qpidPackage : _packages.values())
        {
            qpidPackage.releaseResources();
        }
    }

    public void setMethodInvocationListener(IMethodInvocationListener listener)
    {
        this._methodInvocationListener = listener;
    }
    
    IMethodInvocationListener getMethodInvocationListener ()
    {
        return _methodInvocationListener;
    }
}
