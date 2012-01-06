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

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.apache.qpid.management.domain.handler.impl.IMethodInvocationListener;
import org.apache.qpid.management.domain.handler.impl.MethodOrEventDataTransferObject;
import org.apache.qpid.management.domain.model.type.Binary;

/**
 * Qpid package definition.
 * A grouping of class definitions that are related to a single software component. 
 * The package concept is used to extend the management schema beyond just the QPID software components.
 * The name is prefixed with "Qpid" for avoiding name conficts with java.lang.Package.
 * 
 * @author Andrea Gazzarini
 */
final class QpidPackage
{  
    /**
     * Qpid class identity.
     * Each qpid class is uniquely identifier by its name and schema-hash.
     * The schema hash is an MD5 checksum of the schema for a class. 
     * It is there so we can support the case where two different versions of the same class are present at the same time.
     * 
     * @author Andrea Gazzarini
     */
    class QpidClassIdentity {
        final String name;
        final Binary hash;
        
        /**
         * Builds a new class identity with the given name and hash.
         * 
         * @param name the class name.
         * @param hash is an MD5 checksum of the schema of this outer class.
         */
        QpidClassIdentity(String name,Binary hash) {
            this.name = name;
            this.hash = hash;
        }
        
        @Override
        public int hashCode ()
        {
            return name.hashCode()+hash.hashCode();
        }
        
        @Override
        public boolean equals (Object obj)
        {
            QpidClassIdentity identity = (QpidClassIdentity) obj;
            return name.equals(identity.name) && hash.equals(identity.hash);
        }
    }
    
    private String _name;
    private DomainModel _parent;
    private Map<QpidClassIdentity, QpidClass> _classes = new HashMap<QpidClassIdentity, QpidClass>();
    private Map<QpidClassIdentity, QpidEvent> _events = new HashMap<QpidClassIdentity, QpidEvent>();
    
    /**
     * Builds a new package with the supplied name.
     * 
     * @param name the name of the package.
     */
    QpidPackage(String name, DomainModel parent)
    {
        this._name = name;
        this._parent = parent;
    }

    /**
     * Returns the identifier of the broker which contains this package.
     * @return
     */
    UUID getOwnerId() 
    {
        return _parent.getBrokerId();
    }
    
    /**
     * Returns the name of this package.
     * 
     * @return the name of this package.
     */
    String getName ()
    {
        return _name;
    }

    /**
     * Adds a class definition to this package.
     * The class will be added only if its definition doesn't already exists. 
     * 
     * @param className the name of the class.
     * @param classHash the class schema hash.
     * @param properties the properties of the class.
     * @param statistics the statistics of the class.
     * @param methods the methods of the class.
     * @param events the events of the class.
     * 
     * @throws UnableToBuildFeatureException when the class definition cannot be built due to a feature build failure.
     */
    void addClassDefinition (
            String className, 
            Binary classHash,
            List<Map<String, Object>> properties,
            List<Map<String, Object>> statistics,
            List<MethodOrEventDataTransferObject> methods) throws UnableToBuildFeatureException
    {
        getQpidClass(className,classHash,true).setSchema(properties,statistics,methods);
    }

    void addEventDefinition (
            String eventClassName, 
            Binary classHash,
            List<Map<String, Object>> arguments) throws UnableToBuildFeatureException
    {
        getQpidEvent(eventClassName,classHash,true).setSchema(arguments);
    }
    
    /**
     * Returns true if this package contains the given class definition.
     * 
     * @param className the name of the class.
     * @return true if this package contains the class definition, false otherwise.
     */
    boolean alreadyContainsClassDefinition (String className, Binary hash)
    {
        return _classes.containsKey(new QpidClassIdentity(className,hash));
    }

    /**
     * Injects into a class the given object instance instrumentation data.
     * 
     * @param className the of the class the injected object data belongs to.
     * @param objectId the object identifier.
     * @param rawData the instrumentation data (in raw format).
     */
    void setObjectInstanceInstrumentationRawData (String className, Binary classHash,Binary objectId, byte[] rawData)
    {
        getQpidClass(className, classHash,true).addInstrumentationData(objectId,rawData);
    }
    
    /**
     * Injects into a class the given object instance configuration data.
     * 
     * @param className the of the class the injected object data belongs to.
     * @param objectId the object identifier.
     * @param rawData the configuration data (in raw format).
     */
    void setObjectInstanceConfigurationRawData (String className,Binary classHash, Binary objectId, byte[] rawData)
    {
        getQpidClass(className,classHash,true).addConfigurationData(objectId,rawData);
    }

    void setEventInstanceRawData (String eventName,Binary eventHash, byte[] rawData,long currentTimestamp,int severity)
    {
        getQpidEvent(eventName,eventHash,true).addEventData(rawData, currentTimestamp, severity);
    }    
    
    /**
     * Returns the definition of the class with given name.
     * 
     * @param className the name of the class.
     * @param hash the class hash.
     * @param store a flag indicating if a just created class must be stored or not.
     * @return the definition of the class with given name.
     */
    QpidClass getQpidClass(String className, Binary hash, boolean store) 
    {
        QpidClassIdentity identity = new QpidClassIdentity(className,hash);
        QpidClass classDefinition = _classes.get(identity);
        if (classDefinition == null) 
        {
            classDefinition = new QpidClass(className, hash,this);
            if (store) 
            {
            _classes.put(identity,classDefinition);
            }
         }
        return classDefinition;
    }
    
    /**
     * Returns the definition of the class with given name.
     * 
     * @param className the name of the class.
     * @param hash the class hash.
     * @param store a flag indicating if a just created class must be stored or not.
     * @return the definition of the class with given name.
     */
    QpidEvent getQpidEvent(String className, Binary hash, boolean store) 
    {
        QpidClassIdentity identity = new QpidClassIdentity(className,hash);
        QpidEvent eventDefinition = _events.get(identity);
        if (eventDefinition == null) 
        {
            eventDefinition = new QpidEvent(className, hash,this);
            if (store) 
            {
            _events.put(identity,eventDefinition);
            }
         }
        return eventDefinition;
    }    
    
    /**
     * Returns a string representation of this class.
     * That is, this method returns the simple name (not FQN) of this class.
     */
    @Override
    public String toString ()
    {
        return _name;
    }

    /**
     * Removes the object instance associated to the given parameters.
     * 
     * @param className the class definition of the object instance.
     * @param classHash the class hash
     * @param objectId the object identifier.
     */
    void removeObjectInstance (String className, Binary classHash, Binary objectId)
    {
        QpidClass qpidClass = getQpidClass(className,classHash,false);
        qpidClass.removeObjectInstance(objectId);
    }

    /**
     * Releases all previously acquired resources of this package.
     */
    void releaseResources ()
    {
        for (QpidClass qpidClass : _classes.values())
        {
            qpidClass.releaseResources();
        }
        
        for (QpidEvent qpidEvent: _events.values())
        {
            qpidEvent.releaseResources();
        }        
    }

    /**
     * Returns the method invocation listener of the corresponing parent domain model.
     * 
     * @return the method invocation listener of the corresponing parent domain model.
     */
    IMethodInvocationListener getMethodInvocationListener ()
    {
        return _parent.getMethodInvocationListener();
    }
}
