package org.apache.qpid.management.domain.model;
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


import java.util.HashMap;
import java.util.Map;

import javax.management.Attribute;
import javax.management.AttributeList;
import javax.management.DynamicMBean;
import javax.management.MBeanInfo;
import javax.management.NotificationBroadcasterSupport;
import javax.management.ObjectName;
import javax.management.RuntimeOperationsException;

import org.apache.qpid.management.Messages;
import org.apache.qpid.management.Names;
import org.apache.qpid.management.domain.model.type.Binary;
import org.apache.qpid.management.domain.services.QpidService;
import org.apache.qpid.management.jmx.EntityLifecycleNotification;
import org.apache.qpid.transport.util.Logger;

/**
 * Layer supertype for QMan entities.
 */
public abstract class QpidEntity extends NotificationBroadcasterSupport
{  
	/**
	 * Layer supertype for QMan managed bean entities. 
	 */
	abstract class QManManagedEntity implements DynamicMBean
	{
        // After mbean is registered with the MBean server this collection holds the mbean attribute values.
        Map<String,Object> _attributes = new HashMap<String, Object>();
        
        /**
         * Creates or replace the given attribute.
         * Note that this is not part of the management interface of this object instance and therefore will be accessible only
         * from within this class.
         * It is used to update directly the object attributes bypassing jmx interface.
         * 
         * @param attributeName the name of the attribute.
         * @param property newValue the new value of the attribute.
         */
        void createOrReplaceAttributeValue(String attributeName, Object newValue) 
        {
        	_attributes.put(attributeName, newValue);
        }
        
        /**
         * Get the values of several attributes of the Dynamic MBean.
         *
         * @param attributes A list of the attributes to be retrieved.
         *
         * @return  The list of attributes retrieved.
         */
         public AttributeList getAttributes (String[] attributes)
         {
             if (attributes == null) 
             {
                 throw new RuntimeOperationsException(new IllegalArgumentException("Attributes array must not be null"));
             }
             
             AttributeList result = new AttributeList(attributes.length);
             for (int i = 0; i < attributes.length; i++)
             {
                 String attributeName = attributes[i];
                 try 
                 {
                     result.add(new Attribute(attributeName,getAttribute(attributeName)));
                 } catch(Exception exception) 
                 {
                     // Already logged.
                 }
             } 
             return result;
         }
         
         /**
          * Returns metadata for this object instance.
          */
         // Developer Note : note that this metadata is a member of the outer class definition : in that way we create 
         // that metadata only once and then it will be shared between all object instances (it's a readonly object)
         public MBeanInfo getMBeanInfo ()
         {
             return _metadata;
         }         
	};
	
    final Logger _logger = Logger.get(getClass());
    final static JmxService JMX_SERVICE = new JmxService();
    
    final String _name;
    final Binary _hash;
    
    final QpidPackage _parent;
    MBeanInfo _metadata;
    final QpidService _service;

    protected ObjectName _objectName;
    
    private final String _type;
    
    /**
     * Builds a new class with the given name and package as parent.
     * 
     * @param className the name of the class.
     * @param hash the class schema hash.
     * @param parentPackage the parent of this class.
     */
    QpidEntity(String className, Binary hash, QpidPackage parentPackage,String type)
    {
        this._name = className;
        this._parent = parentPackage;
        this._hash = hash;
        this._type = type;
        this._service = new QpidService(_parent.getOwnerId());
        
        _logger.debug(
                Messages.QMAN_200020_ENTITY_DEFINITION_HAS_BEEN_BUILT, 
                _parent.getOwnerId(),
                _parent.getName(),
                _name);        
    }
    
    public String getName()
    {
    	return _name;
    }
    
    public String getPackageName()
    {
    	return _parent.getName();
    }
    
    /**
     * Internal method used to send a schema request for this entity.
     * 
     * @throws Exception when the request cannot be sent.
     */
    void requestSchema() throws Exception
    {     

       	_objectName = JMX_SERVICE.createEntityDefinitionName(_parent.getName(), _name,_type);
     	 JMX_SERVICE.registerEntityDefinition(_objectName,this,_parent.getName(),_name);

    	try
        {
            _service.connect();
           _service.requestSchema(_parent.getName(), _name, _hash);
            _service.sync();
        } finally
        {
            _service.close();
        }                
        
      	 EntityLifecycleNotification notification = new EntityLifecycleNotification(
          		 EntityLifecycleNotification.SCHEMA_REQUESTED_NOTIFICATION_TYPE,
          		 _parent.getName(), 
          		 _name, 
          		 Names.CLASS,
          		 _objectName);
           sendNotification(notification);
    }    
}
