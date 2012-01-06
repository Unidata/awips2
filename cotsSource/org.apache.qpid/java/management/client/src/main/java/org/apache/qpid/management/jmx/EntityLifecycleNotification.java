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
import javax.management.ObjectName;

import org.apache.qpid.management.Names;
import org.apache.qpid.management.domain.services.SequenceNumberGenerator;

/**
 * Q-Man JMX entity lifecycle notification.
 * A notification is sent to interested listener by Q-Man on the following scenarios :
 * 
 * <br> - A schema (class / event) has been requested (Schema request); 
 * <br> - A schema (class / event) has been injected (Schema response); 
 * <br> - A schema cannot be parsed (probably it is malformed);
 * <br> - An object instance has been created (Instrumentation / Configuration response); 
 * <br> - An event instance has been created (Instrumentation / Configuration response); 
 * <br> - An object instance has been removed (Instrumentation / Configuration response); 
 * 
 * @author Andrea Gazzarini
 */
public class EntityLifecycleNotification extends Notification
{
	private static final long serialVersionUID = -7755773156742412161L;
	
	public static final String SCHEMA_INJECTED_NOTIFICATION_TYPE = "org.apache.qpid.management.lifecycle.entity.schema.injected";
	public static final String SCHEMA_REQUESTED_NOTIFICATION_TYPE = "org.apache.qpid.management.lifecycle.entity.schema.requested";
	public static final String MALFORMED_SCHEMA_NOTIFICATION_TYPE = "org.apache.qpid.management.lifecycle.error.schema";
	
	public static final String INSTANCE_ADDED_NOTIFICATION_TYPE = "qman.lifecycle.entity.instance.created";
	public static final String INSTANCE_REMOVED_NOTIFICATION_TYPE = "qman.lifecycle.entity.instance.removed";
	
	private String _packageName = Names.NOT_AVAILABLE;
	private String _className = Names.NOT_AVAILABLE;
	private String _classKind = Names.NOT_AVAILABLE;
	
	private ObjectName _objectName;
	
	/**
	 * Builds a new notification with the given parameters.
	 * 
	 * @param type the notification type.
	 * @param sequenceNumber the sequence number.
	 * @param packageName the package name.
	 * @param className the class name.
	 * @param classKind the class kind (i.e. class or event)
	 * @param objectName the object name of the affected mbean.
	 */
	public EntityLifecycleNotification(
			String type,
			String packageName, 
			String className, 
			String classKind, 
			ObjectName objectName) 
	{
		super(
				type,
				Names.APPLICATION_NAME,
				SequenceNumberGenerator.getNextSequenceNumber());
		
		this._className = className;
		this._packageName = packageName;
		this._classKind = classKind;
		this._objectName = objectName;
	}
	
	/**
	 * Returns the package name of object contained in this notification.
	 * 
	 * @return the package name of object contained in this notification.
	 */
	public String getPackageName()
	{
		return _packageName;
	}
	
	/**
	 * Returns the class name of object contained in this notification.
	 * 
	 * @return the class name of object contained in this notification.
	 */
	public String getClassName()
	{
		return _className;
	}
	
	/**
	 * Returns the class kind of object contained in this notification.
	 * 
	 * @return the class kind of object contained in this notification.
	 * @see Names#CLASS
	 * @see Names#EVENT
	 */
	public String getClassKind()
	{
		return _classKind;
	}
	
	/**
	 * Returns the object name of object contained in this notification.
	 * 
	 * @return the object name of object contained in this notification.
	 */
	public ObjectName getObjectName()
	{
		return _objectName;
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
			.append(_packageName)
			.append('.')
			.append(_className)
			.append('@')
			.append(_objectName)
			.toString();
	}
}