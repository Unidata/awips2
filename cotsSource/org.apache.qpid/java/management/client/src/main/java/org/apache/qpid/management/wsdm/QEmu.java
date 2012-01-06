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
package org.apache.qpid.management.wsdm;

import javax.management.MBeanRegistration;
import javax.management.MBeanServer;
import javax.management.NotificationBroadcasterSupport;
import javax.management.ObjectName;

import org.apache.qpid.management.Names;
import org.apache.qpid.management.domain.handler.impl.QpidDomainObject;
import org.apache.qpid.management.domain.handler.impl.QpidDomainObjectMBean;
import org.apache.qpid.management.jmx.EntityLifecycleNotification;

/**
 * QEmu is basically an instance creator that is installed separately 
 * as part of QMan test cases & examples.
 * Reason for that is to emulate object creation (queues, exchanges, etc...) 
 * without having Qpid broker connected and therefore controlling the 
 * total number of the instances that are created.
 * 
 * @author Andrea Gazzarini
 */
public class QEmu extends NotificationBroadcasterSupport implements QEmuMBean, MBeanRegistration{
	 
	private MBeanServer _mxServer;
	private final static String PACKAGE_NAME= "org.apache.qpid";
	private final static String QUEUE = "queue";
	
	/**
	 * Unregisters a Queue MBean with MBeanServer.
	 * 
	 * @param objectName the name of the MBean that must unregistered.
	 * @throws Exception when the creation or the registration fails.
	 */
	public void unregister(ObjectName objectName) throws Exception 
	{
		_mxServer.unregisterMBean(objectName);
		
		sendNotification(
				EntityLifecycleNotification.INSTANCE_REMOVED_NOTIFICATION_TYPE,
				objectName);
	}

	/**
	 * Creates and registers a Queue MBean with MBeanServer.
	 * 
	 * @param objectName the name of the queue MBean.
	 * @throws Exception when the creation or the registration fails.
	 */
	public void createQueue(ObjectName objectName) throws Exception 
	{
		QpidDomainObjectMBean queue = new QpidDomainObject();
		_mxServer.registerMBean(queue, objectName);
		
		sendNotification(
				EntityLifecycleNotification.INSTANCE_ADDED_NOTIFICATION_TYPE,
				objectName);
	}

	/**
	 * Sends a notification about a lifecycle event of the mbean associated 
	 * with the given object.
	 * 
	 * @param type the event (notification) type.
	 * @param name the name of the event source.
	 */
	private void sendNotification(String type,ObjectName name)
	{
		sendNotification(
				new EntityLifecycleNotification(
						type,
						PACKAGE_NAME, 
						QUEUE, 
						Names.CLASS,
						name));
	}
	
	/**
	 * Not implemented for this class.
	 */
	public void postDeregister()
	{
		// N.A.
	}

	/**
	 * Not implemented for this class.
	 */
	public void postRegister(Boolean registrationDone)
	{
		// N.A.
	}

	/**
	 * Not implemented for this class.
	 */
	public void preDeregister()
	{
		// N.A.
	}

	/**
	 * MBean server callback.
	 * Stores the value of the owner MBeanServer.
	 */
	public ObjectName preRegister(MBeanServer server, ObjectName name) 
	{
		this._mxServer = server;
		return name;
	}
}