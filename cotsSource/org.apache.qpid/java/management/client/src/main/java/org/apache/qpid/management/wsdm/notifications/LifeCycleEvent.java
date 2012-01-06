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
package org.apache.qpid.management.wsdm.notifications;

import org.apache.muse.util.xml.XmlSerializable;
import org.apache.muse.util.xml.XmlUtils;
import org.apache.qpid.management.Names;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * Object representation of a QMan entity lifecycle event notification.
 * Note that with entity we mean both object(s) and event(s). 
 * 
 * At the moment there are only two types of lifecycle events : CREATE and REMOVE.
 * The first one if fired when a new instance (event or object) is created, while the second
 * one is fired when an object instance (events are transient objects so they are not destroyed) 
 * is removed.
 * 
 * Developer Note : The marshal & unmarshal ops could be handled using JAXB but
 * we are not sure about the running environment (JAXB libs were included only 
 * starting from 1.6)
 * 
 * This is the event XML representation :
 * 
 * <LifecycleEvent Type="created" timemillis="">
		<Resource>
			<ResourceId>16038bd5-b62b-4e86-9833-7560ed57b474</id>
			<Package>org.qpid.apache.broker</package>
			<Name>session</name>
		</Resource>
	</lifecycle-event>
	
 * @author Andrea Gazzarini
 */
public class LifeCycleEvent implements XmlSerializable
{	
	private final String _resourceId;
	private final String _packageName;
	private final String _name;
	
	private final LifeCycleEventType _type;
	
	/**
	 * Builds a new event with the given data.
	 * 
	 * @param resourceId resource identifier.
	 * @param packageName resource package name.
	 * @param name resource name.
	 * @param type event type.
	 */
	private LifeCycleEvent(String resourceId, String packageName, String name, LifeCycleEventType type)
	{
		this._resourceId = resourceId;
		this._packageName = packageName;
		this._name = name;
		this._type = type;
	}

	/**
	 * Factory method for a new "CREATE" event. 
	 * Builds a new "CREATE" event with the given data.
	 * 
	 * @param resourceId resource identifier.
	 * @param packageName resource package name.
	 * @param name resource name.
	 */
	public static LifeCycleEvent newCreateEvent(String resourceId, String packageName, String name)
	{
		return new LifeCycleEvent(resourceId, packageName, name, LifeCycleEventType.CREATED);
	}

	/**
	 * Factory method for a new "REMOVE" event. 
	 * Builds a new "REMOVE" event with the given data.
	 * 
	 * @param resourceId resource identifier.
	 * @param packageName resource package name.
	 * @param name resource name.
	 */
	public static LifeCycleEvent newRemoveEvent(String resourceId, String packageName, String name)
	{
		return new LifeCycleEvent(resourceId, packageName, name, LifeCycleEventType.REMOVED);
	}

	/**
	 * Returns an XML representation of this event.
	 * 
	 * @return an XML representation of this event.
	 */
	public Element toXML()
	{
		return toXML(XmlUtils.EMPTY_DOC);
	}

	/**
	 * Returns an XML representation of this event using the given 
	 * input document as owner.
	 * 
	 * @return an XML representation of this event.
	 */
	public Element toXML(Document factory)
	{
		Element lifeCycleEvent = XmlUtils.createElement(factory, Names.LIFECYCLE_EVENT_QNAME);
		
		lifeCycleEvent.setAttribute(
				"Type", 
				_type.toString());
		
		lifeCycleEvent.setAttribute(
				Names.TIMEMILLIS_ATTRIBUTE_NAME, 
				String.valueOf(System.currentTimeMillis()));

		Element resource = XmlUtils.createElement(factory, Names.RESOURCE_QNAME);
		lifeCycleEvent.appendChild(resource);
		
		Element id = XmlUtils.createElement(factory, Names.RES_ID_QNAME);
		id.setTextContent(_resourceId);
		resource.appendChild(id);
		
		Element packageName = XmlUtils.createElement(factory, Names.PACKAGE_NAME_QNAME);
		packageName.setTextContent(_packageName);
		resource.appendChild(packageName);
		
		Element name = XmlUtils.createElement(factory, Names.ENTITY_NAME_QNAME);
		name.setTextContent(_name);
		resource.appendChild(name);
		
		return lifeCycleEvent;
	}
}