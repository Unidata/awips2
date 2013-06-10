package org.rzo.netty.ahessian.application.jmx.remote.service;

import java.io.IOException;
import java.util.Set;

import javax.management.Attribute;
import javax.management.AttributeList;
import javax.management.AttributeNotFoundException;
import javax.management.InstanceAlreadyExistsException;
import javax.management.InstanceNotFoundException;
import javax.management.IntrospectionException;
import javax.management.InvalidAttributeValueException;
import javax.management.ListenerNotFoundException;
import javax.management.MBeanException;
import javax.management.MBeanInfo;
import javax.management.MBeanRegistrationException;
import javax.management.MBeanServerConnection;
import javax.management.NotCompliantMBeanException;
import javax.management.NotificationFilter;
import javax.management.NotificationListener;
import javax.management.ObjectInstance;
import javax.management.ObjectName;
import javax.management.QueryExp;
import javax.management.ReflectionException;

public class MBeanServerConnectionAsyncAdapter implements MBeanServerConnection
{
	AsyncMBeanServerConnection root;
	public MBeanServerConnectionAsyncAdapter(AsyncMBeanServerConnection root)
	{
		this.root = root;
	}
	
	public void addNotificationListener(ObjectName name, NotificationListener listener, NotificationFilter filter, Object handback)
			throws InstanceNotFoundException, IOException
	{
		root.addNotificationListener(name, listener, filter, handback);
	}

	public void addNotificationListener(ObjectName name, ObjectName listener, NotificationFilter filter, Object handback)
			throws InstanceNotFoundException, IOException
	{
		root.addNotificationListener(name, listener, filter, handback);
	}

	public ObjectInstance createMBean(String className, ObjectName name) throws ReflectionException, InstanceAlreadyExistsException,
			MBeanRegistrationException, MBeanException, NotCompliantMBeanException, IOException
	{
		return (ObjectInstance) root.createMBean(className, name);
	}

	public ObjectInstance createMBean(String className, ObjectName name, ObjectName loaderName) throws ReflectionException,
			InstanceAlreadyExistsException, MBeanRegistrationException, MBeanException, NotCompliantMBeanException, InstanceNotFoundException,
			IOException
	{
		return (ObjectInstance) root.createMBean(className, name, loaderName);
	}

	public ObjectInstance createMBean(String className, ObjectName name, Object[] params, String[] signature) throws ReflectionException,
			InstanceAlreadyExistsException, MBeanRegistrationException, MBeanException, NotCompliantMBeanException, IOException
	{
		return (ObjectInstance) root.createMBean(className, name, params, signature);
	}

	public ObjectInstance createMBean(String className, ObjectName name, ObjectName loaderName, Object[] params, String[] signature)
			throws ReflectionException, InstanceAlreadyExistsException, MBeanRegistrationException, MBeanException, NotCompliantMBeanException,
			InstanceNotFoundException, IOException
	{
		return (ObjectInstance) root.createMBean(className, name, loaderName, params, signature);
	}

	public Object getAttribute(ObjectName name, String attribute) throws MBeanException, AttributeNotFoundException, InstanceNotFoundException,
			ReflectionException, IOException
	{
		return root.getAttribute(name, attribute);
	}

	public AttributeList getAttributes(ObjectName name, String[] attributes) throws InstanceNotFoundException, ReflectionException, IOException
	{
		return (AttributeList) root.getAttributes(name, attributes);
	}

	public String getDefaultDomain() throws IOException
	{
		return (String) root.getDefaultDomain();
	}

	public String[] getDomains() throws IOException
	{
		return (String[]) root.getDomains();
	}

	public Integer getMBeanCount() throws IOException
	{
		return (Integer) root.getMBeanCount();
	}

	public MBeanInfo getMBeanInfo(ObjectName name) throws InstanceNotFoundException, IntrospectionException, ReflectionException, IOException
	{
		return (MBeanInfo) root.getMBeanInfo(name);
	}

	public ObjectInstance getObjectInstance(ObjectName name) throws InstanceNotFoundException, IOException
	{
		return (ObjectInstance)root.getObjectInstance(name);
	}

	public Object invoke(ObjectName name, String operationName, Object[] params, String[] signature) throws InstanceNotFoundException,
			MBeanException, ReflectionException, IOException
	{
		return root.invoke(name, operationName, params, signature);
	}

	public boolean isInstanceOf(ObjectName name, String className) throws InstanceNotFoundException, IOException
	{
		return ((Boolean)root.isInstanceOf(name, className)).booleanValue();
	}

	public boolean isRegistered(ObjectName name) throws IOException
	{
		return ((Boolean)root.isRegistered(name)).booleanValue();
	}

	public Set<ObjectInstance> queryMBeans(ObjectName name, QueryExp query) throws IOException
	{
		return (Set<ObjectInstance>)root.queryMBeans(name, query);
	}

	public Set<ObjectName> queryNames(ObjectName name, QueryExp query) throws IOException
	{
		return (Set<ObjectName>)root.queryNames(name, query);
	}

	public void removeNotificationListener(ObjectName name, ObjectName listener) throws InstanceNotFoundException, ListenerNotFoundException,
			IOException
	{
		root.removeNotificationListener(name, listener);
	}

	public void removeNotificationListener(ObjectName name, NotificationListener listener) throws InstanceNotFoundException,
			ListenerNotFoundException, IOException
	{
		root.removeNotificationListener(name, listener);
	}

	public void removeNotificationListener(ObjectName name, ObjectName listener, NotificationFilter filter, Object handback)
			throws InstanceNotFoundException, ListenerNotFoundException, IOException
	{
		root.removeNotificationListener(name, listener, filter, handback);
	}

	public void removeNotificationListener(ObjectName name, NotificationListener listener, NotificationFilter filter, Object handback)
			throws InstanceNotFoundException, ListenerNotFoundException, IOException
	{
		root.removeNotificationListener(name, listener, filter, handback);
	}

	public void setAttribute(ObjectName name, Attribute attribute) throws InstanceNotFoundException, AttributeNotFoundException,
			InvalidAttributeValueException, MBeanException, ReflectionException, IOException
	{
		root.setAttribute(name, attribute);
	}

	public AttributeList setAttributes(ObjectName name, AttributeList attributes) throws InstanceNotFoundException, ReflectionException, IOException
	{
		return (AttributeList)root.setAttributes(name, attributes);
	}

	public void unregisterMBean(ObjectName name) throws InstanceNotFoundException, MBeanRegistrationException, IOException
	{
		root.unregisterMBean(name);
	}

}
