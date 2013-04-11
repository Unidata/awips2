package org.rzo.netty.ahessian.application.jmx.remote.service;

import java.io.IOException;

import javax.management.Attribute;
import javax.management.AttributeList;
import javax.management.AttributeNotFoundException;
import javax.management.InstanceAlreadyExistsException;
import javax.management.InstanceNotFoundException;
import javax.management.IntrospectionException;
import javax.management.InvalidAttributeValueException;
import javax.management.ListenerNotFoundException;
import javax.management.MBeanException;
import javax.management.MBeanRegistration;
import javax.management.MBeanRegistrationException;
import javax.management.NotCompliantMBeanException;
import javax.management.NotificationFilter;
import javax.management.NotificationListener;
import javax.management.ObjectName;
import javax.management.QueryExp;
import javax.management.ReflectionException;

public interface AsyncMBeanServerConnection
{
	   /**
	    * Adds a NotificationListener to a registered MBean.
	    * A notification emitted by the specified source MBean will be forwarded by the MBeanServer to the given listener,
	    * if the given NotificationFilter allows so. If the filter is null, every notification will be sent to the
	    * listener.
	    * The handback object is transparently passed to the listener by the MBeanServer.
	    * The source of the notification is the source MBean ObjectName.
	    *
	    * @param observed The ObjectName of the source MBean on which the listener should be added.
	    * @param listener The listener which will handle the notifications emitted by the source MBean.
	    * @param filter   The filter which will allow the notification to be forwarded to the listener.
	    * @param handback The context to be sent to the listener when a notification is emitted.
	    * @throws InstanceNotFoundException If the source MBean is not registered in the MBeanServer.
	    * @throws IOException               If a communication problem occurred.
	    * @see #removeNotificationListener(ObjectName, NotificationListener, NotificationFilter, Object)
	    */
	   public Object addNotificationListener(ObjectName observed, NotificationListener listener, NotificationFilter filter, Object handback)
	           throws InstanceNotFoundException, IOException;

	   /**
	    * Adds a NotificationListener to a registered MBean.
	    * A notification emitted by the specified source MBean will be forwarded by the MBeanServer to the given listener MBean,
	    * if the given NotificationFilter allows so. If the filter is null, every notification will be sent to the
	    * listener.
	    * The handback object is transparently passed to the listener by the MBeanServer.
	    * The source of the notification is the source MBean ObjectName.
	    * If the listener MBean is unregistered, it will continue to receive notifications.
	    *
	    * @param observed The ObjectName of the source MBean on which the listener should be added.
	    * @param listener The ObjectName of the listener MBean which will handle the notifications emitted by the source MBean.
	    * @param filter   The filter which will allow the notification to be forwarded to the listener.
	    * @param handback The context to be sent to the listener when a notification is emitted.
	    * @throws InstanceNotFoundException If the source or listener MBean are not registered in the MBeanServer.
	    * @throws IOException               If a communication problem occurred.
	    * @see #removeNotificationListener(ObjectName, ObjectName, NotificationFilter, Object)
	    */
	   public Object addNotificationListener(ObjectName observed, ObjectName listener, NotificationFilter filter, Object handback)
	           throws InstanceNotFoundException, IOException;

	   /**
	    * Removes the specified listener from the named source MBean.
	    * If the listener is registered more than once, for example with different filters or handbacks,
	    * this method will remove all those registrations.
	    *
	    * @param observed The ObjectName of the source MBean on which the listener should be removed.
	    * @param listener The listener to be removed.
	    * @throws InstanceNotFoundException If the source MBean is not registered in the MBeanServer.
	    * @throws ListenerNotFoundException If the listener is not registered in the MBean.
	    * @throws IOException               If a communication problem occurred.
	    * @see #addNotificationListener(ObjectName, NotificationListener, NotificationFilter, Object)
	    */
	   public Object removeNotificationListener(ObjectName observed, NotificationListener listener)
	           throws InstanceNotFoundException, ListenerNotFoundException, IOException;

	   /**
	    * Removes the specified listener MBean from the named source MBean.
	    * If the listener is registered more than once, for example with different filters or handbacks,
	    * this method will remove all those registrations.
	    *
	    * @param observed The ObjectName of the source MBean on which the listener should be removed.
	    * @param listener The ObjectName of the listener MBean to be removed.
	    * @throws InstanceNotFoundException If the source or listener MBean are not registered in the MBeanServer.
	    * @throws ListenerNotFoundException The listener is not registered in the MBean.
	    * @throws IOException               If a communication problem occurred.
	    * @see #addNotificationListener(ObjectName, ObjectName, NotificationFilter, Object)
	    */
	   public Object removeNotificationListener(ObjectName observed, ObjectName listener)
	           throws InstanceNotFoundException, ListenerNotFoundException, IOException;

	   /**
	    * Removes the specified listener from the named source MBean.
	    * The MBean must have a listener that exactly matches the given listener, filter, and handback parameters.
	    *
	    * @param observed The ObjectName of the source MBean on which the listener should be removed.
	    * @param listener The listener to be removed.
	    * @param filter   The filter that was specified when the listener was added.
	    * @param handback The handback that was specified when the listener was added.
	    * @throws InstanceNotFoundException If the source MBean is not registered in the MBeanServer.
	    * @throws ListenerNotFoundException If the listener (along with filter and handback) is not registered in the MBean.
	    * @throws IOException               If a communication problem occurred.
	    * @see #addNotificationListener(ObjectName, NotificationListener, NotificationFilter, Object)
	    * @since JMX 1.2
	    */
	   public Object removeNotificationListener(ObjectName observed, NotificationListener listener, NotificationFilter filter, Object handback)
	           throws InstanceNotFoundException, ListenerNotFoundException, IOException;

	   /**
	    * Removes the specified listener MBean from the named source MBean.
	    * The MBean must have a listener that exactly matches the given listener, filter, and handback parameters.
	    *
	    * @param observed The ObjectName of the source MBean on which the listener should be removed.
	    * @param listener The ObjectName of the listener MBean to be removed.
	    * @param filter   The filter that was specified when the listener was added.
	    * @param handback The handback that was specified when the listener was added.
	    * @throws InstanceNotFoundException If the source MBean is not registered in the MBeanServer.
	    * @throws ListenerNotFoundException If the listener (along with filter and handback) is not registered in the MBean.
	    * @throws IOException               If a communication problem occurred.
	    * @see #addNotificationListener(ObjectName, NotificationListener, NotificationFilter, Object)
	    * @since JMX 1.2
	    */
	   public Object removeNotificationListener(ObjectName observed, ObjectName listener, NotificationFilter filter, Object handback)
	           throws InstanceNotFoundException, ListenerNotFoundException, IOException;

	   /**
	    * Returns the metadata information exposed for management about the named MBean.
	    *
	    * @param objectName The name of the MBean for which retrieve the metadata.
	    * @return An instance of MBeanInfo allowing the retrieval of constructors, attributes, operations and notifications of this MBean.
	    * @throws IntrospectionException    If an exception occured during introspection of the MBean.
	    * @throws InstanceNotFoundException If the named MBean is not registered in the MBeanServer.
	    * @throws ReflectionException       If a reflection-type exception occurred
	    * @throws IOException               If a communication problem occurred.
	    */
	   public Object getMBeanInfo(ObjectName objectName)
	           throws InstanceNotFoundException, IntrospectionException, ReflectionException, IOException;

	   /**
	    * Returns whether the MBean specified is an instance of the specified class.
	    *
	    * @param objectName The ObjectName of the MBean.
	    * @param className  The name of the class.
	    * @return True if the MBean specified is an instance of the specified class.
	    * @throws InstanceNotFoundException If the named MBean is not registered in the MBeanServer.
	    * @throws IOException               If a communication problem occurred.
	    */
	   public Object isInstanceOf(ObjectName objectName, String className)
	           throws InstanceNotFoundException, IOException;

	   /**
	    * Returns the list of different ObjectName domains under which the MBeans in this MBeanServer are registered.
	    *
	    * @return The array of different ObjectName domains present in this MBeanServer.
	    * @throws IOException If a communication problem occurred.
	    * @since JMX 1.2
	    */
	   public Object getDomains()
	           throws IOException;

	   /**
	    * Returns the default domain for this MBeanServer used in case ObjectName domain are not specified.
	    *
	    * @return The default domain of this MBeanServer.
	    * @throws IOException If a communication problem occurred.
	    */
	   public Object getDefaultDomain()
	           throws IOException;

	   /**
	    * A facility method for <code>createMBean(className, objectName, null, null)</code>.
	    *
	    * @see #createMBean(String, ObjectName, Object[], String[])
	    */
	   public Object createMBean(String className, ObjectName objectName)
	           throws ReflectionException, InstanceAlreadyExistsException, MBeanRegistrationException, MBeanException, NotCompliantMBeanException, IOException;

	   /**
	    * Instantiates and registers an MBean of the specified class with the given ObjectName in the MBeanServer.
	    * The MBeanServer will use its ClassLoaderRepository to load the class of the MBean and the specified
	    * constructor's parameter classes, and creates the instance passing the specified arguments.
	    * The ObjectName may be null if the MBean implements {@link MBeanRegistration}
	    *
	    * @param className  The class name of the MBean to be instantiated.
	    * @param objectName The ObjectName of the MBean, may be null.
	    * @param args       An array containing the arguments to pass to the constructor.
	    * @param parameters An array containing the signature of the constructor.
	    * @return An ObjectInstance, containing the ObjectName and the Java class name of the newly instantiated MBean.
	    * @throws ReflectionException            If a reflection exception is thrown.
	    * @throws InstanceAlreadyExistsException If another MBean with the same ObjectName is already registered in the MBeanServer.
	    * @throws MBeanRegistrationException     If an exception is thrown during MBean's registration.
	    * @throws MBeanException                 If the constructor of the MBean has thrown an exception
	    * @throws NotCompliantMBeanException     If the MBean is not a JMX compliant MBean
	    * @throws IOException                    If a communication problem occurred.
	    */
	   public Object createMBean(String className, ObjectName objectName, Object[] args, String[] parameters)
	           throws ReflectionException, InstanceAlreadyExistsException, MBeanRegistrationException, MBeanException, NotCompliantMBeanException, IOException;

	   /**
	    * A facility method for <code>createMBean(className, objectName, loaderName, null, null)</code>.
	    *
	    * @see #createMBean(String, ObjectName, ObjectName, Object[], String[])
	    */
	   public Object createMBean(String className, ObjectName objectName, ObjectName loaderName)
	           throws ReflectionException, InstanceAlreadyExistsException, MBeanRegistrationException, MBeanException, NotCompliantMBeanException, InstanceNotFoundException, IOException;

	   /**
	    * Instantiates and registers an MBean of the specified class with the given ObjectName in the MBeanServer.
	    * The MBeanServer will use the specified classloader MBean to load the class of the MBean and the specified
	    * constructor's parameter classes, and creates the instance passing the specified arguments, or the classloader
	    * of the MBeanServer if the classloader ObjectName is null.
	    * The ObjectName may be null if the MBean implements {@link MBeanRegistration}
	    *
	    * @param className  The class name of the MBean to be instantiated.
	    * @param objectName The ObjectName of the MBean, may be null.
	    * @param loaderName The ObjectName of the classloader MBean to be used.
	    * @param args       An array containing the arguments to pass to the constructor.
	    * @param parameters An array containing the signature of the constructor.
	    * @return An ObjectInstance, containing the ObjectName and the Java class name of the newly instantiated MBean.
	    * @throws ReflectionException            If a reflection exception is thrown.
	    * @throws InstanceAlreadyExistsException If another MBean with the same ObjectName is already registered in the MBeanServer.
	    * @throws MBeanRegistrationException     If an exception is thrown during MBean's registration.
	    * @throws MBeanException                 If the constructor of the MBean has thrown an exception
	    * @throws NotCompliantMBeanException     If the MBean is not a JMX compliant MBean
	    * @throws InstanceNotFoundException      If the specified classloader MBean is not registered in the MBeanServer.
	    * @throws IOException                    If a communication problem occurred.
	    */
	   public Object createMBean(String className, ObjectName objectName, ObjectName loaderName, Object[] args, String[] parameters)
	           throws ReflectionException, InstanceAlreadyExistsException, MBeanRegistrationException, MBeanException, NotCompliantMBeanException, InstanceNotFoundException, IOException;

	   /**
	    * Unregisters the MBean with the specified ObjectName from this MBeanServer.
	    *
	    * @param objectName The ObjectName of the MBean to be unregistered.
	    * @throws InstanceNotFoundException  If the specified MBean is not registered in the MBeanServer.
	    * @throws MBeanRegistrationException If an exception is thrown during MBean's unregistration.
	    * @throws IOException                If a communication problem occurred.
	    */
	   public Object unregisterMBean(ObjectName objectName)
	           throws InstanceNotFoundException, MBeanRegistrationException, IOException;

	   /**
	    * Gets the value of the specified attribute of the named MBean.
	    *
	    * @param objectName The ObjectName of the MBean from which the attribute is to be retrieved.
	    * @param attribute  The attribute name.
	    * @return The value of the specified attribute.
	    * @throws AttributeNotFoundException If the specified attribute does not belong to the management interface of the MBean.
	    * @throws MBeanException             If the MBean's getter method throws an exception.
	    * @throws InstanceNotFoundException  If the specified MBean is not registered in the MBeanServer.
	    * @throws ReflectionException        If a reflection exception is thrown.
	    * @throws IOException                If a communication problem occurred.
	    * @see #setAttribute
	    */
	   public Object getAttribute(ObjectName objectName, String attribute)
	           throws MBeanException, AttributeNotFoundException, InstanceNotFoundException, ReflectionException, IOException;

	   /**
	    * Sets the value of the specified attribute of the named MBean.
	    *
	    * @param objectName The name of the MBean within which the attribute is to be set.
	    * @param attribute  The Attribute to be set.
	    * @throws InstanceNotFoundException      If the specified MBean is not registered in the MBeanServer.
	    * @throws AttributeNotFoundException     If the specified attribute does not belong to the management interface of the MBean.
	    * @throws InvalidAttributeValueException If the value specified for the attribute does not match the attribute's type
	    * @throws MBeanException                 If the MBean's setter method throws an exception.
	    * @throws ReflectionException            If a reflection exception is thrown.
	    * @throws IOException                    If a communication problem occurred.
	    * @see #getAttribute
	    */
	   public Object setAttribute(ObjectName objectName, Attribute attribute)
	           throws InstanceNotFoundException, AttributeNotFoundException, InvalidAttributeValueException, MBeanException, ReflectionException, IOException;

	   /**
	    * Gets the values of several attributes of the named MBean.
	    *
	    * @param objectName The ObjectName of the MBean from which the attributes are to be retrieved.
	    * @param attributes The attribute names.
	    * @return An AttributeList containing the values of the attributes that it has been possible to retrieve.
	    * @throws InstanceNotFoundException If the specified MBean is not registered in the MBeanServer.
	    * @throws ReflectionException       If a reflection exception is thrown.
	    * @throws IOException               If a communication problem occurred.
	    * @see #setAttributes
	    */
	   public Object getAttributes(ObjectName objectName, String[] attributes)
	           throws InstanceNotFoundException, ReflectionException, IOException;

	   /**
	    * Sets the values of several attributes of the named MBean.
	    *
	    * @param objectName The name of the MBean within which the attribute is to be set.
	    * @param attributes The AttributeList containing the Attributes to be set.
	    * @return The AttributeList containing the attributes that it has been possible to set.
	    * @throws InstanceNotFoundException If the specified MBean is not registered in the MBeanServer.
	    * @throws ReflectionException       If a reflection exception is thrown.
	    * @throws IOException               If a communication problem occurred.
	    * @see #getAttributes
	    */
	   public Object setAttributes(ObjectName objectName, AttributeList attributes)
	           throws InstanceNotFoundException, ReflectionException, IOException;

	   /**
	    * Invokes the specified operation on the named MBean.
	    *
	    * @param objectName The ObjectName of the MBean on which the method is to be invoked.
	    * @param methodName The name of the operation to be invoked.
	    * @param args       An array containing the arguments to pass to the operation.
	    * @param parameters An array containing the signature of the operation.
	    * @return The return value of the operation, or null if the operation returns void.
	    * @throws InstanceNotFoundException If the specified MBean is not registered in the MBeanServer.
	    * @throws MBeanException            If the MBean's operation method throws an exception.
	    * @throws ReflectionException       If a reflection exception is thrown.
	    * @throws IOException               If a communication problem occurred.
	    */
	   public Object invoke(ObjectName objectName, String methodName, Object[] args, String[] parameters)
	           throws InstanceNotFoundException, MBeanException, ReflectionException, IOException;

	   /**
	    * Returns the number of MBeans registered in this MBeanServer.
	    *
	    * @throws IOException If a communication problem occurred.
	    */
	   public Object getMBeanCount()
	           throws IOException;

	   /**
	    * Checks whether the given ObjectName identifies an MBean registered in this MBeanServer.
	    *
	    * @param objectName The ObjectName to be checked.
	    * @return True if an MBean with the specified ObjectName is already registered in the MBeanServer.
	    * @throws IOException If a communication problem occurred.
	    */
	   public Object isRegistered(ObjectName objectName)
	           throws IOException;

	   /**
	    * Gets the ObjectInstance for the named MBean registered with the MBeanServer.
	    *
	    * @param objectName The ObjectName of the MBean.
	    * @return The ObjectInstance associated with the named MBean.
	    * @throws InstanceNotFoundException If the specified MBean is not registered in the MBeanServer.
	    * @throws IOException               If a communication problem occurred.
	    */
	   public Object getObjectInstance(ObjectName objectName)
	           throws InstanceNotFoundException, IOException;

	   /**
	    * Gets a subset of the ObjectInstances belonging to MBeans registered in this MBeanServer.
	    * It is possible to filter the set of MBeans by specifying a pattern for MBean's ObjectNames, and a query expression
	    * to be evaluated to further filter the set of MBeans.
	    * The set can be further restricted if any exception is thrown during retrieval of MBean (for example for
	    * security reasons): the failing MBean will not be included.
	    *
	    * @param patternName The ObjectName pattern identifying the MBeans to be retrieved, or null to retrieve all MBeans.
	    * @param filter      The query expression to be evaluated for selecting MBeans, or null.
	    * @return A set containing the ObjectInstance objects for the selected MBeans.
	    * @throws IOException If a communication problem occurred.
	    */
	   public Object queryMBeans(ObjectName patternName, QueryExp filter)
	           throws IOException;

	   /**
	    * Gets a subset of the ObjectNames belonging to MBeans registered in this MBeanServer.
	    * It is possible to filter the set of MBeans by specifying a pattern for MBean's ObjectNames, and a query expression
	    * to be evaluated to further filter the set of MBeans.
	    * The set can be further restricted if any exception is thrown during retrieval of MBean (for example for
	    * security reasons): the failing MBean will not be included.
	    *
	    * @param patternName The ObjectName pattern identifying the MBeans to be retrieved, or null to retrieve all MBeans.
	    * @param filter      The query expression to be evaluated for selecting MBeans, or null.
	    * @return A set containing the ObjectNames for the selected MBeans.
	    * @throws IOException If a communication problem occurred.
	    */
	   public Object queryNames(ObjectName patternName, QueryExp filter)
	           throws IOException;
}
