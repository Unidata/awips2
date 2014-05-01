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

import java.lang.management.ManagementFactory;
import java.lang.reflect.Method;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import javax.management.InstanceNotFoundException;
import javax.management.MBeanServer;
import javax.management.Notification;
import javax.management.NotificationFilter;
import javax.management.NotificationListener;
import javax.management.ObjectName;
import javax.xml.namespace.QName;

import org.apache.muse.core.AbstractCapability;
import org.apache.muse.core.Resource;
import org.apache.muse.core.ResourceManager;
import org.apache.muse.core.routing.MessageHandler;
import org.apache.muse.core.serializer.SerializerRegistry;
import org.apache.muse.ws.addressing.EndpointReference;
import org.apache.muse.ws.addressing.soap.SoapFault;
import org.apache.muse.ws.notification.NotificationProducer;
import org.apache.muse.ws.notification.WsnConstants;
import org.apache.qpid.management.Messages;
import org.apache.qpid.management.Names;
import org.apache.qpid.management.configuration.Configuration;
import org.apache.qpid.management.jmx.EntityLifecycleNotification;
import org.apache.qpid.management.wsdm.common.ThreadSessionManager;
import org.apache.qpid.management.wsdm.common.UnableToConnectWithBrokerFault;
import org.apache.qpid.management.wsdm.muse.engine.WSDMAdapterEnvironment;
import org.apache.qpid.management.wsdm.muse.serializer.ByteArraySerializer;
import org.apache.qpid.management.wsdm.notifications.LifeCycleEvent;
import org.apache.qpid.transport.util.Logger;

/**
 * QMan Adapter capability.
 * Basically it acts as a lifecycle manager of all ws resource that correspond to entities on JMX side.
 * 
 * @author Andrea Gazzarini
*/
@SuppressWarnings("serial")
public class QManAdapterCapability extends AbstractCapability
{	
	private final static Logger LOGGER = Logger.get(QManAdapterCapability.class);

	private MBeanServer _mxServer;
	private WsArtifactsFactory _artifactsFactory; 
	private URI _resourceURI;
	private NotificationProducer _publisherCapability;
	private ThreadPoolExecutor _workManager;
	private Map<String, QName> _lifeCycleTopics = new HashMap<String, QName>();
	
	/**
	 * Runnable wrapper used for sending asynchronous 
	 * notifications.
	 * 
	 * @author Andrea Gazzarini
	 */
	private final class AsynchNotificationTask implements Runnable 
	{
		private final QName topicName;
		private final LifeCycleEvent event;
		
		AsynchNotificationTask(QName tName, LifeCycleEvent evt)
		{
			topicName = tName;
			event = evt;
		}
		
		public void run()
		{
			try
			{
				_publisherCapability.publish(topicName,event);
			} catch (SoapFault exception)
			{
				LOGGER.error(
						exception,
						Messages.QMAN_100038_UNABLE_TO_SEND_WS_NOTIFICATION);
			}			
		}
	};
	
	/**
	 * NotificationFilter for "create" only events.
	 */
	private final NotificationFilter _filterForNewInstances = new NotificationFilter(){

		/**
		 * Returns true when the notification is related to a creation of a new instance. 
		 * 
		 * @return true when the notification is related to a creation of a new instance.
		 */
		public boolean isNotificationEnabled(Notification notification)
		{
			return EntityLifecycleNotification.INSTANCE_ADDED_NOTIFICATION_TYPE.equals(notification.getType());
		}
		
	};

	/**
	 * NotificationFilter for "remove" only events.
	 */
	private final NotificationFilter _filterForRemovedInstances = new NotificationFilter(){

		/**
		 * Returns true when the notification is related to a deletion of an existing instance. 
		 * 
		 * @return true when the notification is related to a deletion of an existing instance.
		 */
		public boolean isNotificationEnabled(Notification notification)
		{
			return EntityLifecycleNotification.INSTANCE_REMOVED_NOTIFICATION_TYPE.equals(notification.getType());
		}
	};
	
	/**
	 * This listener handles "create" mbean events and therefore provides procedure to create and initialize
	 * corresponding ws resources.
	 */
	private final NotificationListener _listenerForNewInstances = new NotificationListener() 
	{
		/**
		 * Handles JMX "create" notification type.
		 * 
		 * @param notification the entity lifecycle notification.
		 * @param data user data associated with the incoming notifiication : it is not used at the moment.
		 */
		public void handleNotification(Notification notification, Object data) 
		{
			ObjectName eventSourceName = null;
			try 
			{				
				EntityLifecycleNotification lifecycleNotification = (EntityLifecycleNotification) notification;
				eventSourceName = lifecycleNotification.getObjectName();
				
				ThreadSessionManager.getInstance().getSession().setObjectName(eventSourceName);
			
				LOGGER.debug(Messages.QMAN_200039_DEBUG_JMX_NOTIFICATION, notification);

				ResourceManager resourceManager = getResource().getResourceManager();
				Resource resource = resourceManager.createResource(Names.QMAN_RESOURCE_NAME);
				
				WsArtifacts artifacts = _artifactsFactory.getArtifactsFor(resource,eventSourceName);
				MBeanCapability capability = _artifactsFactory.createCapability(
						artifacts.getCapabilityClass(), 
						eventSourceName);
				
				ThreadSessionManager.getInstance().getSession().setWsdlDocument(artifacts.getWsdl());
				ThreadSessionManager.getInstance().getSession().setResourceMetadataDescriptor(artifacts.getResourceMetadataDescriptor());
				
				resource.setWsdlPortType(Names.QMAN_RESOURCE_PORT_TYPE_NAME);
				capability.setCapabilityURI(Names.NAMESPACE_URI+"/"+capability.getClass().getSimpleName());
				capability.setMessageHandlers(createMessageHandlers(capability));
				
				resource.addCapability(capability);
				resource.initialize();
				resourceManager.addResource(resource.getEndpointReference(), resource);
				
				LOGGER.info(
						Messages.QMAN_000030_RESOURCE_HAS_BEEN_CREATED,
						eventSourceName);
				
				AsynchNotificationTask asynchNotificationTask = new AsynchNotificationTask(
						getTopicName(lifecycleNotification.getClassKind()),
						LifeCycleEvent.newCreateEvent(
								eventSourceName.getKeyProperty(Names.OBJECT_ID), 
								lifecycleNotification.getPackageName(),
								lifecycleNotification.getClassName()));
				
				_workManager.execute(asynchNotificationTask);
				
			} catch (ArtifactsNotAvailableException exception) 
			{
				LOGGER.error(
						exception,
						Messages.QMAN_100023_BUILD_WS_ARTIFACTS_FAILURE);
			} catch (IllegalAccessException exception) 
			{
				LOGGER.error(
						exception,
						Messages.QMAN_100024_CAPABILITY_INSTANTIATION_FAILURE,
						eventSourceName);
			} catch (InstantiationException exception) 
			{
				LOGGER.error(
						exception,
						Messages.QMAN_100024_CAPABILITY_INSTANTIATION_FAILURE,
						eventSourceName);
			} catch (SoapFault exception) 
			{
				LOGGER.error(
						exception,Messages.QMAN_100025_WSRF_FAILURE,
						eventSourceName);	
			} catch (Exception exception) 
			{
				LOGGER.error(
						exception,
						Messages.QMAN_100025_WSRF_FAILURE,
						eventSourceName);	
			} 
		}
	};
	
	/**
	 * This listener handles "remove" mbean events and therefore provides procedure to shutdown and remove
	 * corresponding ws resources.
	 */
	private final NotificationListener _listenerForRemovedInstances = new NotificationListener() 
	{
		/**
		 * Handles JMX "remove" notification type.
		 * 
		 * @param notification the entity lifecycle notification.
		 * @param data user data associated with the incoming notifiication : it is not used at the moment.
		 */
		public void handleNotification(Notification notification, Object data) 
		{
			EntityLifecycleNotification lifecycleNotification = (EntityLifecycleNotification) notification;
			ObjectName eventSourceName = lifecycleNotification.getObjectName();

			LOGGER.debug(Messages.QMAN_200042_REMOVING_RESOURCE, eventSourceName);

			EndpointReference endpointPointReference = new EndpointReference(_resourceURI);			
			endpointPointReference.addParameter(
					Names.RESOURCE_ID_QNAME, 
					eventSourceName.getKeyProperty(Names.OBJECT_ID));
			
			ResourceManager resourceManager = getResource().getResourceManager();
			try 
			{
				Resource resource = resourceManager.getResource(endpointPointReference);
				resource.shutdown();
					
				LOGGER.info(
						Messages.QMAN_000031_RESOURCE_HAS_BEEN_REMOVED, 
						eventSourceName);

				AsynchNotificationTask asynchNotificationTask = new AsynchNotificationTask(
						getTopicName(lifecycleNotification.getClassKind()),
						LifeCycleEvent.newRemoveEvent(
								eventSourceName.getKeyProperty(Names.OBJECT_ID), 
								lifecycleNotification.getPackageName(),
								lifecycleNotification.getClassName()));
				
				_workManager.execute(asynchNotificationTask);

			}
			catch(Exception exception) 
			{
				LOGGER.error(
						exception, 
						Messages.QMAN_100027_RESOURCE_SHUTDOWN_FAILURE, 
						eventSourceName);
			}
		}
	};	
			
	/**
	 * Initializes this capability.
	 * 
	 * @throws SoapFault when the initialization fails..
	 */
	@Override
	public void initialize() throws SoapFault 
	{
		super.initialize();
		
		registerByteArraySerializer();
		
		createLifeCycleTopics();
		
		initializeWorkManager();
		
		createQManResourceURI();

		_mxServer = ManagementFactory.getPlatformMBeanServer();
		_artifactsFactory = new WsArtifactsFactory(getEnvironment(),_mxServer);
		
		registerQManLifecycleListeners();	
	}

	/**
	 * Connects QMan with a broker with the given connection data.
	 * 
	 * @param host the host where the broker is running.
	 * @param port the port number where the broker is running.
	 * @param username username for estabilshing connection.
	 * @param password password for estabilshing connection.
	 * @param virtualHost the virtualHost name.
	 * @param initialPoolCapacity the initial size of broker connection pool. 
	 * @param maxPoolCapacity the max allowed size of broker connection pool.
	 * @param maxWaitTimeout the max wait timeout for retrieving connections.
	 * @throws SoapFault when the connection with broker cannot be estabilished.
	 */
	@SuppressWarnings("unchecked")
	public void connect(
			String host, 
			int port, 
			String username, 
			String password, 
			String virtualHost,
			int initialPoolCapacity,
			int maxPoolCapacity, 
			long maxWaitTimeout) throws SoapFault 
	{
		try 
		{
			_mxServer.invoke(
					Names.QMAN_OBJECT_NAME, 
					"addBroker", 
					new Object[]{host,port,username,password,virtualHost,initialPoolCapacity,maxPoolCapacity,maxWaitTimeout}, 
					new String[]{
							String.class.getName(),
							int.class.getName(),
							String.class.getName(),
							String.class.getName(),
							String.class.getName(),
							int.class.getName(),
							int.class.getName(),
							long.class.getName()});
		} catch(Exception exception)
		{			
			LOGGER.error(Messages.QMAN_100017_UNABLE_TO_CONNECT,host,port);
			throw new UnableToConnectWithBrokerFault(
					getResource().getEndpointReference(),
					host,
					port,
					username,
					virtualHost,
					exception.getMessage());
		}
	}
		
	/**
	 * Creates the message handlers for the given capability.
	 * 
	 * @param capability the QMan capability.
	 * @return a collection with message handlers for the given capability.
	 */
	protected Collection<MessageHandler> createMessageHandlers(MBeanCapability capability)
	{
        Collection<MessageHandler> handlers = new ArrayList<MessageHandler>();
        
        for (Method method :  capability.getClass().getDeclaredMethods())
        {
        	String name = method.getName();
        	
        	QName requestName = new QName(
        			Names.NAMESPACE_URI,
        			name,
        			Names.PREFIX);
        	
        	QName returnValueName = new QName(
        			Names.NAMESPACE_URI,
        			name+"Response",
        			Names.PREFIX);
        	
        	String actionURI = Names.NAMESPACE_URI+"/"+name;
            
        	MessageHandler handler = new QManMessageHandler(
            		actionURI, 
            		requestName, 
            		returnValueName);
            
        	handler.setMethod(method);
            handlers.add(handler);
        }
        return handlers;	
    }

	/**
	 * Returns the publisher capability associated with the owner resource.
	 * 
	 * @return the publisher capability associated with the owner resource.
	 */
	NotificationProducer getPublisherCapability()
	{
		return (NotificationProducer) getResource().getCapability(WsnConstants.PRODUCER_URI);
	}
	
	/**
	 * Creates events & objects lifecycle topic that will be used to publish lifecycle event
	 * messages..
	 */
	void createLifeCycleTopics() 
	{
		try 
		{
			_publisherCapability = getPublisherCapability();
			
			_publisherCapability.addTopic(Names.EVENTS_LIFECYLE_TOPIC_NAME);
			_lifeCycleTopics.put(Names.EVENT,Names.EVENTS_LIFECYLE_TOPIC_NAME);

			LOGGER.info(
					Messages.QMAN_000032_EVENTS_LIFECYCLE_TOPIC_HAS_BEEN_CREATED, 
					Names.OBJECTS_LIFECYLE_TOPIC_NAME);
			
			_publisherCapability.addTopic(Names.OBJECTS_LIFECYLE_TOPIC_NAME);		
			_lifeCycleTopics.put(Names.CLASS,Names.OBJECTS_LIFECYLE_TOPIC_NAME);

			LOGGER.info(
					Messages.QMAN_000033_OBJECTS_LIFECYCLE_TOPIC_HAS_BEEN_CREATED, 
					Names.OBJECTS_LIFECYLE_TOPIC_NAME);
			
			_publisherCapability.addTopic(Names.UNKNOWN_OBJECT_TYPE_LIFECYLE_TOPIC_NAME);					
			LOGGER.info(
					Messages.QMAN_000034_UNCLASSIFIED_LIFECYCLE_TOPIC_HAS_BEEN_CREATED, 
					Names.OBJECTS_LIFECYLE_TOPIC_NAME);
		} catch(Exception exception) 
		{
			LOGGER.error(exception, Messages.QMAN_100036_TOPIC_DECLARATION_FAILURE);
		}
	}
	
	/**
	 * Starting from an object type (i.e. event or class) returns the name of the
	 * corresponding topic where the lifecycle message must be published.
	 * Note that if the given object type is unknown then the "Unclassified Object Types" topic 
	 * will be returned (and therefore the message will be published there).
	 * 
	 * @param objectType the type of the object.
	 * @return the name of the topic associated with the given object type.
	 */
	QName getTopicName(String objectType) 
	{
		QName topicName = _lifeCycleTopics.get(objectType);
		return (topicName != null) 
			? topicName 
			: Names.UNKNOWN_OBJECT_TYPE_LIFECYLE_TOPIC_NAME;
	}
	
	/** 
	 * Workaround : it seems that is not possibile to declare a serializer 
	 * for a byte array using muse descriptor...
	*	What is the stringified name of the class? 
	*	byte[].getClass().getName() is [B but is not working (ClassNotFound).
	* 	So, at the end, this is hard-coded here!
	*/
	private void registerByteArraySerializer()
	{
		SerializerRegistry.getInstance().registerSerializer(
				byte[].class, 
				new ByteArraySerializer());		
	}
	
	/**
	 * Creates the URI that will be later used to identify a QMan WS-Resource.
	 * Note that the resources that will be created are identified also with their resource id.
	 * Briefly we could say that this is the soap:address of the WS-Resource definition.
	 * 
	 * @throws SoapFault when the URI cannot be built (probably it is malformed).
	 */
	private void createQManResourceURI() throws SoapFault
	{
		WSDMAdapterEnvironment environment = (WSDMAdapterEnvironment) getEnvironment();
		String resourceURI = environment.getDefaultURIPrefix()+Names.QMAN_RESOURCE_NAME;
		try 
		{
			_resourceURI = URI.create(resourceURI);
			
		} catch(IllegalArgumentException exception)
		{
			LOGGER.info(
					exception,
					Messages.QMAN_100029_MALFORMED_RESOURCE_URI_FAILURE,
					resourceURI);			
			throw new SoapFault(exception);
		}
	}	
	
	/**
	 * Initializes the work manager used for asynchronous notifications.
	 */
	private void initializeWorkManager()
	{
		Configuration configuration = Configuration.getInstance();
		_workManager = new ThreadPoolExecutor(
				configuration.getWorkerManagerPoolSize(),
				configuration.getWorkerManagerMaxPoolSize(),
				configuration.getWorkerManagerKeepAliveTime(),
				TimeUnit.MILLISECONDS,
				new ArrayBlockingQueue<Runnable>(30));
	}

	/**
	 * This adapter capability needs to be an event listener of QMan JMX core 
	 * in order to detect relevant lifecycle events and therefore create WS artifacts & notification(s).
	 * 
	 * @throws SoapFault when it's not possible to register event listener : is QMan running?
	 */
	@SuppressWarnings("serial")
	private void registerQManLifecycleListeners() throws SoapFault
	{
		try 
		{			
			_mxServer.addNotificationListener(
					Names.QMAN_OBJECT_NAME, 
					_listenerForNewInstances, 
					_filterForNewInstances, 
					null);
			
			_mxServer.addNotificationListener(
					Names.QMAN_OBJECT_NAME, 
					_listenerForRemovedInstances, 
					_filterForRemovedInstances, 
					null);

			try 
			{
				_mxServer.addNotificationListener(
						Names.QPID_EMULATOR_OBJECT_NAME, 
						_listenerForNewInstances, 
						_filterForNewInstances, null);

				_mxServer.addNotificationListener(
						Names.QPID_EMULATOR_OBJECT_NAME, 
						_listenerForRemovedInstances, 
						_filterForRemovedInstances, null);

			} catch (Exception exception) 
			{
				LOGGER.info(Messages.QMAN_000028_TEST_MODULE_NOT_FOUND);
			} 
		}  catch(InstanceNotFoundException exception) 
		{
			throw new SoapFault(exception);	
		}
	}	
}