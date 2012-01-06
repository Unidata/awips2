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
package org.apache.qpid.management.domain.services;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import javax.management.Attribute;
import javax.management.AttributeList;
import javax.management.DynamicMBean;
import javax.management.MBeanException;
import javax.management.MBeanInfo;
import javax.management.MBeanOperationInfo;
import javax.management.MBeanParameterInfo;
import javax.management.Notification;
import javax.management.NotificationBroadcasterSupport;
import javax.management.NotificationListener;
import javax.management.ReflectionException;

import org.apache.log4j.xml.DOMConfigurator;
import org.apache.qpid.management.Messages;
import org.apache.qpid.management.Names;
import org.apache.qpid.management.configuration.BrokerAlreadyConnectedException;
import org.apache.qpid.management.configuration.BrokerConnectionData;
import org.apache.qpid.management.configuration.BrokerConnectionException;
import org.apache.qpid.management.configuration.Configuration;
import org.apache.qpid.management.configuration.Configurator;
import org.apache.qpid.management.domain.model.JmxService;
import org.apache.qpid.transport.util.Logger;

/**
 * Main entry point for starting Q-Man application.
 */
public class QMan extends NotificationBroadcasterSupport implements DynamicMBean, NotificationListener
{
    private final static Logger LOGGER = Logger.get(QMan.class);
    private final List<ManagementClient> managementClients = new ArrayList<ManagementClient>();
    
    private Configurator _configurator = new Configurator();
    private ThreadPoolExecutor _workManager;
    
    /**
     * Starts QMan.
     * @throws StartupFailureException when it's not possible to proceed with startup.
     */
    public void start() throws StartupFailureException
    {
        LOGGER.info(Messages.QMAN_000001_STARTING_QMAN);
        LOGGER.info(Messages.QMAN_000002_READING_CONFIGURATION);

        try
        {
        	 registerQManService();
        	 
            _configurator.configure();            
            
            configureWorkManager();
            
            LOGGER.info(Messages.QMAN_000019_QMAN_STARTED);
       } catch(Exception exception) {
            LOGGER.error(exception,Messages.QMAN_100018_UNABLE_TO_STARTUP_CORRECTLY );
            throw new StartupFailureException(exception);
        } 
    }
    
	/**
	 * Connects Q-Man with a broker defined by the given parameter.
	 * 
	 * @param host the hostname where the broker is running.
	 * @param port the port where the broker is running.
	 * @param username the username for connecting with the broker.
	 * @param password the password for connecting with the broker.
	 * @param virtualHost the virtual host.
	 * @param initialPoolCapacity the number of the connection that must  be immediately opened.
	 * @param maxPoolCapacity the maximum number of opened connection.
	 * @param maxWaitTimeout the maximum amount of time that a client will wait for obtaining a connection.
	 * @throws MBeanException when it's not possible to connect with the broker.
	 */
	public void addBroker(
			String host, 
			int port, 
			String username,
			String password, 
			String virtualHost, 
			int initialPoolCapacity,
			int maxPoolCapacity, 
			long maxWaitTimeout) throws BrokerAlreadyConnectedException, BrokerConnectionException
	{
		Configurator configurator = new Configurator();
		try {
			UUID brokerId = UUID.randomUUID();
			BrokerConnectionData data = configurator.createAndReturnBrokerConnectionData(
					brokerId,
					host,
					port, 
					username,
					password, 
					virtualHost, 
					initialPoolCapacity,
					maxPoolCapacity, 
					maxWaitTimeout);
			createManagementClient(brokerId, data);
		} catch (BrokerAlreadyConnectedException exception) 
		{
			LOGGER.warn(Messages.QMAN_300003_BROKER_ALREADY_CONNECTED, exception.getBrokerConnectionData());
			throw exception;
		}
	}
	
    /**
     * Stop Qman
     */
    public void stop() 
    {
        LOGGER.info(Messages.QMAN_000020_SHUTTING_DOWN_QMAN);
        try 
        {
            for (ManagementClient client : managementClients)
            {   
                client.shutdown();  
            }
        } catch(Exception exception)
        {
        }
        LOGGER.info(Messages.QMAN_000021_SHUT_DOWN);                    	
    }
    
	/**
     * Creates a management client using the given data.
     * 
     * @param brokerId the broker identifier.
     * @param data the broker connection data.
     */
    public void createManagementClient(UUID brokerId, BrokerConnectionData data)
    {
        try 
        {
            ManagementClient client = new ManagementClient(brokerId,data);
            client.estabilishFirstConnectionWithBroker();
            managementClients.add(client);
            
            LOGGER.info(Messages.QMAN_000004_MANAGEMENT_CLIENT_CONNECTED,brokerId);
        } catch(StartupFailureException exception) {
            LOGGER.error(exception, Messages.QMAN_100017_UNABLE_TO_CONNECT,brokerId,data);
        }
    }    
    
    /**
     * Returns the list of management clients currently handled by QMan.
     * 
     * @return the list of management clients currently handled by QMan.
     */
    public List<ManagementClient> getManagementClients()
    {
    	return managementClients;
    }
    
    /**
     * Injects the configurator on this QMan instance.
     * That configutator later will be responsible to manage the configuration.
     * 
     * @param configurator the configurator to be injected.
     */
    public void setConfigurator(Configurator configurator){
    	this._configurator = configurator;
    }
    
    /**
     * Main method used for starting Q-Man.
     * 
     * @param args the command line arguments.
     */
    public static void main (String[] args)
    {  
    	if (args.length == 1) 
    	{
    		String logFileName = args[0];
    		DOMConfigurator.configureAndWatch(logFileName,5000);
    	}
    	
		final QMan qman = new QMan();
		
		Thread hook = new Thread()
        {
            @Override
            public void run ()
            {
            	qman.stop();
            }
        };    	
		
		Runtime.getRuntime().addShutdownHook(hook);
		try 
		{
			qman.start();
		    
			System.out.println("Type \"q\" to quit.");
            BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
            while ( !"q".equals(reader.readLine()) )
            {
            	
            }
            Runtime.getRuntime().removeShutdownHook(hook);
            qman.stop();
			System.exit(-1);
		} catch (StartupFailureException exception) 
		{
			qman.stop();
			System.exit(-1);
		} catch (IOException exception)
		{
			System.exit(-1);					
		}
    }
    
    /**
     * Not implemented for this MBean.
     */
	public Object getAttribute(String attribute)
	{
		return null;
	}

    /**
     * Not implemented for this MBean.
     */
	public AttributeList getAttributes(String[] attributes) 
	{
		return null;
	}

    /**
     * Returns the metadata for this MBean
     * 
     * @return the metadata for this MBean
     */
	public MBeanInfo getMBeanInfo() 
	{
		MBeanParameterInfo parameters [] = new MBeanParameterInfo[8];
		
		parameters[0] = new MBeanParameterInfo(
				"host",
				String.class.getName(),
				"The IP address or DNS name that Qpid Broker uses to listen for incoming connections.");
		parameters[1] = new MBeanParameterInfo(
				"port",
				int.class.getName(),
				"The port number that Qpid Broker uses to listen for incoming connections.");
		parameters[2] = new MBeanParameterInfo(
				"username",
				String.class.getName(),
				"The Qpid account name used in the physical connection.");
		parameters[3] = new MBeanParameterInfo(
				"password",
				String.class.getName(),
				"The Qpid account password used in the physical connection.");
		parameters[4]= new MBeanParameterInfo(
				"virtualHost",
				String.class.getName(),
				"The virtualHost name.");
		parameters[5]= new MBeanParameterInfo(
				"initialPoolCapacity",
				int.class.getName(),
				"The number of physical connections (between 0 and a positive 32-bit integer) to create when creating the (Qpid) connection pool.");
		parameters[6]= new MBeanParameterInfo(
				"maxPoolCapacity",
				int.class.getName(),
				"The maximum number of physical database connections (between 0 and a positive 32-bit integer) that the (Qpid) connection pool can contain. ");
		parameters[7]= new MBeanParameterInfo(
				"maxWaitTimeout",
				long.class.getName(),
				"The maximum amount of time to wait for an idle connection.A value of -1 indicates an illimted amount of time (i.e. forever)");
				
		MBeanOperationInfo operation = new MBeanOperationInfo(
				"addBroker",
				"Connects QMan with a broker.",
				parameters,
				void.class.getName(),
				MBeanOperationInfo.ACTION);
		
		MBeanInfo mbean = new MBeanInfo(
				QMan.class.getName(),
				"QMan Management & Administration interface.",
				null,
				null,
				new MBeanOperationInfo[]{operation},
				null);
		
		return mbean;
	}

	/**
	 * Invokes an operation on QMan (MBean).
	 * 
	 * @param actionName the operation name.
	 * @param params the operation parameters.
	 * @param signature the operation signature.
	 * @return the result of the invocation (if the operation is not void);
	 * @exception MBeanException  Wraps a <CODE>java.lang.Exception</CODE> thrown by the MBean's invoked method.
     * @exception ReflectionException  Wraps a <CODE>java.lang.Exception</CODE> thrown while trying to invoke the method
	 */
	public Object invoke(String actionName, Object[] params, String[] signature) throws MBeanException, ReflectionException 
	{
		if (Names.ADD_BROKER_OPERATION_NAME.equals(actionName))
		{
			try 
			{
				addBroker(
						(String)params[0], 
						(Integer)params[1], 
						(String)params[2], 
						(String)params[3], 
						(String)params[4], 
						(Integer)params[5], 
						(Integer)params[6], 
						(Long)params[7]); 
			} catch(Exception exception)
			{
				throw new MBeanException(exception);
			}
		} else 
		{
			throw new ReflectionException(new NoSuchMethodException(actionName));
		}
		return null;
	}

    /**
     * Not implemented for this MBean.
     */
	public void setAttribute(Attribute attribute) 
	{		
	}

    /**
     * Not implemented for this MBean.
     */
	public AttributeList setAttributes(AttributeList attributes) 
	{
		return null;
	}    

	/**
	 * Simply dispatches the incoming notification to registered listeners.
	 * Consider that the notification is sent asynchronously so the QMan current thread is not 
	 * waiting for completion of receiver task.
	 * 
	 * @param notification the incoming notification.
	 * @param handback the context associated to this notification.
	 */
	public void handleNotification(final Notification notification, Object handback) 
	{
		_workManager.execute(new Runnable(){
			public void run()
			{
				sendNotification(notification);
			}
		});
	}	
	
    /**
     * Registers QMan as an MBean on MBeanServer.
     * 
     * @throws MBeanException when it's not possible to proceeed with registration.
     */
    private void registerQManService() throws MBeanException 
    {    	
    	JmxService service = new JmxService(); 
    	service.registerQManService(this);
    	
    	LOGGER.info(Messages.QMAN_000023_QMAN_REGISTERED_AS_MBEAN);
	}
    
    /**
     * Configures work manager component. 
     */
	private void configureWorkManager()
	{
		Configuration configuration = Configuration.getInstance();
		_workManager = new ThreadPoolExecutor(
				configuration.getWorkerManagerPoolSize(),
				configuration.getWorkerManagerMaxPoolSize(),
				configuration.getWorkerManagerKeepAliveTime(),
				TimeUnit.MILLISECONDS,
				new ArrayBlockingQueue<Runnable>(30));
	}
}
