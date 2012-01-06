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
package org.apache.qpid.management.configuration;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.util.UUID;

import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.apache.qpid.management.Messages;
import org.apache.qpid.management.Names;
import org.apache.qpid.management.Protocol;
import org.apache.qpid.management.domain.handler.impl.ConfigurationMessageHandler;
import org.apache.qpid.management.domain.handler.impl.EventContentMessageHandler;
import org.apache.qpid.management.domain.handler.impl.HeartBeatIndicationMessageHandler;
import org.apache.qpid.management.domain.handler.impl.InstrumentationMessageHandler;
import org.apache.qpid.management.domain.handler.impl.MethodResponseMessageHandler;
import org.apache.qpid.management.domain.handler.impl.SchemaResponseMessageHandler;
import org.apache.qpid.transport.util.Logger;
import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

/**
 * Director used for coordinating the build process of configuration.
 * This is the only component which has a read-write permission on Configuration object.
 */
public class Configurator extends DefaultHandler
{    
    private final static Logger LOGGER = Logger.get(Configurator.class);
	
	/**
     * Default (empty) parser used when there's no need to process data (non relevant elements).
     */
    final static IParser DEFAULT_PARSER = new IParser() {

        public void setCurrrentAttributeValue (String value)
        {
        }

        public void setCurrentAttributeName (String name)
        {
        }
    };
    
    IParser _brokerConfigurationParser = new BrokerConnectionDataParser();
    IParser _workerManagerConfigurationParser = new WorkerManagerConfigurationParser();
    IParser _currentParser = DEFAULT_PARSER;

    /**
     * Delegates the processing to the current parser.
     */
    @Override
    public void characters (char[] ch, int start, int length) throws SAXException
    {
        String value = new String(ch,start,length).trim();
        if (value.length() != 0) {
            _currentParser.setCurrrentAttributeValue(value);
        }
    }

    /**
     * Here is defined what parser needs to be used for processing the current data.
     */
    @Override
    public void startElement (String uri, String localName, String name, Attributes attributes) throws SAXException
    {
        switch(Tag.get(name)) 
        {
            case BROKERS : 
            {
                _currentParser = _brokerConfigurationParser;
                break;
            }
            case WORK_MANAGER : 
            {
            	_currentParser = _workerManagerConfigurationParser;
            	break;
            }
        }
    }
    
    @Override
    public void endElement (String uri, String localName, String name) throws SAXException
    {
        _currentParser.setCurrentAttributeName(name);
    }

    /**
     * Builds whole configuration.
     * 
     * @throws ConfigurationException when the build fails.
     */
    public void configure() throws ConfigurationException 
    {
    	BufferedReader reader = null;
        try 
        {
        	String initialConfigFileName = System.getProperty(Names.QMAN_CONFIG_OPTION_NAME);
        	if (initialConfigFileName != null && initialConfigFileName.trim().length() != 0)
        	{
        		File initialConfigurationFile = new File(initialConfigFileName);
        		if (initialConfigurationFile.canRead())
        		{
	                SAXParser parser = SAXParserFactory.newInstance().newSAXParser();
	                reader = new BufferedReader(
	                		new InputStreamReader(
	                				new FileInputStream(initialConfigFileName)));
	                InputSource source = new InputSource(reader);
	                parser.parse(source, this);        		
        		} else {
        			LOGGER.warn(
        					Messages.QMAN_300004_INVALID_CONFIGURATION_FILE, 
        					initialConfigFileName);
        		}
        	}
            
            addMandatoryManagementMessageHandlers();
            addMandatoryMethodReplyMessageHandlers();                        
        } catch (Exception exception)
        {
            throw new ConfigurationException(exception);
        } finally 
        {
        	try 
        	{
				reader.close();
			} catch (Exception ignore) 
			{
			}
        }
    }
     
    /**
     * Creates and return a value object (BrokerConnectionData) with the given parameters.
     * Note that that object will be stored on configuration and it could be used to set a connection with the broker.
     * This happens when the "initialPoolCapacity" is greater than 0 : in this case the caller is indicatinf that it wants to open
     * one or more connections immediately at startup and therefore Q-Man will try to do that.
     * 
 	 * @param host the hostname where the broker is running.
	 * @param port the port where the broker is running.
	 * @param username the username for connecting with the broker.
	 * @param password the password for connecting with the broker.
	 * @param virtualHost the virtual host.
	 * @param initialPoolCapacity the number of the connection that must  be immediately opened.
	 * @param maxPoolCapacity the maximum number of opened connection.
	 * @param maxWaitTimeout the maximum amount of time that a client will wait for obtaining a connection.
     * @return the value object containing the data above.
     * @throws BrokerAlreadyConnectedException when the broker is already connected.
     * @throws BrokerConnectionException when a connection cannot be estabilished.
     */
    public BrokerConnectionData createAndReturnBrokerConnectionData(
    		UUID brokerId,
    		String host, 
    		int port, 
			String username,
			String password, 
			String virtualHost, 
			int initialPoolCapacity,
			int maxPoolCapacity, 
			long maxWaitTimeout)  throws BrokerAlreadyConnectedException, BrokerConnectionException
    {
    	BrokerConnectionData data = new BrokerConnectionData(
    			host, 
    			port,
    			virtualHost,
    			username,
    			password,
    			initialPoolCapacity,
    			maxPoolCapacity,
    			maxWaitTimeout);
    	Configuration.getInstance().addBrokerConnectionData(brokerId, data);
    	return data;
    }
    
    /**
     * Configures the mandatory management message handlers.
     */
    void addMandatoryMethodReplyMessageHandlers ()
    {
        Configuration.getInstance().addMethodReplyMessageHandlerMapping(
                new MessageHandlerMapping(
                        Protocol.OPERATION_INVOCATION_RESPONSE_OPCODE,
                        new MethodResponseMessageHandler()));
        
        Configuration.getInstance().addMethodReplyMessageHandlerMapping(
                new MessageHandlerMapping(
                        Protocol.SCHEMA_RESPONSE_OPCODE,
                        new SchemaResponseMessageHandler()));  
    }

    /**
     * Configures the mandatory management message handlers.
     */
    void addMandatoryManagementMessageHandlers ()
    {
        Configuration.getInstance().addManagementMessageHandlerMapping(
                new MessageHandlerMapping(
                        Protocol.INSTRUMENTATION_CONTENT_RESPONSE_OPCODE,
                        new InstrumentationMessageHandler()));
      
        Configuration.getInstance().addManagementMessageHandlerMapping(
                new MessageHandlerMapping(
                        Protocol.CONFIGURATION_CONTENT_RESPONSE_OPCDE,
                        new ConfigurationMessageHandler()));        
        
        Configuration.getInstance().addManagementMessageHandlerMapping(
                new MessageHandlerMapping(
                        Protocol.EVENT_CONTENT_RESPONSE_OPCDE,
                        new EventContentMessageHandler()));        
        
        Configuration.getInstance().addManagementMessageHandlerMapping(
                new MessageHandlerMapping(
                        Protocol.HEARTBEAT_INDICATION_RESPONSE_OPCODE,
                        new HeartBeatIndicationMessageHandler()));          
    }
}
