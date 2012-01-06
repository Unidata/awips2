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
package org.apache.qpid.management.domain.handler.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.apache.qpid.management.Messages;
import org.apache.qpid.management.Names;
import org.apache.qpid.management.Protocol;
import org.apache.qpid.management.domain.handler.base.BaseMessageHandler;
import org.apache.qpid.management.domain.model.type.Binary;
import org.apache.qpid.transport.codec.Decoder;

/**
 * Schema Response message handler.
 * This handler is responsible to process 'S'(opcode) messages sent by the management broker containing the full
 * schema details for a class.
 * 
 * @author Andrea Gazzarini
 */
public class SchemaResponseMessageHandler extends BaseMessageHandler
{  
	/**
	 * Behavioural interface for classes that are responsible to deal with schema messages.
	 * 
	 * @author Andrea Gazzarini
	 */
	interface IProcessor 
	{
		/**
		 * Processes the incoming message using the given decoder.
		 * 
		 * @param decoder the decoder used for dealing with incoming message.
		 */
		void process(Decoder decoder);
	}
	
	/**
	 * Processor responsible to deal with class schema related messages.
	 */
	final IProcessor _classSchemaProcessor = new IProcessor()
	{
		public void process(Decoder decoder) 
		{
	        try 
	        {	        	
	            String packageName = decoder.readStr8();
	            String className = decoder.readStr8();
	            
	            Binary schemaHash = new Binary(decoder.readBin128());

	            int howManyProperties = decoder.readUint16();
	            int howManyStatistics = decoder.readUint16();
	            int howManyMethods = decoder.readUint16();

	            _domainModel.addSchema(
	                    packageName, 
	                    className, 
	                    schemaHash,
	                    getAttributes(decoder, howManyProperties), 
	                    getAttributes(decoder, howManyStatistics), 
	                    getMethods(decoder, howManyMethods));
	        } catch(Exception exception) 
	        {
	            _logger.error(exception,Messages.QMAN_100005_CLASS_SCHEMA_PROCESSING_FAILURE);
	        }
		}
	};
	
	/**
	 * Processor responsible to deal with class event related messages.
	 */
	final IProcessor _eventSchemaProcessor = new IProcessor()
	{
		public void process(Decoder decoder) 
		{
	        try 
	        {	        
	            String packageName = decoder.readStr8();
	            String className = decoder.readStr8();
	            Binary hash = new Binary(decoder.readBin128());
	            int howManyArguments = decoder.readUint16();
	            
	            _domainModel.addEventSchema(
	            		packageName, 
	            		className, 
	            		hash, 
	            		getAttributes(decoder, howManyArguments));
	        } catch(Exception exception) 
	        {
	            _logger.error(exception,Messages.QMAN_100006_EVENT_SCHEMA_PROCESSING_FAILURE);
	        }
		}
	};	
	
    /**
     * Processes an incoming  schema response.
     * This will be used for building the corresponding class definition.
     * 
     *  @param decoder the decoder used for parsing the incoming stream.
     *  @param sequenceNumber the sequence number of the incoming message.
     */
    public void process (Decoder decoder, int sequenceNumber)
    {        
        try 
        {
        	int classKind = decoder.readUint8();
        	switch(classKind)
        	{
        		case Protocol.CLASS :
        		{
        			_classSchemaProcessor.process(decoder);
        			break;
        		}
        		case Protocol.EVENT : 
        		{
        			_eventSchemaProcessor.process(decoder);   
        			break;
        		}
        		default : 
        		{
        			_logger.error(Messages.QMAN_100011_UNKNOWN_CLASS_KIND,classKind);
        		}
        	}
        } catch(Exception exception) 
        {
            _logger.error(exception,Messages.QMAN_100012_SCHEMA_MESSAGE_PROCESSING_FAILURE);
        }        	
    }
    
    /**
     * Reads from the incoming message stream the properties definitions.
     * 
     * @param decoder the decoder used for decode incoming data.
     * @param howManyProperties the number of properties to read. 
     * @return a list of maps. Each map contains a property definition.
     */
    List<Map<String, Object>> getAttributes(Decoder decoder,int howMany) 
    {
        List<Map<String, Object>> result = new ArrayList<Map<String, Object>>(howMany);        
        for (int i = 0; i < howMany; i++ )
        {
            result.add(decoder.readMap());
        }
        return result;
    }
    
    /**
     * Reads the methods definitions from the incoming message stream.
     * 
     * @param decoder the decoder used for decode incoming data.
     * @param howManyMethods the number of methods to read. 
     * @return a list method definitions.
     */
    List<MethodOrEventDataTransferObject> getMethods(Decoder decoder, int howManyMethods)
    {
        List<MethodOrEventDataTransferObject> result = new ArrayList<MethodOrEventDataTransferObject>(howManyMethods);
        for (int i  = 0; i < howManyMethods; i++) 
        {   
            Map<String,Object> method = decoder.readMap();
            int howManyArguments = (Integer) method.get(Names.ARG_COUNT_PARAM_NAME);

            List<Map<String,Object>> arguments = new ArrayList<Map<String,Object>>(howManyArguments);
            for (int argIndex = 0; argIndex < howManyArguments; argIndex++)
            {
                arguments.add(decoder.readMap());
            }
            result.add(new MethodOrEventDataTransferObject(method,arguments));
        }        
        return result;
    }
    
    /**
     * Reads the events definitions from the incoming message stream.
     * 
     * @param decoder the decoder used for decode incoming data.
     * @param howManyEvents the number of events to read. 
     * @return a list event definitions.
     */
    List<MethodOrEventDataTransferObject> getEvents(Decoder decoder, int howManyEvents)
    {
        List<MethodOrEventDataTransferObject> result = new ArrayList<MethodOrEventDataTransferObject>(howManyEvents);
        for (int i  = 0; i < howManyEvents; i++) 
        {   
            Map<String,Object> method = decoder.readMap();
            int howManyArguments = (Integer) method.get(Names.ARG_COUNT_PARAM_NAME);

            List<Map<String,Object>> arguments = new ArrayList<Map<String,Object>>(howManyArguments);
            for (int argIndex = 0; argIndex < howManyArguments; argIndex++)
            {
                arguments.add(decoder.readMap());
            }
            result.add(new MethodOrEventDataTransferObject(method,arguments));
        }        
        return result;
    }
 }
