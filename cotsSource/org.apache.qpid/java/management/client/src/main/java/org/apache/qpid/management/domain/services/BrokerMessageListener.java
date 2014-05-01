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

import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.qpid.api.Message;
import org.apache.qpid.management.Messages;
import org.apache.qpid.management.Protocol;
import org.apache.qpid.management.domain.handler.base.IMessageHandler;
import org.apache.qpid.management.domain.model.DomainModel;
import org.apache.qpid.nclient.util.MessageListener;
import org.apache.qpid.transport.codec.BBDecoder;
import org.apache.qpid.transport.util.Logger;

/**
 * Message listener used for processing incoming messages.
 * So it is installed as a consumer on a specific channel and when a new message arrives: 
 * 
 * 1) Performs a sanity check on the message (magic number, sequence number)
 * 2) Extracts the opcode and looks for one message handler associated with that opcode.
 * 3) If a message handler is found the delegates the message processing; otherwise a log message is written to indicate
 * that the message will be skipped.
 * 
 * @author Andrea Gazzarini
 */
class BrokerMessageListener implements MessageListener
{
    private  final static Logger LOGGER = Logger.get(BrokerMessageListener.class);
    
    private static class Log 
    {            
        // Debugs the content of the incoming message.
        static void debugIncomingMessage(ByteBuffer message)
        {
            if (LOGGER.isDebugEnabled()) 
            { 
                LOGGER.debug(Messages.QMAN_200001_INCOMING_MESSAGE_HAS_BEEN_RECEIVED, Arrays.toString(message.array()));
            }
        }

        // Debugs all the configured handlers.
        static void debugConfiguredHandlers (Map<Character, IMessageHandler> _handlers)
        {
            if (LOGGER.isDebugEnabled())
            {
                for (Entry<Character, IMessageHandler> entry : _handlers.entrySet())
                {
                  LOGGER.debug(Messages.QMAN_200002_OPCODE_HANDLER_ASSOCIATION,entry.getKey(),entry.getValue());  
                }
            }
        }
    }
    
    Map<Character, IMessageHandler> _handlers = new HashMap<Character, IMessageHandler>();
    private DomainModel _domainModel;
        
    /**
     * Builds a new message listener with the given  broker domain model.
     * 
     * @param model the managed broker domain model.
     */
    BrokerMessageListener(DomainModel model) 
    {
        this._domainModel = model;
    }
    
    /**
     * When a new message arrives this method is called. 
     * 1) Performs a sanity check on the message (magic number, sequence number)
     * 2) Extracts the opcode and looks for one message handler associated with that opcode.
     * 3) If a message handler is found the delegates the message processing; otherwise a log message is written to indicate
     * that the message will be skipped.
     * 
     * @param message the incoming message.
     */
    public void onMessage (Message compoundMessage)
    {
		try 
		{
			MessageTokenizer tokenizer = new MessageTokenizer(compoundMessage);
			while (tokenizer.hasMoreElements())
			{
    			dispatch(tokenizer.nextElement());
			}
		} catch(IOException exception) 
        {
			LOGGER.error(exception,Messages.QMAN_100002_MESSAGE_READ_FAILURE);                        
        } catch(Exception exception) 
	    {
        	LOGGER.error(exception,Messages.QMAN_100003_MESSAGE_PROCESS_FAILURE);            
	    }
    }
    
    /**
     * Configures a new handler with this listener.
     * After that, each time a message arrives with the specified opcode, this handler will be responsible for 
     * processing.
     * Note that calling this method will switch this listener to a WORKING state.
     * 
     * @param opcode the operation code.
     * @param handler the message handler.
     */
    void setHandlers(Map<Character, IMessageHandler> handlers)
    {
        for (Entry<Character, IMessageHandler> entry : handlers.entrySet())
        {
            char opcode = entry.getKey();
            IMessageHandler handler = entry.getValue();
            try 
            {
				handler.setDomainModel(_domainModel);
				_handlers.put(opcode, handler);
			} catch (Exception exception) {
				LOGGER.error(exception,
						Messages.QMAN_100004_HANDLER_INITIALIZATION_FAILURE,
						opcode);
			}
        }
    }
    
    
    
    /**
	 * Dispatches the given message to the appropriate handler.
	 * 
	 * @param message
	 *            the incoming message.
	 * @throws IOException
	 *             when the message content cannot be read.
	 */
    private void dispatch(Message message) throws IOException
    {
    	 ByteBuffer buffer = message.readData();

         String magicNumber = new String(new byte[] {buffer.get(),buffer.get(),buffer.get()});
         if (!Protocol.MAGIC_NUMBER.equals(magicNumber))
         {
             LOGGER.error(Messages.QMAN_100001_BAD_MAGIC_NUMBER_FAILURE,magicNumber);
             return;
         }
         
         char opcode = (char)buffer.get();
         
         IMessageHandler handler = _handlers.get(opcode); 
         if (handler != null) 
         {
             BBDecoder decoder = new BBDecoder();
             decoder.init(buffer);
             
             LOGGER.debug(Messages.QMAN_200003_MESSAGE_FORWARDING,opcode,handler);
             
             handler.process(decoder,decoder.readSequenceNo());
         } else 
         {
             LOGGER.warn(Messages.QMAN_300001_MESSAGE_DISCARDED,opcode);
             Log.debugConfiguredHandlers(_handlers);
         }
    }
}
