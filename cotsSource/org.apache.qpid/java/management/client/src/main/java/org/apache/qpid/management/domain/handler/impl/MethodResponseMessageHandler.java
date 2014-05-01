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

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.BlockingQueue;

import org.apache.qpid.management.Messages;
import org.apache.qpid.management.domain.handler.base.BaseMessageHandler;
import org.apache.qpid.management.domain.model.DomainModel;
import org.apache.qpid.management.domain.model.InvocationEvent;
import org.apache.qpid.transport.codec.Decoder;
import org.apache.qpid.transport.util.Logger;

/**
 * Message handler for method response messages.
 * This handler is installed on domain model as a method invocation result listener. 
 * When a method is going to be invoked this listener is notified with the exchange channel that will be used between it and
 * the event (method invocation) source object.
 * 
 * @author Andrea Gazzarini
 *
 */
public class MethodResponseMessageHandler extends BaseMessageHandler
{
    private final static Logger LOGGER = Logger.get(MethodResponseMessageHandler.class);
        
    private Map<Integer, BlockingQueue<InvocationResult>> _exchangeChannels = new HashMap<Integer, BlockingQueue<InvocationResult>>();
    
    /**
     * This is the listener installed on domain model for method invocations.
     */
    private final IMethodInvocationListener methodInvocationListener = new IMethodInvocationListener()
    {
        /**
         * Event source callback. 
         * A method is going to be invoked and this method lets this listener take the exchange channel that will be used
         * with the event source for synchronous communication.
         * 
         * @param event the operation invocation event.
         */
        public void operationIsGoingToBeInvoked (InvocationEvent event)
        {
            _exchangeChannels.put(event.getSequenceNumber(), event.getExchangeChannel());
        }        
    };
    
    /**
     * Processes the incoming message.
     * 
     * @param decoder the decoder used for parsing incoming data.
     * @param sequenceNumber the sequence number of the incoming message.
     */
    public void process (Decoder decoder, int sequenceNumber)
    {        
        InvocationResult result = new InvocationResult(decoder.readUint32(), decoder.readStr16(),decoder.readReaminingBytes());
        BlockingQueue<InvocationResult> exchangeChannel = _exchangeChannels.remove(sequenceNumber);
        if (exchangeChannel != null)
        {
            try
            {
                exchangeChannel.put(result);
            } catch (InterruptedException exception)
            {
                LOGGER.error(exception,Messages.QMAN_100010_METHOD_INVOCATION_RESULT_FAILURE,sequenceNumber);
            }
        } else 
        {
            LOGGER.warn(
                    "Unable to deal with incoming message because it contains a unknown sequence number (%s).", 
                    sequenceNumber);
        }
    }

    /**
     * Sets the domain model on this handler.
     * In addiction, this handler registers a method invocation listener on the domain model.
     * 
     *  @param domainModel the managed broker domain model.
     */
    @Override
    public void setDomainModel (DomainModel domainModel)
    {
        super.setDomainModel(domainModel);
        domainModel.setMethodInvocationListener(methodInvocationListener);
    }    
}
