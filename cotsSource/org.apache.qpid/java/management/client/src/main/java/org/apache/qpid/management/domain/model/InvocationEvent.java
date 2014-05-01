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
package org.apache.qpid.management.domain.model;

import java.util.EventObject;
import java.util.concurrent.BlockingQueue;

import org.apache.qpid.management.domain.handler.impl.InvocationResult;

/**
 * Operation invocation event. 
 * This encapsulates all the information that a method invocation listener needs to know about an operation which is 
 * going to be invoked.
 * 
 * @author Andrea Gazzarini
 */
public class InvocationEvent extends EventObject
{
    private static final long serialVersionUID = 240229490753008597L;

    private final int _sequenceNumber;
    private final BlockingQueue<InvocationResult> _exchangeChannel;
    
    /**
     * Builds a new invocation event with the given data.
     * 
     * @param source the event source.
     * @param sequenceNumber the sequence number of the method invocation.
     * @param exchangeChannel the exchange channel for synchronous communication.
     */
    InvocationEvent(Object source, int sequenceNumber, BlockingQueue<InvocationResult> exchangeChannel)
    {
        super(source);
        this._sequenceNumber = sequenceNumber;
        this._exchangeChannel = exchangeChannel;
    }
    
    /**
     * Returns the sequence number that will be / has been used for method invocation.
     * 
     * @return the sequence number that will be / has been used for method invocation.
     */
    public int getSequenceNumber() 
    {
        return _sequenceNumber;
    }
    
    /**
     * Returns the exchange channel that will be used between event source and event listener for synchronous 
     * communication.
     * 
     * @return the exchange channel that will be used for synchronous communication.
     */
    public BlockingQueue<InvocationResult> getExchangeChannel()
    {
        return _exchangeChannel;
    }
}