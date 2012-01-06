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

import org.apache.qpid.management.domain.handler.base.BaseMessageHandler;
import org.apache.qpid.management.domain.model.type.Binary;
import org.apache.qpid.transport.codec.Decoder;

/**
 * Base class for content indication message handlers.
 * 
 * @author Andrea Gazzarini
 */
public class EventContentMessageHandler extends BaseMessageHandler
{
    /**
     * Processes the income message.
     * 
     * @param decoder the decoder used to parse the message.
     * @param sequenceNumber the sequence number of the message.
     */
    public final void process (Decoder decoder, int sequenceNumber)
    {      
        String packageName = decoder.readStr8();
        String eventName = decoder.readStr8();
        Binary eventHash = new Binary(decoder.readBin128());
        long timeStampOfCurrentSample = decoder.readDatetime();
        int severity = decoder.readUint8();
        byte[] argumentsData = decoder.readReaminingBytes();
        
        _domainModel.addEventRawData(packageName, eventName, eventHash, argumentsData,timeStampOfCurrentSample,severity);
    }
}
