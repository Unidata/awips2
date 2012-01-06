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
package org.apache.qpid.management.domain.handler.base;

import org.apache.qpid.management.domain.model.DomainModel;
import org.apache.qpid.transport.codec.Decoder;

/**
 * Interface definition for a processor able to deal with a specific message.
 * The concrete implementor must define what has to be done with the supplied (incoming) stream and the sequence 
 * number.
 * 
 * @author Andrea Gazzarini.
 */
public interface IMessageHandler
{
    /**
     * Processes the (incoming) stream message.
     * Note that the main controller (the component that is controlling this handler) has already read the magic number and 
     * the sequence number so here concrete implementors must start from that point (that is, just after the  sequence 
     * number).
     * 
     * @param decoder the stream decoder.
     * @param sequenceNumber the sequence number of the message.
     */
    void process (Decoder decoder, int sequenceNumber);
    
    /**
     * Injects the domain model into this handler.
     * 
     * @param domainModel the domain model.
     */
    void setDomainModel(DomainModel domainModel);
}