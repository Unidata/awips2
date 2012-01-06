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

import org.apache.qpid.management.domain.model.type.Binary;
import org.apache.qpid.transport.codec.Decoder;

/**
 * Base class for content indication message handlers.
 * 
 * @author Andrea Gazzarini
 */
public abstract class ContentIndicationMessageHandler extends BaseMessageHandler
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
        String className = decoder.readStr8();
        Binary classHash = new Binary(decoder.readBin128());
    
        long timeStampOfCurrentSample = decoder.readDatetime();
        long timeObjectWasCreated = decoder.readDatetime();
        long timeObjectWasDeleted = decoder.readDatetime();
        
        Binary objectId = new Binary(decoder.readBin128());
                
        if (objectHasBeenRemoved(timeObjectWasDeleted, timeStampOfCurrentSample))
        {
            removeObjectInstance(packageName,className,classHash,objectId);
        } else 
        {
            updateDomainModel(
                    packageName,
                    className,
                    classHash,
                    objectId,
                    timeStampOfCurrentSample,
                    timeObjectWasCreated,
                    timeObjectWasDeleted,
                    decoder.readReaminingBytes());
        }
    }
    
    /**
     * Removes an object instance from the domain model.
     * 
     * @param packageName the package name.
     * @param className the class name.
     * @param classHash the class hash.
     * @param objectId the object identifier.
     */
    void removeObjectInstance(String packageName, String className,Binary classHash, Binary objectId)
    {
        _domainModel.removeObjectInstance(packageName,className,classHash,objectId);        
    }
    
    /**
     * Checks if the timestamps contained in the message indicate that the object has been removed.
     *  
     * @param deletionTimestamp time object was deleted.
     * @param now timestamp of the current message.
     * @return true if the object has been removed, false otherwise.
     */
    boolean objectHasBeenRemoved(long deletionTimestamp, long now) {
        return (deletionTimestamp != 0) && (now > deletionTimestamp);
    }
    
    /**
     * Updates domain model with the incoming data.
     *  This is a template method that each concrete subclass must implement in order to update the domain model
     *  with the incoming data.
     *  
     * @param packageName the name of the package.
     * @param className the name of the class.
     * @param objectId the object identifier.
     * @param timeStampOfCurrentSample timestamp of current sample.
     * @param timeObjectWasCreated time object was created.
     * @param timeObjectWasDeleted time object was deleted.
     * @param contentData object instance incoming data.
     */
    protected abstract void updateDomainModel(
            String packageName, 
            String className, 
            Binary classHash,
            Binary objectId, 
            long timeStampOfCurrentSample,
            long timeObjectWasCreated,
            long timeObjectWasDeleted,
            byte []contentData );
}