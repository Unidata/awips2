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

import org.apache.qpid.management.domain.handler.base.ContentIndicationMessageHandler;
import org.apache.qpid.management.domain.model.type.Binary;

/**
 * Schema Response message handler.
 * This handler is responsible to process 'c'(opcode) messages sent by the management broker.
 * 
 * @author Andrea Gazzarini
 */
public class ConfigurationMessageHandler extends ContentIndicationMessageHandler
{  
    /**
     * Broker domain model is going to be updated with incoming configuration data.
     * 
     * @param packageName the name of the package.
     * @param className the name of the class.
     * @param objectId the object identifier.
     * @param timeStampOfCurrentSample the timestamp of incoming data.
     * @param timeObjectWasCreated time object was created.
     * @param timeObjectWasDeleted time object was deleted.
     */
    @Override
    protected void updateDomainModel (
            String packageName, 
            String className, 
            Binary classHash,
            Binary objectId,
            long timeStampOfCurrentSample, 
            long timeObjectWasCreated, 
            long timeObjectWasDeleted, 
            byte[] contentData)
    {
        _domainModel.addConfigurationRawData(packageName,className,classHash,objectId,contentData);        
    }
 }
