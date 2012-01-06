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

package org.apache.qpid.server;

import java.io.IOException;

import javax.management.JMException;

/**
 * The managed interface exposed to allow management of channels.
 * @author   Bhupendra Bhardwaj
 * @version  0.1
 */
public interface ManagedChannel
{
    static final String TYPE = "Channel";

    /**
     * Tells whether the channel is transactional.
     * @return true if the channel is transactional.
     * @throws IOException
     */
    boolean isTransactional() throws IOException;

    /**
     * Tells the number of unacknowledged messages in this channel.
     * @return number of unacknowledged messages.
     * @throws IOException
     */
    int getUnacknowledgedMessageCount() throws IOException;

    
    //********** Operations *****************//

    /**
     * Commits the transactions if the channel is transactional.
     * @throws IOException
     * @throws JMException
     */
    void commitTransactions() throws IOException, JMException;

    /**
     * Rollsback the transactions if the channel is transactional.
     * @throws IOException
     * @throws JMException
     */
    void rollbackTransactions() throws IOException, JMException;

}
