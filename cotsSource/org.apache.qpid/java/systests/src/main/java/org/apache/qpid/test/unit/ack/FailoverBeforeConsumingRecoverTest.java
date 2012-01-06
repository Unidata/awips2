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
package org.apache.qpid.test.unit.ack;

import org.apache.qpid.jms.Session;

import javax.jms.Message;
import javax.jms.Queue;

public class FailoverBeforeConsumingRecoverTest extends RecoverTest
{

    @Override
    protected void initTest() throws Exception
    {
        super.initTest();
        failBroker(getFailingPort());

        Queue queue = _consumerSession.createQueue(getTestQueueName());
        sendMessage(_connection.createSession(false, Session.AUTO_ACKNOWLEDGE), queue, SENT_COUNT);        
    }
}
