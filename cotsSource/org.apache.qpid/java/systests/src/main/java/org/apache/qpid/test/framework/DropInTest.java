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
package org.apache.qpid.test.framework;

import javax.jms.JMSException;
import javax.jms.Message;

/**
 * A DropIn test is a test case that can accept late joining test clients into a running test. This can be usefull,
 * for interactive experimentation.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities
 * <tr><td> Accept late joining test clients.
 * </table>
 */
public interface DropInTest
{
    /**
     * Should accept a late joining client into a running test case. The client will be enlisted with a control message
     * with the 'CONTROL_TYPE' field set to the value 'LATEJOIN'. It should also provide values for the fields:
     *
     * <p/><table>
     * <tr><td> CLIENT_NAME <td> A unique name for the new client.
     * <tr><td> CLIENT_PRIVATE_CONTROL_KEY <td> The key for the route on which the client receives its control messages.
     * </table>
     *
     * @param message The late joiners join message.
     *
     * @throws JMSException Any JMS Exception are allowed to fall through, indicating that the join failed.
     */
    public void lateJoin(Message message) throws JMSException;
}
