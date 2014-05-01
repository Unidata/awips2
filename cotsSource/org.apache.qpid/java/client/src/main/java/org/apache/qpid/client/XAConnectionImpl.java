/* Licensed to the Apache Software Foundation (ASF) under one
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
 */
package org.apache.qpid.client;

import org.apache.qpid.jms.ConnectionURL;
import org.apache.qpid.AMQException;

import javax.jms.*;

/**
 * This class implements the javax.njms.XAConnection interface
 */
public class XAConnectionImpl extends AMQConnection implements XAConnection, XAQueueConnection, XATopicConnection
{
    //-- constructor
    /**
     * Create a XAConnection from a connectionURL
     */
    public XAConnectionImpl(ConnectionURL connectionURL, SSLConfiguration sslConfig) throws AMQException
    {
        super(connectionURL, sslConfig);
    }

    //-- interface XAConnection
    /**
     * Creates an XASession.
     *
     * @return A newly created XASession.
     * @throws JMSException If the XAConnectiono fails to create an XASession due to
     *                      some internal error.
     */
    public synchronized XASession createXASession() throws JMSException
    {
        checkNotClosed();
        return _delegate.createXASession();
    }

    //-- Interface  XAQueueConnection
    /**
     * Creates an XAQueueSession.
     *
     * @return A newly created XASession.
     * @throws JMSException If the XAQueueConnectionImpl fails to create an XASession due to
     *                      some internal error.
     */
    public XAQueueSession createXAQueueSession() throws JMSException
    {
        return (XAQueueSession) createXASession();
    }

    //-- Interface  XATopicConnection
    /**
     * Creates an XAQueueSession.
     *
     * @return A newly created XASession.
     * @throws JMSException If the XAQueueConnectionImpl fails to create an XASession due to
     *                      some internal error.
     */
    public XATopicSession createXATopicSession() throws JMSException
    {
        return (XATopicSession) createXASession();
    }
}
