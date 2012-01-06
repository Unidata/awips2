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

import org.apache.qpid.client.message.MessageFactoryRegistry;

import javax.jms.*;
import javax.transaction.xa.XAResource;

/**
 * This is an implementation of the javax.njms.XASEssion interface.
 */
public class XASessionImpl extends AMQSession_0_10 implements XASession, XATopicSession, XAQueueSession
{
    /**
     * XAResource associated with this XASession
     */
    private final XAResourceImpl _xaResource;

    /**
     * This XASession Qpid DtxSession
     */
    private org.apache.qpid.transport.Session _qpidDtxSession;

    /**
     * The standard session
     */
    private Session _jmsSession;


    //-- Constructors
    /**
     * Create a JMS XASession
     */
    public XASessionImpl(org.apache.qpid.transport.Connection qpidConnection, AMQConnection con, int channelId,
                         int defaultPrefetchHigh, int defaultPrefetchLow)
    {
        super(qpidConnection, con, channelId, false,  // this is not a transacted session
              Session.AUTO_ACKNOWLEDGE, // the ack mode is transacted
              MessageFactoryRegistry.newDefaultRegistry(), defaultPrefetchHigh, defaultPrefetchLow);
        createSession();
        _xaResource = new XAResourceImpl(this);
    }

    //-- public methods

    /**
     * Create a qpid session.
     */
    public void createSession()
    {
        _qpidDtxSession = _qpidConnection.createSession(0);
        _qpidDtxSession.setSessionListener(this);
        _qpidDtxSession.dtxSelect();
    }


    //--- javax.njms.XASEssion API

    /**
     * Gets the session associated with this XASession.
     *
     * @return The session object.
     * @throws JMSException if an internal error occurs.
     */
    public Session getSession() throws JMSException
    {
        if (_jmsSession == null)
        {
            _jmsSession = getAMQConnection().createSession(true, getAcknowledgeMode());
        }
        return _jmsSession;
    }

    /**
     * Returns an XA resource.
     *
     * @return An XA resource.
     */
    public XAResource getXAResource()
    {
        return _xaResource;
    }

    //-- overwritten mehtods
    /**
     * Throws a {@link TransactionInProgressException}, since it should
     * not be called for an XASession object.
     *
     * @throws TransactionInProgressException always.
     */
    public void commit() throws JMSException
    {
        throw new TransactionInProgressException(
                "XASession:  A direct invocation of the commit operation is probibited!");
    }

    /**
     * Throws a {@link TransactionInProgressException}, since it should
     * not be called for an XASession object.
     *
     * @throws TransactionInProgressException always.
     */
    public void rollback() throws JMSException
    {
        throw new TransactionInProgressException(
                "XASession: A direct invocation of the rollback operation is probibited!");
    }

    /**
     * Access to the underlying Qpid Session
     *
     * @return The associated Qpid Session.
     */
    protected org.apache.qpid.transport.Session getQpidSession()
    {
        return _qpidDtxSession;
    }

    //--- interface  XAQueueSession
    /**
     * Gets the topic session associated with this <CODE>XATopicSession</CODE>.
     *
     * @return the topic session object
     * @throws JMSException If an internal error occurs.
     */
    public QueueSession getQueueSession() throws JMSException
    {
        return (QueueSession) getSession();
    }

    //--- interface  XATopicSession

    /**
     * Gets the topic session associated with this <CODE>XATopicSession</CODE>.
     *
     * @return the topic session object
     * @throws JMSException If an internal error occurs.
     */
    public TopicSession getTopicSession() throws JMSException
    {
        return (TopicSession) getSession();
    }
}
