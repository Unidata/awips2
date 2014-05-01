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
package org.apache.qpid.client;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.jms.IllegalStateException;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.Queue;
import javax.jms.QueueBrowser;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.concurrent.atomic.AtomicBoolean;

public class AMQQueueBrowser implements QueueBrowser
{
    private static final Logger _logger = LoggerFactory.getLogger(AMQQueueBrowser.class);

    private AtomicBoolean _isClosed = new AtomicBoolean();
    private final AMQSession _session;
    private final AMQQueue _queue;
    private final ArrayList<BasicMessageConsumer> _consumers = new ArrayList<BasicMessageConsumer>();
    private final String _messageSelector;

    AMQQueueBrowser(AMQSession session, AMQQueue queue, String messageSelector) throws JMSException
    {
        _session = session;
        _queue = queue;
        _messageSelector = ((messageSelector == null) || (messageSelector.trim().length() == 0)) ? null : messageSelector;
        // Create Consumer to verify message selector.
        BasicMessageConsumer consumer =
                (BasicMessageConsumer) _session.createBrowserConsumer(_queue, _messageSelector, false);
        // Close this consumer as we are not looking to consume only to establish that, at least for now,
        // the QB can be created
        consumer.close();
    }

    public Queue getQueue() throws JMSException
    {
        checkState();

        return _queue;
    }

    private void checkState() throws JMSException
    {
        if (_isClosed.get())
        {
            throw new IllegalStateException("Queue Browser");
        }

        if (_session.isClosed())
        {
            throw new IllegalStateException("Session is closed");
        }

    }

    public String getMessageSelector() throws JMSException
    {

        checkState();

        return _messageSelector;
    }

    public Enumeration getEnumeration() throws JMSException
    {
        checkState();
        final BasicMessageConsumer consumer =
                (BasicMessageConsumer) _session.createBrowserConsumer(_queue, _messageSelector, false);

        _consumers.add(consumer);

        return new QueueBrowserEnumeration(consumer);
    }

    public void close() throws JMSException
    {
        for (BasicMessageConsumer consumer : _consumers)
        {
            consumer.close();
        }

        _consumers.clear();
    }

    private class QueueBrowserEnumeration implements Enumeration
    {
        Message _nextMessage;
        private BasicMessageConsumer _consumer;

        public QueueBrowserEnumeration(BasicMessageConsumer consumer) throws JMSException
        {
            _nextMessage = consumer == null ? null : consumer.receiveBrowse();
            _logger.info("QB:created with first element:" + _nextMessage);
            _consumer = consumer;
        }

        public boolean hasMoreElements()
        {
            _logger.info("QB:hasMoreElements:" + (_nextMessage != null));
            return (_nextMessage != null);
        }

        public Object nextElement()
        {
            Message msg = _nextMessage;
            try
            {
                _logger.info("QB:nextElement about to receive");
                _nextMessage = _consumer.receiveBrowse();
                _logger.info("QB:nextElement received:" + _nextMessage);
            }
            catch (JMSException e)
            {
                _logger.warn("Exception caught while queue browsing", e);
                _nextMessage = null;
            }
            return msg;
        }
    }    
}
