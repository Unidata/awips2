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
package org.apache.qpid.client.message;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.framing.BasicDeliverBody;
import org.apache.qpid.framing.ContentBody;
import org.apache.qpid.framing.ContentHeaderBody;

/**
 * This class contains everything needed to process a JMS message. It assembles the deliver body, the content header and
 * the content body/ies.
 *
 * Note that the actual work of creating a JMS message for the client code's use is done outside of the MINA dispatcher
 * thread in order to minimise the amount of work done in the MINA dispatcher thread.
 */
public class UnprocessedMessage_0_8 extends UnprocessedMessage
{
    private long _bytesReceived = 0;


    private AMQShortString _exchange;
    private AMQShortString _routingKey;
    private final long _deliveryId;
    protected boolean _redelivered;

    private BasicDeliverBody _deliverBody;
    private ContentHeaderBody _contentHeader;

    /** List of ContentBody instances. Due to fragmentation you don't know how big this will be in general */
    private List<ContentBody> _bodies;

    public UnprocessedMessage_0_8(long deliveryId, int consumerTag, AMQShortString exchange, AMQShortString routingKey, boolean redelivered)
    {
        super(consumerTag);
        _exchange = exchange;
        _routingKey = routingKey;

        _redelivered = redelivered;
        _deliveryId = deliveryId;
    }


    public AMQShortString getExchange()
    {
        return _exchange;
    }

    public AMQShortString getRoutingKey()
    {
        return _routingKey;
    }

    public long getDeliveryTag()
    {
        return _deliveryId;
    }

    public boolean isRedelivered()
    {
        return _redelivered;
    }


    public void receiveBody(ContentBody body)
    {

        if (body.payload != null)
        {
            final long payloadSize = body.payload.remaining();

            if (_bodies == null)
            {
                if (payloadSize == getContentHeader().bodySize)
                {
                    _bodies = Collections.singletonList(body);
                }
                else
                {
                    _bodies = new ArrayList<ContentBody>();
                    _bodies.add(body);
                }

            }
            else
            {
                _bodies.add(body);
            }
            _bytesReceived += payloadSize;
        }
    }

    public void setMethodBody(BasicDeliverBody deliverBody)
    {
        _deliverBody = deliverBody;
    }

    public void setContentHeader(ContentHeaderBody contentHeader)
    {
        this._contentHeader = contentHeader;
    }

    public boolean isAllBodyDataReceived()
    {
        return _bytesReceived == getContentHeader().bodySize;
    }

    public BasicDeliverBody getDeliverBody()
    {
        return _deliverBody;
    }

    public ContentHeaderBody getContentHeader()
    {
        return _contentHeader;
    }

    public List<ContentBody> getBodies()
    {
        return _bodies;
    }

    public String toString()
    {
        StringBuilder buf = new StringBuilder();

        if (_contentHeader != null)
        {
          buf.append("ContentHeader " + _contentHeader);
        }
        if(_deliverBody != null)
        {
            buf.append("Delivery tag " + _deliverBody.getDeliveryTag());
            buf.append("Consumer tag " + _deliverBody.getConsumerTag());
            buf.append("Deliver Body " + _deliverBody);
        }

        return buf.toString();
    }

}
