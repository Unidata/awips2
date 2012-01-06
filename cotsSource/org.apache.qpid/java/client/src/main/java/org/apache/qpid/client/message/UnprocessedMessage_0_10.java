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

import org.apache.qpid.transport.MessageTransfer;

/**
 * This class contains everything needed to process a JMS message. It assembles the deliver body, the content header and
 * the content body/ies.
 *
 * Note that the actual work of creating a JMS message for the client code's use is done outside of the MINA dispatcher
 * thread in order to minimise the amount of work done in the MINA dispatcher thread.
 */
public class UnprocessedMessage_0_10 extends UnprocessedMessage
{
    private MessageTransfer _transfer;

    public UnprocessedMessage_0_10(MessageTransfer xfr)
    {
        super(Integer.parseInt(xfr.getDestination()));
        _transfer = xfr;
    }

    // additional 0_10 method

    public long getDeliveryTag()
    {
        return _transfer.getId();
    }

    public MessageTransfer getMessageTransfer()
    {
        return _transfer;
    }
}
