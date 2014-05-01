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
package org.apache.qpid.client.transport;

import java.io.IOException;
import java.net.InetSocketAddress;

import org.apache.mina.common.ByteBuffer;
import org.apache.mina.common.IoConnector;
import org.apache.mina.common.SimpleByteBufferAllocator;
import org.apache.qpid.client.SSLConfiguration;
import org.apache.qpid.client.protocol.AMQProtocolHandler;
import org.apache.qpid.jms.BrokerDetails;
import org.apache.qpid.ssl.SSLContextFactory;
import org.apache.qpid.transport.network.mina.MINANetworkDriver;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class SocketTransportConnection implements ITransportConnection
{
    private static final Logger _logger = LoggerFactory.getLogger(SocketTransportConnection.class);
    private static final int DEFAULT_BUFFER_SIZE = 32 * 1024;

    private SocketConnectorFactory _socketConnectorFactory;

    static interface SocketConnectorFactory
    {
        IoConnector newSocketConnector();
    }

    public SocketTransportConnection(SocketConnectorFactory socketConnectorFactory)
    {
        _socketConnectorFactory = socketConnectorFactory;
    }

    public void connect(AMQProtocolHandler protocolHandler, BrokerDetails brokerDetail) throws IOException
    {
        ByteBuffer.setUseDirectBuffers(Boolean.getBoolean("amqj.enableDirectBuffers"));

        // the MINA default is currently to use the pooled allocator although this may change in future
        // once more testing of the performance of the simple allocator has been done
        if (!Boolean.getBoolean("amqj.enablePooledAllocator"))
        {
            _logger.info("Using SimpleByteBufferAllocator");
            ByteBuffer.setAllocator(new SimpleByteBufferAllocator());
        }

        final IoConnector ioConnector = _socketConnectorFactory.newSocketConnector();
        final InetSocketAddress address;

        if (brokerDetail.getTransport().equals(BrokerDetails.SOCKET))
        {
            address = null;
        }
        else
        {
            address = new InetSocketAddress(brokerDetail.getHost(), brokerDetail.getPort());
            _logger.info("Attempting connection to " + address);
        }
        
        SSLConfiguration sslConfig = protocolHandler.getConnection().getSSLConfiguration();
        SSLContextFactory sslFactory = null;
        if (sslConfig != null)
        {
            sslFactory = new SSLContextFactory(sslConfig.getKeystorePath(), sslConfig.getKeystorePassword(), sslConfig.getCertType());
        }
        
        MINANetworkDriver driver = new MINANetworkDriver(ioConnector);
        driver.open(brokerDetail.getPort(), address.getAddress(), protocolHandler, null, sslFactory);
        protocolHandler.setNetworkDriver(driver);
    }
}
