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

import org.apache.mina.common.ConnectFuture;
import org.apache.mina.transport.vmpipe.QpidVmPipeConnector;
import org.apache.mina.transport.vmpipe.VmPipeAddress;
import org.apache.mina.transport.vmpipe.VmPipeConnector;
import org.apache.qpid.client.protocol.AMQProtocolHandler;
import org.apache.qpid.jms.BrokerDetails;
import org.apache.qpid.transport.network.mina.MINANetworkDriver;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class VmPipeTransportConnection implements ITransportConnection
{
    private static final Logger _logger = LoggerFactory.getLogger(VmPipeTransportConnection.class);

    private static int _port;

    private MINANetworkDriver _networkDriver;

    public VmPipeTransportConnection(int port)
    {
        _port = port;
    }

    public void connect(AMQProtocolHandler protocolHandler, BrokerDetails brokerDetail) throws IOException
    {
        final VmPipeConnector ioConnector = new QpidVmPipeConnector();

        final VmPipeAddress address = new VmPipeAddress(_port);
        _logger.info("Attempting connection to " + address);
        _networkDriver = new MINANetworkDriver(ioConnector, protocolHandler);
        protocolHandler.setNetworkDriver(_networkDriver);
        ConnectFuture future = ioConnector.connect(address, _networkDriver);
        // wait for connection to complete
        future.join();
        // we call getSession which throws an IOException if there has been an error connecting
        future.getSession();
        _networkDriver.setProtocolEngine(protocolHandler);
    }
}
