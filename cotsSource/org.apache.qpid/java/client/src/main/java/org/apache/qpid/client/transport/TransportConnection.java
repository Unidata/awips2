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
import java.net.Socket;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.mina.common.IoConnector;
import org.apache.mina.common.IoHandlerAdapter;
import org.apache.mina.common.IoServiceConfig;
import org.apache.mina.transport.socket.nio.ExistingSocketConnector;
import org.apache.mina.transport.socket.nio.MultiThreadSocketConnector;
import org.apache.mina.transport.socket.nio.SocketConnector;
import org.apache.mina.transport.vmpipe.VmPipeAcceptor;
import org.apache.mina.transport.vmpipe.VmPipeAddress;
import org.apache.qpid.client.vmbroker.AMQVMBrokerCreationException;
import org.apache.qpid.jms.BrokerDetails;
import org.apache.qpid.protocol.ProtocolEngineFactory;
import org.apache.qpid.thread.QpidThreadExecutor;
import org.apache.qpid.transport.network.mina.MINANetworkDriver;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The TransportConnection is a helper class responsible for connecting to an AMQ server. It sets up the underlying
 * connector, which currently always uses TCP/IP sockets. It creates the "protocol handler" which deals with MINA
 * protocol events. <p/> Could be extended in future to support different transport types by turning this into concrete
 * class/interface combo.
 */
public class TransportConnection
{
    private static ITransportConnection _instance;

    private static Map _inVmPipeAddress = new HashMap();
    private static VmPipeAcceptor _acceptor;
    private static int _currentInstance = -1;
    private static int _currentVMPort = -1;

    private static final int TCP = 0;
    private static final int VM = 1;
    private static final int SOCKET = 2;

    private static Logger _logger = LoggerFactory.getLogger(TransportConnection.class);

    private static final String DEFAULT_QPID_SERVER = "org.apache.qpid.server.protocol.AMQProtocolEngineFactory";

    private static Map<String, Socket> _openSocketRegister = new ConcurrentHashMap<String, Socket>();

    public static void registerOpenSocket(String socketID, Socket openSocket)
    {
        _openSocketRegister.put(socketID, openSocket);
    }

    public static Socket removeOpenSocket(String socketID)
    {
        return _openSocketRegister.remove(socketID);
    }

    public static synchronized ITransportConnection getInstance(final BrokerDetails details) throws AMQTransportConnectionException
    {
        int transport = getTransport(details.getTransport());

        if (transport == -1)
        {
            throw new AMQNoTransportForProtocolException(details, null, null);
        }

        switch (transport)
        {
            case SOCKET:
                return new SocketTransportConnection(new SocketTransportConnection.SocketConnectorFactory()
                {
                    public IoConnector newSocketConnector()
                    {
                        ExistingSocketConnector connector = new ExistingSocketConnector(1,new QpidThreadExecutor());

                        Socket socket = TransportConnection.removeOpenSocket(details.getHost());

                        if (socket != null)
                        {
                            _logger.info("Using existing Socket:" + socket);

                            ((ExistingSocketConnector) connector).setOpenSocket(socket);
                        }
                        else
                        {
                            throw new IllegalArgumentException("Active Socket must be provided for broker " +
                                                               "with 'socket://<SocketID>' transport:" + details);
                        }
                        return connector;
                    }
                });
            case TCP:
                return new SocketTransportConnection(new SocketTransportConnection.SocketConnectorFactory()
                {
                    public IoConnector newSocketConnector()
                    {
                        SocketConnector result;
                        // FIXME - this needs to be sorted to use the new Mina MultiThread SA.
                        if (Boolean.getBoolean("qpidnio"))
                        {
                            _logger.warn("Using Qpid MultiThreaded NIO - " + (System.getProperties().containsKey("qpidnio")
                                                                              ? "Qpid NIO is new default"
                                                                              : "Sysproperty 'qpidnio' is set"));
                            result = new MultiThreadSocketConnector(1, new QpidThreadExecutor());
                        }
                        else
                        {
                            _logger.info("Using Mina NIO");
                            result = new SocketConnector(1, new QpidThreadExecutor()); // non-blocking connector
                        }
                        // Don't have the connector's worker thread wait around for other connections (we only use
                        // one SocketConnector per connection at the moment anyway). This allows short-running
                        // clients (like unit tests) to complete quickly.
                        result.setWorkerTimeout(0);
                        return result;
                    }
                });
            case VM:
            {
                return getVMTransport(details, Boolean.getBoolean("amqj.AutoCreateVMBroker"));
            }
            default:
                throw new AMQNoTransportForProtocolException(details, "Transport not recognised:" + transport, null);
        }
    }

    private static int getTransport(String transport)
    {
        if (transport.equals(BrokerDetails.SOCKET))
        {
            return SOCKET;
        }

        if (transport.equals(BrokerDetails.TCP))
        {
            return TCP;
        }

        if (transport.equals(BrokerDetails.VM))
        {
            return VM;
        }

        return -1;
    }

    private static ITransportConnection getVMTransport(BrokerDetails details, boolean AutoCreate)
            throws AMQVMBrokerCreationException
    {
        int port = details.getPort();

        synchronized (_inVmPipeAddress)
        {
            if (!_inVmPipeAddress.containsKey(port))
            {
                if (AutoCreate)
                {
                    if (AutoCreate)
                    {
                        _logger.warn("Auto Creating InVM Broker on port:" + port);
                        createVMBroker(port);
                    }
                    else
                    {
                        throw new AMQVMBrokerCreationException(null, port, "VM Broker on port " + port
                                                                           + " does not exist. Auto create disabled.", null);
                    }
                }
                else
                {
                    throw new AMQVMBrokerCreationException(null, port, "VM Broker on port " + port
                                                                       + " does not exist. Auto create disabled.", null);
                }
            }
        }

        return new VmPipeTransportConnection(port);
    }

    public static void createVMBroker(int port) throws AMQVMBrokerCreationException
    {
        if (_acceptor == null)
        {
            _acceptor = new VmPipeAcceptor();

            IoServiceConfig config = _acceptor.getDefaultConfig();
        }
        synchronized (_inVmPipeAddress)
        {

            if (!_inVmPipeAddress.containsKey(port))
            {
                _logger.info("Creating InVM Qpid.AMQP listening on port " + port);
                IoHandlerAdapter provider = null;
                try
                {
                    VmPipeAddress pipe = new VmPipeAddress(port);

                    provider = createBrokerInstance(port);

                    _acceptor.bind(pipe, provider);

                    _inVmPipeAddress.put(port, pipe);
                    _logger.info("Created InVM Qpid.AMQP listening on port " + port);
                }
                catch (IOException e)
                {
                    _logger.error("Got IOException.", e);

                    // Try and unbind provider
                    try
                    {
                        VmPipeAddress pipe = new VmPipeAddress(port);

                        try
                        {
                            _acceptor.unbind(pipe);
                        }
                        catch (Exception ignore)
                        {
                            // ignore
                        }

                        if (provider == null)
                        {
                            provider = createBrokerInstance(port);
                        }

                        _acceptor.bind(pipe, provider);
                        _inVmPipeAddress.put(port, pipe);
                        _logger.info("Created InVM Qpid.AMQP listening on port " + port);
                    }
                    catch (IOException justUseFirstException)
                    {
                        String because;
                        if (e.getCause() == null)
                        {
                            because = e.toString();
                        }
                        else
                        {
                            because = e.getCause().toString();
                        }

                        throw new AMQVMBrokerCreationException(null, port, because + " Stopped binding of InVM Qpid.AMQP", e);
                    }
                }

            }
            else
            {
                _logger.info("InVM Qpid.AMQP on port " + port + " already exits.");
            }
        }
    }

    private static IoHandlerAdapter createBrokerInstance(int port) throws AMQVMBrokerCreationException
    {
        String protocolProviderClass = System.getProperty("amqj.protocolprovider.class", DEFAULT_QPID_SERVER);
        _logger.info("Creating Qpid protocol provider: " + protocolProviderClass);

        // can't use introspection to get Provider as it is a server class.
        // need to go straight to IoHandlerAdapter but that requries the queues and exchange from the ApplicationRegistry which we can't access.

        // get right constructor and pass in instancec ID - "port"
        IoHandlerAdapter provider;
        try
        {
            Class[] cnstr = {Integer.class};
            Object[] params = {port};
            
            provider = new MINANetworkDriver();
            ProtocolEngineFactory engineFactory = (ProtocolEngineFactory) Class.forName(protocolProviderClass).getConstructor(cnstr).newInstance(params);
            ((MINANetworkDriver) provider).setProtocolEngineFactory(engineFactory, true);
            // Give the broker a second to create
            _logger.info("Created VMBroker Instance:" + port);
        }
        catch (Exception e)
        {
            _logger.info("Unable to create InVM Qpid.AMQP on port " + port + ". Because: " + e.getCause());
            String because;
            if (e.getCause() == null)
            {
                because = e.toString();
            }
            else
            {
                because = e.getCause().toString();
            }

            AMQVMBrokerCreationException amqbce =
                    new AMQVMBrokerCreationException(null, port, because + " Stopped InVM Qpid.AMQP creation", e);
            throw amqbce;
        }

        return provider;
    }

    public static void killAllVMBrokers()
    {
        _logger.info("Killing all VM Brokers");
        if (_acceptor != null)
        {
            _acceptor.unbindAll();
        }
        synchronized (_inVmPipeAddress)
        {
            _inVmPipeAddress.clear();
        }
        _acceptor = null;
        _currentInstance = -1;
        _currentVMPort = -1;
    }

    public static void killVMBroker(int port)
    {
        synchronized (_inVmPipeAddress)
        {
            VmPipeAddress pipe = (VmPipeAddress) _inVmPipeAddress.get(port);
            if (pipe != null)
            {
                _logger.info("Killing VM Broker:" + port);
                _inVmPipeAddress.remove(port);
                // This does need to be sychronized as otherwise mina can hang
                // if a new connection is made
                _acceptor.unbind(pipe);
            }
        }
    }

}
