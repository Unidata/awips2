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
package org.apache.qpid.client.handler;

import java.util.Map;
import java.util.HashMap;

import org.apache.qpid.framing.*;
import org.apache.qpid.AMQException;
import org.apache.qpid.client.state.AMQStateManager;
import org.apache.qpid.client.state.AMQMethodNotImplementedException;
import org.apache.qpid.client.protocol.AMQProtocolSession;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ClientMethodDispatcherImpl implements MethodDispatcher
{

    private static final BasicCancelOkMethodHandler _basicCancelOkMethodHandler = BasicCancelOkMethodHandler.getInstance();
    private static final BasicDeliverMethodHandler _basicDeliverMethodHandler = BasicDeliverMethodHandler.getInstance();
    private static final BasicReturnMethodHandler _basicReturnMethodHandler = BasicReturnMethodHandler.getInstance();
    private static final ChannelCloseMethodHandler _channelCloseMethodHandler = ChannelCloseMethodHandler.getInstance();
    private static final ChannelFlowOkMethodHandler _channelFlowOkMethodHandler = ChannelFlowOkMethodHandler.getInstance();
    private static final ChannelFlowMethodHandler _channelFlowMethodHandler = ChannelFlowMethodHandler.getInstance();
    private static final ConnectionCloseMethodHandler _connectionCloseMethodHandler = ConnectionCloseMethodHandler.getInstance();
    private static final ConnectionOpenOkMethodHandler _connectionOpenOkMethodHandler = ConnectionOpenOkMethodHandler.getInstance();
    private static final ConnectionRedirectMethodHandler _connectionRedirectMethodHandler = ConnectionRedirectMethodHandler.getInstance();
    private static final ConnectionSecureMethodHandler _connectionSecureMethodHandler = ConnectionSecureMethodHandler.getInstance();
    private static final ConnectionStartMethodHandler _connectionStartMethodHandler = ConnectionStartMethodHandler.getInstance();
    private static final ConnectionTuneMethodHandler _connectionTuneMethodHandler = ConnectionTuneMethodHandler.getInstance();
    private static final ExchangeBoundOkMethodHandler _exchangeBoundOkMethodHandler = ExchangeBoundOkMethodHandler.getInstance();
    private static final QueueDeleteOkMethodHandler _queueDeleteOkMethodHandler = QueueDeleteOkMethodHandler.getInstance();

    private static final Logger _logger = LoggerFactory.getLogger(ClientMethodDispatcherImpl.class);

    private static interface DispatcherFactory
    {
        public ClientMethodDispatcherImpl createMethodDispatcher(AMQProtocolSession session);
    }

    private static final Map<ProtocolVersion, DispatcherFactory> _dispatcherFactories =
            new HashMap<ProtocolVersion, DispatcherFactory>();

    static
    {
        _dispatcherFactories.put(ProtocolVersion.v8_0,
                                 new DispatcherFactory()
                                 {
                                     public ClientMethodDispatcherImpl createMethodDispatcher(AMQProtocolSession session)
                                     {
                                         return new ClientMethodDispatcherImpl_8_0(session);
                                     }
                                 });

        _dispatcherFactories.put(ProtocolVersion.v0_9,
                                 new DispatcherFactory()
                                 {
                                     public ClientMethodDispatcherImpl createMethodDispatcher(AMQProtocolSession session)
                                     {
                                         return new ClientMethodDispatcherImpl_0_9(session);
                                     }
                                 });


        _dispatcherFactories.put(ProtocolVersion.v0_91,
                                 new DispatcherFactory()
                                 {
                                     public ClientMethodDispatcherImpl createMethodDispatcher(AMQProtocolSession session)
                                     {
                                         return new ClientMethodDispatcherImpl_0_91(session);
                                     }
                                 });

    }

    public static ClientMethodDispatcherImpl newMethodDispatcher(ProtocolVersion version, AMQProtocolSession session)
    {
        if (_logger.isInfoEnabled())
        {
            _logger.info("New Method Dispatcher:" + session);
        }
        
        DispatcherFactory factory = _dispatcherFactories.get(version);
        return factory.createMethodDispatcher(session);
    }

    AMQProtocolSession _session;

    public ClientMethodDispatcherImpl(AMQProtocolSession session)
    {
        _session = session;
    }

    public AMQStateManager getStateManager()
    {
        return _session.getStateManager();
    }

    public boolean dispatchAccessRequestOk(AccessRequestOkBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchBasicCancelOk(BasicCancelOkBody body, int channelId) throws AMQException
    {
        _basicCancelOkMethodHandler.methodReceived(_session, body, channelId);
        return true;
    }

    public boolean dispatchBasicConsumeOk(BasicConsumeOkBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchBasicDeliver(BasicDeliverBody body, int channelId) throws AMQException
    {
        _basicDeliverMethodHandler.methodReceived(_session, body, channelId);
        return true;
    }

    public boolean dispatchBasicGetEmpty(BasicGetEmptyBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchBasicGetOk(BasicGetOkBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchBasicQosOk(BasicQosOkBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchBasicReturn(BasicReturnBody body, int channelId) throws AMQException
    {
        _basicReturnMethodHandler.methodReceived(_session, body, channelId);
        return true;
    }

    public boolean dispatchChannelClose(ChannelCloseBody body, int channelId) throws AMQException
    {
        _channelCloseMethodHandler.methodReceived(_session, body, channelId);
        return true;
    }

    public boolean dispatchChannelCloseOk(ChannelCloseOkBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchChannelFlow(ChannelFlowBody body, int channelId) throws AMQException
    {
        _channelFlowMethodHandler.methodReceived(_session, body, channelId);
        return true;
    }

    public boolean dispatchChannelFlowOk(ChannelFlowOkBody body, int channelId) throws AMQException
    {
        _channelFlowOkMethodHandler.methodReceived(_session, body, channelId);
        return true;
    }

    public boolean dispatchChannelOpenOk(ChannelOpenOkBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchConnectionClose(ConnectionCloseBody body, int channelId) throws AMQException
    {
        _connectionCloseMethodHandler.methodReceived(_session, body, channelId);
        return true;
    }

    public boolean dispatchConnectionCloseOk(ConnectionCloseOkBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchConnectionOpenOk(ConnectionOpenOkBody body, int channelId) throws AMQException
    {
        _connectionOpenOkMethodHandler.methodReceived(_session, body, channelId);
        return true;
    }

    public boolean dispatchConnectionRedirect(ConnectionRedirectBody body, int channelId) throws AMQException
    {
        _connectionRedirectMethodHandler.methodReceived(_session, body, channelId);
        return true;
    }

    public boolean dispatchConnectionSecure(ConnectionSecureBody body, int channelId) throws AMQException
    {
        _connectionSecureMethodHandler.methodReceived(_session, body, channelId);
        return true;
    }

    public boolean dispatchConnectionStart(ConnectionStartBody body, int channelId) throws AMQException
    {
        _connectionStartMethodHandler.methodReceived(_session, body, channelId);
        return true;
    }

    public boolean dispatchConnectionTune(ConnectionTuneBody body, int channelId) throws AMQException
    {
        _connectionTuneMethodHandler.methodReceived(_session, body, channelId);
        return true;
    }

    public boolean dispatchQueueDeleteOk(QueueDeleteOkBody body, int channelId) throws AMQException
    {
        _queueDeleteOkMethodHandler.methodReceived(_session, body, channelId);
        return true;
    }

    public boolean dispatchQueuePurgeOk(QueuePurgeOkBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchStreamCancelOk(StreamCancelOkBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchStreamConsumeOk(StreamConsumeOkBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchAccessRequest(AccessRequestBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchBasicAck(BasicAckBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchBasicCancel(BasicCancelBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchBasicConsume(BasicConsumeBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchBasicGet(BasicGetBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchBasicPublish(BasicPublishBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchBasicQos(BasicQosBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchBasicRecover(BasicRecoverBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchBasicReject(BasicRejectBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchChannelOpen(ChannelOpenBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchConnectionOpen(ConnectionOpenBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchConnectionSecureOk(ConnectionSecureOkBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchConnectionStartOk(ConnectionStartOkBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchConnectionTuneOk(ConnectionTuneOkBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchDtxSelect(DtxSelectBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchDtxStart(DtxStartBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchExchangeBound(ExchangeBoundBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchExchangeDeclare(ExchangeDeclareBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchExchangeDelete(ExchangeDeleteBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchFileAck(FileAckBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchFileCancel(FileCancelBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchFileConsume(FileConsumeBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchFilePublish(FilePublishBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchFileQos(FileQosBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchFileReject(FileRejectBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchQueueBind(QueueBindBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchQueueDeclare(QueueDeclareBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchQueueDelete(QueueDeleteBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchQueuePurge(QueuePurgeBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchStreamCancel(StreamCancelBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchStreamConsume(StreamConsumeBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchStreamPublish(StreamPublishBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchStreamQos(StreamQosBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchTunnelRequest(TunnelRequestBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchTxCommit(TxCommitBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchTxRollback(TxRollbackBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchTxSelect(TxSelectBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchDtxSelectOk(DtxSelectOkBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchDtxStartOk(DtxStartOkBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchExchangeBoundOk(ExchangeBoundOkBody body, int channelId) throws AMQException
    {
        _exchangeBoundOkMethodHandler.methodReceived(_session, body, channelId);
        return true;
    }

    public boolean dispatchExchangeDeclareOk(ExchangeDeclareOkBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchExchangeDeleteOk(ExchangeDeleteOkBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchFileCancelOk(FileCancelOkBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchFileConsumeOk(FileConsumeOkBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchFileDeliver(FileDeliverBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchFileOpen(FileOpenBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchFileOpenOk(FileOpenOkBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchFileQosOk(FileQosOkBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchFileReturn(FileReturnBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchFileStage(FileStageBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchQueueBindOk(QueueBindOkBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchQueueDeclareOk(QueueDeclareOkBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchStreamDeliver(StreamDeliverBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchStreamQosOk(StreamQosOkBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchStreamReturn(StreamReturnBody body, int channelId) throws AMQException
    {
        throw new AMQMethodNotImplementedException(body);
    }

    public boolean dispatchTxCommitOk(TxCommitOkBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchTxRollbackOk(TxRollbackOkBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchTxSelectOk(TxSelectOkBody body, int channelId) throws AMQException
    {
        return false;
    }

}
