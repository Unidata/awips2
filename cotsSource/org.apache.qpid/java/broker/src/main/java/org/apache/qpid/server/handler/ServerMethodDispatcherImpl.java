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
package org.apache.qpid.server.handler;

import java.util.Map;
import java.util.HashMap;

import org.apache.qpid.server.state.AMQStateManager;
import org.apache.qpid.framing.*;
import org.apache.qpid.AMQException;

public class ServerMethodDispatcherImpl implements MethodDispatcher
{
    private final AMQStateManager _stateManager;

    private static interface DispatcherFactory
        {
            public MethodDispatcher createMethodDispatcher(AMQStateManager stateManager);
        }

        private static final Map<ProtocolVersion, DispatcherFactory> _dispatcherFactories =
                new HashMap<ProtocolVersion, DispatcherFactory>();


    static
        {
            _dispatcherFactories.put(ProtocolVersion.v8_0,
                                     new DispatcherFactory()
                                     {
                                         public MethodDispatcher createMethodDispatcher(AMQStateManager stateManager)
                                         {
                                             return new ServerMethodDispatcherImpl_8_0(stateManager);
                                         }
                                     });

            _dispatcherFactories.put(ProtocolVersion.v0_9,
                                     new DispatcherFactory()
                                     {
                                         public MethodDispatcher createMethodDispatcher(AMQStateManager stateManager)
                                         {
                                             return new ServerMethodDispatcherImpl_0_9(stateManager);
                                         }
                                     });
            _dispatcherFactories.put(ProtocolVersion.v0_91,
                         new DispatcherFactory()
                         {
                             public MethodDispatcher createMethodDispatcher(AMQStateManager stateManager)
                             {
                                 return new ServerMethodDispatcherImpl_0_91(stateManager);
                             }
                         });

        }


    private static final AccessRequestHandler _accessRequestHandler = AccessRequestHandler.getInstance();
    private static final ChannelCloseHandler _channelCloseHandler = ChannelCloseHandler.getInstance();
    private static final ChannelOpenHandler _channelOpenHandler = ChannelOpenHandler.getInstance();
    private static final ChannelCloseOkHandler _channelCloseOkHandler = ChannelCloseOkHandler.getInstance();
    private static final ConnectionCloseMethodHandler _connectionCloseMethodHandler = ConnectionCloseMethodHandler.getInstance();
    private static final ConnectionCloseOkMethodHandler _connectionCloseOkMethodHandler = ConnectionCloseOkMethodHandler.getInstance();
    private static final ConnectionOpenMethodHandler _connectionOpenMethodHandler = ConnectionOpenMethodHandler.getInstance();
    private static final ConnectionTuneOkMethodHandler _connectionTuneOkMethodHandler = ConnectionTuneOkMethodHandler.getInstance();
    private static final ConnectionSecureOkMethodHandler _connectionSecureOkMethodHandler = ConnectionSecureOkMethodHandler.getInstance();
    private static final ConnectionStartOkMethodHandler _connectionStartOkMethodHandler = ConnectionStartOkMethodHandler.getInstance();
    private static final ExchangeDeclareHandler _exchangeDeclareHandler = ExchangeDeclareHandler.getInstance();
    private static final ExchangeDeleteHandler _exchangeDeleteHandler = ExchangeDeleteHandler.getInstance();
    private static final ExchangeBoundHandler _exchangeBoundHandler = ExchangeBoundHandler.getInstance();
    private static final BasicAckMethodHandler _basicAckMethodHandler = BasicAckMethodHandler.getInstance();
    private static final BasicRecoverMethodHandler _basicRecoverMethodHandler = BasicRecoverMethodHandler.getInstance();
    private static final BasicConsumeMethodHandler _basicConsumeMethodHandler = BasicConsumeMethodHandler.getInstance();
    private static final BasicGetMethodHandler _basicGetMethodHandler = BasicGetMethodHandler.getInstance();
    private static final BasicCancelMethodHandler _basicCancelMethodHandler = BasicCancelMethodHandler.getInstance();
    private static final BasicPublishMethodHandler _basicPublishMethodHandler = BasicPublishMethodHandler.getInstance();
    private static final BasicQosHandler _basicQosHandler = BasicQosHandler.getInstance();
    private static final QueueBindHandler _queueBindHandler = QueueBindHandler.getInstance();
    private static final QueueDeclareHandler _queueDeclareHandler = QueueDeclareHandler.getInstance();
    private static final QueueDeleteHandler _queueDeleteHandler = QueueDeleteHandler.getInstance();
    private static final QueuePurgeHandler _queuePurgeHandler = QueuePurgeHandler.getInstance();
    private static final ChannelFlowHandler _channelFlowHandler = ChannelFlowHandler.getInstance();
    private static final TxSelectHandler _txSelectHandler = TxSelectHandler.getInstance();
    private static final TxCommitHandler _txCommitHandler = TxCommitHandler.getInstance();
    private static final TxRollbackHandler _txRollbackHandler = TxRollbackHandler.getInstance();
    private static final BasicRejectMethodHandler _basicRejectMethodHandler = BasicRejectMethodHandler.getInstance();



    public static MethodDispatcher createMethodDispatcher(AMQStateManager stateManager, ProtocolVersion protocolVersion)
    {
        return _dispatcherFactories.get(protocolVersion).createMethodDispatcher(stateManager);
    }


    public ServerMethodDispatcherImpl(AMQStateManager stateManager)
    {
        _stateManager = stateManager;
    }


    protected AMQStateManager getStateManager()
    {
        return _stateManager;
    }
    


    public boolean dispatchAccessRequest(AccessRequestBody body, int channelId) throws AMQException
    {
        _accessRequestHandler.methodReceived(_stateManager, body, channelId);
        return true;
    }

    public boolean dispatchBasicAck(BasicAckBody body, int channelId) throws AMQException
    {
        _basicAckMethodHandler.methodReceived(_stateManager, body, channelId);
        return true;
    }

    public boolean dispatchBasicCancel(BasicCancelBody body, int channelId) throws AMQException
    {
        _basicCancelMethodHandler.methodReceived(_stateManager, body, channelId);
        return true;
    }

    public boolean dispatchBasicConsume(BasicConsumeBody body, int channelId) throws AMQException
    {
        _basicConsumeMethodHandler.methodReceived(_stateManager, body, channelId);
        return true;
    }

    public boolean dispatchBasicGet(BasicGetBody body, int channelId) throws AMQException
    {
        _basicGetMethodHandler.methodReceived(_stateManager, body, channelId);
        return true;
    }

    public boolean dispatchBasicPublish(BasicPublishBody body, int channelId) throws AMQException
    {
        _basicPublishMethodHandler.methodReceived(_stateManager, body, channelId);
        return true;
    }

    public boolean dispatchBasicQos(BasicQosBody body, int channelId) throws AMQException
    {
        _basicQosHandler.methodReceived(_stateManager, body, channelId);
        return true;
    }

    public boolean dispatchBasicRecover(BasicRecoverBody body, int channelId) throws AMQException
    {
        _basicRecoverMethodHandler.methodReceived(_stateManager, body, channelId);
        return true;
    }

    public boolean dispatchBasicReject(BasicRejectBody body, int channelId) throws AMQException
    {
        _basicRejectMethodHandler.methodReceived(_stateManager, body, channelId);
        return true;
    }

    public boolean dispatchChannelOpen(ChannelOpenBody body, int channelId) throws AMQException
    {
        _channelOpenHandler.methodReceived(_stateManager, body, channelId);
        return true;
    }


    public boolean dispatchAccessRequestOk(AccessRequestOkBody body, int channelId) throws AMQException
    {
        throw new UnexpectedMethodException(body);
    }

    public boolean dispatchBasicCancelOk(BasicCancelOkBody body, int channelId) throws AMQException
    {
        throw new UnexpectedMethodException(body);
    }

    public boolean dispatchBasicConsumeOk(BasicConsumeOkBody body, int channelId) throws AMQException
    {
        throw new UnexpectedMethodException(body);
    }

    public boolean dispatchBasicDeliver(BasicDeliverBody body, int channelId) throws AMQException
    {
        throw new UnexpectedMethodException(body);
    }

    public boolean dispatchBasicGetEmpty(BasicGetEmptyBody body, int channelId) throws AMQException
    {
        throw new UnexpectedMethodException(body);
    }

    public boolean dispatchBasicGetOk(BasicGetOkBody body, int channelId) throws AMQException
    {
        throw new UnexpectedMethodException(body);
    }

    public boolean dispatchBasicQosOk(BasicQosOkBody body, int channelId) throws AMQException
    {
        throw new UnexpectedMethodException(body);
    }

    public boolean dispatchBasicReturn(BasicReturnBody body, int channelId) throws AMQException
    {
        throw new UnexpectedMethodException(body);
    }

    public boolean dispatchChannelClose(ChannelCloseBody body, int channelId) throws AMQException
    {
        _channelCloseHandler.methodReceived(_stateManager, body, channelId);
        return true;
    }


    public boolean dispatchChannelCloseOk(ChannelCloseOkBody body, int channelId) throws AMQException
    {
        _channelCloseOkHandler.methodReceived(_stateManager, body, channelId);
        return true;
    }


    public boolean dispatchChannelFlow(ChannelFlowBody body, int channelId) throws AMQException
    {
        _channelFlowHandler.methodReceived(_stateManager, body, channelId);
        return true;
    }

    public boolean dispatchChannelFlowOk(ChannelFlowOkBody body, int channelId) throws AMQException
    {
        throw new UnexpectedMethodException(body);
    }

    public boolean dispatchChannelOpenOk(ChannelOpenOkBody body, int channelId) throws AMQException
    {
        throw new UnexpectedMethodException(body);
    }


    public boolean dispatchConnectionOpen(ConnectionOpenBody body, int channelId) throws AMQException
    {
        _connectionOpenMethodHandler.methodReceived(_stateManager, body, channelId);
        return true;
    }


    public boolean dispatchConnectionClose(ConnectionCloseBody body, int channelId) throws AMQException
    {
        _connectionCloseMethodHandler.methodReceived(_stateManager, body, channelId);
        return true;
    }


    public boolean dispatchConnectionCloseOk(ConnectionCloseOkBody body, int channelId) throws AMQException
    {
        _connectionCloseOkMethodHandler.methodReceived(_stateManager, body, channelId);
        return true;
    }

    public boolean dispatchConnectionOpenOk(ConnectionOpenOkBody body, int channelId) throws AMQException
    {
        throw new UnexpectedMethodException(body);
    }

    public boolean dispatchConnectionRedirect(ConnectionRedirectBody body, int channelId) throws AMQException
    {
        throw new UnexpectedMethodException(body);
    }

    public boolean dispatchConnectionSecure(ConnectionSecureBody body, int channelId) throws AMQException
    {
        throw new UnexpectedMethodException(body);
    }

    public boolean dispatchConnectionStart(ConnectionStartBody body, int channelId) throws AMQException
    {
        throw new UnexpectedMethodException(body);
    }

    public boolean dispatchConnectionTune(ConnectionTuneBody body, int channelId) throws AMQException
    {
        throw new UnexpectedMethodException(body);
    }

    public boolean dispatchDtxSelectOk(DtxSelectOkBody body, int channelId) throws AMQException
    {
        throw new UnexpectedMethodException(body);
    }

    public boolean dispatchDtxStartOk(DtxStartOkBody body, int channelId) throws AMQException
    {
        throw new UnexpectedMethodException(body);
    }

    public boolean dispatchExchangeBoundOk(ExchangeBoundOkBody body, int channelId) throws AMQException
    {
        throw new UnexpectedMethodException(body);
    }

    public boolean dispatchExchangeDeclareOk(ExchangeDeclareOkBody body, int channelId) throws AMQException
    {
        throw new UnexpectedMethodException(body);
    }

    public boolean dispatchExchangeDeleteOk(ExchangeDeleteOkBody body, int channelId) throws AMQException
    {
        throw new UnexpectedMethodException(body);
    }

    public boolean dispatchFileCancelOk(FileCancelOkBody body, int channelId) throws AMQException
    {
        throw new UnexpectedMethodException(body);
    }

    public boolean dispatchFileConsumeOk(FileConsumeOkBody body, int channelId) throws AMQException
    {
        throw new UnexpectedMethodException(body);
    }

    public boolean dispatchFileDeliver(FileDeliverBody body, int channelId) throws AMQException
    {
        throw new UnexpectedMethodException(body);
    }

    public boolean dispatchFileOpen(FileOpenBody body, int channelId) throws AMQException
    {
        throw new UnexpectedMethodException(body);
    }

    public boolean dispatchFileOpenOk(FileOpenOkBody body, int channelId) throws AMQException
    {
        throw new UnexpectedMethodException(body);
    }

    public boolean dispatchFileQosOk(FileQosOkBody body, int channelId) throws AMQException
    {
        throw new UnexpectedMethodException(body);
    }

    public boolean dispatchFileReturn(FileReturnBody body, int channelId) throws AMQException
    {
        throw new UnexpectedMethodException(body);
    }

    public boolean dispatchFileStage(FileStageBody body, int channelId) throws AMQException
    {
        throw new UnexpectedMethodException(body);
    }

    public boolean dispatchQueueBindOk(QueueBindOkBody body, int channelId) throws AMQException
    {
        throw new UnexpectedMethodException(body);
    }

    public boolean dispatchQueueDeclareOk(QueueDeclareOkBody body, int channelId) throws AMQException
    {
        throw new UnexpectedMethodException(body);
    }

    public boolean dispatchQueueDeleteOk(QueueDeleteOkBody body, int channelId) throws AMQException
    {
        throw new UnexpectedMethodException(body);
    }

    public boolean dispatchQueuePurgeOk(QueuePurgeOkBody body, int channelId) throws AMQException
    {
        throw new UnexpectedMethodException(body);
    }

    public boolean dispatchStreamCancelOk(StreamCancelOkBody body, int channelId) throws AMQException
    {
        throw new UnexpectedMethodException(body);
    }

    public boolean dispatchStreamConsumeOk(StreamConsumeOkBody body, int channelId) throws AMQException
    {
        throw new UnexpectedMethodException(body);
    }

    public boolean dispatchStreamDeliver(StreamDeliverBody body, int channelId) throws AMQException
    {
        throw new UnexpectedMethodException(body);
    }

    public boolean dispatchStreamQosOk(StreamQosOkBody body, int channelId) throws AMQException
    {
        throw new UnexpectedMethodException(body);
    }

    public boolean dispatchStreamReturn(StreamReturnBody body, int channelId) throws AMQException
    {
        throw new UnexpectedMethodException(body);
    }

    public boolean dispatchTxCommitOk(TxCommitOkBody body, int channelId) throws AMQException
    {
        throw new UnexpectedMethodException(body);
    }

    public boolean dispatchTxRollbackOk(TxRollbackOkBody body, int channelId) throws AMQException
    {
        throw new UnexpectedMethodException(body);
    }

    public boolean dispatchTxSelectOk(TxSelectOkBody body, int channelId) throws AMQException
    {
        throw new UnexpectedMethodException(body);
    }


    public boolean dispatchConnectionSecureOk(ConnectionSecureOkBody body, int channelId) throws AMQException
    {
        _connectionSecureOkMethodHandler.methodReceived(_stateManager, body, channelId);
        return true;
    }

    public boolean dispatchConnectionStartOk(ConnectionStartOkBody body, int channelId) throws AMQException
    {
        _connectionStartOkMethodHandler.methodReceived(_stateManager, body, channelId);
        return true;
    }

    public boolean dispatchConnectionTuneOk(ConnectionTuneOkBody body, int channelId) throws AMQException
    {
        _connectionTuneOkMethodHandler.methodReceived(_stateManager, body, channelId);
        return true;
    }

    public boolean dispatchDtxSelect(DtxSelectBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchDtxStart(DtxStartBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchExchangeBound(ExchangeBoundBody body, int channelId) throws AMQException
    {
        _exchangeBoundHandler.methodReceived(_stateManager, body, channelId);
        return true;
    }

    public boolean dispatchExchangeDeclare(ExchangeDeclareBody body, int channelId) throws AMQException
    {
        _exchangeDeclareHandler.methodReceived(_stateManager, body, channelId);
        return true;
    }

    public boolean dispatchExchangeDelete(ExchangeDeleteBody body, int channelId) throws AMQException
    {
        _exchangeDeleteHandler.methodReceived(_stateManager, body, channelId);
        return true;
    }

    public boolean dispatchFileAck(FileAckBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchFileCancel(FileCancelBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchFileConsume(FileConsumeBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchFilePublish(FilePublishBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchFileQos(FileQosBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchFileReject(FileRejectBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchQueueBind(QueueBindBody body, int channelId) throws AMQException
    {
        _queueBindHandler.methodReceived(_stateManager, body, channelId);
        return true;
    }

    public boolean dispatchQueueDeclare(QueueDeclareBody body, int channelId) throws AMQException
    {
        _queueDeclareHandler.methodReceived(_stateManager, body, channelId);
        return true;
    }

    public boolean dispatchQueueDelete(QueueDeleteBody body, int channelId) throws AMQException
    {
        _queueDeleteHandler.methodReceived(_stateManager, body, channelId);
        return true;
    }

    public boolean dispatchQueuePurge(QueuePurgeBody body, int channelId) throws AMQException
    {
        _queuePurgeHandler.methodReceived(_stateManager, body, channelId);
        return true;
    }

    public boolean dispatchStreamCancel(StreamCancelBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchStreamConsume(StreamConsumeBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchStreamPublish(StreamPublishBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchStreamQos(StreamQosBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchTunnelRequest(TunnelRequestBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchTxCommit(TxCommitBody body, int channelId) throws AMQException
    {
        _txCommitHandler.methodReceived(_stateManager, body, channelId);
        return true;
    }

    public boolean dispatchTxRollback(TxRollbackBody body, int channelId) throws AMQException
    {
        _txRollbackHandler.methodReceived(_stateManager, body, channelId);
        return true;
    }

    public boolean dispatchTxSelect(TxSelectBody body, int channelId) throws AMQException
    {
        _txSelectHandler.methodReceived(_stateManager, body, channelId);
        return true;
    }




}
