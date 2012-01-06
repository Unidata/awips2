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


import org.apache.qpid.framing.*;
import org.apache.qpid.framing.amqp_0_91.MethodDispatcher_0_91;
import org.apache.qpid.server.state.AMQStateManager;
import org.apache.qpid.AMQException;


public class ServerMethodDispatcherImpl_0_91
        extends ServerMethodDispatcherImpl
        implements MethodDispatcher_0_91

{

    private static final BasicRecoverSyncMethodHandler _basicRecoverSyncMethodHandler =
            BasicRecoverSyncMethodHandler.getInstance();
    private static final QueueUnbindHandler _queueUnbindHandler =
            QueueUnbindHandler.getInstance();


    public ServerMethodDispatcherImpl_0_91(AMQStateManager stateManager)
    {
        super(stateManager);
    }

    public boolean dispatchBasicRecoverSync(BasicRecoverSyncBody body, int channelId) throws AMQException
    {
        _basicRecoverSyncMethodHandler.methodReceived(getStateManager(), body, channelId);
        return true;
    }

    public boolean dispatchBasicRecoverSyncOk(BasicRecoverSyncOkBody body, int channelId) throws AMQException
    {
        throw new UnexpectedMethodException(body);
    }

    public boolean dispatchChannelOk(ChannelOkBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchChannelPing(ChannelPingBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchChannelPong(ChannelPongBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchChannelResume(ChannelResumeBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchMessageAppend(MessageAppendBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchMessageCancel(MessageCancelBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchMessageCheckpoint(MessageCheckpointBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchMessageClose(MessageCloseBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchMessageConsume(MessageConsumeBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchMessageEmpty(MessageEmptyBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchMessageGet(MessageGetBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchMessageOffset(MessageOffsetBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchMessageOk(MessageOkBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchMessageOpen(MessageOpenBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchMessageQos(MessageQosBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchMessageRecover(MessageRecoverBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchMessageReject(MessageRejectBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchMessageResume(MessageResumeBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchMessageTransfer(MessageTransferBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchBasicRecoverOk(BasicRecoverOkBody body, int channelId) throws AMQException
    {
        return false;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public boolean dispatchQueueUnbindOk(QueueUnbindOkBody body, int channelId) throws AMQException
    {
        throw new UnexpectedMethodException(body);
    }

    public boolean dispatchQueueUnbind(QueueUnbindBody body, int channelId) throws AMQException
    {
        _queueUnbindHandler.methodReceived(getStateManager(),body,channelId);
        return true;
    }
}