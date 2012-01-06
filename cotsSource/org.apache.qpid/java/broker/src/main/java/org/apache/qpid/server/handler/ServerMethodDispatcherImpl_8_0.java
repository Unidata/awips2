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

import org.apache.qpid.framing.amqp_8_0.MethodDispatcher_8_0;
import org.apache.qpid.framing.*;
import org.apache.qpid.server.state.AMQStateManager;
import org.apache.qpid.AMQException;

public class ServerMethodDispatcherImpl_8_0
        extends ServerMethodDispatcherImpl
        implements MethodDispatcher_8_0
{
    public ServerMethodDispatcherImpl_8_0(AMQStateManager stateManager)
    {
        super(stateManager);
    }

    public boolean dispatchBasicRecoverOk(BasicRecoverOkBody body, int channelId) throws AMQException
    {
        throw new UnexpectedMethodException(body);
    }

    public boolean dispatchChannelAlert(ChannelAlertBody body, int channelId) throws AMQException
    {
        throw new UnexpectedMethodException(body);
    }

    public boolean dispatchTestContent(TestContentBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchTestContentOk(TestContentOkBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchTestInteger(TestIntegerBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchTestIntegerOk(TestIntegerOkBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchTestString(TestStringBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchTestStringOk(TestStringOkBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchTestTable(TestTableBody body, int channelId) throws AMQException
    {
        return false;
    }

    public boolean dispatchTestTableOk(TestTableOkBody body, int channelId) throws AMQException
    {
        return false;
    }
}
