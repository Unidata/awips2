/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.    
 *
 * 
 */
package org.apache.qpid.test.unit.client.channelclose;

import org.apache.qpid.client.state.AMQStateManager;
import org.apache.qpid.client.state.AMQState;
import org.apache.qpid.client.handler.ConnectionStartMethodHandler;
import org.apache.qpid.client.handler.ConnectionCloseMethodHandler;
import org.apache.qpid.client.handler.ConnectionTuneMethodHandler;
import org.apache.qpid.client.handler.ConnectionSecureMethodHandler;
import org.apache.qpid.client.handler.ConnectionOpenOkMethodHandler;
import org.apache.qpid.client.handler.ChannelCloseOkMethodHandler;
import org.apache.qpid.client.handler.BasicDeliverMethodHandler;
import org.apache.qpid.client.handler.BasicReturnMethodHandler;
import org.apache.qpid.client.handler.BasicCancelOkMethodHandler;
import org.apache.qpid.client.handler.ChannelFlowOkMethodHandler;
import org.apache.qpid.client.handler.QueueDeleteOkMethodHandler;
import org.apache.qpid.client.handler.ExchangeBoundOkMethodHandler;
import org.apache.qpid.client.protocol.AMQProtocolSession;
import org.apache.qpid.framing.ConnectionStartBody;
import org.apache.qpid.framing.ConnectionCloseBody;
import org.apache.qpid.framing.ConnectionTuneBody;
import org.apache.qpid.framing.ConnectionSecureBody;
import org.apache.qpid.framing.ConnectionOpenOkBody;
import org.apache.qpid.framing.ChannelCloseBody;
import org.apache.qpid.framing.ChannelCloseOkBody;
import org.apache.qpid.framing.BasicDeliverBody;
import org.apache.qpid.framing.BasicReturnBody;
import org.apache.qpid.framing.BasicCancelOkBody;
import org.apache.qpid.framing.ChannelFlowOkBody;
import org.apache.qpid.framing.QueueDeleteOkBody;
import org.apache.qpid.framing.ExchangeBoundOkBody;

import java.util.Map;
import java.util.HashMap;

public class NoCloseOKStateManager extends AMQStateManager
{
    public NoCloseOKStateManager(AMQProtocolSession protocolSession)
    {
        super(protocolSession);
    }

    


}
