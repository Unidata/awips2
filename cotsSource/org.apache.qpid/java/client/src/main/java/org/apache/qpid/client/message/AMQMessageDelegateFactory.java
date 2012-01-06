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

import org.apache.mina.common.ByteBuffer;
import org.apache.qpid.framing.BasicContentHeaderProperties;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.AMQException;

public interface AMQMessageDelegateFactory<D extends AMQMessageDelegate>
{
    public static AMQMessageDelegateFactory DEFAULT_FACTORY = null;

    public static AMQMessageDelegateFactory<AMQMessageDelegate_0_8> FACTORY_0_8 =
            new AMQMessageDelegateFactory<AMQMessageDelegate_0_8>()
            {
                public AMQMessageDelegate_0_8 createDelegate()
                {
                    return new AMQMessageDelegate_0_8();
                }
            };

    public static AMQMessageDelegateFactory<AMQMessageDelegate_0_10> FACTORY_0_10 =
            new AMQMessageDelegateFactory<AMQMessageDelegate_0_10>()
            {
                public AMQMessageDelegate_0_10 createDelegate()
                {
                    return new AMQMessageDelegate_0_10();
                }
            };


    public D createDelegate();

}
