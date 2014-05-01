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
using System.Collections;
using Apache.Qpid.Framing;

namespace Apache.Qpid.Client.Message
{
    public class UnprocessedMessage
    {
        private ulong _bytesReceived = 0;

        public BasicDeliverBody DeliverBody;
        public BasicReturnBody BounceBody;
        public ushort ChannelId;
        public ContentHeaderBody ContentHeader;

        /// <summary>
        /// List of ContentBody instances. Due to fragmentation you don't know how big this will be in general
        /// </summary>
        /// TODO: write and use linked list class
        public IList Bodies = new ArrayList();

        public void ReceiveBody(ContentBody body)
        {
            Bodies.Add(body);
            if (body.Payload != null)
            {
                _bytesReceived += (uint)body.Payload.Remaining;
            }
        }

        public bool IsAllBodyDataReceived()
        {
            return _bytesReceived == ContentHeader.BodySize;
        }
    }
}


