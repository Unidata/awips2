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
using log4net;
using Apache.Qpid.Buffer;

namespace Apache.Qpid.Client.Message
{
    public abstract class AbstractQmsMessageFactory : IMessageFactory
    {
        public abstract AbstractQmsMessage CreateMessage(string mimeType);

        private static readonly ILog _logger = LogManager.GetLogger(typeof (AbstractQmsMessageFactory));

        protected abstract AbstractQmsMessage CreateMessage(long messageNbr, ByteBuffer data, ContentHeaderBody contentHeader);

        protected AbstractQmsMessage CreateMessageWithBody(long messageNbr,
                                                           ContentHeaderBody contentHeader,
                                                           IList bodies)
        {
            ByteBuffer data;

            // we optimise the non-fragmented case to avoid copying
            if (bodies != null && bodies.Count == 1)
            {
                _logger.Debug("Non-fragmented message body (bodySize=" + contentHeader.BodySize +")");
                data = ((ContentBody)bodies[0]).Payload;
            }
            else
            {
                _logger.Debug("Fragmented message body (" + bodies.Count + " frames, bodySize=" + contentHeader.BodySize + ")");
                data = ByteBuffer.Allocate((int)contentHeader.BodySize); // XXX: Is cast a problem?
                foreach (ContentBody body in bodies) {
                    data.Put(body.Payload);
                    //body.Payload.Release();
                }

                data.Flip();
            }
            _logger.Debug("Creating message from buffer with position=" + data.Position + " and remaining=" + data.Remaining);

            return CreateMessage(messageNbr, data, contentHeader);
        }

        public AbstractQmsMessage CreateMessage(long messageNbr, bool redelivered,
                                                ContentHeaderBody contentHeader,
                                                IList bodies)
        {
            AbstractQmsMessage msg = CreateMessageWithBody(messageNbr, contentHeader, bodies);
            msg.Redelivered = redelivered;
            return msg;
        }
    }
}
