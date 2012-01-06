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
using System.Collections.Generic;
using System.IO;
using org.apache.qpid.transport;

namespace org.apache.qpid.client
{
    public class Message : IMessage
    {
        private readonly MessageTransfer _message;

        public Message(MessageTransfer m)
        {
            _message = m;    
        }

        public Message()
        {
            _message = new MessageTransfer();
            _message.Header = new Header( new MessageProperties(), new DeliveryProperties());
            ((MessageProperties) _message.Header.Structs[0]).SetApplicationHeaders(new Dictionary<string, object>());
        }

        public MessageProperties MessageProperties
        {
            get
            {
                if (_message.Header != null && Header.Structs.Length > 1)
                    return (MessageProperties) Header.Structs[0];
                return null;
            }
            set
            {
                if (_message.Header != null)
                {
                    Header.Structs[0] = value;
                }
            }
        }

        public DeliveryProperties DeliveryProperties
        {
            get
            {
                if (Header != null)
                {
                    if( Header.Structs.Length > 1 )
                        return (DeliveryProperties)Header.Structs[1];
                    return (DeliveryProperties)Header.Structs[0];
                }
                    
                return null;
            }
            set
            {
                if (Header != null)
                {
                    Header.Structs[1] = value;
                }
            }
        }

        public Dictionary<string, object> ApplicationHeaders
        {
            get
            {
                if (Header != null)
                    return ((MessageProperties) Header.Structs[0]).GetApplicationHeaders();
                return null;
            }
            set
            {
                if (Header != null)
                {
                    ((MessageProperties) Header.Structs[0]).SetApplicationHeaders(value);
                }
            }
        }

        public void AppendData(byte[] bytes)
        {
            Body.Write(bytes, 0, bytes.Length);
        }

        public void ClearData()
        {
            Body.Seek(0, SeekOrigin.Begin);
        }

        public Header Header
        {
            get{ return _message.Header;}
            set{ _message.Header = value;}
        }

        public MemoryStream Body
        {
            get { return _message.Body; }
            set { _message.Body = value; }
        }

        public int Id
        {
            get { return _message.Id; }
        }

        public string Destination
        {
            get{ return _message.GetDestination();}
        }
    }
}
