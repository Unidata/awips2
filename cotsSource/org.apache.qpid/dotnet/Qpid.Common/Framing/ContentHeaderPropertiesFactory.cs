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
using Apache.Qpid.Buffer;

namespace Apache.Qpid.Framing
{
    public class ContentHeaderPropertiesFactory
    {

        private static readonly ContentHeaderPropertiesFactory _instance = new ContentHeaderPropertiesFactory();

        public static ContentHeaderPropertiesFactory GetInstance()
        {
            return _instance;
        }

        private ContentHeaderPropertiesFactory()
        {
        }

        /// <summary>
        /// Creates the content header properties from a buffer.
        /// </summary>
        /// <param name="classId">The class id.</param>
        /// <param name="propertyFlags">The property flags.</param>
        /// <param name="buffer">The buffer.</param>
        /// <returns>a populated properties structure</returns>
        /// <exception cref="AMQFrameDecodingException">if the buffer cannot be decoded</exception>
        public IContentHeaderProperties CreateContentHeaderProperties(ushort classId, ushort propertyFlags,
                                                                      ByteBuffer buffer)             
        {
            IContentHeaderProperties properties;
            switch (classId)
            {
                case 60:
                    properties = new BasicContentHeaderProperties();
                    break;
                default:
                    throw new AMQFrameDecodingException("Unsupport content header class id: " + classId);
            }
            properties.PopulatePropertiesFromBuffer(buffer, propertyFlags);
            return properties;
        }
    }
}
