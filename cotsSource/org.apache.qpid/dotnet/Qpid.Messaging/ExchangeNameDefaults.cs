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
namespace Apache.Qpid.Messaging
{
    public class ExchangeNameDefaults
    {
        public readonly static string TOPIC = "amq.topic";
        public readonly static string DIRECT = "amq.direct";
        public readonly static string HEADERS = "amq.match"; 
        public readonly static string FANOUT = "amq.fanout"; 

        /// <summary> Defines the identifying type name of topic exchanges. </summary>
        public readonly static string TOPIC_EXCHANGE_CLASS = "topic";
        
        /// <summary> Defines the identifying type name of direct exchanges. </summary>
        public readonly static string DIRECT_EXCHANGE_CLASS = "direct";
        
        /// <summary> Defines the identifying type name of headers exchanges. </summary>
        public readonly static string HEADERS_EXCHANGE_CLASS = "headers";
        
        /// <summary> Defines the identifying type name of fanout exchanges. </summary>
        public readonly static string FANOUT_EXCHANGE_CLASS = "fanout";       
    }
}
