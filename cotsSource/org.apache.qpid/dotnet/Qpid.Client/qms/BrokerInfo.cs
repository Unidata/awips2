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
using System;

namespace Apache.Qpid.Client.Qms
{
    /// <summary>
    /// Know URL option names.
    /// <seealso cref="IConnectionInfo"/>
    /// </summary>
    public class BrokerInfoConstants
    {
        public const String OPTIONS_RETRY = "retries";
        public const String OPTIONS_SSL = ConnectionUrlConstants.OPTIONS_SSL;
        public const String OPTIONS_CONNECT_TIMEOUT = "connecttimeout";
        public const int DEFAULT_PORT = 5672;
        public const String DEFAULT_TRANSPORT = "tcp";

        public readonly string URL_FORMAT_EXAMPLE =
                "<transport>://<hostname>[:<port Default=\"" + DEFAULT_PORT + "\">][?<option>='<value>'[,<option>='<value>']]";

        public const long DEFAULT_CONNECT_TIMEOUT = 30000L;
    }
    
    public interface IBrokerInfo
    {
        string Host { get; set; }
        int Port { get; set; }
        string Transport { get; set; }
        bool UseSSL { get; set; }
        long Timeout { get; set; }
        SslOptions SslOptions { get; }
        
        String GetOption(string key);
        void SetOption(string key, string value);
    }
}
