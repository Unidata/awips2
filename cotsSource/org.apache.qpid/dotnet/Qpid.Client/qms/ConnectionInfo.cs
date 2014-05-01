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

namespace Apache.Qpid.Client.Qms
{
    class ConnectionUrlConstants 
    {
        public const string AMQ_PROTOCOL = "amqp";
        public const string OPTIONS_BROKERLIST = "brokerlist";
        public const string OPTIONS_FAILOVER = "failover";
        public const string OPTIONS_FAILOVER_CYCLE = "cyclecount";
        public const string OPTIONS_SSL = "ssl";
    }

    /// <summary>
    /// Connection URL format
    /// amqp://[user:pass@][clientid]/virtualhost?brokerlist='tcp://host:port?option=\'value\'&amp;option=\'value\';vm://:3/virtualpath?option=\'value\''&amp;failover='method?option=\'value\'&amp;option='value''"
    /// Options are of course optional except for requiring a single broker in the broker list.
    /// The option seperator is defined to be either '&amp;' or ','
    /// </summary>
    public interface IConnectionInfo
    {
        string AsUrl();

        string FailoverMethod { get; set; }
        string ClientName { get; set; }
        string Username { get; set; }
        string Password { get; set; }
        string VirtualHost { get; set; }
        string GetFailoverOption(string key);
        
        int BrokerCount { get; }

        IBrokerInfo GetBrokerInfo(int index);

        void AddBrokerInfo(IBrokerInfo broker);

        IList GetAllBrokerInfos();

        string GetOption(string key);

        void SetOption(string key, string value);
    }
}
