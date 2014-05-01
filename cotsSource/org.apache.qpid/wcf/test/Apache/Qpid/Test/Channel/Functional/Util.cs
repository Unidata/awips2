/*
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
*/

namespace Apache.Qpid.Test.Channel.Functional
{
    using System.Collections.Generic;
    using System.IO;
    using System.ServiceModel;
    using System.ServiceModel.Channels;
    using Apache.Qpid.Channel;

    internal class Util
    {
        public static Dictionary<string, string> GetProperties(string path)
        {
            string fileData = string.Empty;
            using (StreamReader sr = new StreamReader(path))
            {
                fileData = sr.ReadToEnd().Replace("\r", string.Empty);
            }

            Dictionary<string, string> properties = new Dictionary<string, string>();
            string[] kvp;
            string[] records = fileData.Split("\n".ToCharArray());
            foreach (string record in records)
            {
                if (record[0] == '/' || record[0] == '*')
                {
                    continue;
                }

                kvp = record.Split("=".ToCharArray());
                properties.Add(kvp[0], kvp[1]);
            }

            return properties;
        }

        public static Binding GetBinding()
        {
            return new AmqpBinding();
        }

        public static Binding GetCustomBinding()
        {
            AmqpTransportBindingElement transportElement = new AmqpTransportBindingElement();
            RawMessageEncodingBindingElement encodingElement = new RawMessageEncodingBindingElement();
            transportElement.BrokerHost = "127.0.0.1";
            transportElement.TransferMode = TransferMode.Streamed;

            CustomBinding brokerBinding = new CustomBinding();
            brokerBinding.Elements.Add(encodingElement);
            brokerBinding.Elements.Add(transportElement);

            return brokerBinding;
        }
    }
}
