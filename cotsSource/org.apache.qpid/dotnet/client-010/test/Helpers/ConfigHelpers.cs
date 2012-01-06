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

﻿using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Xml;
using log4net.Config;

namespace test.Helpers
{
    class ConfigHelpers
    {
        public static Dictionary<string, string> LoadConfig()
        {
            Dictionary<string, string> properties = new Dictionary<string, string>();

            XmlConfigurator.Configure(new FileInfo("/log.xml"));
            // populate default properties
            properties.Add("Username", "guest");
            properties.Add("Password", "guest");
            properties.Add("Host", "localhost");
            properties.Add("Port", "5672");
            properties.Add("VirtualHost", "test");
            //Read the test config file  
            XmlTextReader reader = new XmlTextReader(Environment.CurrentDirectory + "/Qpid Test.dll.config");
            while (reader.Read())
            {
                // if node type is an element
                if (reader.NodeType == XmlNodeType.Element && reader.Name.Equals("add"))
                {
                    if (properties.ContainsKey(reader.GetAttribute("key")))
                    {
                        properties[reader.GetAttribute("key")] = reader.GetAttribute("value");
                    }
                    else
                    {
                        properties.Add(reader.GetAttribute("key"), reader.GetAttribute("value"));
                    }
                }
            }

            return properties;
        }
    }
}
