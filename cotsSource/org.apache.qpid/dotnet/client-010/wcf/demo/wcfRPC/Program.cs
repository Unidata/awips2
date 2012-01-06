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
using System.ServiceModel;
using System.ServiceModel.Channels;
using System.Threading;
using org.apache.qpid.wcf.model;


namespace org.apache.qpid.wcf.demo.rpc
{
    internal class Program
    {
        private ServiceHost _service;
        private ChannelFactory<IUpperCase> fac;

        public void StartService(Binding binding)
        {
            try
            {
                Console.WriteLine("  Binding Service...");
                _service = new ServiceHost(typeof (UpperCase), new Uri("soap.amqp:///"));
                _service.AddServiceEndpoint(typeof(IUpperCase), binding, "UpperCase");
                _service.Open();
                Thread.Sleep(500);
                Console.WriteLine("[DONE]");
            }
            catch (Exception e)
            {
                Console.WriteLine(e);
            }
        }

        public void StopService()
        {
            Console.WriteLine("  Stopping Service...");
            _service.Close();
            Console.WriteLine("[DONE]");
        }

        public IUpperCase StartClient(Binding binding)
        {
            IUpperCase res = null;
            try
            {
                Console.WriteLine("  Starting Client...");
                fac = new ChannelFactory<IUpperCase>(binding, "soap.amqp:///UpperCase");
                fac.Open();
                res = fac.CreateChannel();
                Console.WriteLine("[DONE]");              
            }
            catch (Exception e)
            {
                Console.WriteLine(e);
            }
            return res;
        }

        public void StopClient(IUpperCase client)
        {
            Console.WriteLine("  Stopping Client...");
            ((IChannel) client).Close();
            fac.Close();
            Console.WriteLine("[DONE]");
        }

        private static void Main(string[] args)
        {
            var p = new Program();

            Binding binding = new QpidBinding("192.168.1.14", 5673);
            p.StartService(binding);


            IUpperCase calc = p.StartClient(new QpidBinding("192.168.1.14", 5673));

            string[] messages = {"Twas brillig, and the slithy toves",
                                 "Did gire and gymble in the wabe.  ",
                                 "All mimsy were the borogroves,    ",
                                 "And the mome raths outgrabe.      "};
            foreach (string m in messages)
            {
                Console.Write(m + "  --UperCase-->  " );
                Console.Write(calc.ToUpperCase(m));
                Console.WriteLine();
            }
            
            Console.ReadLine();
   
           p.StopClient(calc);
           p.StopService();
        }
    }
}
