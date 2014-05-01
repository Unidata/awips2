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


namespace org.apache.qpid.wcf.demo.bookingServer
{
    internal class Program
    {
        private ServiceHost _service;
        private ChannelFactory<IBooking> fac;

        public void StartService(Binding binding)
        {
            try
            {
                Console.WriteLine("  Binding Service...");
                _service = new ServiceHost(typeof(Booking), new Uri("soap.amqp:///"));
                _service.AddServiceEndpoint(typeof(IBooking), binding, "Booking");
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

        public IBooking StartClient(Binding binding)
        {
            IBooking res = null;
            try
            {
                Console.WriteLine("  Starting Client...");
                fac = new ChannelFactory<IBooking>(binding, "soap.amqp:///Booking");
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

        public void StopClient(IBooking client)
        {
            Console.WriteLine("  Stopping Client...");
            ((IChannel)client).Close();
            fac.Close();
            Console.WriteLine("[DONE]");
        }

        private static void Main(string[] args)
        {
            var p = new Program();

            Binding binding = new QpidBinding("192.168.1.14", 5673);
            p.StartService(binding);

            Console.ReadLine();

            p.StopService();
        }
    }
}
