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
using System.ServiceModel;
using System.Windows.Forms;
using org.apache.qpid.wcf.demo;
using org.apache.qpid.wcf.model;
using org.apache.qpid.wcf.demo.bookingServer;

namespace WindowsFormsBooking
{
    public partial class Form1 : Form
    {
        private ChannelFactory<IBooking> _fac;
        private readonly Order _order = new Order();
        private IBooking _calc;
         
        public Form1()
        {
            InitializeComponent();
            _calc = StartClient(new QpidBinding("192.168.1.14", 5673));
            _order.Type = "Default";
            _order.Price = 0;
        }

        public IBooking StartClient(System.ServiceModel.Channels.Binding binding)
        {
            IBooking res = null;
            try
            {
                Console.WriteLine("  Starting Client...");
                _fac = new ChannelFactory<IBooking>(binding, "soap.amqp:///Booking");
                _fac.Open();
                res = _fac.CreateChannel();
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
            ((System.ServiceModel.Channels.IChannel)client).Close();
            _fac.Close();
            Console.WriteLine("[DONE]");
        }

        private void comboBox1_SelectedIndexChanged(object sender, EventArgs e)
        {
            _order.Type = ((ComboBox) sender).SelectedItem.ToString();
        }

        private void numericUpDown1_ValueChanged(object sender, EventArgs e)
        {
            _order.Price = (double) ((NumericUpDown) sender).Value;
        }

        private void button1_Click(object sender, EventArgs e)
        {
            _calc.Add(_order);
        }

        private void button2_Click(object sender, EventArgs e)
        {
            Receipt r = _calc.Checkout();
            richTextBox1.Text = r.Summary + "\n" + "Total Price = " + r.Price;            
            // reset
            _calc = StartClient(new QpidBinding("192.168.1.14", 5673));            
        }

      
    }
}
