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
using System.Collections.Generic;
using System.ServiceModel;


namespace org.apache.qpid.wcf.demo.bookingServer
{
    [ServiceBehavior(InstanceContextMode = InstanceContextMode.PerSession)]
    public class Booking : IBooking
    {
        private Guid _id;
        private List<Order> _orders;

        public Booking()
        {
            _id = Guid.NewGuid();
            _orders = new List<Order>();
        }

        public void Add(Order order)
        {
            _orders.Add(order);
        }

        public Receipt Checkout()
        {
            var r = new Receipt();
            foreach (Order order in _orders)
            {
                r.Price += order.Price;
                r.Summary = r.Summary + " \n " + order.Type + " Price: " + order.Price;
            }
            return r;
        }

        public Guid Id
        {
            get { return _id; }
        }
    }
}
