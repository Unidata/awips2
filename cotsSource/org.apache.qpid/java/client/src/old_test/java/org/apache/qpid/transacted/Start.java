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
package org.apache.qpid.transacted;

import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.AMQException;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.exchange.ExchangeDefaults;
import org.apache.qpid.client.AMQQueue;

import javax.jms.Connection;
import javax.jms.JMSException;
import javax.jms.Session;

public class Start
{
    public static void main(String[] argv) throws Exception
    {
        Connection con = new Config(argv).createConnection();
        AMQQueue ping = new AMQQueue(ExchangeDefaults.DIRECT_EXCHANGE_NAME, new AMQShortString("ping"));
        Session session = con.createSession(false, Session.AUTO_ACKNOWLEDGE);
        session.createProducer(ping).send(session.createTextMessage("start"));
        session.close();
        con.close();
    }
}
