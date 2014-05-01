/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.    
 *
 * 
 */
package org.apache.qpid.example.pubsub;

import javax.jms.JMSException;
import javax.jms.MessageProducer;
import javax.jms.Session;

/**
 * A simple Publisher example.
 *
 * The class can take two arguments.
 * java Publisher <destination> <msgCount>
 * Where:
 * destination is either 'topic' or 'queue'  (Default: topic)
 * msgCount is the number of messages to send (Default : 100)
 *
 */
public class Publisher extends Client
{
    int _msgCount;

    public Publisher(String destination, int msgCount)
    {
        super(destination);
        _msgCount = msgCount;
    }

    public void start()
    {
        try
        {
            _session = _connection.createSession(false, Session.AUTO_ACKNOWLEDGE);

            MessageProducer _producer = _session.createProducer(_destination);

            for (int msgCount = 0; msgCount < _msgCount; msgCount++)
            {
                _producer.send(_session.createTextMessage("msg:" + msgCount));
                System.out.println("Sent:" + msgCount);
            }

            System.out.println("Done.");
            _connection.close();
        }
        catch (JMSException e)
        {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }
    }


    public static void main(String[] args)
    {

        String destination = args.length > 2 ? args[1] : null;

        int msgCount = args.length > 2 ? Integer.parseInt(args[2]) : 100;

        new Publisher(destination, msgCount).start();
    }

}
