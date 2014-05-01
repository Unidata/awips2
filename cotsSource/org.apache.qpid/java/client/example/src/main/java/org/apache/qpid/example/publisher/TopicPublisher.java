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
package org.apache.qpid.example.publisher;

import org.apache.qpid.client.BasicMessageProducer;
import org.apache.qpid.example.shared.InitialContextHelper;
import org.slf4j.LoggerFactory;
import org.slf4j.Logger;

import javax.jms.*;
import javax.naming.InitialContext;

/**
 * Subclass of Publisher which sends messages to a topic destination defined in example.properties
 */
public class TopicPublisher extends Publisher
{

    private static final Logger _log = LoggerFactory.getLogger(Publisher.class);

    public TopicPublisher()
    {
        super();

        try
        {
            _contextHelper = new InitialContextHelper(null);
            InitialContext ctx = _contextHelper.getInitialContext();

           //lookup the example topic and use it
           _destination = (Topic) ctx.lookup("MyTopic");

           //create a message producer
           _producer = _session.createProducer(_destination);
        }
        catch (Exception e)
        {
            //argh
            _log.error("Exception trying to construct TopicPublisher" + e);
        }

    }
}