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
package org.apache.qpid.example.publisher;

import java.io.File;

import javax.jms.JMSException;
import javax.jms.TextMessage;


import org.apache.qpid.example.shared.FileUtils;
import org.apache.qpid.example.shared.Statics;
import org.slf4j.LoggerFactory;
import org.slf4j.Logger;

/**
 * Class that sends parameterised number of message files to the Publisher
 * Must set properties for host in properties file or uses in vm broker
 */
public class MultiMessageDispatcher
{

    protected static final Logger _logger = LoggerFactory.getLogger(FileMessageDispatcher.class);

    protected static Publisher _publisher = null;

    /**
     * To use this main method you need to specify a path or file to use for input
     * This class then uses file contents from the dir/file specified to generate
     * messages to publish
     * Intended to be a very simple way to get going with publishing using the broker
     * @param args - must specify one value, the path to file(s) for publisher
     */
    public static void main(String[] args)
    {

        // Check command line args ok - must provide a path or file for us to dispatch
        if (args.length < 2)
        {
            System.out.println("Usage: MultiMessageDispatcher <numberOfMessagesToSend> <topic(true|false)>" + "");
        }
        else
        {
            boolean topicPublisher = true;
            
            try
            {
                // publish message(s)
                topicPublisher = new Boolean(args[1]).booleanValue();
                publish(new Integer(args[0]).intValue(),topicPublisher);

                // Move payload file(s) to archive location as no error
                FileUtils.moveFileToNewDir(args[0], System.getProperties().getProperty(Statics.ARCHIVE_PATH));
            }
            catch (Exception e)
            {
                // log error and exit
                _logger.error("Error trying to dispatch message: " + e);
                System.exit(1);
            }
            finally
            {

                cleanup(topicPublisher);
            }
        }

        if (_logger.isDebugEnabled())
        {
            _logger.debug("Finished dispatching message");
        }

        System.exit(0);
    }

    /**
     * Publish the content of a file or files from a directory as messages
     * @param numMessages - from main args
     * @throws javax.jms.JMSException
     * @throws org.apache.qpid.example.publisher.MessageFactoryException - if cannot create message from file content
     */
    public static void publish(int numMessages, boolean topicPublisher) throws JMSException, MessageFactoryException
    {
        {
            // Send the message generated from the payload using the _publisher
            getPublisher(topicPublisher).sendMessage(numMessages);
        }
    }

    /**
     * Cleanup before exit
     */
    public static void cleanup(boolean topicPublisher)
    {
        if (getPublisher(topicPublisher) != null)
        {
            getPublisher(topicPublisher).cleanup();
        }
    }

    /**
     * @return A Publisher instance
     */
    private static Publisher getPublisher(boolean topic)
    {
        if (_publisher != null)
        {
            return _publisher;
        }

        if (!topic)
        {
            // Create a _publisher
            _publisher = new Publisher();
        }
        else
        {
            _publisher = new TopicPublisher();
        }
        return _publisher;
    }

}