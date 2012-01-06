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

package org.apache.qpid.ping;

import java.util.Properties;

import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.ObjectMessage;

import org.apache.log4j.Logger;

import org.apache.qpid.client.message.TestMessageFactory;
import org.apache.qpid.util.CommandLineParser;

/**
 * <p><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * </table>
 */
public class PingSendOnlyClient extends PingDurableClient
{
    private static final Logger log = Logger.getLogger(PingSendOnlyClient.class);

    public PingSendOnlyClient(Properties overrides) throws Exception
    {
        super(overrides);
    }

    /**
     * Starts the ping/wait/receive process.
     *
     * @param args The command line arguments.
     */
    public static void main(String[] args)
    {
        try
        {
            // Create a ping producer overriding its defaults with all options passed on the command line.
            Properties options = CommandLineParser.processCommandLine(args, new CommandLineParser(new String[][] {}), System.getProperties());
            PingSendOnlyClient pingProducer = new PingSendOnlyClient(options);

            // Create a shutdown hook to terminate the ping-pong producer.
            Runtime.getRuntime().addShutdownHook(pingProducer.getShutdownHook());

            // Ensure that the ping pong producer is registered to listen for exceptions on the connection too.
            // pingProducer.getConnection().setExceptionListener(pingProducer);

            // Run the test procedure.
            int sent = pingProducer.send();
            pingProducer.waitForUser("Press return to close connection and quit.");
            pingProducer.closeConnection();

            System.exit(0);
        }
        catch (Exception e)
        {
            System.err.println(e.getMessage());
            log.error("Top level handler caught execption.", e);
            System.exit(1);
        }
    }

    public Message getTestMessage(Destination replyQueue, int messageSize, boolean persistent) throws JMSException
    {
        Message msg = TestMessageFactory.newTextMessage(_producerSession, messageSize);

        // Timestamp the message in nanoseconds.
        msg.setLongProperty(MESSAGE_TIMESTAMP_PROPNAME, System.nanoTime());

        return msg;
    }
}
