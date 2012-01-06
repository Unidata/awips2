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

import org.apache.log4j.Logger;

import org.apache.qpid.requestreply.PingPongProducer;

import javax.jms.Destination;

import java.util.List;
import java.util.Properties;

/**
 * PingClient is a {@link PingPongProducer} that does not need a {@link org.apache.qpid.requestreply.PingPongBouncer}
 * to send replies to its pings. It simply listens to its own ping destinations, rather than seperate reply queues.
 * It is an all in one ping client, that produces and consumes its own pings.
 *
 * <p/>The constructor increments a count of the number of ping clients created. It is assumed that where many
 * are created they will all be run in parallel and be active in sending and consuming pings at the same time.
 * If the unique destinations flag is not set and a pub/sub ping cycle is being run, this means that they will all hear
 * pings sent by each other. The expected number of pings received will therefore be multiplied up by the number of
 * active ping clients. The {@link #getConsumersPerDestination()} method is used to supply this multiplier under these
 * conditions.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Create a ping producer that listens to its own pings <td> {@link PingPongProducer}
 * <tr><td> Count the number of ping producers and produce multiplier for scaling up messages expected over topic pings.
 * </table>
 */
public class PingClient extends PingPongProducer
{
    /** Used for debugging. */
    private final Logger log = Logger.getLogger(PingClient.class);

    /** Used to count the number of ping clients created. */
    private static int _pingClientCount;

    /**
     * Creates a ping producer with the specified parameters, of which there are many. See the class level comments
     * for {@link PingPongProducer} for details. This constructor creates a connection to the broker and creates
     * producer and consumer sessions on it, to send and recieve its pings and replies on.
     *
     * @param  overrides Properties containing any desired overrides to the defaults.
     *
     * @throws Exception Any exceptions are allowed to fall through.
     */
    public PingClient(Properties overrides) throws Exception
    {
        super(overrides);

        _pingClientCount++;
    }

    /**
     * Returns the ping destinations themselves as the reply destinations for this pinger to listen to. This has the
     * effect of making this pinger listen to its own pings.
     *
     * @return The ping destinations.
     */
    public List<Destination> getReplyDestinations()
    {
        return _pingDestinations;
    }

    /**
     * Supplies the multiplier for the number of ping clients that will hear each ping when doing pub/sub pinging.
     *
     * @return The scaling up of the number of expected pub/sub pings.
     */
    public int getConsumersPerDestination()
    {
        log.debug("public int getConsumersPerDestination(): called");

        if (_isUnique)
        {
            log.debug(_noOfConsumers + " consumer per destination.");

            return _noOfConsumers;
        }
        else
        {
            log.debug((_pingClientCount * _noOfConsumers) + " consumers per destination.");

            return _pingClientCount * _noOfConsumers;
        }
    }
}
