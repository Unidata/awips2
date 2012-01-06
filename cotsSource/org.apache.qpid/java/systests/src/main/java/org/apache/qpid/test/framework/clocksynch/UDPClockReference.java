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
package org.apache.qpid.test.framework.clocksynch;

import org.apache.qpid.junit.extensions.ShutdownHookable;

import java.io.IOException;
import java.net.*;
import java.nio.ByteBuffer;

/**
 * UDPClockReference supplies a refernce clock signal (generated from System.nanoTime()).
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Supply a reference clock signal.
 * </table>
 *
 * @todo Port hard coded. Make configurable.
 *
 * @todo Errors rethrown as runtimes, or silently terminate the service. Could add better error handling if needed.
 */
public class UDPClockReference implements Runnable, ShutdownHookable
{
    /** Used for debugging. */
    // private static final Logger log = Logger.getLogger(UDPClockReference.class);

    /** Defines the timeout to use when polling the socket for time requests. */
    private static final int TIMEOUT = 200;

    /** Defines the port to run the clock reference on. */
    public static final int REFERENCE_PORT = 4444;

    /** Holds the socket to receive clock reference requests on. */
    protected DatagramSocket socket = null;

    /** Flag used to indicate that the time server should keep running. Set to false to terminate. */
    protected boolean publish = true;

    /**
     * Creates a clock reference service on the standard port.
     */
    public UDPClockReference()
    {
        try
        {
            socket = new DatagramSocket(REFERENCE_PORT);
            socket.setSoTimeout(TIMEOUT);
        }
        catch (SocketException e)
        {
            throw new RuntimeException(e);
        }
    }

    /**
     * Implements the run loop for this reference time server. This waits for incoming time requests, and replies to
     * any, with a message with the local time stamp in it. Periodically (controlled by {@link #TIMEOUT}), the run
     * loop will check if the {@link #publish} flag has been cleared, and terminate the reference time service if so.
     */
    public void run()
    {
        byte[] buf = new byte[256];
        ByteBuffer bbuf = ByteBuffer.wrap(buf);

        while (publish)
        {
            try
            {
                // Wait for a reference time request.
                DatagramPacket packet = new DatagramPacket(buf, buf.length);
                boolean timedOut = false;

                try
                {
                    socket.receive(packet);
                }
                catch (SocketTimeoutException e)
                {
                    timedOut = true;
                }

                if (!timedOut)
                {
                    // Work out from the received packet, where to reply to.
                    InetAddress address = packet.getAddress();
                    int port = packet.getPort();

                    // Respond to the time request by sending back the local clock as the reference time.
                    bbuf.putLong(System.nanoTime());
                    bbuf.flip();
                    packet = new DatagramPacket(bbuf.array(), bbuf.capacity(), address, port);

                    socket.send(packet);
                }
            }
            catch (IOException e)
            {
                publish = false;
            }
        }

        socket.close();
    }

    /**
     * Supplies a shutdown hook.
     *
     * @return The shut down hook.
     */
    public Thread getShutdownHook()
    {
        return new Thread(new Runnable()
                {
                    public void run()
                    {
                        publish = false;
                    }
                });
    }

    /**
     * For testing purposes. Runs a reference clock on the default port.
     *
     * @param args None.
     */
    public static void main(String[] args)
    {
        try
        {
            // Create the clock reference service.
            UDPClockReference clock = new UDPClockReference();

            // Set up a shutdown hook for it.
            Runtime.getRuntime().addShutdownHook(clock.getShutdownHook());

            // Start the service.
            clock.run();
        }
        catch (Exception e)
        {
            e.printStackTrace();
            System.exit(1);
        }
    }
}

