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
package org.apache.qpid.transport;

import java.io.IOException;
import java.nio.ByteBuffer;

import org.apache.qpid.transport.network.ConnectionBinding;
import org.apache.qpid.transport.network.io.IoAcceptor;

/**
 * Sink
 *
 */

public class Sink implements SessionListener
{

    private static final String FORMAT_HDR = "%-12s %-18s %-18s %-18s";
    private static final String FORMAT_ROW = "SSN#%-8X %-18s %-18s %-18s";

    private long interval = 100000;
    private long start = System.currentTimeMillis();
    private long count = 0;
    private long bytes = 0;
    private long interval_start = start;
    private long bytes_start = bytes;
    private long time = start;
    private int id = System.identityHashCode(this);

    public Sink()
    {
    }

    private double msg_rate()
    {
        return 1000 * (double) count / (double) (time - start);
    }

    private double byte_rate()
    {
        return (1000 * (double) bytes / (double) (time - start)) / (1024*1024);
    }

    private double msg_interval_rate()
    {
        return 1000 * (double) interval / (double) (time - interval_start);
    }

    private double byte_interval_rate()
    {
        return (1000 * (double) (bytes - bytes_start) / (double) (time - interval_start)) / (1024*1024);
    }

    private String rates()
    {
        return String.format("%.2f/%.2f", msg_rate(), byte_rate());
    }

    private String interval_rates()
    {
        return String.format("%.2f/%.2f", msg_interval_rate(), byte_interval_rate());
    }

    private String counts()
    {
        return String.format("%d/%.2f", count, ((double) bytes)/(1024*1024));
    }

    public void opened(Session ssn) {}

    public void resumed(Session ssn) {}

    public void message(Session ssn, MessageTransfer xfr)
    {
        count++;
        bytes += xfr.getBody().remaining();
        if ((count % interval) == 0)
        {
            time = System.currentTimeMillis();
            System.out.println
                (String.format
                 (FORMAT_ROW, id, counts(), rates(), interval_rates()));
            interval_start = time;
            bytes_start = bytes;
        }
        ssn.processed(xfr);
    }

    public void exception(Session ssn, SessionException exc)
    {
        exc.printStackTrace();
    }

    public void closed(Session ssn) {}

    public static final void main(String[] args) throws IOException
    {
        ConnectionDelegate delegate = new ServerDelegate()
        {
            @Override public Session getSession(Connection conn, SessionAttach atc)
            {
                Session ssn = super.getSession(conn, atc);
                ssn.setSessionListener(new Sink());
                return ssn;
            }
        };

        IoAcceptor ioa = new IoAcceptor
            ("0.0.0.0", 5672, ConnectionBinding.get(delegate));
        System.out.println
            (String.format
             (FORMAT_HDR, "Session", "Count/MBytes", "Cumulative Rate", "Interval Rate"));
        System.out.println
            (String.format
             (FORMAT_HDR, "-------", "------------", "---------------", "-------------"));
        ioa.start();
    }

}
