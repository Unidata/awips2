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
 * Echo
 *
 */

public class Echo implements SessionListener
{

    public void opened(Session ssn) {}

    public void resumed(Session ssn) {}

    public void message(Session ssn, MessageTransfer xfr)
    {
        int id = xfr.getId();
        ssn.invoke(xfr);
        ssn.processed(id);
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
                ssn.setSessionListener(new Echo());
                return ssn;
            }
        };

        IoAcceptor ioa = new IoAcceptor
            ("0.0.0.0", 5672, ConnectionBinding.get(delegate));
        ioa.start();
    }

}
