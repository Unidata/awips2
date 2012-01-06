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

import org.apache.qpid.transport.util.Logger;

import static org.apache.qpid.transport.Connection.State.*;


/**
 * ConnectionDelegate
 *
 * @author Rafael H. Schloming
 */

/**
 * Currently only implemented client specific methods
 * the server specific methods are dummy impls for testing
 *
 * the connectionClose is kind of different for both sides
 */
public abstract class ConnectionDelegate
    extends MethodDelegate<Connection>
    implements ProtocolDelegate<Connection>
{

    private static final Logger log = Logger.get(ConnectionDelegate.class);

    public void control(Connection conn, Method method)
    {
        method.dispatch(conn, this);
    }

    public void command(Connection conn, Method method)
    {
        method.dispatch(conn, this);
    }

    public void error(Connection conn, ProtocolError error)
    {
        conn.exception(new ConnectionException(error.getMessage()));
    }

    public void handle(Connection conn, Method method)
    {
        conn.dispatch(method);
    }

    @Override public void connectionHeartbeat(Connection conn, ConnectionHeartbeat hearbeat)
    {
        // do nothing
    }

    @Override public void connectionClose(Connection conn, ConnectionClose close)
    {
        conn.connectionCloseOk();
        conn.getSender().close();
        conn.closeCode(close);
        conn.setState(CLOSE_RCVD);
    }

    @Override public void connectionCloseOk(Connection conn, ConnectionCloseOk ok)
    {
        conn.getSender().close();
    }

    @Override public void sessionDetach(Connection conn, SessionDetach dtc)
    {
        Session ssn = conn.getSession(dtc.getChannel());
        conn.unmap(ssn);
        ssn.sessionDetached(dtc.getName(), SessionDetachCode.NORMAL);
        ssn.closed();
    }

    @Override public void sessionDetached(Connection conn, SessionDetached dtc)
    {
        Session ssn = conn.getSession(dtc.getChannel());
        if (ssn != null)
        {
            conn.unmap(ssn);
            ssn.closed();
        }
    }

}
