/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 *
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 *
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.edex.registry.ebxml.web.security;

import java.io.IOException;
import java.nio.channels.SelectionKey;
import java.nio.channels.SocketChannel;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import org.eclipse.jetty.io.NetworkTrafficListener;
import org.eclipse.jetty.io.SelectChannelEndPoint;
import org.eclipse.jetty.io.SelectorManager;
import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.server.ServerConnector;
import org.eclipse.jetty.util.ssl.SslContextFactory;

/**
 * Custom version of
 * org.eclipse.jetty.server.nio.NetworkTrafficSelectChannelConnector to create
 * an updated version of
 * org.eclipse.jetty.io.NetworkTrafficSelectChannelEndPoint which does not
 * suffer from the bug described here:
 * https://bugs.eclipse.org/bugs/show_bug.cgi?id=431519
 *
 * This class should not be needed when Jetty is upgraded beyond 9.1.5.
 *
 * <pre>
 *
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 1, 2015  5152      nabowle     Initial creation
 *
 * </pre>
 *
 * @author nabowle
 * @version 1.0
 */

public class NetworkTrafficSelectChannelConnector extends ServerConnector {

    private final List<NetworkTrafficListener> listeners = new CopyOnWriteArrayList<NetworkTrafficListener>();

    public NetworkTrafficSelectChannelConnector(Server server,
            SslContextFactory sslContextFactory) {
        super(server, sslContextFactory);
    }

    /**
     * @param listener
     *            the listener to add
     */
    public void addNetworkTrafficListener(NetworkTrafficListener listener) {
        listeners.add(listener);
    }

    /**
     * @param listener
     *            the listener to remove
     */
    public void removeNetworkTrafficListener(NetworkTrafficListener listener) {
        listeners.remove(listener);
    }

    @Override
    protected SelectChannelEndPoint newEndPoint(SocketChannel channel,
            SelectorManager.ManagedSelector selectSet, SelectionKey key)
            throws IOException {
        NetworkTrafficSelectChannelEndPoint endPoint = new NetworkTrafficSelectChannelEndPoint(
                channel, selectSet, key, getScheduler(), getIdleTimeout(),
                listeners);
        endPoint.notifyOpened();
        return endPoint;
    }
}
