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
import org.eclipse.jetty.io.nio.NetworkTrafficSelectChannelEndPoint;
import org.eclipse.jetty.io.nio.SelectChannelEndPoint;
import org.eclipse.jetty.io.nio.SelectorManager;
import org.eclipse.jetty.server.ssl.SslSelectChannelConnector;
import org.eclipse.jetty.util.ssl.SslContextFactory;

/**
 * 
 * Custom SSL connector for logging traffic.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 3/27/2014    1712       bphillip    Initial Creation
 * </pre>
 * 
 * @author bphillip
 * @version 1
 **/
public class SslNetworkTrafficSelectChannelConnector extends
        SslSelectChannelConnector {
    private final List<NetworkTrafficListener> listeners = new CopyOnWriteArrayList<NetworkTrafficListener>();

    public SslNetworkTrafficSelectChannelConnector() {
        super();
    }

    public SslNetworkTrafficSelectChannelConnector(
            SslContextFactory sslContextFactory) {
        super(sslContextFactory);
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
            SelectorManager.SelectSet selectSet, SelectionKey key)
            throws IOException {
        NetworkTrafficSelectChannelEndPoint endPoint = new NetworkTrafficSelectChannelEndPoint(
                channel, selectSet, key, _maxIdleTime, listeners);
        endPoint.setConnection(selectSet.getManager().newConnection(channel,
                endPoint, key.attachment()));
        endPoint.notifyOpened();
        return endPoint;
    }

    @Override
    protected void endPointClosed(SelectChannelEndPoint endpoint) {
        super.endPointClosed(endpoint);
        ((NetworkTrafficSelectChannelEndPoint) endpoint).notifyClosed();
    }
}
