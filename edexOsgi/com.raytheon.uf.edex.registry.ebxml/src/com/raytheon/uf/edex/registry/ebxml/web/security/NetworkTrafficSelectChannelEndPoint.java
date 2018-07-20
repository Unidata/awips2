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
import java.nio.ByteBuffer;
import java.nio.channels.SelectionKey;
import java.nio.channels.SocketChannel;
import java.util.List;

import org.eclipse.jetty.io.NetworkTrafficListener;
import org.eclipse.jetty.io.SelectChannelEndPoint;
import org.eclipse.jetty.io.SelectorManager;
import org.eclipse.jetty.util.thread.Scheduler;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Updated version of org.eclipse.jetty.io.NetworkTrafficSelectChannelEndPoint
 * that does not suffer from the bug described here:
 * https://bugs.eclipse.org/bugs/show_bug.cgi?id=431519
 *
 * This is done by the ByteBuffer view in flush() being created from
 * {@link ByteBuffer#slice()} before the flush.
 * 
 * This class should not be needed when Jetty is upgraded beyond 9.1.5.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 1, 2015            nabowle     Initial creation
 *
 * </pre>
 *
 * @author nabowle
 * @version 1.0
 */

public class NetworkTrafficSelectChannelEndPoint extends SelectChannelEndPoint {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(NetworkTrafficSelectChannelEndPoint.class);

    private final List<NetworkTrafficListener> listeners;

    public NetworkTrafficSelectChannelEndPoint(SocketChannel channel,
            SelectorManager.ManagedSelector selectSet, SelectionKey key,
            Scheduler scheduler, long idleTimeout,
            List<NetworkTrafficListener> listeners) throws IOException {
        super(channel, selectSet, key, scheduler, idleTimeout);
        this.listeners = listeners;
    }

    @Override
    public int fill(ByteBuffer buffer) throws IOException {
        int read = super.fill(buffer);
        notifyIncoming(buffer, read);
        return read;
    }

    @Override
    public boolean flush(ByteBuffer... buffers) throws IOException {
        boolean flushed = true;
        for (ByteBuffer b : buffers) {
            if (b.hasRemaining()) {
                int position = b.position();
                ByteBuffer view = b.slice();
                flushed &= super.flush(b);
                int l = b.position() - position;
                view.limit(view.position() + l);
                notifyOutgoing(view);
                if (!flushed)
                    break;
            }
        }
        return flushed;
    }

    public void notifyOpened() {
        if (listeners != null && !listeners.isEmpty()) {
            for (NetworkTrafficListener listener : listeners) {
                try {
                    listener.opened(getSocket());
                } catch (Exception e) {
                    statusHandler.handle(Priority.WARN,
                            "Error notifying listener.", e);
                }
            }
        }
    }

    public void notifyIncoming(ByteBuffer buffer, int read) {
        if (listeners != null && !listeners.isEmpty() && read > 0) {
            for (NetworkTrafficListener listener : listeners) {
                try {
                    ByteBuffer view = buffer.asReadOnlyBuffer();
                    listener.incoming(getSocket(), view);
                } catch (Exception e) {
                    statusHandler.handle(Priority.WARN,
                            "Error notifying listener.", e);
                }
            }
        }
    }

    public void notifyOutgoing(ByteBuffer view) {
        if (listeners != null && !listeners.isEmpty() && view.hasRemaining()) {
            for (NetworkTrafficListener listener : listeners) {
                try {
                    listener.outgoing(getSocket(), view);
                } catch (Exception e) {
                    statusHandler.handle(Priority.WARN,
                            "Error notifying listener.", e);
                }
            }
        }
    }

    public void notifyClosed() {
        if (listeners != null && !listeners.isEmpty()) {
            for (NetworkTrafficListener listener : listeners) {
                try {
                    listener.closed(getSocket());
                } catch (Exception e) {
                    statusHandler.handle(Priority.WARN,
                            "Error notifying listener.", e);
                }
            }
        }
    }
}
