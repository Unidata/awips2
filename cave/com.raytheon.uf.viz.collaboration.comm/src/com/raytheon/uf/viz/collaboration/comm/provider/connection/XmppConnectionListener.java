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
package com.raytheon.uf.viz.collaboration.comm.provider.connection;

import org.jivesoftware.smack.ConnectionListener;
import org.jivesoftware.smack.XMPPException;
import org.jivesoftware.smack.packet.StreamError;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.viz.collaboration.comm.provider.event.ServerDisconnectEvent;

/**
 * Handles connection events from the XMPP server
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 11, 2014 2903       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class XmppConnectionListener implements ConnectionListener {

    private static final IUFStatusHandler statusHandler = CollaborationConnection
            .getStatusHandler();

    private final CollaborationConnection manager;

    /**
     * @param manager
     */
    public XmppConnectionListener(CollaborationConnection manager) {
        this.manager = manager;
    }

    @Override
    public void reconnectionSuccessful() {
        /* this will not be called since we don't allow auto reconnect */
        statusHandler.debug("Client successfully reconnected to server");
        manager.postSystemMessageToVenues("Connection to collaboration server reestablished.");
    }

    @Override
    public void reconnectionFailed(Exception e) {
        /* this will not be called since we don't allow auto reconnect */
        String reason = getErrorReason(e);
        statusHandler.error("Client can't reconnect to server: " + reason, e);
        sendDisconnectNotice(reason);
    }

    @Override
    public void reconnectingIn(int seconds) {
        /* this will not be called since we don't allow auto reconnect */
        statusHandler.debug("Client reconnecting to server in " + seconds
                + " seconds");
    }

    @Override
    public void connectionClosedOnError(Exception e) {
        String reason = getErrorReason(e);
        statusHandler.error("Server closed on error: " + reason, e);
        sendDisconnectNotice(reason);
    }

    /**
     * Attempt to get the most meaningful message from XMPP exception
     * 
     * @param e
     * @return
     */
    private String getErrorReason(Exception e) {
        String msg = null;
        if (e instanceof XMPPException) {
            StreamError streamError = ((XMPPException) e).getStreamError();
            if (streamError != null) {
                if ("conflict".equalsIgnoreCase(streamError.getCode())) {
                    msg = "User account in use on another client";
                }
            }
        }
        return msg == null ? e.getLocalizedMessage() : msg;
    }

    @Override
    public void connectionClosed() {
        statusHandler.info("Server closed connection");
        /* don't send notice for normal termination */
    }

    /**
     * Alert event bus listeners of disconnect
     * 
     * @param reason
     */
    private void sendDisconnectNotice(String reason) {
        ServerDisconnectEvent event = new ServerDisconnectEvent(reason);
        manager.postEvent(event);
    }

}
