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
package com.raytheon.uf.viz.collaboration.comm.provider.account;

import java.util.Arrays;
import java.util.Map;

import org.jivesoftware.smack.Roster.SubscriptionMode;
import org.jivesoftware.smack.XMPPConnection;
import org.jivesoftware.smack.XMPPException;
import org.jivesoftware.smack.filter.PacketTypeFilter;
import org.jivesoftware.smack.packet.Presence;

import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.IAccountManager;
import com.raytheon.uf.viz.collaboration.comm.identity.ISession;
import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.ISubscriptionResponder;
import com.raytheon.uf.viz.collaboration.comm.provider.Tools;
import com.raytheon.uf.viz.collaboration.comm.provider.connection.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.event.UserPresenceChangedEvent;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;

/**
 * Manages account information on server
 * 
 * <ul>
 * EventBus subscription events.
 * <li>ISubscriptionResponseEvent : This event is posted when a subscription
 * request has been responded to.</li>
 * </ul>
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 16, 2012            jkorman     Initial creation
 * Dec  6, 2013 2561       bclement    removed ECF
 * Jan 07, 2013 2563       bclement    fixed id parsing in auto responder
 * Jan 27, 2014 2700       bclement    changes to subscription request responders
 * Jan 31, 2014 2700       bclement    fixed subscribe back after accepting subscription
 * Feb 12, 2014 2797       bclement    added protective copy to sendPresence
 * Feb 13, 2014 2755       bclement    added user input for which group to add contact to
 * Apr 07, 2014 2785       mpduff      Moved PacketListener implementation to its own class
 * Apr 14, 2014 2903       bclement    moved from session subpackage to account, made constructor public
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class AccountManager implements IAccountManager {

    private ISubscriptionResponder responder;

    private CollaborationConnection sessionManager = null;

    private final org.jivesoftware.smack.AccountManager smackManager;

    private final SubscriptionPacketListener subscriptionEventListener;

    /**
     * 
     * @param adapter
     */
    public AccountManager(CollaborationConnection manager) {
        sessionManager = manager;
        subscriptionEventListener = new SubscriptionPacketListener(
                sessionManager);
        XMPPConnection xmppConnection = manager.getXmppConnection();
        smackManager = new org.jivesoftware.smack.AccountManager(xmppConnection);
        xmppConnection.getRoster().setSubscriptionMode(SubscriptionMode.manual);
        sessionManager.getXmppConnection()
                .addPacketListener(subscriptionEventListener,
                        new PacketTypeFilter(Presence.class));
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IAccountManager#
     * autoSubscribeEnabled()
     */
    @Override
    public boolean isSubscriptionRequestResponderSet() {
        return responder != null;
    }

    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IAccountManager#setSubscriptionRequestResponder(com.raytheon.uf.viz.collaboration.comm.identity.roster.ISubscriptionResponder)
     */
    @Override
    public void setSubscriptionRequestResponder(ISubscriptionResponder responder) {
        this.responder = responder;
        subscriptionEventListener.setResponder(responder);
    }

    /**
     * 
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IAccountManager#removeSubscriptionRequestResponder()
     */
    @Override
    public void removeSubscriptionRequestResponder() {
        responder = null;
        subscriptionEventListener.setResponder(responder);
    }

    /**
     * 
     * @param password
     *            The new password. For security the password is a character
     *            array that will be zero'd after use.
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IAccountManager#changePassword(char[])
     */
    @Override
    public void changePassword(char[] password) throws CollaborationException {
        try {
            smackManager.changePassword(new String(password));
        } catch (XMPPException e) {
            throw new CollaborationException(
                    "Could not change account password");
        } finally {
            // all done so clear the password.
            Arrays.fill(password, (char) 0);
        }
    }

    /**
     * 
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IAccountManager#deleteAccount()
     */
    @Override
    public void deleteAccount() throws CollaborationException {
        try {
            smackManager.deleteAccount();
        } catch (XMPPException e) {
            throw new CollaborationException("Could not delete account");
        }
    }

    /**
     * Determines if the server allows new accounts to be created by the user.
     * 
     * @throws CollaborationException
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IAccountManager#canCreateAccount()
     */
    @Override
    public boolean canCreateAccount() throws CollaborationException {
        return smackManager.supportsAccountCreation();
    }

    /**
     * Create a new account on the server
     * 
     * @param password
     * @param attributes
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IAccountManager#createAccount(java.lang.String,
     *      char[], java.util.Map)
     */
    @Override
    public void createAccount(String name, char[] password,
            Map<String, String> attributes) throws CollaborationException {
        // create the account
        try {
            smackManager.createAccount(name, new String(password), attributes);
        } catch (XMPPException e) {
            throw new CollaborationException(
                    "Could not create account for user: " + name, e);
        } finally {
            // all done so clear the password.
            Arrays.fill(password, (char) 0);
        }
    }

    /**
     * broadcast new presence to server
     * 
     * @param userPresence
     * @throws CollaborationException
     */
    @Override
    public void sendPresence(Presence userPresence)
            throws CollaborationException {
        userPresence.setTo(null);
        sessionManager.getXmppConnection().sendPacket(userPresence);
        sessionManager.setPresence(userPresence);
        for (ISession session : sessionManager.getSessions()) {
            if (session instanceof IVenueSession) {
                Presence copy = new Presence(userPresence.getType(),
                        userPresence.getStatus(), userPresence.getPriority(),
                        userPresence.getMode());
                Tools.copyProperties(userPresence, copy);
                ((IVenueSession) session).sendPresence(copy);
            }
        }
        sessionManager.postEvent(new UserPresenceChangedEvent(userPresence));
    }

    /**
     * send presence to specific address
     * 
     * @param userPresence
     * @throws CollaborationException
     */
    @Override
    public void sendPresence(UserId toId, Presence userPresence)
            throws CollaborationException {
        userPresence.setFrom(sessionManager.getUser().getFQName());
        userPresence.setTo(toId.getNormalizedId());
        sessionManager.getXmppConnection().sendPacket(userPresence);
    }
}
