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
package com.raytheon.uf.viz.collaboration.comm.provider.session;

import java.util.Arrays;
import java.util.Map;

import org.jivesoftware.smack.PacketListener;
import org.jivesoftware.smack.XMPPException;
import org.jivesoftware.smack.filter.PacketTypeFilter;
import org.jivesoftware.smack.packet.Packet;
import org.jivesoftware.smack.packet.Presence;
import org.jivesoftware.smack.packet.Presence.Type;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.IAccountManager;
import com.raytheon.uf.viz.collaboration.comm.identity.ISession;
import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.ISubscriptionResponder;
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
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class AccountManager implements IAccountManager {

    private final IUFStatusHandler log = UFStatus.getHandler(this.getClass());

    private PacketListener autoResponder = new PacketListener() {
        
        @Override
        public void processPacket(Packet packet) {
            if ( packet instanceof Presence){
                Presence pres = (Presence) packet;
                Type type = pres.getType();
                if (type == null) {
                    return;
                }
                UserId fromId = new UserId(pres.getFrom(), sessionManager
                        .getXmppConnection().getHost());
                switch (type) {
                case subscribe:
                    handleSubRequest(fromId);
                    break;
                case subscribed:
                    if (responder != null) {
                        responder.handleSubscribed(fromId);
                    }
                    break;
                case unsubscribed:
                    if (responder != null) {
                        responder.handleUnsubscribed(fromId);
                    }
                    break;
                default:
                    // do nothing
                    break;
                }
            }
        }

        private void handleSubRequest(UserId fromId) {
            Presence.Type subscribedType;
            if (responder != null) {
                subscribedType = responder.handleSubscribeRequest(fromId);
            } else {
                subscribedType = Presence.Type.subscribed;
            }

            Presence presence = new Presence(subscribedType, null, 0,
                    Presence.Mode.available);
            try {
                sendPresence(fromId, presence);
            } catch (CollaborationException e) {
                AccountManager.this.log.error("Unable to send presence", e);
            }
        }

    };

    private ISubscriptionResponder responder;

    private CollaborationConnection sessionManager = null;

    private org.jivesoftware.smack.AccountManager smackManager;

    /**
     * 
     * @param adapter
     */
    AccountManager(
            CollaborationConnection manager) {
        sessionManager = manager;
        smackManager = new org.jivesoftware.smack.AccountManager(
                manager.getXmppConnection());
        sessionManager.getXmppConnection().addPacketListener(autoResponder,
                new PacketTypeFilter(Presence.class));
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IAccountManager#
     * disableAutoSubscribe()
     */
    public void disableAutoSubscribe() {
        responder = null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IAccountManager#
     * autoSubscribeEnabled()
     */
    @Override
    public boolean autoSubscribeEnabled() {
        return responder != null;
    }

    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IAccountManager#setSubscriptionRequestResponder(com.raytheon.uf.viz.collaboration.comm.identity.roster.ISubscriptionResponder)
     */
    @Override
    public void setSubscriptionRequestResponder(ISubscriptionResponder responder) {
        this.responder = responder;
    }

    /**
     * 
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IAccountManager#removeSubscriptionRequestResponder()
     */
    @Override
    public void removeSubscriptionRequestResponder() {
        responder = null;
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
     * 
     * 
     * @param userPresence
     * @throws CollaborationException
     */
    @Override
    public void sendPresence(Presence userPresence)
            throws CollaborationException {

        sessionManager.getXmppConnection().sendPacket(userPresence);
        sessionManager.setPresence(userPresence);
        for (ISession session : sessionManager.getSessions()) {
            if (session instanceof IVenueSession) {
                ((IVenueSession) session).sendPresence(userPresence);
            }
        }
        sessionManager.postEvent(new UserPresenceChangedEvent(userPresence));
    }

    /**
     * 
     * 
     * @param userPresence
     * @throws CollaborationException
     */
    public void sendPresence(UserId toId, Presence userPresence)
            throws CollaborationException {
        userPresence.setFrom(sessionManager.getUser().getFQName());
        userPresence.setTo(toId.getNormalizedId());
        sessionManager.setPresence(userPresence);
        sessionManager.getXmppConnection().sendPacket(userPresence);
    }

}
