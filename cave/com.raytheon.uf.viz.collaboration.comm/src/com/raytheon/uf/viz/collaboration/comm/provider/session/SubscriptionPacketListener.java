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

import org.jivesoftware.smack.PacketListener;
import org.jivesoftware.smack.RosterEntry;
import org.jivesoftware.smack.packet.Packet;
import org.jivesoftware.smack.packet.Presence;
import org.jivesoftware.smack.packet.Presence.Type;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IRosterChangeEvent;
import com.raytheon.uf.viz.collaboration.comm.identity.event.RosterChangeType;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.ISubscriptionResponder;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.SubscriptionResponse;
import com.raytheon.uf.viz.collaboration.comm.provider.event.RosterChangeEvent;
import com.raytheon.uf.viz.collaboration.comm.provider.user.ContactsManager;
import com.raytheon.uf.viz.collaboration.comm.provider.user.IDConverter;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;

/**
 * Listens for subscription events including subscription requests. If the
 * subscription responder is null, the default action is to accept subscription
 * requests.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 04, 2014    2785    mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class SubscriptionPacketListener implements PacketListener,
        ISubscriptionRequestCompleteAction {

    private static final IUFStatusHandler log = UFStatus
            .getHandler(SubscriptionPacketListener.class);

    /**
     * The responder for this listener.
     */
    private ISubscriptionResponder responder;

    /**
     * The CollaborationConnection for his listener.
     */
    private CollaborationConnection sessionManager = null;

    /**
     * Constructor.
     * 
     * @param sessionManager
     *            The CollaborationConnection to use
     */
    public SubscriptionPacketListener(CollaborationConnection sessionManager) {
        this.sessionManager = sessionManager;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void processPacket(Packet packet) {
        if (packet instanceof Presence) {
            Presence pres = (Presence) packet;
            Type type = pres.getType();
            if (type == null) {
                return;
            }
            UserId fromId = IDConverter.convertFrom(pres.getFrom());
            switch (type) {
            case subscribe:
                responder.handleSubscribeRequest(fromId, this);
                break;
            case subscribed:
                handleSubscribed(fromId);
                break;
            case unsubscribed:
                handleUnsubscribed(fromId);
                break;
            default:
                // do nothing
                break;
            }
        }
    }

    /**
     * notify UI that someone subscribed to user
     * 
     * @param fromID
     */
    private void handleSubscribed(UserId fromID) {
        ContactsManager cm = sessionManager.getContactsManager();
        RosterEntry entry = cm.getRosterEntry(fromID);
        IRosterChangeEvent event = new RosterChangeEvent(RosterChangeType.ADD,
                entry);
        sessionManager.postEvent(event);
    }

    /**
     * notify UI that someone unsubscribed to user
     * 
     * @param fromID
     */
    private void handleUnsubscribed(UserId fromID) {
        ContactsManager cm = sessionManager.getContactsManager();
        RosterEntry entry = cm.getRosterEntry(fromID);
        if (entry == null) {
            return;
        }
        IRosterChangeEvent event = new RosterChangeEvent(
                RosterChangeType.DELETE, entry);
        sessionManager.postEvent(event);
    }

    /**
     * process subscription request
     * 
     * @param fromId
     */
    private void handleSubResponse(UserId fromId, SubscriptionResponse response) {
        Presence.Type subscribedType;
        ContactsManager cm = sessionManager.getContactsManager();
        boolean addToRoster = false;
        if (response.isAccepted()) {
            subscribedType = Presence.Type.subscribed;
            RosterEntry entry = cm.getRosterEntry(fromId);
            if (entry == null) {
                addToRoster = true;
            }
        } else {
            subscribedType = Presence.Type.unsubscribed;
        }

        Presence presence = new Presence(subscribedType);
        try {
            sendPresence(fromId, presence);
            if (addToRoster) {
                if (response.addToGroup()) {
                    cm.addToGroup(response.getGroup(), fromId);
                } else {
                    cm.addToRoster(fromId);
                }

            }
        } catch (CollaborationException e) {
            log.error("Unable to send presence", e);
        }
    }

    private void sendPresence(UserId toId, Presence userPresence) {
        userPresence.setFrom(sessionManager.getUser().getFQName());
        userPresence.setTo(toId.getNormalizedId());
        sessionManager.getXmppConnection().sendPacket(userPresence);
    }

    @Override
    public void executeSubscriptionRequestComplete(UserId userId,
            SubscriptionResponse response) {
        handleSubResponse(userId, response);
    }

    public void setResponder(ISubscriptionResponder responder) {
        this.responder = responder;
    }
}
