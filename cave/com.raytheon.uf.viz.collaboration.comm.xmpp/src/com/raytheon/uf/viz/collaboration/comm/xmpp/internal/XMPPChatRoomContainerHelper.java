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
package com.raytheon.uf.viz.collaboration.comm.xmpp.internal;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.eclipse.ecf.core.identity.ID;
import org.eclipse.ecf.core.identity.Namespace;
import org.eclipse.ecf.internal.provider.xmpp.events.ChatMembershipEvent;
import org.eclipse.ecf.internal.provider.xmpp.events.PresenceEvent;
import org.eclipse.ecf.internal.provider.xmpp.smack.ECFConnection;
import org.eclipse.ecf.presence.IIMMessageListener;
import org.eclipse.ecf.presence.IPresence;
import org.eclipse.ecf.presence.chatroom.IChatRoomParticipantListener;
import org.jivesoftware.smack.XMPPConnection;
import org.jivesoftware.smack.packet.Presence;

/**
 * Extend the ECF XMPPChatRoomContainerHelper with three changes (1) add
 * properties to presence events in a room. (2) expose methods needed by the Viz
 * version of XMPPChatRoomContainer. (3) disable the default chatMembership
 * events and instead fire ChatMembershipEvents when the presence changes.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 20, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@SuppressWarnings("restriction")
public class XMPPChatRoomContainerHelper extends
        org.eclipse.ecf.internal.provider.xmpp.XMPPChatRoomContainerHelper {

    /**
     * Need our own list to track who is in the room.
     */
    private final List<ID> chatRoomContainerParticipants = Collections
            .synchronizedList(new ArrayList<ID>());

    public XMPPChatRoomContainerHelper(Namespace usernamespace,
            XMPPConnection conn) {
        super(usernamespace, conn);
    }

    /**
     * exposed method to this package.
     */
    @Override
    protected void setRoomID(ID roomID) {
        super.setRoomID(roomID);
    }

    /**
     * exposed method to this package.
     */
    @Override
    protected void disconnect() {
        super.disconnect();
    }

    /**
     * exposed method to this package.
     */
    @Override
    protected void addChatParticipantListener(
            IChatRoomParticipantListener listener) {
        super.addChatParticipantListener(listener);
    }

    /**
     * exposed method to this package.
     */
    @Override
    protected void removeChatParticipantListener(
            IChatRoomParticipantListener listener) {
        super.removeChatParticipantListener(listener);
    }

    /**
     * exposed method to this package.
     */
    @Override
    protected void addChatRoomMessageListener(IIMMessageListener msgListener) {
        super.addChatRoomMessageListener(msgListener);
    }

    /**
     * exposed method to this package.
     */
    @Override
    protected ID createUserIDFromName(String name) {
        return super.createUserIDFromName(name);
    }

    /**
     * adapted from super.createIPresence, but modified to add properties from
     * the packet.
     */
    @Override
    protected IPresence createIPresence(Presence xmppPresence) {
        final String status = xmppPresence.getStatus();
        final IPresence newPresence = new org.eclipse.ecf.presence.Presence(
                createIPresenceType(xmppPresence), status,
                createIPresenceMode(xmppPresence),
                ECFConnection.getPropertiesFromPacket(xmppPresence));
        return newPresence;
    }

    /**
     * Overridden to also fire ChatMembershipEvent, see
     * handleChatMembershipEvent for more information on why this is done.
     */
    @Override
    protected void handlePresenceEvent(PresenceEvent evt) {
        super.handlePresenceEvent(evt);
        final Presence xmppPresence = evt.getPresence();
        final String from = canonicalizeRoomFrom(xmppPresence.getFrom());
        final ID fromID = createUserIDFromName(from);
        if (xmppPresence.getType().equals(Presence.Type.available)) {
            if (!chatRoomContainerParticipants.contains(fromID)) {
                chatRoomContainerParticipants.add(fromID);
                super.handleChatMembershipEvent(new ChatMembershipEvent(
                        xmppPresence.getFrom(), true));
            }
        } else {
            chatRoomContainerParticipants.remove(fromID);
            super.handleChatMembershipEvent(new ChatMembershipEvent(
                    xmppPresence.getFrom(), false));
        }
    }

    /**
     * The ECF version of this class will not always correctly fire these events
     * for the user that created this room. The reason is because
     * XMPPChatRoomManager.createChatRoom creates a temporary MultiUserChat to
     * create the room but this MultiUserChat registers with the
     * RoomListenerMultiplexor and it does not deregister until finalize. When
     * we connect to the room it will create a second MultiUserChat and it is
     * possible they are both registered. When the temporary chat is garbage
     * collected it removes the room from the RoomListenerMultiplexor and events
     * for that room are ignored. Most events do not come from the
     * RoomListenerMultiplexor so there is no problems but the
     * ChatMembershipEvent comes from that and breaks sometimes so we disable it
     * and instead fire these events from handlePresenceEvent since that event
     * is fired separately from the RoomListenerMultiplexor.
     */
    @Override
    protected void handleChatMembershipEvent(ChatMembershipEvent evt) {
        ; // disabled
    }

}
