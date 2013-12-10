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

import java.util.Iterator;
import java.util.List;

import org.apache.commons.lang.StringUtils;
import org.jivesoftware.smack.PacketListener;
import org.jivesoftware.smack.XMPPConnection;
import org.jivesoftware.smack.XMPPException;
import org.jivesoftware.smack.packet.Message;
import org.jivesoftware.smack.packet.Message.Type;
import org.jivesoftware.smack.packet.Packet;
import org.jivesoftware.smack.packet.Presence;
import org.jivesoftware.smackx.Form;
import org.jivesoftware.smackx.ServiceDiscoveryManager;
import org.jivesoftware.smackx.muc.MultiUserChat;
import org.jivesoftware.smackx.muc.ParticipantStatusListener;
import org.jivesoftware.smackx.packet.DiscoverItems;

import com.google.common.eventbus.EventBus;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.Activator;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.IMessage;
import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.identity.event.ParticipantEventType;
import com.raytheon.uf.viz.collaboration.comm.identity.info.IVenue;
import com.raytheon.uf.viz.collaboration.comm.identity.info.IVenueInfo;
import com.raytheon.uf.viz.collaboration.comm.identity.invite.VenueInvite;
import com.raytheon.uf.viz.collaboration.comm.provider.CollaborationMessage;
import com.raytheon.uf.viz.collaboration.comm.provider.SessionPayload;
import com.raytheon.uf.viz.collaboration.comm.provider.SessionPayload.PayloadType;
import com.raytheon.uf.viz.collaboration.comm.provider.TextMessage;
import com.raytheon.uf.viz.collaboration.comm.provider.Tools;
import com.raytheon.uf.viz.collaboration.comm.provider.event.VenueParticipantEvent;
import com.raytheon.uf.viz.collaboration.comm.provider.info.Venue;
import com.raytheon.uf.viz.collaboration.comm.provider.user.IDConverter;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;

/**
 * 
 * <ul>
 * <li>EventBus subscription events.</li>
 * <ul>
 * <li><strong>IVenueParticipantEvent</strong> : This event is posted when a
 * venue participant enters, leaves a venue, or updates their status in the
 * venue.</li>
 * <li><strong>TextMessage</strong> : Text messages send between users. Meant to
 * be displayed as conversation.</li>
 * <li><strong>CollaborationMessage</strong> : These messages are CAVE to CAVE
 * command messages.</li>
 * </ul>
 * </ul>
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 24, 2012            jkorman     Initial creation
 * Apr 17, 2012            njensen      Major refactor
 * Dec  6, 2013 2561       bclement    removed ECF
 * Dec 18, 2013 2562       bclement    moved data to packet extension
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 * @see com.raytheon.uf.viz.collaboration.comm.identity.event.IVenueParticipantEvent
 * @see com.raytheon.uf.viz.collaboration.comm.provider.TextMessage
 * @see com.raytheon.uf.viz.collaboration.comm.provider.CollaborationMessage
 */

public class VenueSession extends BaseSession implements IVenueSession {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(VenueSession.class);

    private static final String SEND_TXT = "[[TEXT]]";

    public static final String SEND_HISTORY = "[[HISTORY]]";

    private MultiUserChat muc = null;

    private PacketListener intListener = null;

    private PacketListener participantListener = null;

    private Venue venue;

    /**
     * 
     * @param container
     * @param eventBus
     */
    protected VenueSession(EventBus externalBus,
            CollaborationConnection manager, String sessionId)
            throws CollaborationException {
        super(externalBus, manager, sessionId);
    }

    /**
     * 
     * @param container
     * @param eventBus
     */
    protected VenueSession(EventBus externalBus, CollaborationConnection manager)
            throws CollaborationException {
        super(externalBus, manager);
    }

    /**
     * Close this session. Closing clears all listeners and disposes of the
     * container. No errors for attempting to close an already closed session.
     * 
     * @see com.raytheon.uf.viz.collaboration.comm.identity.ISession#close()
     */
    @Override
    public void close() {
        if (muc == null) {
            return;
        }
        if (intListener != null) {
            muc.removeMessageListener(intListener);
            intListener = null;
        }
        if (participantListener != null) {
            muc.removeParticipantListener(participantListener);
            participantListener = null;
        }
        muc.leave();
        muc = null;

        super.close();
    }

    /**
     * Get information about this venue.
     * 
     * @return The information about this venue. May return a null reference if
     *         the venue is not connected.
     */
    @Override
    public IVenue getVenue() {
        return venue;
    }

    /**
     * Send an invitation from this venue to another user.
     * 
     * @param room
     *            The target venue for this invitation.
     * @param id
     *            The target user for this invitation.
     * @param subject
     *            The intended subject of the venue conversation.
     * @param body
     *            Any text that the user may wish to include.
     * @throws CollaborationException
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession#sendInvitation(java.lang.String,
     *      java.lang.String, java.lang.String, java.lang.String)
     */
    @Override
    public void sendInvitation(UserId id, VenueInvite invite)
            throws CollaborationException {
        SessionPayload payload = new SessionPayload(PayloadType.Invitation,
                invite);
        Message msg = new Message();
        msg.setTo(id.getNormalizedId());
        msg.setFrom(getUserID().getNormalizedId());
        msg.setType(Type.normal);
        msg.addExtension(payload);
        String reason = "";
        if (!StringUtils.isBlank(invite.getMessage())) {
            reason = invite.getMessage();
        } else if (!StringUtils.isBlank(invite.getSubject())) {
            reason = invite.getSubject();
        }
        muc.invite(msg, id.getNormalizedId(), reason);
    }

    /**
     * Send an invitation from this venue to another user.
     * 
     * @param room
     *            The target venue for this invitation.
     * @param id
     *            The target user for this invitation.
     * @param subject
     *            The intended subject of the venue conversation.
     * @param body
     *            Any text that the user may wish to include.
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession#sendInvitation(java.lang.String,
     *      java.lang.String, java.lang.String, java.lang.String)
     */
    @Override
    public void sendInvitation(List<UserId> ids, VenueInvite invite)
            throws CollaborationException {
        if (ids != null) {
            for (UserId id : ids) {
                sendInvitation(id, invite);
            }
        }
    }

    @Override
    public void sendChatMessage(String message) throws CollaborationException {
        // Assume success
        if ((muc != null) && (message != null)) {
            Activator.getDefault().getNetworkStats()
                    .log(Activator.VENUE, message.length(), 0);
            try {
                muc.sendMessage(message);
            } catch (XMPPException e) {
                throw new CollaborationException("Error sending messge", e);
            }
        }
    }

    /**
     * Set up venue configuration and listeners. Must call
     * {@link VenueSession#connectToRoom()} to actually join room
     * 
     * @param venueName
     * @return
     * @throws CollaborationException
     */
    protected IVenueInfo configureVenue(String venueName)
            throws CollaborationException {
        CollaborationConnection manager = getSessionManager();
        XMPPConnection conn = manager.getXmppConnection();
        String roomId = getRoomId(conn.getHost(), venueName);
        this.muc = new MultiUserChat(conn, roomId);
        this.venue = new Venue(conn, muc);
        createListeners();
        return this.venue.getInfo();
    }

    /**
     * Construct room id from name and host
     * 
     * @param host
     * @param roomName
     * @return
     */
    private String getRoomId(String host, String roomName) {
        return roomName + "@conference." + host;
    }

    /**
     * Create room and connect to it
     * 
     * @param venueName
     * @throws CollaborationException
     * @throws Exception
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession#createVenue(java.lang.String,
     *      java.lang.String)
     */
    protected IVenueInfo createVenue(String venueName, String subject)
            throws CollaborationException {
        try {
            CollaborationConnection manager = getSessionManager();
            XMPPConnection conn = manager.getXmppConnection();
            String roomId = getRoomId(conn.getHost(), venueName);
            if (roomExistsOnServer(roomId)) {
                throw new CollaborationException("Session name already in use");
            }
            this.muc = new MultiUserChat(conn, roomId);
            createListeners();
            UserId user = manager.getUser();
            muc.create(user.getName());
            muc.sendConfigurationForm(new Form(Form.TYPE_SUBMIT));
            muc.changeSubject(subject);
            this.venue = new Venue(conn, muc);
            sendPresence(CollaborationConnection.getConnection().getPresence());
            return this.venue.getInfo();
        } catch (XMPPException e) {
            throw new CollaborationException("Error creating venue "
                    + venueName, e);
        }
    }

    /**
     * @param roomId
     * @return true if room exists on server
     * @throws XMPPException
     */
    protected boolean roomExistsOnServer(String roomId) throws XMPPException {
        String host = Tools.parseHost(roomId);
        CollaborationConnection manager = getSessionManager();
        XMPPConnection conn = manager.getXmppConnection();
        ServiceDiscoveryManager serviceDiscoveryManager = new ServiceDiscoveryManager(
                conn);
        DiscoverItems result = serviceDiscoveryManager.discoverItems(host);

        for (Iterator<DiscoverItems.Item> items = result.getItems(); items
                .hasNext();) {
            DiscoverItems.Item item = items.next();
            if (roomId.equals(item.getEntityID())) {
                return true;
            }
        }
        return false;
    }

    /**
     * register chat room listeners with muc
     */
    private void createListeners() {
        muc.addParticipantStatusListener(new ParticipantStatusListener() {

            @Override
            public void voiceRevoked(String participant) {
                // TODO Auto-generated method stub

            }

            @Override
            public void voiceGranted(String participant) {
                // TODO Auto-generated method stub

            }

            @Override
            public void ownershipRevoked(String participant) {
                // TODO Auto-generated method stub

            }

            @Override
            public void ownershipGranted(String participant) {
                // TODO Auto-generated method stub

            }

            @Override
            public void nicknameChanged(String participant, String newNickname) {
                // TODO how do we pass along new nickname?
                UserId user = IDConverter.convertFromRoom(muc, participant);
                postEvent(new VenueParticipantEvent(user,
                        ParticipantEventType.UPDATED));
            }

            @Override
            public void moderatorRevoked(String participant) {
                // TODO Auto-generated method stub

            }

            @Override
            public void moderatorGranted(String participant) {
                // TODO Auto-generated method stub

            }

            @Override
            public void membershipRevoked(String participant) {
                // TODO Auto-generated method stub

            }

            @Override
            public void membershipGranted(String participant) {
                // TODO Auto-generated method stub

            }

            @Override
            public void left(String participant) {
                UserId user = IDConverter.convertFromRoom(muc, participant);
                postEvent(new VenueParticipantEvent(user,
                        ParticipantEventType.DEPARTED));

            }

            @Override
            public void kicked(String participant, String actor, String reason) {
                this.left(participant);
            }

            @Override
            public void joined(String participant) {
                UserId user = IDConverter.convertFromRoom(muc, participant);
                postEvent(new VenueParticipantEvent(user,
                        ParticipantEventType.ARRIVED));
            }

            @Override
            public void banned(String participant, String actor, String reason) {
                this.left(participant);
            }

            @Override
            public void adminRevoked(String participant) {
                // TODO Auto-generated method stub

            }

            @Override
            public void adminGranted(String participant) {
                // TODO Auto-generated method stub

            }

        });
        // presence listener
        muc.addParticipantListener(new PacketListener() {

            @Override
            public void processPacket(Packet packet) {
                if (packet instanceof Presence) {
                    Presence p = (Presence) packet;
                    String fromID = p.getFrom();
                    UserId user = IDConverter.convertFromRoom(muc, fromID);
                    venue.handlePresenceUpdated(user, p);
                    postEvent(new VenueParticipantEvent(user, p,
                            ParticipantEventType.PRESENCE_UPDATED));
                }
            }
        });
        // message listener
        this.muc.addMessageListener(new PacketListener() {

            @Override
            public void processPacket(Packet packet) {
                if (packet instanceof Message) {
                    Message m = (Message) packet;
                    Activator.getDefault().getNetworkStats()
                            .log(Activator.VENUE, 0, m.getBody().length());
                    if (accept(m)) {
                        distributeMessage(convertMessage(m));
                    }
                }

            }
        });
    }

    /**
     * Allows users to connect after the fact so that they do not miss any
     * messages coming from the room (after the dialog/view has been
     * instantiated)
     */
    public void connectToRoom() {
        if (this.muc.isJoined()) {
            return;
        }
        try {
            UserId user = getSessionManager().getUser();
            this.muc.join(user.getName());
            sendPresence(CollaborationConnection.getConnection().getPresence());
        } catch (CollaborationException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        } catch (XMPPException e) {
            statusHandler.handle(Priority.ERROR,
                    "Unable to connect to container", e);
        }
    }

    /**
     * Examine the incoming message to determine if it should be forwarded. The
     * method looks at the from identifier to determine who sent the message.
     * 
     * @param message
     *            A message to accept.
     * @return Should the message be accepted.
     */
    private boolean accept(Message message) {
        if (this.muc == null) {
            // we don't seem to be in a room
            return false;
        }

        String from = message.getFrom();
        String roomName = Tools.parseName(from);
        String thisRoom = Tools.parseName(this.muc.getRoom());
        if (!thisRoom.equals(roomName)) {
            // this message is for another room, they should have a listener to
            // pick it up
            return false;
        }

        UserId account = getSessionManager().getUser();
        UserId fromUser = IDConverter.convertFromRoom(muc, from);

        String body = message.getBody();

        if (!body.startsWith(SEND_HISTORY) && fromUser.isSameUser(account)) {
            // ignore from ourselves except for history
            return false;
        } else if (body.startsWith(Tools.CMD_PREAMBLE)
                || body.startsWith(Tools.CONFIG_PREAMBLE)) {
            return false;
        }

        return true;
    }

    /**
     * 
     * @param message
     */
    private void distributeMessage(IMessage message) {
        if (message != null) {

            String body = message.getBody();
            if (body != null) {
                if (body.startsWith(SEND_HISTORY)) {
                    String[] vars = body.split("\\|");
                    String timeString = vars[0]
                            .substring(SEND_HISTORY.length());
                    long time = Long.parseLong(timeString);
                    String username = vars[1];
                    String site = vars[2];
                    // add the SEND_HISTORY tag length, and the timestamp
                    // length, username length, and the site length plus the
                    // three pipe characters
                    String moddedBody = body.substring(SEND_HISTORY.length()
                            + timeString.length() + username.length()
                            + site.length() + 3);
                    message.setBody(moddedBody);
                    TextMessage msg = new TextMessage(message.getFrom(),
                            message.getBody());
                    UserId id = new UserId(username, CollaborationConnection
                            .getConnection().getConnectionData().getServer());
                    msg.setFrom(id);
                    msg.setTimeStamp(time);
                    msg.setSubject(site);
                    msg.setStatus(SEND_HISTORY);
                    this.postEvent(msg);
                } else {
                    message.setBody(body);
                    TextMessage msg = new TextMessage(message.getTo(),
                            message.getBody());
                    msg.setFrom(message.getFrom());

                    this.postEvent(msg);
                }
            }
        }
    }

    /**
     * Convert from an ECF chat room message to an IMessage instance.
     * 
     * @param msg
     *            The ECF chat room message to convert.
     * @return The converted message.
     */
    private IMessage convertMessage(Message msg) {
        IMessage message = null;

        String body = msg.getBody();
        if (body != null) {
            if (body.startsWith(SEND_TXT)) {
                body = body.substring(SEND_TXT.length());
            }
            message = new CollaborationMessage(null, body);
            message.setFrom(IDConverter.convertFromRoom(muc, msg.getFrom()));
        }
        return message;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession#sendPresence
     * (org.jivesoftware.smack.packet.Presence)
     */
    @Override
    public void sendPresence(Presence presence) throws CollaborationException {
        presence.setTo(venue.getInfo().getVenueID());
        XMPPConnection conn = getConnection().getXmppConnection();
        conn.sendPacket(presence);
    }

}
