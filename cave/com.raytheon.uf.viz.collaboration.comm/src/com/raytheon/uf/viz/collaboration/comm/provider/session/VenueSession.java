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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.ecf.core.IContainer;
import org.eclipse.ecf.core.identity.ID;
import org.eclipse.ecf.core.identity.IDFactory;
import org.eclipse.ecf.core.user.IUser;
import org.eclipse.ecf.core.util.ECFException;
import org.eclipse.ecf.presence.IIMMessageEvent;
import org.eclipse.ecf.presence.IIMMessageListener;
import org.eclipse.ecf.presence.IPresence;
import org.eclipse.ecf.presence.chatroom.IChatRoomContainer;
import org.eclipse.ecf.presence.chatroom.IChatRoomInfo;
import org.eclipse.ecf.presence.chatroom.IChatRoomInvitationSender;
import org.eclipse.ecf.presence.chatroom.IChatRoomManager;
import org.eclipse.ecf.presence.chatroom.IChatRoomMessage;
import org.eclipse.ecf.presence.chatroom.IChatRoomMessageEvent;
import org.eclipse.ecf.presence.chatroom.IChatRoomMessageSender;
import org.eclipse.ecf.presence.chatroom.IChatRoomParticipantListener;
import org.eclipse.ecf.provider.xmpp.identity.XMPPRoomID;

import com.google.common.eventbus.EventBus;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.collaboration.comm.Activator;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.IMessage;
import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IVenueParticipantEvent;
import com.raytheon.uf.viz.collaboration.comm.identity.event.ParticipantEventType;
import com.raytheon.uf.viz.collaboration.comm.identity.info.IVenue;
import com.raytheon.uf.viz.collaboration.comm.identity.invite.VenueInvite;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID;
import com.raytheon.uf.viz.collaboration.comm.provider.CollaborationMessage;
import com.raytheon.uf.viz.collaboration.comm.provider.TextMessage;
import com.raytheon.uf.viz.collaboration.comm.provider.Tools;
import com.raytheon.uf.viz.collaboration.comm.provider.event.VenueParticipantEvent;
import com.raytheon.uf.viz.collaboration.comm.provider.info.InfoAdapter;
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

    private static final String SEND_CMD = "[[COMMAND";

    private static final String SEND_TXT = "[[TEXT]]";

    private IChatRoomManager venueManager = null;

    private IChatRoomInfo venueInfo = null;

    private IChatRoomContainer venueContainer = null;

    private IIMMessageListener intListener = null;

    private IChatRoomParticipantListener participantListener = null;

    private String subject;

    /**
     * 
     * @param container
     * @param eventBus
     */
    protected VenueSession(IContainer container, EventBus externalBus,
            CollaborationConnection manager, String sessionId)
            throws CollaborationException {
        super(container, externalBus, manager, sessionId);
    }

    /**
     * 
     * @param container
     * @param eventBus
     */
    protected VenueSession(IContainer container, EventBus externalBus,
            CollaborationConnection manager) throws CollaborationException {
        super(container, externalBus, manager);
    }

    /**
     * Close this session. Closing clears all listeners and disposes of the
     * container. No errors for attempting to close an already closed session.
     * 
     * @see com.raytheon.uf.viz.collaboration.comm.identity.ISession#close()
     */
    @Override
    public void close() {

        if (intListener != null) {
            venueContainer.removeMessageListener(intListener);
            intListener = null;
        }
        if (participantListener != null) {
            venueContainer
                    .removeChatRoomParticipantListener(participantListener);
            participantListener = null;
        }
        if (venueContainer != null) {
            venueContainer.disconnect();
            venueContainer = null;
        }

        venueManager = null;
        venueInfo = null;

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
        IVenue venue = null;
        if (isConnected() && (venueContainer != null)) {
            venue = new Venue();
            ID[] ids = venueContainer.getChatRoomParticipants();
            for (ID id : ids) {
                UserId vp = IDConverter.convertFrom(id);
                venue.addParticipant(vp);
            }
            venue.setInfo(InfoAdapter.createVenueInfo(venueInfo));
        } else {

        }
        return venue;
    }

    /**
     * @return the subject
     */
    public String getSubject() {
        return subject;
    }

    /**
     * @param subject
     *            the subject to set
     */
    public void setSubject(String subject) {
        this.subject = subject;
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
        IChatRoomInvitationSender sender = getConnectionPresenceAdapter()
                .getChatRoomManager().getInvitationSender();
        if (sender != null) {
            String msgBody = Tools.marshallData(invite);
            ID roomId = venueInfo.getConnectedID();
            ID userId = IDFactory.getDefault().createID(
                    getConnectionNamespace(), id.getFQName());

            try {
                sender.sendInvitation(roomId, userId, invite.getSubject(),
                        msgBody);
            } catch (ECFException e) {
                throw new CollaborationException("Error sending invitation", e);
            }
        }
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
        this.sendMessageToVenue(message);
    }

    protected void sendMessageToVenue(String message)
            throws CollaborationException {
        // Assume success
        if ((venueContainer != null) && (message != null)) {
            Activator.getDefault().getNetworkStats()
                    .log(Activator.VENUE, message.length(), 0);
            IChatRoomMessageSender sender = venueContainer
                    .getChatRoomMessageSender();
            try {
                if (message.startsWith(SEND_CMD)) {
                    sender.sendMessage(message);
                } else {
                    sender.sendMessage(SEND_TXT + message);
                }
            } catch (ECFException e) {
                throw new CollaborationException("Error sending messge", e);
            }
        }
    }

    protected void joinVenue(String venueName) throws CollaborationException {
        try {
            // Create chat room container from manager
            venueManager = getConnectionPresenceAdapter().getChatRoomManager();
            if (venueManager != null) {
                venueInfo = venueManager.getChatRoomInfo(venueName);
                subject = venueInfo.getDescription();
                if (venueInfo != null) {
                    completeVenueConnection(venueInfo);
                }
            }
        } catch (Exception e) {
            throw new CollaborationException(
                    "Error joining venue " + venueName, e);
        }
    }

    /**
     * 
     * @param venueName
     * @throws CollaborationException
     * @throws Exception
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession#createVenue(java.lang.String,
     *      java.lang.String)
     */
    protected void createVenue(String venueName, String subject)
            throws CollaborationException {
        try {
            this.subject = subject;
            // Create chat room container from manager
            venueManager = getConnectionPresenceAdapter().getChatRoomManager();
            if (venueManager != null) {
                venueInfo = venueManager.getChatRoomInfo(venueName);
                if (venueInfo == null) {
                    Map<String, String> props = null;
                    if (subject != null) {
                        props = new HashMap<String, String>();
                        props.put(Tools.VENUE_SUBJECT_PROP, subject);
                    }
                    venueInfo = venueManager.createChatRoom(venueName, props);
                    completeVenueConnection(venueInfo);
                }
            }
        } catch (Exception e) {
            throw new CollaborationException("Error creating venue "
                    + venueName, e);
        }
    }

    /**
     * 
     * @return
     * @throws CollaborationException
     */
    private void completeVenueConnection(IChatRoomInfo venueInfo)
            throws CollaborationException {
        if (venueInfo != null) {
            try {
                venueContainer = venueInfo.createChatRoomContainer();

                IChatRoomParticipantListener pListener = new IChatRoomParticipantListener() {
                    @Override
                    public void handleArrived(IUser participant) {
                    }

                    @Override
                    public void handleUpdated(IUser participant) {
                    }

                    @Override
                    public void handleDeparted(IUser participant) {
                    }

                    @Override
                    public void handlePresenceUpdated(ID fromID,
                            org.eclipse.ecf.presence.IPresence presence) {

                        UserId vp = IDConverter.convertFrom(fromID);
                        IVenueParticipantEvent event = null;
                        if (IPresence.Type.AVAILABLE.equals(presence.getType())) {
                            event = new VenueParticipantEvent(vp, presence,
                                    ParticipantEventType.ARRIVED);
                            getEventPublisher().post(event);
                        } else if (IPresence.Type.UNAVAILABLE.equals(presence
                                .getType())) {
                            event = new VenueParticipantEvent(vp, presence,
                                    ParticipantEventType.DEPARTED);
                            getEventPublisher().post(event);
                        }
                    }
                };
                venueContainer.addChatRoomParticipantListener(pListener);

                venueContainer.connect(venueInfo.getRoomID(), null);
                if (venueContainer.getConnectedID() != null) {

                    intListener = new IIMMessageListener() {
                        public void handleMessageEvent(
                                IIMMessageEvent messageEvent) {
                            if (messageEvent instanceof IChatRoomMessageEvent) {
                                IChatRoomMessage m = ((IChatRoomMessageEvent) messageEvent)
                                        .getChatRoomMessage();
                                Activator
                                        .getDefault()
                                        .getNetworkStats()
                                        .log(Activator.VENUE, 0,
                                                m.getMessage().length());
                                if (accept(m)) {
                                    distributeMessage(convertMessage(m));
                                }
                            }
                        }
                    };
                    venueContainer.addMessageListener(intListener);

                }
            } catch (Exception e) {
                throw new CollaborationException(
                        "Error completing connection to venue", e);
            }
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
    private boolean accept(IChatRoomMessage message) {
        boolean acceptMessage = true;

        String body = message.getMessage();
        // Command data only
        if (body.startsWith(SEND_CMD)) {
            ID from = message.getFromID();

            String name = Tools.parseName(from.getName());

            UserId account = getSessionManager().getUser();
            String aName = account.getFQName();
            if (aName.equals(name)) {
                acceptMessage = false;
            }
        }
        return acceptMessage;
    }

    /**
     * 
     * @param message
     */
    private void distributeMessage(IMessage message) {
        if (message != null) {

            String body = message.getBody();
            if (body != null) {
                if (body.startsWith(SEND_CMD)) {
                    Object o = null;
                    try {
                        o = Tools.unMarshallData(body);
                        if (o != null) {
                            getEventPublisher().post(o);
                        }
                    } catch (CollaborationException ce) {
                        statusHandler.error(
                                "Error deserializing received message on venue "
                                        + venueInfo.getName(), ce);
                    }
                } else if (body.startsWith(SEND_TXT)) {
                    body = body.substring(SEND_TXT.length());
                    message.setBody(body);

                    TextMessage msg = new TextMessage(message.getTo(),
                            message.getBody());
                    msg.setFrom(message.getFrom());

                    getEventPublisher().post(msg);
                } else {
                    // attempt to handle outside clients as text only since the
                    // SEND_TXT won't be appended to the first portion of the
                    // body
                    // message.setBody(body);
                    // TextMessage msg = new TextMessage(message.getTo(),
                    // message.getBody());
                    // msg.setFrom(message.getFrom());
                    //
                    // getEventPublisher().post(msg);
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
    private IMessage convertMessage(IChatRoomMessage msg) {
        IMessage message = null;

        String body = msg.getMessage();
        if (body != null) {
            message = new CollaborationMessage(null, msg.getMessage());

            org.eclipse.ecf.presence.im.IChatID cID = (org.eclipse.ecf.presence.im.IChatID) msg
                    .getFromID();
            XMPPRoomID rID = (XMPPRoomID) msg.getChatRoomID();

            IQualifiedID id = new UserId(cID.getUsername(), rID.getHostname());
            message.setFrom(id);
        }
        return message;
    }
}
