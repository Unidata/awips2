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
import org.eclipse.ecf.presence.IPresenceSender;
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
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.IMessage;
import com.raytheon.uf.viz.collaboration.comm.identity.IPresence;
import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IDisplayEvent;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IRenderable;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IVenueParticipantEvent;
import com.raytheon.uf.viz.collaboration.comm.identity.event.ParticipantEventType;
import com.raytheon.uf.viz.collaboration.comm.identity.info.IVenue;
import com.raytheon.uf.viz.collaboration.comm.identity.invite.SharedDisplayInvite;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterManager;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IChatID;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IVenueParticipant;
import com.raytheon.uf.viz.collaboration.comm.identity.user.ParticipantRole;
import com.raytheon.uf.viz.collaboration.comm.provider.CollaborationMessage;
import com.raytheon.uf.viz.collaboration.comm.provider.Errors;
import com.raytheon.uf.viz.collaboration.comm.provider.Presence;
import com.raytheon.uf.viz.collaboration.comm.provider.TextMessage;
import com.raytheon.uf.viz.collaboration.comm.provider.Tools;
import com.raytheon.uf.viz.collaboration.comm.provider.event.VenueParticipantEvent;
import com.raytheon.uf.viz.collaboration.comm.provider.info.InfoAdapter;
import com.raytheon.uf.viz.collaboration.comm.provider.info.Venue;
import com.raytheon.uf.viz.collaboration.comm.provider.user.RosterId;
import com.raytheon.uf.viz.collaboration.comm.provider.user.VenueParticipant;

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
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 * @see com.raytheon.uf.viz.collaboration.comm.identity.event.IVenueParticipantEvent
 * @see com.raytheon.uf.viz.collaboration.comm.provider.TextMessage
 * @see com.raytheon.uf.viz.collaboration.comm.provider.CollaborationMessage
 */

public class VenueSession extends BaseSession implements IVenueSession,
        ISharedDisplaySession {

    private static final String SEND_CMD = "[[COMMAND";

    private static final String SEND_TXT = "[[TEXT]]";

    private IChatRoomManager venueManager = null;

    private IChatRoomInfo venueInfo = null;

    private IChatRoomContainer venueContainer = null;

    private IIMMessageListener intListener = null;

    private IChatRoomParticipantListener participantListener = null;

    private IQualifiedID userID = null;

    private IVenueParticipant sessionLeader = null;

    private IVenueParticipant dataProvider = null;

    private String subject;

    /**
     * 
     * @param container
     * @param eventBus
     */
    VenueSession(IContainer container, EventBus externalBus,
            SessionManager manager, String sessionId)
            throws CollaborationException {
        super(container, externalBus, manager, sessionId);
    }

    /**
     * 
     * @param container
     * @param eventBus
     */
    VenueSession(IContainer container, EventBus externalBus,
            SessionManager manager) throws CollaborationException {
        super(container, externalBus, manager);
    }

    /**
     * Get the identification of the owner of this session.
     * 
     * @see com.raytheon.uf.viz.collaboration.comm.identity.ISession#getUserID()
     */
    @Override
    public IQualifiedID getUserID() {
        return userID;
    }

    /**
     * Return this session as an ISharedDisplaySession if it is supported. If
     * the interface is not supported the method must return a null reference.
     * 
     * @return
     */
    @Override
    public ISharedDisplaySession spawnSharedDisplaySession() {
        return this;
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
                String fullName = id.getName();
                IVenueParticipant vp = new VenueParticipant();
                vp.setName(Tools.parseName(fullName));
                vp.setHost(Tools.parseHost(fullName));
                vp.setResource(Tools.parseResource(fullName));
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
     * @return
     * @throws CollaborationException
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession#sendInvitation(java.lang.String,
     *      java.lang.String, java.lang.String, java.lang.String)
     */
    @Override
    public int sendInvitation(String id, String body)
            throws CollaborationException {
        // Assume success
        int status = Errors.NO_ERROR;
        IChatRoomInvitationSender sender = getConnectionPresenceAdapter()
                .getChatRoomManager().getInvitationSender();
        if (sender != null) {
            SharedDisplayInvite invite = new SharedDisplayInvite();
            invite.setDataProvider(this.getCurrentDataProvider());
            invite.setSessionLeader(this.getCurrentSessionLeader());
            invite.setMessage(body);
            invite.setSessionId(this.sessionId);
            invite.setSubject(this.getSubject());
            String msgBody = Tools.marshallData(invite);

            ID roomId = venueInfo.getConnectedID();

            ID userId = IDFactory.getDefault().createID(
                    getConnectionNamespace(), id);

            try {
                sender.sendInvitation(roomId, userId, subject, msgBody);
            } catch (ECFException e) {
                e.printStackTrace();
            }
        }
        return status;
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
     * @return
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession#sendInvitation(java.lang.String,
     *      java.lang.String, java.lang.String, java.lang.String)
     */
    @Override
    public int sendInvitation(List<String> ids, String body)
            throws CollaborationException {
        // Assume success
        int status = Errors.NO_ERROR;
        if (ids != null) {
            for (String id : ids) {
                sendInvitation(id, body);
            }
        } else {
            status = -1;
        }
        return status;
    }

    /**
     * 
     * @see com.raytheon.uf.viz.collaboration.comm.identity.ISession#getSessionId()
     */
    @Override
    public String getSessionId() {
        return sessionId;
    }

    // ***************************
    // ISharedDisplaySession
    // ***************************

    /**
     * 
     * @param participant
     * @param event
     * @throws CollaborationException
     */
    @Override
    public void sendEvent(
            com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID participant,
            IDisplayEvent event) throws CollaborationException {

        PeerToPeerChat session = null;
        session = getP2PSession();
        if (session != null) {
            String message = Tools.marshallData(event);
            if (message != null) {

                TextMessage msg = new TextMessage(participant, message);
                msg.setProperty(Tools.PROP_SESSION_ID, getSessionId());
                session.sendPeerToPeer(msg);
            }
        }
    }

    /**
     * 
     * @param event
     * @throws CollaborationException
     * @see com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession#sendEvent(com.raytheon.uf.viz.collaboration.comm.identity.event.IDisplayEvent)
     */
    @Override
    public void sendEvent(IDisplayEvent event) throws CollaborationException {
        if (event != null) {
            String message = Tools.marshallData(event);
            if (message != null) {
                sendTextMessage(message);
            }
        }
    }

    /**
     * @param renderable
     * @throws CollaborationException
     * @see com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession#sendRenderableObject(com.raytheon.uf.viz.collaboration.comm.identity.event.IRenderable)
     */
    @Override
    public void sendRenderableObject(IRenderable renderable)
            throws CollaborationException {
        if (renderable != null) {
            String message = Tools.marshallData(renderable);
            if (message != null) {
                sendTextMessage(message);
            }
        }
    }

    @Override
    public void sendObjectToVenue(Object obj) throws CollaborationException {
        if (obj != null) {
            String message = Tools.marshallData(obj);
            if (message != null) {
                sendTextMessage(message);
            }
        }
    }

    @Override
    public void sendObjectToPeer(
            com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID participant,
            Object obj) throws CollaborationException {
        PeerToPeerChat session = getP2PSession();
        if (session != null) {
            String message = Tools.marshallData(obj);
            if (message != null) {

                TextMessage msg = new TextMessage(participant, message);
                msg.setProperty(Tools.PROP_SESSION_ID, getSessionId());

                session.sendPeerToPeer(msg);
            }
        }
    }

    /**
     * Get the identification of the user who is the DataProvider.
     * 
     * @return The DataProvider user identification.
     * @see com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession#getCurrentDataProvider()
     */
    @Override
    public IVenueParticipant getCurrentDataProvider() {
        return dataProvider;
    }

    /**
     * Get the identification of the user who is the Session Leader.
     * 
     * @return The Session Leader user identification.
     * @see com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession#getCurrentSessionLeader()
     */
    @Override
    public IVenueParticipant getCurrentSessionLeader() {
        return sessionLeader;
    }

    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession#hasRole(com.raytheon.uf.viz.collaboration.comm.identity.user.ParticipantRole)
     */
    @Override
    public boolean hasRole(ParticipantRole role) {
        boolean result = true;
        if (role.equals(ParticipantRole.DATA_PROVIDER)
                && !this.getUserID().equals(this.getCurrentDataProvider())) {
            result = false;
        } else if (role.equals(ParticipantRole.SESSION_LEADER)
                && !this.getUserID().equals(this.getCurrentSessionLeader())) {
            result = false;
        }
        System.out
                .println(this.getUserID() + " hasRole " + role + " " + result);
        return result;
    }

    // ***************************
    // ISharedDisplaySession
    // ***************************

    /**
     * @param message
     *            A message to send.
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession#sendTextMessage(java.lang.String)
     */
    @Override
    public void sendTextMessage(String message) throws CollaborationException {
        // Assume success
        if ((venueContainer != null) && (message != null)) {
            IChatRoomMessageSender sender = venueContainer
                    .getChatRoomMessageSender();
            try {
                if (message.startsWith(SEND_CMD)) {
                    sender.sendMessage(message);
                } else {
                    sender.sendMessage(SEND_TXT + message);
                }
            } catch (ECFException e) {
                throw new CollaborationException("Error sending text messge", e);
            }
        }
    }

    @Override
    public void setCurrentSessionLeader(IVenueParticipant id) {
        sessionLeader = id;
    }

    @Override
    public void setCurrentDataProvider(IVenueParticipant id) {
        dataProvider = id;
    }

    protected void setUserId(IVenueParticipant id) {
        this.userID = id;
    }

    /**
     * 
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession#joinVenue(java.lang.String)
     */
    int joinVenue(String venueName) {
        int errorStatus = -1;
        try {
            // Create chat room container from manager
            venueManager = getConnectionPresenceAdapter().getChatRoomManager();
            if (venueManager != null) {
                venueInfo = venueManager.getChatRoomInfo(venueName);
                subject = venueInfo.getDescription();
                if (venueInfo != null) {
                    errorStatus = completeVenueConnection(venueInfo);
                } else {
                    // Could not join venue.
                }
            } else {

            }
        } catch (Exception e) {
            System.out.println(String.format("joinVenue(%s)", venueName));
            e.printStackTrace();
        }
        return errorStatus;
    }

    /**
     * 
     * @param venueName
     * @throws Exception
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession#createVenue(java.lang.String,
     *      java.lang.String)
     */
    int createVenue(String venueName, String subject) {
        int errorStatus = -1;
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
                    errorStatus = completeVenueConnection(venueInfo);
                } else {
                    errorStatus = Errors.VENUE_EXISTS;
                }
            } else {
                errorStatus = Errors.CANNOT_CONNECT; // is this correct?
            }
        } catch (Exception e) {
            // TODO this is bad, we assume it's a bad venue name but it might
            // not be
            // and we'd give a poor error message
            System.out.println(String.format("createVenue(%s)", venueName));
            e.printStackTrace();
            errorStatus = Errors.BAD_NAME;
        }
        // TODO :
        // sendSubscription(subject);
        return errorStatus;
    }

    private void sendSubscription(final String name) {

        Runnable r = new Runnable() {
            @Override
            public void run() {
                try {
                    System.out.println("Sending subscribe message.");

                    boolean remove = "remove".equals(name);

                    String[] groups = { "group2", };

                    // String [] groups = new String [];

                    IRosterManager r = getSessionManager().getRosterManager();
                    if (remove) {
                        IChatID id = new RosterId("pkorman",
                                "awipscm.omaha.us.ray.com", null);
                        r.sendRosterRemove(id);
                    } else {

                        r.sendRosterAdd("pkorman@awipscm.omaha.us.ray.com",
                                "Paul", groups);
                    }
                    if ("subscribe".equals(name)) {
                        IPresenceSender sender = getConnectionPresenceAdapter()
                                .getRosterManager().getPresenceSender();
                        org.eclipse.ecf.presence.IPresence presence = new org.eclipse.ecf.presence.Presence(
                                org.eclipse.ecf.presence.IPresence.Type.SUBSCRIBE);

                        sender.sendPresenceUpdate(
                                createID("pkorman@awipscm.omaha.us.ray.com"),
                                presence);
                    }

                    System.out.println("Subscribe message sent.");

                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        };
        Thread t = new Thread(r);
        t.start();
        System.out.println("The subscribe test has started.");
    }

    /**
     * 
     * @return
     */
    private int completeVenueConnection(IChatRoomInfo venueInfo) {
        int errorStatus = Errors.NO_ERROR;

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

                        IVenueParticipant vp = new VenueParticipant();
                        String fullName = fromID.getName();
                        vp.setName(Tools.parseName(fullName));
                        vp.setHost(Tools.parseHost(fullName));
                        vp.setResource(Tools.parseResource(fullName));
                        IPresence p = Presence.convertPresence(presence);
                        IVenueParticipantEvent event = null;
                        if (IPresence.Type.AVAILABLE.equals(p.getType())) {
                            event = new VenueParticipantEvent(vp, p,
                                    ParticipantEventType.ARRIVED);
                            getEventPublisher().post(event);
                        } else if (IPresence.Type.UNAVAILABLE.equals(p
                                .getType())) {
                            event = new VenueParticipantEvent(vp, p,
                                    ParticipantEventType.DEPARTED);
                            getEventPublisher().post(event);
                        }
                        event = new VenueParticipantEvent(vp, p,
                                ParticipantEventType.PRESENCE_UPDATED);
                        getEventPublisher().post(event);
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

                                if (accept(m)) {
                                    distributeMessage(convertMessage(m));
                                }
                            }
                        }
                    };
                    venueContainer.addMessageListener(intListener);

                }
            } catch (Exception e) {
                errorStatus = -1;
            }
        }
        return errorStatus;
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
            String host = Tools.parseHost(from.getName());

            String account = getSessionManager().getAccount();
            String aName = Tools.parseName(account);
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
                if (body.startsWith(SEND_TXT)) {
                    body = body.substring(SEND_TXT.length());
                    message.setBody(body);

                    TextMessage msg = new TextMessage(message.getTo(),
                            message.getBody());
                    msg.setFrom(message.getFrom());

                    getEventPublisher().post(msg);
                } else if (body.startsWith(SEND_CMD)) {
                    Object o = null;
                    try {
                        o = Tools.unMarshallData(body);
                        if (o != null) {
                            getEventPublisher().post(o);
                        }
                    } catch (CollaborationException ce) {
                        // TODO : more robust!!
                        ce.printStackTrace();
                    }
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

            IQualifiedID id = new RosterId(cID.getUsername(), rID.getHostname());
            message.setFrom(id);
        }
        return message;
    }
}
