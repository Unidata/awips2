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

import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumSet;
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
import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.IMessage;
import com.raytheon.uf.viz.collaboration.comm.identity.IPresence;
import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IDisplayEvent;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IInitData;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IRenderable;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IVenueParticipantEvent;
import com.raytheon.uf.viz.collaboration.comm.identity.event.ParticipantEventType;
import com.raytheon.uf.viz.collaboration.comm.identity.info.IVenue;
import com.raytheon.uf.viz.collaboration.comm.identity.listener.IInvitation;
import com.raytheon.uf.viz.collaboration.comm.identity.listener.IMessageFilter;
import com.raytheon.uf.viz.collaboration.comm.identity.listener.IMessageListener;
import com.raytheon.uf.viz.collaboration.comm.identity.listener.IPresenceListener;
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
import com.raytheon.uf.viz.collaboration.comm.provider.user.VenueParticipant;
import com.raytheon.uf.viz.collaboration.comm.provider.user.VenueUserId;

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

    /**
     * 
     * TODO Add Description
     * 
     * <pre>
     * 
     * SOFTWARE HISTORY
     * 
     * Date         Ticket#    Engineer    Description
     * ------------ ---------- ----------- --------------------------
     * Feb 27, 2012            jkorman     Initial creation
     * 
     * </pre>
     * 
     * @author jkorman
     * @version 1.0
     */
    private static class InternalListener {

        private IMessageListener messageListener;

        private IPresenceListener presenceListener;

        private IMessageFilter filter;

        /**
         * 
         * @param listener
         * @param filter
         */
        public InternalListener(IMessageListener listener, IMessageFilter filter) {
            messageListener = listener;
            this.filter = filter;

        }

        /**
         * 
         * @param listener
         * @param filter
         */
        public InternalListener(IPresenceListener listener,
                IMessageFilter filter) {
            presenceListener = listener;
            this.filter = filter;
        }

        /**
         * 
         * @param message
         */
        public void processMessage(IMessage message) {
            messageListener.processMessage(message);
        }

        /**
         * 
         * @param presence
         */
        public void processPresence(IPresence presence) {
            presenceListener.notifyPresence(presence);
        }

        /**
         * 
         * @param message
         * @return
         */
        public boolean filter(IMessage message) {
            return filter.filter(message);
        }
    }

    private static final String SEND_CMD = "[[COMMAND";

    private static final String SEND_TXT = "[[TEXT]]";

    private IChatRoomManager venueManager = null;

    private IChatRoomInfo venueInfo = null;

    private IChatRoomContainer venueContainer = null;

    private List<InternalListener> collaborationListeners = null;

    private List<InternalListener> presenceListeners = null;

    private IIMMessageListener intListener = null;

    private IChatRoomParticipantListener participantListener = null;

    private IQualifiedID userID = null;

    private IChatID sessionLeader = null;

    private IChatID sessionDataProvider = null;

    private EnumSet<ParticipantRole> roles = EnumSet
            .noneOf(ParticipantRole.class);

    /**
     * 
     * @param container
     * @param eventBus
     */
    VenueSession(IContainer container, EventBus externalBus,
            SessionManager manager) {
        super(container, externalBus, manager);
        try {
            setup();
        } catch (ECFException e) {

        } finally {
            initListeners();
        }

        Runnable r = new Runnable() {
            @Override
            public void run() {
                for (int i = 0; i < 10; i++) {

                    IRenderable r = new TestObject("Test1");
                    ((TestObject) r).setValue(i);

                    try {
                        System.out.println("Sending renderable " + i);
                        sendRenderableObject(r);
                        if (i == 5) {
                            sendTextMessage(Tools
                                    .marshallData("This is a text message as a String"));
                        }

                    } catch (CollaborationException ce) {
                        ce.printStackTrace();
                    }
                    try {
                        Thread.sleep(10000);
                    } catch (InterruptedException ie) {
                    }
                }
            }
        };
        Thread t = new Thread(r);
        t.start();

        registerEventHandler(this);
    }

    @Subscribe
    public void handle(IRenderable renderable) {
        System.out.println("IRenderable " + renderable.getClass().getName()
                + " was received");
    }

    @Subscribe
    public void handle(IDisplayEvent event) {
        System.out.println("IDisplayEvent " + event.getClass().getName()
                + " was received");
    }

    @Subscribe
    public void handle(String string) {
        System.out.println("String \"" + string + "\" was received");
    }

    @Subscribe
    public void handle(IVenueParticipantEvent event) {
        System.out.println("IVenueParticipantEvent " + event.getEventType()
                + " was received");
    }

    /**
     * 
     * @throws ECFException
     */
    void setup() throws ECFException {
        super.setup();
    }

    /**
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
        }
        if (participantListener != null) {
            venueContainer
                    .removeChatRoomParticipantListener(participantListener);
        }

        collaborationListeners.clear();
        collaborationListeners = null;

        presenceListeners.clear();
        presenceListeners = null;

        venueContainer.disconnect();
        venueContainer = null;

        venueManager = null;
        venueInfo = null;

        super.close();
    }

    /**
     * 
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession#joinVenue(java.lang.String)
     */
    public int joinVenue(String venueName) {
        int errorStatus = -1;
        try {
            // Create chat room container from manager
            venueManager = getConnectionPresenceAdapter().getChatRoomManager();
            if (venueManager != null) {
                venueInfo = venueManager.getChatRoomInfo(venueName);
                if (venueInfo != null) {
                    errorStatus = completeVenueConnection(venueInfo);

                    roles.add(ParticipantRole.PARTICIPANT);
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
    public int createVenue(String venueName, String subject) {
        int errorStatus = -1;
        try {
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

                    roles.add(ParticipantRole.DATA_PROVIDER);
                    roles.add(ParticipantRole.SESSION_LEADER);
                } else {
                    errorStatus = Errors.VENUE_EXISTS;
                }
            } else {

            }
        } catch (Exception e) {
            System.out.println(String.format("createVenue(%s)", venueName));
            e.printStackTrace();
        }
        return errorStatus;
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
                venueContainer.connect(venueInfo.getRoomID(), null);
                if (venueContainer.getConnectedID() != null) {

                    intListener = new IIMMessageListener() {
                        public void handleMessageEvent(
                                IIMMessageEvent messageEvent) {
                            if (messageEvent instanceof IChatRoomMessageEvent) {
                                IChatRoomMessage m = ((IChatRoomMessageEvent) messageEvent)
                                        .getChatRoomMessage();

                                distributeMessage(convertMessage(m));
                            }
                        }
                    };
                    venueContainer.addMessageListener(intListener);

                    IChatRoomParticipantListener pListener = new IChatRoomParticipantListener() {
                        @Override
                        public void handleArrived(IUser participant) {
                            IVenueParticipant p = new VenueParticipant(
                                    participant.getName(),
                                    participant.getNickname());

                            IVenueParticipantEvent event = new VenueParticipantEvent(
                                    p, ParticipantEventType.ARRIVED);

                            getEventPublisher().post(event);
                        }

                        @Override
                        public void handleUpdated(IUser participant) {
                            IVenueParticipant p = new VenueParticipant(
                                    participant.getName(),
                                    participant.getNickname());

                            IVenueParticipantEvent event = new VenueParticipantEvent(
                                    p, ParticipantEventType.UPDATED);
                            getEventPublisher().post(event);
                        }

                        @Override
                        public void handleDeparted(IUser participant) {
                            IVenueParticipant p = new VenueParticipant(
                                    participant.getName(),
                                    participant.getNickname());

                            IVenueParticipantEvent event = new VenueParticipantEvent(
                                    p, ParticipantEventType.DEPARTED);
                            getEventPublisher().post(event);
                        }

                        @Override
                        public void handlePresenceUpdated(ID fromID,
                                org.eclipse.ecf.presence.IPresence presence) {

                            fromID.getName();
                            IVenueParticipant vp = new VenueParticipant();
                            vp.setName(fromID.getName());

                            IPresence p = Presence.convertPresence(presence);

                            IVenueParticipantEvent event = new VenueParticipantEvent(
                                    vp, p,
                                    ParticipantEventType.PRESENCE_UPDATED);
                            getEventPublisher().post(event);
                        }
                    };
                    venueContainer.addChatRoomParticipantListener(pListener);
                }
            } catch (Exception e) {
                errorStatus = -1;
            }
        }
        return errorStatus;
    }

    /**
     * 
     * @return The information about this venue. May return a null reference if
     *         the venue is not connected.
     */
    public IVenue getVenue() {
        IVenue venue = null;
        if (isConnected() && (venueContainer != null)) {
            venue = new Venue();
            ID[] ids = venueContainer.getChatRoomParticipants();
            for (ID id : ids) {
                IVenueParticipant participant = new VenueParticipant();
                participant.setName(id.getName());
                venue.addParticipant(participant);
            }
            venue.setInfo(InfoAdapter.createVenueInfo(venueInfo));
        } else {

        }
        return venue;
    }

    /**
     * Send an invitation from this venue to another user.
     * 
     * @param invitation
     *            An invitation
     * @return
     */
    public int sendInvitation(IInvitation invitation) {
        int status = Errors.NO_ERROR;
        IChatRoomInvitationSender sender = getConnectionPresenceAdapter()
                .getChatRoomManager().getInvitationSender();
        if (sender != null) {
            ID roomId = getConnectionPresenceAdapter().getChatRoomManager()
                    .getChatRoomInfo(invitation.getRoomId()).getConnectedID();

            // *******************
            // ** TODO : The host part of this need to defined
            ID userId = IDFactory.getDefault().createID(
                    getConnectionNamespace(),
                    invitation.getFrom() + "@awipscm.omaha.us.ray.com");
            // *******************

            try {
                sender.sendInvitation(roomId, userId, invitation.getSubject(),
                        invitation.getBody());
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
    public int sendInvitation(String room, String id, String subject,
            String body) {
        // Assume success
        int status = Errors.NO_ERROR;
        IChatRoomInvitationSender sender = getConnectionPresenceAdapter()
                .getChatRoomManager().getInvitationSender();
        if (sender != null) {

            ID roomId = getConnectionPresenceAdapter().getChatRoomManager()
                    .getChatRoomInfo(room).getConnectedID();
            ID userId = IDFactory.getDefault().createID(
                    getConnectionNamespace(), id + "@awipscm.omaha.us.ray.com");

            try {
                sender.sendInvitation(roomId, userId, subject, body);
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
    public int sendInvitation(String room, List<String> ids, String subject,
            String body) {
        // Assume success
        int status = Errors.NO_ERROR;
        if (ids != null) {
            for (String id : ids) {
                sendInvitation(room, id, subject, body);
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

    @Override
    public void sendInitData(
            com.raytheon.uf.viz.collaboration.comm.identity.user.IChatID participant,
            IInitData initData) throws CollaborationException {

        PeerToPeerChat session = null;
        session = getP2PSession();
        if (session != null) {
            String message = Tools.marshallData(initData);
            if (message != null) {
                session.sendPeerToPeer(participant.getFQName(), message);
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

    /**
     * Get the identification of the user who is the DataProvider.
     * 
     * @return The DataProvider user identification.
     * @see com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession#getCurrentDataProvider()
     */
    @Override
    public IChatID getCurrentDataProvider() {
        return sessionDataProvider;
    }

    /**
     * Get the identification of the user who is the Session Leader.
     * 
     * @return The Session Leader user identification.
     * @see com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession#getCurrentSessionLeader()
     */
    @Override
    public IChatID getCurrentSessionLeader() {
        return sessionLeader;
    }

    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession#hasRole(com.raytheon.uf.viz.collaboration.comm.identity.user.ParticipantRole)
     */
    @Override
    public boolean hasRole(ParticipantRole role) {
        return roles.contains(role);
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
                throw new CollaborationException("Error sending text messge");
            }
        }
    }

    /**
     * Set up the various message listener lists. Ensures that all listener
     * collections are not null prior to use.
     */
    private void initListeners() {
        presenceListeners = Collections
                .synchronizedList(new ArrayList<InternalListener>());
        collaborationListeners = Collections
                .synchronizedList(new ArrayList<InternalListener>());
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
     * 
     * @param message
     */
    private void firePresenceListeners(IMessage message) {
        synchronized (presenceListeners) {
            if (message instanceof IPresence) {
                IPresence presence = (IPresence) message;
                for (InternalListener listener : presenceListeners) {
                    if (listener.filter(message)) {
                        listener.processPresence(presence);
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

            IQualifiedID id = new VenueUserId(cID.getUsername(),
                    rID.getHostname());
            message.setFrom(id);
        }
        return message;
    }

}
