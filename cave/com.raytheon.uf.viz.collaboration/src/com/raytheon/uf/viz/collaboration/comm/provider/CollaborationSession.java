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
package com.raytheon.uf.viz.collaboration.comm.provider;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.ecf.core.ContainerConnectException;
import org.eclipse.ecf.core.ContainerFactory;
import org.eclipse.ecf.core.IContainer;
import org.eclipse.ecf.core.identity.ID;
import org.eclipse.ecf.core.identity.IDFactory;
import org.eclipse.ecf.core.identity.Namespace;
import org.eclipse.ecf.core.security.ConnectContextFactory;
import org.eclipse.ecf.core.user.IUser;
import org.eclipse.ecf.core.util.ECFException;
import org.eclipse.ecf.presence.IIMMessageEvent;
import org.eclipse.ecf.presence.IIMMessageListener;
import org.eclipse.ecf.presence.IPresenceContainerAdapter;
import org.eclipse.ecf.presence.IPresenceSender;
import org.eclipse.ecf.presence.chatroom.IChatRoomContainer;
import org.eclipse.ecf.presence.chatroom.IChatRoomInfo;
import org.eclipse.ecf.presence.chatroom.IChatRoomInvitationSender;
import org.eclipse.ecf.presence.chatroom.IChatRoomManager;
import org.eclipse.ecf.presence.chatroom.IChatRoomMessage;
import org.eclipse.ecf.presence.chatroom.IChatRoomMessageEvent;
import org.eclipse.ecf.presence.chatroom.IChatRoomMessageSender;
import org.eclipse.ecf.presence.chatroom.IChatRoomParticipantListener;
import org.eclipse.ecf.presence.im.IChatID;
import org.eclipse.ecf.presence.im.IChatMessage;
import org.eclipse.ecf.presence.im.IChatMessageSender;
import org.eclipse.ecf.provider.xmpp.identity.XMPPRoomID;

import com.raytheon.uf.viz.collaboration.comm.SessionManager;
import com.raytheon.uf.viz.collaboration.comm.identity.IMessage;
import com.raytheon.uf.viz.collaboration.comm.identity.IPresence;
import com.raytheon.uf.viz.collaboration.comm.identity.IPropertied.Property;
import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.identity.info.IVenue;
import com.raytheon.uf.viz.collaboration.comm.identity.listener.IMessageFilter;
import com.raytheon.uf.viz.collaboration.comm.identity.listener.IMessageListener;
import com.raytheon.uf.viz.collaboration.comm.identity.listener.IPresenceListener;
import com.raytheon.uf.viz.collaboration.comm.identity.listener.IVenueParticipantListener;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterManager;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IVenueParticipant;
import com.raytheon.uf.viz.collaboration.comm.provider.info.InfoAdapter;
import com.raytheon.uf.viz.collaboration.comm.provider.info.Venue;
import com.raytheon.uf.viz.collaboration.comm.provider.user.VenueParticipant;
import com.raytheon.uf.viz.collaboration.comm.provider.user.VenueUserId;

/**
 * 
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
 */

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 5, 2012            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */
/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 5, 2012            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */
/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 7, 2012            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */
/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 7, 2012            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */
public class CollaborationSession implements IVenueSession {

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
        public InternalListener(IPresenceListener listener, IMessageFilter filter) {
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
    
    private IContainer container = null;

    private IPresenceContainerAdapter presence = null;
    
    private IChatMessageSender chatSender = null;
    
    private Namespace namespace = null;

    private IChatRoomManager venueManager = null;
    private IChatRoomContainer venueContainer = null;
    private IChatRoomInfo venueInfo = null;
    
    private List<InternalListener> messageListeners = null;
    
    private List<IVenueParticipantListener> venueParticipantListeners = null;
    
    private List<InternalListener> collaborationListeners = null;
    
    private List<InternalListener> presenceListeners = null;

    private IIMMessageListener intListener = null;
    
    private IQualifiedID receiver = null;
    
    private IQualifiedID userID = null;
    
    /**
     * 
     */
    public CollaborationSession(IContainer container) {
        this.container = container;
        initListeners();
        try {
            setup();
        } catch (ECFException e) {
        }
    }

    /**
     * 
     * @param userName
     * @param password
     * @see com.raytheon.uf.viz.collaboration.comm.identity.ISession#connect(java.lang.String, java.lang.String)
     */
    @Override
    public void connect(String userName, String password) {

        if(!isConnected()) {
            ID targetID = IDFactory.getDefault().createID(namespace, userName);
            // Now connect
            try {
                container.connect(targetID, ConnectContextFactory.createPasswordConnectContext(password));
                
                System.out.println("Container connected as " + container.getConnectedID());
                
            } catch (ContainerConnectException e) {
                System.out.println("Error attempting to connect");
                e.printStackTrace();
            }
        }
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
     * 
     * @see com.raytheon.uf.viz.collaboration.comm.identity.ISession#isConnected()
     */
    @Override
    public boolean isConnected() {
        boolean connected = false;
        if(container != null) {
            connected = (container.getConnectedID() != null);
        }
        return connected;
    }
    
    /**
     * 
     * @see com.raytheon.uf.viz.collaboration.comm.identity.ISession#close()
     */
    @Override
    public void close() {
        if(container != null) {
            // Ensure the listeners are cleared first.
            // unhook the internal listener first.
            if(intListener != null) {
                venueContainer.removeMessageListener(intListener);
            }

            messageListeners.clear();
            messageListeners = null;

            collaborationListeners.clear();
            collaborationListeners = null;

            presenceListeners.clear();
            presenceListeners = null;
            
            // Now dispose of the comm container.
            container.dispose();
            container = null;
        }
    }

    /**
     * 
     * @throws ECFException
     */
    private void setup() throws ECFException {
        
        if (container == null) {
            container = ContainerFactory.getDefault().createContainer(SessionManager.PROVIDER);
        }
        if(container != null) {
            namespace = container.getConnectNamespace();
            
            presence = (IPresenceContainerAdapter) container
            .getAdapter(IPresenceContainerAdapter.class);

            chatSender = presence.getChatManager().getChatMessageSender();
        }
    }
    
    /**
     * 
     * @param name
     * @return
     */
    private ID createID(String name) {
        return IDFactory.getDefault().createID(container.getConnectNamespace(), name);
    }

    /**
     * 
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession#joinVenue(java.lang.String)
     */
    @Override
    public int joinVenue(String venueName) {
        int errorStatus = -1;
        try {
            // Create chat room container from manager
            venueManager = presence.getChatRoomManager();
            venueInfo = venueManager.getChatRoomInfo(venueName);
            if(venueInfo != null) {
                errorStatus = completeVenueConnection(venueInfo);
            } else {
                // Could not join venue.
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
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession#createVenue(java.lang.String, java.lang.String)
     */
    @Override
    public int createVenue(String venueName, String subject) {
        int errorStatus = -1;
        try {
            // Create chat room container from manager
            venueManager = presence.getChatRoomManager();
            venueInfo = venueManager.getChatRoomInfo(venueName);
            if(venueInfo == null) {
                Map<String, String> props = null;
                if(subject != null) {
                    props = new HashMap<String,String>();
                    props.put(Tools.VENUE_SUBJECT_PROP, subject);
                }
                venueInfo = venueManager.createChatRoom(venueName, props);
                errorStatus = completeVenueConnection(venueInfo);
            } else {
                // The venue already exists.
                errorStatus = -2;
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
        int errorStatus = 0;

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

                                distributeMessage(createMessage(m));
                            }
                        }
                    };
                    venueContainer.addMessageListener(intListener);

                    IChatRoomParticipantListener pListener = new IChatRoomParticipantListener() {
                        @Override
                        public void handleArrived(IUser participant) {
                            IVenueParticipant p = new VenueParticipant(participant.getName(), participant.getNickname());
                            for(IVenueParticipantListener listener : venueParticipantListeners) {
                                listener.handleArrived(p);
                            }
                        }

                        @Override
                        public void handleUpdated(IUser participant) {
                            IVenueParticipant p = new VenueParticipant(participant.getName(), participant.getNickname());
                            for(IVenueParticipantListener listener : venueParticipantListeners) {
                                listener.handleUpdated(p);
                            }
                        }

                        @Override
                        public void handleDeparted(IUser participant) {
                            IVenueParticipant p = new VenueParticipant(participant.getName(), participant.getNickname());
                            for(IVenueParticipantListener listener : venueParticipantListeners) {
                                listener.handleDeparted(p);
                            }
                        }

                        @Override
                        public void handlePresenceUpdated(
                                ID fromID,
                                org.eclipse.ecf.presence.IPresence presence) {

                            fromID.getName();
                            IVenueParticipant vp = new VenueParticipant();
                            vp.setName(fromID.getName());
                            
                            IPresence p = Presence.convertPresence(presence);
                            for(IVenueParticipantListener listener : venueParticipantListeners) {
                                listener.handlePresenceUpdated(vp, p);
                            }
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
     * @return The information about this venue. May return a null reference
     * if the venue is not connected.
     */
    public IVenue getVenue() {
        IVenue venue = null;
        if(isConnected()) {
            venue = new Venue();
            ID [] ids = venueContainer.getChatRoomParticipants();
            for(ID id : ids) {
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
     * 
     */
    @Override
    public int sendPresence(IPresence userPresence) {
        // Assume success
        int status = 0;
        
        IPresenceSender sender = presence.getRosterManager().getPresenceSender();
        try {
            sender.sendPresenceUpdate(null, Presence.convertPresence(userPresence));
        } catch (ECFException e) {
            status = -1;
        }
        return status;
    }

    /**
     * @return Get the roster manager for this session.
     */
    @Override
    public IRosterManager getRosterManager() {
        return null;
    }

    /**
     * 
     */
    @Override
    public int sendTextMessage(IMessage message) {
        // Assume success
        int status = 0;
        if(chatSender != null) {
            ID toID = createID(message.getTo().getName());
            String subject = message.getSubject();
            String body = message.getBody();
            Collection<Property> properties = message.getProperties();
            Map<String, String> props = null;
            if((properties != null) && (properties.size() > 0)) {
                props = new HashMap<String, String>();
                for(Property p : properties) {
                    props.put(p.getKey(),p.getValue());
                }
            }
            try {
                chatSender.sendChatMessage(toID, null, IChatMessage.Type.CHAT, subject, body, props);
            } catch (ECFException e) {
                System.out.println("Error sending message");
                e.printStackTrace();
            }
            fireMessageListeners(message);
        }
        return status;
    }

    /**
     *
     * @param to
     * @param message
     */
    @Override
    public int sendTextMessage(String to, String message) {
        // Assume success
        int status = 0;
        ID toID = createID(to);
        try {
            chatSender.sendChatMessage(toID, message);

            IMessage msg = new TextMessage(receiver, message);
            status = sendTextMessage(msg);
        
        } catch (ECFException e) {
            System.out.println("Error sending message");
            e.printStackTrace();
        }
        
        return status;
    }
    
    /**
     * 
     * @param message
     */
    @Override
    public int sendTextMessage(String message) {
        IMessage msg = new TextMessage(receiver, message);
        return sendTextMessage(msg);
    }
    
    /**
     * @param message A message to send.
     */
    public int sendMessageToVenue(String message) {
        // Assume success
        int status = 0;
        if(venueContainer != null) {
            IChatRoomMessageSender sender = venueContainer.getChatRoomMessageSender();
            try {
                sender.sendMessage(message);
            } catch (ECFException e) {
                e.printStackTrace();
            }
        }
        return status;
    }
    
    /**
     * 
     * @param message
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession#sendCollaborationMessage(com.raytheon.uf.viz.collaboration.comm.identity.IMessage)
     */
    @Override
    public int sendCollaborationMessage(IMessage message) {
        // Assume success
        int status = 0;
        // for now we're sending everything via regular messages.
        return sendMessageToVenue(message.getBody()); 
    }

    /**
     * 
     * @param message
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession#sendCollaborationMessage(java.lang.String)
     */
    @Override
    public int sendCollaborationMessage(String message) {
        IMessage msg = new CollaborationMessage(receiver, message);
        return sendCollaborationMessage(msg);
    }

    /**
     * Send an invitation from this venue to another user.
     * @param room The target venue for this invitation.
     * @param id The target user for this invitation.
     * @param subject The intended subject of the venue conversation.
     * @param body Any text that the user may wish to include.
     * @return
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession#sendInvitation(java.lang.String, java.lang.String, java.lang.String, java.lang.String)
     */
    @Override
    public int sendInvitation(String room, String id, String subject, String body) {
        // Assume success
        int status = 0;
        IChatRoomInvitationSender sender = presence.getChatRoomManager().getInvitationSender();
        if(sender != null) {
            
            ID roomId = presence.getChatRoomManager().getChatRoomInfo(room).getConnectedID();
            ID userId = IDFactory.getDefault().createID(namespace, id + "@awipscm.omaha.us.ray.com");
            
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
     * @param room The target venue for this invitation.
     * @param id The target user for this invitation.
     * @param subject The intended subject of the venue conversation.
     * @param body Any text that the user may wish to include.
     * @return
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession#sendInvitation(java.lang.String, java.lang.String, java.lang.String, java.lang.String)
     */
    @Override
    public int sendInvitation(String room, List<String> ids, String subject, String body) {
        // Assume success
        int status = 0;
        if(ids != null) {
            for(String id : ids) {
                sendInvitation(room, id, subject, body);
            }
        } else {
            status = -1;
        }
        return status;
    }
    
    
    
    @Override
    public IMessageListener addMessageListener(IMessageListener listener, IMessageFilter filter) {
        InternalListener messageListener = new InternalListener(listener, filter);
        messageListeners.add(messageListener);
        return listener;
    }

    /**
     * 
     */
    @Override
    public Collection<IMessageListener> getMessageListeners() {
        Collection<IMessageListener> listeners = new ArrayList<IMessageListener>();
        synchronized(messageListeners) {
            for(InternalListener intListener : messageListeners) {
                listeners.add(intListener.messageListener);
            }
        }
        return listeners;
    }

    /**
     * 
     */
    @Override
    public IMessageListener removeMessageListener(IMessageListener listener) {
        IMessageListener removed = null;
        if(messageListeners.remove(listener)) {
            removed = listener;
        }
        return removed;
    }

    /**
     * 
     */
    @Override
    public IVenueParticipantListener addVenueParticipantListener(IVenueParticipantListener listener) {
        if(listener != null) {
            venueParticipantListeners.add(listener);
        } else {
            // TODO : Need some error condition here?
        }
        return listener;
    }

    /**
     * 
     */
    @Override
    public Collection<IVenueParticipantListener> getVenueParticipantListeners() {
        Collection<IVenueParticipantListener> listeners = new ArrayList<IVenueParticipantListener>();
        synchronized(collaborationListeners) {
            for(IVenueParticipantListener listener : venueParticipantListeners) {
                listeners.add(listener);
            }
        }
        return listeners;
    }

    /**
     * 
     */
    @Override
    public IVenueParticipantListener removeVenueParticipantListener(IVenueParticipantListener listener) {
        IVenueParticipantListener removed = null;
        if(venueParticipantListeners.remove(listener)) {
            removed = listener;
        }
        return removed;
    }

    /**
     * 
     */
    @Override
    public IMessageListener addCollaborationListener(IMessageListener listener, IMessageFilter filter) {
        InternalListener messageListener = new InternalListener(listener, filter);
        collaborationListeners.add(messageListener);
        return listener;
    }

    /**
     * 
     */
    @Override
    public Collection<IMessageListener> getCollaborationListeners() {
        Collection<IMessageListener> listeners = new ArrayList<IMessageListener>();
        synchronized(collaborationListeners) {
            for(InternalListener intListener : collaborationListeners) {
                listeners.add(intListener.messageListener);
            }
        }
        return listeners;
    }

    /**
     * 
     */
    @Override
    public IMessageListener removeCollaborationListener(IMessageListener listener) {
        IMessageListener removed = null;
        if(collaborationListeners.remove(listener)) {
            removed = listener;
        }
        return removed;
    }

    /**
     * 
     */
    @Override
    public IPresenceListener addPresenceListener(IPresenceListener listener, IMessageFilter filter) {
        InternalListener presenceListener = new InternalListener(listener, filter);
        presenceListeners.add(presenceListener);
        return listener;
    }

    /**
     * 
     */
    @Override
    public Collection<IPresenceListener> getPresenceListeners() {
        Collection<IPresenceListener> listeners = new ArrayList<IPresenceListener>();
        synchronized(presenceListeners) {
            for(InternalListener intListener : presenceListeners) {
                listeners.add(intListener.presenceListener);
            }
        }
        return listeners;
    }

    @Override
    public IPresenceListener removePresenceListener(IPresenceListener listener) {
        IPresenceListener removed = null;
        if(presenceListeners.remove(listener)) {
            removed = listener;
        }
        return removed;
    }

    /**
     * Set up the various message listener lists.
     */
    private void initListeners() {
        messageListeners = Collections.synchronizedList(new ArrayList<InternalListener>());
        venueParticipantListeners = Collections.synchronizedList(new ArrayList<IVenueParticipantListener>());
        presenceListeners = Collections.synchronizedList(new ArrayList<InternalListener>());
        collaborationListeners = Collections.synchronizedList(new ArrayList<InternalListener>());
    }

    /**
     * 
     * @param message
     */
    private void distributeMessage(IMessage message) {
        if(message != null) {
            fireMessageListeners(message);

//            if(IMessage.MessageType.CHAT.equals(message.getMessageType())) {
//                fireMessageListeners(message);
//            } else if (IMessage.MessageType.COLLABORATION.equals(message.getMessageType())) {
//                fireCollaborationListeners(message);
//            }
        }
    }

    /**
     * 
     * @param message
     */
    private void fireMessageListeners(IMessage message) {
        synchronized(messageListeners) {
            for(InternalListener listener : messageListeners) {
                if(listener.filter(message)) {
                    listener.processMessage(message);
                }
            }
        }
    }

    /**
     * 
     * @param message
     */
    private void fireCollaborationListeners(IMessage message) {
        synchronized(collaborationListeners) {
            for(InternalListener listener : collaborationListeners) {
                if(listener.filter(message)) {
                    listener.processMessage(message);
                }
            }
        }
    }
    
    /**
     * 
     * @param message
     */
    private void firePresenceListeners(IMessage message) {
        synchronized(presenceListeners) {
            if(message instanceof IPresence) {
                IPresence presence = (IPresence) message;
                for(InternalListener listener : presenceListeners) {
                    if(listener.filter(message)) {
                        listener.processPresence(presence);
                    }
                }
            }
        }
    }

    /**
     * 
     * @param msg
     * @return
     */
    private IMessage createMessage(IChatMessage msg) {
        IMessage message = null;
        Map props = msg.getProperties();
        if(props != null) {
            Map<String, String> p = new HashMap<String, String>();
            for(Object k : props.keySet()) {
                Object v = props.get(k);
                if((k instanceof String) && (v instanceof String)) {
                    p.put((String) k,(String) v);
                }
            }
            String s = (String) props.get(IMessage.MESSAGE_TYPE);
            if(IMessage.MessageType.CHAT.name().equals(s)) {
                IQualifiedID to = null;
                message = new TextMessage(to, msg.getBody());
            } else if(IMessage.MessageType.COLLABORATION.name().equals(s)) {
                IQualifiedID to = null;
                message = new CollaborationMessage(to, msg.getBody());
            } else {
                
            }
        }
        return message;
    }
    
    /**
     * 
     * @param msg
     * @return
     */
    private IMessage createMessage(IChatRoomMessage msg) {
        IMessage message = null;
        
        String body = msg.getMessage();
        if(body != null) {
            message = new CollaborationMessage(null, msg.getMessage());

            IChatID cID = (IChatID) msg.getFromID();
            XMPPRoomID rID = (XMPPRoomID) msg.getChatRoomID();
            
            System.out.println("nickname = " + rID.getNickname());
            IQualifiedID id = new VenueUserId(cID.getUsername(), rID.getHostname());
            message.setFrom(id);
        }
        return message;
    }

}
