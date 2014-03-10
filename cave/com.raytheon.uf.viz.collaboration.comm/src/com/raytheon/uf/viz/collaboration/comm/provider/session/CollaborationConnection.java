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

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.commons.lang.StringUtils;
import org.jivesoftware.smack.Connection;
import org.jivesoftware.smack.ConnectionConfiguration;
import org.jivesoftware.smack.ConnectionListener;
import org.jivesoftware.smack.Roster;
import org.jivesoftware.smack.RosterEntry;
import org.jivesoftware.smack.RosterListener;
import org.jivesoftware.smack.XMPPConnection;
import org.jivesoftware.smack.XMPPException;
import org.jivesoftware.smack.filter.PacketTypeFilter;
import org.jivesoftware.smack.packet.Message;
import org.jivesoftware.smack.packet.Presence;
import org.jivesoftware.smack.packet.Presence.Mode;
import org.jivesoftware.smack.packet.Presence.Type;
import org.jivesoftware.smack.packet.StreamError;
import org.jivesoftware.smack.packet.XMPPError;
import org.jivesoftware.smack.provider.ProviderManager;
import org.jivesoftware.smackx.muc.InvitationListener;
import org.jivesoftware.smackx.muc.MultiUserChat;

import com.google.common.eventbus.EventBus;
import com.google.common.net.HostAndPort;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.xmpp.PacketConstants;
import com.raytheon.uf.common.xmpp.iq.AuthInfo;
import com.raytheon.uf.common.xmpp.iq.AuthInfoProvider;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.IAccountManager;
import com.raytheon.uf.viz.collaboration.comm.identity.ISession;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IEventPublisher;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IRosterChangeEvent;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IVenueInvitationEvent;
import com.raytheon.uf.viz.collaboration.comm.identity.event.RosterChangeType;
import com.raytheon.uf.viz.collaboration.comm.identity.invite.SharedDisplayVenueInvite;
import com.raytheon.uf.viz.collaboration.comm.identity.invite.VenueInvite;
import com.raytheon.uf.viz.collaboration.comm.packet.SessionPayload;
import com.raytheon.uf.viz.collaboration.comm.packet.SessionPayload.PayloadType;
import com.raytheon.uf.viz.collaboration.comm.packet.SessionPayloadProvider;
import com.raytheon.uf.viz.collaboration.comm.provider.Tools;
import com.raytheon.uf.viz.collaboration.comm.provider.event.RosterChangeEvent;
import com.raytheon.uf.viz.collaboration.comm.provider.event.ServerDisconnectEvent;
import com.raytheon.uf.viz.collaboration.comm.provider.event.VenueInvitationEvent;
import com.raytheon.uf.viz.collaboration.comm.provider.event.VenueUserEvent;
import com.raytheon.uf.viz.collaboration.comm.provider.user.ContactsManager;
import com.raytheon.uf.viz.collaboration.comm.provider.user.IDConverter;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserSearch;
import com.raytheon.uf.viz.collaboration.comm.provider.user.VenueId;
import com.raytheon.uf.viz.collaboration.comm.provider.user.VenueParticipant;

/**
 * 
 * <ul>
 * <li>EventBus subscription events.</li>
 * <ul>
 * <li><strong>IVenueInvitationEvent</strong> : This event is posted when the
 * SessionManager receives a venue invitation requesting that the user join some
 * particular collaboration session.</li>
 * <li><strong>IConnectionStatusEvent</strong> : This event is posted when the
 * state of the underlying connection changes, reconnecting, connecting,
 * disconnected, for example.</li>
 * <li><strong>IRosterChangeEvent</strong> : This event is posted when roster
 * changes have occurred.</li>
 * <li><strong>---------------</strong> : ---------------.</li>
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
 * Apr 18, 2012            njensen      Major cleanup
 * Dec  6, 2013 2561       bclement    removed ECF
 * Dec 18, 2013 2562       bclement    added smack compression, fixed invite parsing
 * Dec 19, 2013 2563       bclement    added connection listener, 
 *                                     added better error message on failed connection
 * Jan 07, 2013 2563       bclement    use getServiceName instead of getHost when creating room id
 * Jan 08, 2014 2563       bclement    fixed custom port and service name in user id
 * Jan 15, 2014 2630       bclement    connection data stores status as Mode object
 * Jan 24, 2014 2701       bclement    removed roster manager
 * Jan 28, 2014 2698       bclement    fixed compression default
 *                                     cleaned up createCollaborationVenue, removed getVenueInfo
 * Jan 30, 2014 2698       bclement    changed arguments to create sessions, moved room connection from SessionView
 * Feb  3, 2014 2699       bclement    removed unneeded catch in joinTextOnlyVenue
 * Feb 13, 2014 2751       bclement    better types for venueid and invitor
 * Feb 18, 2014 2793       bclement    improved disconnection notification and handling
 * Feb 24, 2014 2632       mpduff      Fix roster change type for presence change.
 * Feb 28, 2014 2756       bclement    added authManager
 * Mar 07, 2014 2848       bclement    removed join*Venue methods, now only creates venue objects
 *                                      changed session map to a concurrent hash map
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 * @see com.raytheon.uf.viz.collaboration.comm.identity.event.IVenueInvitationEvent
 */

public class CollaborationConnection implements IEventPublisher {

    static {
        ProviderManager pm = ProviderManager.getInstance();
        pm.addExtensionProvider(SessionPayload.ELEMENT_NAME,
                PacketConstants.COLLAB_XMLNS, new SessionPayloadProvider());
        pm.addIQProvider(PacketConstants.QUERY_ELEMENT_NAME,
                AuthInfo.AUTH_QUERY_XMLNS, new AuthInfoProvider());
    }

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CollaborationConnection.class);

    private static CollaborationConnection instance = null;

    private static Map<CollaborationConnectionData, CollaborationConnection> instanceMap = new HashMap<CollaborationConnectionData, CollaborationConnection>();

    private final Map<String, ISession> sessions;

    private final UserId user;

    private Presence userPresence;

    private PeerToPeerChat chatInstance = null;

    private IAccountManager accountManager = null;

    private final EventBus eventBus;

    private final ContactsManager contactsMgr;

    private final CollaborationConnectionData connectionData;

    private XMPPConnection connection;

    private final ClientAuthManager authManager;

    public static boolean COMPRESS = true;

    static {
        try {
            final String compressionProperty = "collaboration.compression";
            if (System.getProperty(compressionProperty) != null) {
                COMPRESS = Boolean.getBoolean(compressionProperty);
            }
        } catch (Exception e) {
            // must not have permission to access system properties. ignore and
            // use default.
        }
    }

    private CollaborationConnection(CollaborationConnectionData connectionData)
            throws CollaborationException {
        this.connectionData = connectionData;
        String password = connectionData.getPassword();
        Mode mode = connectionData.getStatus();
        if (mode == null) {
            mode = Mode.available;
        }
        Presence initialPresence = new Presence(Type.available,
                connectionData.getMessage(), 0, mode);
        Tools.setProperties(initialPresence, connectionData.getAttributes());

        eventBus = new EventBus();
        sessions = new ConcurrentHashMap<String, ISession>();

        HostAndPort hnp = HostAndPort.fromString(connectionData.getServer());
        ConnectionConfiguration conConfig;
        if (hnp.hasPort()) {
            conConfig = new ConnectionConfiguration(hnp.getHostText(),
                    hnp.getPort());
        } else {
            conConfig = new ConnectionConfiguration(hnp.getHostText());
        }

        conConfig.setCompressionEnabled(COMPRESS);

        connection = new XMPPConnection(conConfig);

        connectInternal(connectionData.getUserName(), password);

        this.user = new UserId(connectionData.getUserName(),
                connection.getServiceName());

        setupConnectionListener();
        setupAccountManager();
        setupInternalConnectionListeners();
        setupInternalVenueInvitationListener();
        setupP2PComm();
        getPeerToPeerSession();

        authManager = new ClientAuthManager(connection);

        userPresence = initialPresence;
        if (accountManager != null && initialPresence != null) {
            accountManager.sendPresence(initialPresence);
        }

        contactsMgr = new ContactsManager(this, connection);
        this.registerEventHandler(contactsMgr);

        instanceMap.put(connectionData, this);
        if (instance == null) {
            instance = this;
        }
    }

    /**
     * connect to XMPP server and login
     * 
     * @param username
     * @param password
     * @throws CollaborationException
     */
    private void connectInternal(String username, String password)
            throws CollaborationException {
        try {
            connection.connect();
            connection.login(username, password);
        } catch (XMPPException e) {
            closeInternals();
            // get a nice reason for the user
            String msg;
            XMPPError xmppErr = e.getXMPPError();
            if (xmppErr != null) {
                switch (xmppErr.getCode()) {
                case 401:
                    msg = "Bad username or password";
                    break;
                case 403:
                    msg = "User not allowed to connect to server";
                    break;
                case 409:
                    msg = "User account already in use by another client";
                    break;
                default:
                    msg = e.getLocalizedMessage();
                }
            } else {
                msg = e.getLocalizedMessage();
            }
            throw new CollaborationException("Login failed: " + msg, e);
        }
    }

    public CollaborationConnectionData getConnectionData() {
        return connectionData;
    }

    /**
     * @return
     * @see com.raytheon.uf.viz.collaboration.comm.identity.roster.IRoster#getUser()
     */
    public UserId getUser() {
        return user;
    }

    /**
     * 
     * @return
     */
    public Presence getPresence() {
        return userPresence;
    }

    /**
     * 
     * @return
     */
    public void setPresence(Presence presence) {
        userPresence = presence;
    }

    /**
     * 
     */
    private void setupAccountManager() {
        if (accountManager == null) {
            if (isConnected()) {
                accountManager = new AccountManager(this);
            }
        }
    }

    /**
     * Get the account manager for this connection.
     * 
     * @return The account manager for this connection.
     */
    public IAccountManager getAccountManager() {
        if (accountManager == null) {
            setupAccountManager();
        }
        return accountManager;
    }

    /**
     * Is this SessionManager currently connected?
     * 
     * @return Is this SessionManager currently connected?
     */
    public boolean isConnected() {
        return ((connection != null) && (connection.getConnectionID() != null));
    }

    private void closeInternals() {
        if (connection != null) {

            chatInstance = null;
            // Get rid of the account and roster managers
            if (connection.isConnected()) {
                connection.disconnect();
            }
            connection = null;
        }
        PeerToPeerCommHelper.reset();
        instanceMap.remove(connectionData);
        if (this == instance) {
            instance = null;
        }
    }

    /**
     *  
     */
    public void close() {
        if (connection != null) {
            // Close any created sessions.
            Collection<ISession> toRemove = sessions.values();
            sessions.clear();
            for (ISession session : toRemove) {
                if ((chatInstance != null) && chatInstance.equals(session)) {
                    chatInstance.close();
                    chatInstance = null;
                } else {
                    session.close();
                }
            }
            chatInstance = null;
        }
        closeInternals();
    }

    /**
     * Get the PeerToPeerChat session instance.
     * 
     * @return
     */
    public ISession getPeerToPeerSession() throws CollaborationException {
        if (chatInstance == null) {
            chatInstance = new PeerToPeerChat(eventBus, this);
            sessions.put(chatInstance.getSessionId(), chatInstance);
            postEvent(chatInstance);
        }
        return chatInstance;
    }

    /**
     * Create shared display venue object. This does not create the venue on the
     * server. The session should be unregistered when no longer active using
     * {@link CollaborationConnection#removeSession(ISession)}
     * 
     * @param invitation
     * @param handle
     * @return
     */
    public SharedDisplaySession createCollaborationVenue(
            IVenueInvitationEvent invitation, String handle) {
        SharedDisplayVenueInvite sdvInvite = (SharedDisplayVenueInvite) invitation
                .getInvite();
        String sessionId = invitation.getInvite().getSessionId();
        String venueName = invitation.getRoomId().getName();
        SharedDisplaySession rval = new SharedDisplaySession(eventBus, this,
                venueName, handle, sessionId);
        setupCollaborationVenue(rval, sdvInvite.getSessionLeader(),
                sdvInvite.getDataProvider());
        return rval;
    }

    /**
     * Configure shared display venue object and register with session map
     * 
     * @param session
     * @param leader
     * @param provider
     */
    private void setupCollaborationVenue(SharedDisplaySession session,
            VenueParticipant leader, VenueParticipant provider) {
        session.setCurrentSessionLeader(leader);
        session.setCurrentDataProvider(provider);
        sessions.put(session.getSessionId(), session);
    }

    /**
     * Create shared display venue object. This does not create the venue on the
     * server. The session should be unregistered when no longer active using
     * {@link CollaborationConnection#removeSession(ISession)}
     * 
     * @param data
     * @return
     * @throws CollaborationException
     */
    public SharedDisplaySession createCollaborationVenue(CreateSessionData data) {
        SharedDisplaySession session = new SharedDisplaySession(eventBus, this,
                data);
        VenueParticipant leader = session.getUserID();
        setupCollaborationVenue(session, leader, leader);
        return session;
    }

    /**
     * Create text only venue object. This does not create the venue on the
     * server. The session should be unregistered when no longer active using
     * {@link CollaborationConnection#removeSession(ISession)}
     * 
     * @param venueName
     * @param handle
     * @return
     * @throws CollaborationException
     */
    public VenueSession createTextOnlyVenue(String venueName, String handle) {
        return createTextOnlyVenue(new CreateSessionData(venueName, handle));
    }

    /**
     * Create text only venue object. This does not create the venue on the
     * server. The session should be unregistered when no longer active using
     * {@link CollaborationConnection#removeSession(ISession)}
     * 
     * @param data
     * @return
     */
    public VenueSession createTextOnlyVenue(CreateSessionData data) {
        VenueSession session = new VenueSession(eventBus, this, data);
        sessions.put(session.getSessionId(), session);
        return session;
    }

    /**
     * Check if venue exists on server
     * 
     * @param venueName
     * @return false on error
     */
    public boolean venueExistsOnServer(String venueName) {
        String roomId = VenueSession.getRoomId(connection.getServiceName(),
                venueName);
        try {
            return VenueSession.roomExistsOnServer(connection, roomId);
        } catch (XMPPException e) {
            statusHandler.error("Unable to check for room on server", e);
            return false;
        }
    }

    /**
     * 
     * @param session
     */
    public void removeSession(ISession session) {
        sessions.remove(session.getSessionId());
        postEvent(session);
    }

    // ***************************
    // Connection listener
    // ***************************

    /**
     * 
     */
    private void setupInternalConnectionListeners() {
        final Roster roster = connection.getRoster();
        roster.addRosterListener(new RosterListener() {

            @Override
            public void presenceChanged(Presence presence) {
                String fromId = presence.getFrom();
                if (contactsMgr != null) {
                    UserId u = IDConverter.convertFrom(fromId);
                    if (u != null) {
                        RosterEntry entry = contactsMgr.getRosterEntry(u);
                        eventBus.post(entry);
                        IRosterChangeEvent event = new RosterChangeEvent(
                                RosterChangeType.PRESENCE, entry, presence);
                        eventBus.post(event);
                    }
                }
            }

            @Override
            public void entriesUpdated(Collection<String> addresses) {
                send(addresses, RosterChangeType.MODIFY);
            }

            @Override
            public void entriesDeleted(Collection<String> addresses) {
                send(addresses, RosterChangeType.DELETE);
            }

            @Override
            public void entriesAdded(Collection<String> addresses) {
                send(addresses, RosterChangeType.ADD);
            }

            /**
             * Send event bus notification for roster
             * 
             * @param addresses
             * @param type
             */
            private void send(Collection<String> addresses,
                    RosterChangeType type) {
                for (String addy : addresses) {
                    RosterEntry entry = roster.getEntry(addy);
                    if (entry != null) {
                        IRosterChangeEvent event = new RosterChangeEvent(type,
                                entry);
                        eventBus.post(event);
                    }
                }
            }
        });
    }

    public ISession getSession(String sessionId) {
        return sessions.get(sessionId);
    }

    private void setupP2PComm() {
        if (isConnected()) {
            PeerToPeerCommHelper helper = new PeerToPeerCommHelper(this);
            connection.addPacketListener(helper, new PacketTypeFilter(
                    Message.class));
        }
    }

    private void setupConnectionListener() {
        if (isConnected()) {
            connection.addConnectionListener(new ConnectionListener() {

                @Override
                public void reconnectionSuccessful() {
                    statusHandler
                            .debug("Client successfully reconnected to server");
                    postSystemMessageToVenues("Connection to collaboration server reestablished.");
                }

                @Override
                public void reconnectionFailed(Exception e) {
                    String reason = getErrorReason(e);
                    statusHandler.error("Client can't reconnect to server: "
                            + reason, e);
                    sendDisconnectNotice(reason);
                }

                @Override
                public void reconnectingIn(int seconds) {
                    statusHandler.debug("Client reconnecting to server in "
                            + seconds + " seconds");
                }

                @Override
                public void connectionClosedOnError(Exception e) {
                    String reason = getErrorReason(e);
                    statusHandler.error("Server closed on error: " + reason, e);
                    // don't shutdown yet, we might be able to reconnect
                    postSystemMessageToVenues("Not currently connected to collaboration server.");
                }

                private String getErrorReason(Exception e) {
                    String msg = null;
                    if (e instanceof XMPPException) {
                        StreamError streamError = ((XMPPException) e)
                                .getStreamError();
                        if (streamError != null) {
                            if ("conflict".equalsIgnoreCase(streamError
                                    .getCode())) {
                                msg = "User account in use on another client";
                            }
                        }
                    }
                    return msg == null ? e.getLocalizedMessage() : msg;
                }

                @Override
                public void connectionClosed() {
                    statusHandler.info("Server closed connection");
                    sendDisconnectNotice("Normal termination");
                }

                private void sendDisconnectNotice(String reason) {
                    ServerDisconnectEvent event = new ServerDisconnectEvent(
                            reason);
                    eventBus.post(event);
                }
            });
        }
    }

    // ***************************
    // Venue invitation listener management
    // ***************************

    /**
     * 
     */
    private void setupInternalVenueInvitationListener() {
        if (isConnected()) {
            MultiUserChat.addInvitationListener(connection,
                    new InvitationListener() {
                        @Override
                        public void invitationReceived(Connection conn,
                                String room, String inviter, String reason,
                                String password, Message message) {
                            // TODO handle password protected rooms
                            VenueId venueId = new VenueId();
                            venueId.setName(Tools.parseName(room));
                            venueId.setHost(Tools.parseHost(room));
                            UserId invitor = IDConverter.convertFrom(inviter);

                            if (message != null) {
                                SessionPayload payload = (SessionPayload) message
                                        .getExtension(PacketConstants.COLLAB_XMLNS);
                                if (payload != null) {
                                    handleCollabInvite(venueId, invitor,
                                            payload);
                                    return;
                                }
                            }
                            if (reason != null
                                    && reason.startsWith(Tools.CMD_PREAMBLE)) {
                                reason = "Shared display invitation from incompatible version of CAVE. "
                                        + "Session will be chat-only if invitation is accepted";
                            }
                            handleChatRoomInvite(venueId, invitor, reason,
                                    message);
                        }
                    });
        }
    }

    private void handleChatRoomInvite(VenueId venueId, UserId invitor,
            String reason, Message message) {
        VenueInvite invite = new VenueInvite();
        if (!StringUtils.isBlank(reason)) {
            invite.setMessage(reason);
        } else if (!StringUtils.isBlank(message.getBody())) {
            invite.setMessage(message.getBody());
        } else {
            invite.setMessage("");
        }
        invite.setSubject(message.getSubject());
        IVenueInvitationEvent event = new VenueInvitationEvent(venueId,
                invitor, invite);
        eventBus.post(event);
    }

    private void handleCollabInvite(VenueId venueId, UserId invitor,
            SessionPayload payload) {
        Object obj = payload.getData();
        if (obj == null
                || !payload.getPayloadType().equals(PayloadType.Invitation)
                || !(obj instanceof VenueInvite)) {
            statusHandler.warn("Received unsupported invite payload");
            return;
        }
        VenueInvite invite = (VenueInvite) obj;
        IVenueInvitationEvent event = new VenueInvitationEvent(venueId,
                invitor, invite);
        eventBus.post(event);
    }

    /**
     * Register an event handler with this
     * 
     * @see com.raytheon.uf.viz.collaboration.comm.identity.event.IEventPublisher#registerEventHandler(java.lang.Object)
     */
    @Override
    public void registerEventHandler(Object handler) {
        eventBus.register(handler);
    }

    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.event.IEventPublisher#unregisterEventHandler(java.lang.Object)
     */
    @Override
    public void unregisterEventHandler(Object handler) {
        eventBus.unregister(handler);
    }

    @Override
    public void postEvent(Object event) {
        if (event != null) {
            eventBus.post(event);
        }
    }

    public ContactsManager getContactsManager() {
        return contactsMgr;
    }

    public Collection<ISession> getSessions() {
        return sessions.values();
    }

    /**
     * Returns the currently connected connection or null if it's not connected
     * 
     * @return
     */
    public static CollaborationConnection getConnection() {
        return instance;
    }

    /**
     * Create a {@link CollaborationConnection} given the
     * {@link CollaborationConnectionData}
     * 
     * @param userData
     * @return
     * @throws CollaborationException
     */
    public static CollaborationConnection connect(
            CollaborationConnectionData userData) throws CollaborationException {
        if (instance != null) {
            throw new CollaborationException("Already connected");
        } else {
            instance = new CollaborationConnection(userData);
            return getConnection();
        }
    }

    /**
     * @return Smack connection object
     */
    protected XMPPConnection getXmppConnection() {
        return connection;
    }

    /**
     * Construct a new UserSearch object
     * 
     * @return
     */
    public UserSearch createSearch() {
        return new UserSearch(connection);
    }

    /**
     * Post a system message to all venue windows this client has. This is for
     * local system messages, it does not go out to the server.
     * 
     * @param message
     */
    public void postSystemMessageToVenues(String message) {
        for (Entry<String, ISession> entry : sessions.entrySet()) {
            entry.getValue().postEvent(new VenueUserEvent(message));
        }
    }

    /**
     * @return the authManager
     */
    public ClientAuthManager getAuthManager() {
        return authManager;
    }

}
