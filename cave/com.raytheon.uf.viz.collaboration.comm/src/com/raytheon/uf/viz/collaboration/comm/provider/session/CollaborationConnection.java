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
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.ecf.core.ContainerConnectException;
import org.eclipse.ecf.core.ContainerCreateException;
import org.eclipse.ecf.core.ContainerFactory;
import org.eclipse.ecf.core.IContainer;
import org.eclipse.ecf.core.identity.ID;
import org.eclipse.ecf.core.identity.IDCreateException;
import org.eclipse.ecf.core.identity.IDFactory;
import org.eclipse.ecf.core.identity.Namespace;
import org.eclipse.ecf.core.security.ConnectContextFactory;
import org.eclipse.ecf.core.user.IUser;
import org.eclipse.ecf.presence.IPresence;
import org.eclipse.ecf.presence.IPresence.Mode;
import org.eclipse.ecf.presence.IPresence.Type;
import org.eclipse.ecf.presence.IPresenceContainerAdapter;
import org.eclipse.ecf.presence.IPresenceListener;
import org.eclipse.ecf.presence.Presence;
import org.eclipse.ecf.presence.chatroom.IChatRoomInfo;
import org.eclipse.ecf.presence.chatroom.IChatRoomInvitationListener;
import org.eclipse.ecf.presence.chatroom.IChatRoomManager;
import org.eclipse.ecf.presence.roster.IRoster;
import org.eclipse.ecf.presence.roster.IRosterEntry;
import org.eclipse.ecf.presence.roster.IRosterItem;
import org.eclipse.ecf.presence.roster.IRosterListener;
import org.eclipse.ecf.presence.roster.IRosterManager;
import org.eclipse.ecf.provider.xmpp.identity.XMPPRoomID;

import com.google.common.eventbus.EventBus;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.IAccountManager;
import com.raytheon.uf.viz.collaboration.comm.identity.ISession;
import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.identity.MultipleLoginException;
import com.raytheon.uf.viz.collaboration.comm.identity.UsernamePasswordException;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IEventPublisher;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IRosterChangeEvent;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IVenueInvitationEvent;
import com.raytheon.uf.viz.collaboration.comm.identity.event.RosterChangeType;
import com.raytheon.uf.viz.collaboration.comm.identity.info.IVenueInfo;
import com.raytheon.uf.viz.collaboration.comm.identity.invite.SharedDisplayVenueInvite;
import com.raytheon.uf.viz.collaboration.comm.identity.invite.VenueInvite;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID;
import com.raytheon.uf.viz.collaboration.comm.provider.Tools;
import com.raytheon.uf.viz.collaboration.comm.provider.event.RosterChangeEvent;
import com.raytheon.uf.viz.collaboration.comm.provider.event.VenueInvitationEvent;
import com.raytheon.uf.viz.collaboration.comm.provider.info.VenueInfo;
import com.raytheon.uf.viz.collaboration.comm.provider.user.ContactsManager;
import com.raytheon.uf.viz.collaboration.comm.provider.user.IDConverter;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.comm.provider.user.VenueId;

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
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 * @see com.raytheon.uf.viz.collaboration.comm.identity.event.IVenueInvitationEvent
 */

public class CollaborationConnection implements IEventPublisher {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CollaborationConnection.class);

    private static final String PROVIDER = "com.raytheon.uf.viz.collaboration.comm.xmpp";// "ecf.xmpp.smack";

    private static CollaborationConnection instance = null;

    private static Map<CollaborationConnectionData, CollaborationConnection> instanceMap = new HashMap<CollaborationConnectionData, CollaborationConnection>();

    private Map<String, ISession> sessions;

    private UserId account;

    private String password;

    private UserId user;

    private IPresence userPresence;

    private IChatRoomInvitationListener intInvitationListener;

    private IPresenceContainerAdapter presenceAdapter;

    private Namespace connectionNamespace = null;

    private PeerToPeerChat chatInstance = null;

    private IAccountManager accountManager = null;

    private IRosterManager rosterManager = null;

    private IContainer container = null;

    private EventBus eventBus;

    private ContactsManager contactsMgr;

    private CollaborationConnectionData connectionData;

    private CollaborationConnection(CollaborationConnectionData connectionData)
            throws CollaborationException {
        this.connectionData = connectionData;
        UserId account = new UserId(connectionData.getUserName(),
                connectionData.getServer());
        String password = connectionData.getPassword();
        IPresence initialPresence = new Presence(Type.AVAILABLE,
                connectionData.getMessage(), Mode.fromString(connectionData
                        .getStatus().toLowerCase()),
                connectionData.getAttributes());

        eventBus = new EventBus();
        sessions = new HashMap<String, ISession>();

        try {
            container = ContainerFactory.getDefault().createContainer(PROVIDER);

            if (container != null) {
                // add the listeners before we connect so we don't potentially
                // miss something
                presenceAdapter = Tools.getPresenceContainerAdapter(container,
                        IPresenceContainerAdapter.class);
                this.setupInternalConnectionListeners();
            }

        } catch (ContainerCreateException cce) {
            closeInternals();
            throw new CollaborationException(String.format(
                    "Could not create container for provider [%s]", PROVIDER));
        }
        this.account = account;
        this.password = password;
        try {
            connectToContainer();
        } catch (ContainerConnectException e) {
            closeInternals();
            // ECF does a very good job of wrapping up login exceptions so it is
            // hard to tell why the login failed. This code will attempt to
            // analyze the cause of the failure and in some circumstances
            // produce a helpful error. Since this is relying on string
            // comparison of exception messages it is very likely that if the
            // ECF provider is changed or updated that this will stop producing
            // helpful messages. Unfortunately ECF does not provide any other
            // mechanism for figuring out why login failed.
            for (Throwable t = e; t != null && t != t.getCause(); t = t
                    .getCause()) {
                if (t.getMessage().contains("authentication failed")) {
                    throw new UsernamePasswordException(
                            "Login failed.  Invalid username or password", e);
                } else if (t.getMessage().equals("conflict(409)")) {
                    throw new MultipleLoginException(
                            "Login failed.  User already logged in elsewhere",
                            e);
                }
            }
            // In cases where we can't produce anything helpful, at least let
            // the user know that it failed and hopeful something in the stack
            // trace will be useful in the DR.
            throw new CollaborationException("Login failed.", e);

        }
        ID id = container.getConnectedID();
        if (id != null) {
            String name = Tools.parseName(id.getName());
            String host = Tools.parseHost(id.getName());
            String resource = Tools.parseResource(id.getName());
            user = new UserId(name, host, resource);
            user.setId(id);
        }

        setupAccountManager();

        setupInternalVenueInvitationListener();
        setupP2PComm(presenceAdapter);
        getPeerToPeerSession();

        userPresence = initialPresence;
        if (accountManager != null && initialPresence != null) {
            accountManager.sendPresence(initialPresence);
        }

        contactsMgr = new ContactsManager(this);
        this.registerEventHandler(contactsMgr);

        instanceMap.put(connectionData, this);
        if (instance == null) {
            instance = this;
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
    public IPresence getPresence() {
        return userPresence;
    }

    /**
     * 
     * @return
     */
    public void setPresence(IPresence presence) {
        userPresence = presence;
    }

    /**
     * @throws CollaborationException
     * @throws ContainerConnectException
     * 
     */
    private void connectToContainer() throws CollaborationException,
            ContainerConnectException {
        if (container.getConnectedID() == null) {
            connectionNamespace = container.getConnectNamespace();

            // Now connect
            ID targetID = createID(account);
            presenceAdapter = Tools.getPresenceContainerAdapter(container,
                    IPresenceContainerAdapter.class);
            container.connect(targetID, ConnectContextFactory
                    .createPasswordConnectContext(password));
        }
    }

    /**
     * 
     */
    private void setupAccountManager() {
        if (accountManager == null) {
            if (isConnected() && (presenceAdapter != null)) {
                accountManager = new AccountManager(presenceAdapter, this);
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
     * 
     */
    private void setupRosterManager() {
        rosterManager = presenceAdapter.getRosterManager();
    }

    /**
     * 
     * @return
     */
    public IPresenceContainerAdapter getPresenceContainerAdapter() {
        return presenceAdapter;
    }

    /**
     * 
     * @return
     */
    public IRosterManager getRosterManager() {
        if (rosterManager == null) {
            setupRosterManager();
        }
        return rosterManager;
    }

    /**
     * Is this SessionManager currently connected?
     * 
     * @return Is this SessionManager currently connected?
     */
    public boolean isConnected() {
        return ((container != null) && (container.getConnectedID() != null));
    }

    private void closeInternals() {
        if (container != null) {

            chatInstance = null;
            // Get rid of the account and roster managers
            container.disconnect();
            container.dispose();
            container = null;
        }
        instanceMap.remove(connectionData);
        if (this == instance) {
            instance = null;
        }
    }

    /**
     *  
     */
    public void close() {
        if (container != null) {
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
            chatInstance = new PeerToPeerChat(container, eventBus, this);
            sessions.put(chatInstance.getSessionId(), chatInstance);
            postEvent(chatInstance);
        }
        return chatInstance;
    }

    public ISharedDisplaySession joinCollaborationVenue(
            IVenueInvitationEvent invitation) throws CollaborationException {
        SharedDisplaySession session = null;
        String venueName = invitation.getRoomId().getName();
        String sessionId = invitation.getInvite().getSessionId();
        session = new SharedDisplaySession(container, eventBus, this, sessionId);
        if (session != null) {
            session.joinVenue(venueName);

            if (invitation.getInvite() instanceof SharedDisplayVenueInvite) {
                SharedDisplayVenueInvite invite = (SharedDisplayVenueInvite) invitation
                        .getInvite();
                session.setCurrentDataProvider(invite.getDataProvider());
                session.setCurrentSessionLeader(invite.getSessionLeader());
            }

            sessions.put(session.getSessionId(), session);
            postEvent(session);
        }
        return session;
    }

    /**
     * 
     * @param venueName
     * @return
     * @throws CollaborationException
     */
    public ISharedDisplaySession createCollaborationVenue(String venueName,
            String subject) throws CollaborationException {
        SharedDisplaySession session = null;
        try {
            session = new SharedDisplaySession(container, eventBus, this);

            session.createVenue(venueName, subject);
            session.setCurrentSessionLeader(user);
            session.setCurrentDataProvider(user);

            sessions.put(session.getSessionId(), session);
            postEvent(session);
            return session;
        } catch (Exception e) {
            throw new CollaborationException(
                    "Error creating collaboration venue " + venueName, e);
        }
    }

    /**
     * 
     * @param venueName
     * @return
     * @throws CollaborationException
     */
    public IVenueSession joinTextOnlyVenue(String venueName)
            throws CollaborationException {
        VenueSession session = null;
        try {
            session = new VenueSession(container, eventBus, this);
            if (session != null) {
                session.joinVenue(venueName);
                sessions.put(session.getSessionId(), session);
                postEvent(session);
            }
        } catch (Exception e) {
            throw new CollaborationException(
                    "Error joining venue " + venueName, e);
        }
        return session;
    }

    /**
     * 
     * @param venueName
     * @return
     * @throws CollaborationException
     */
    public IVenueSession createTextOnlyVenue(String venueName, String subject)
            throws CollaborationException {
        VenueSession session = null;
        try {
            session = new VenueSession(container, eventBus, this);
            if (session != null) {
                session.createVenue(venueName, subject);
                sessions.put(session.getSessionId(), session);
                postEvent(session);
            }
        } catch (Exception e) {
            throw new CollaborationException("Error creating venue "
                    + venueName, e);
        }
        return session;
    }

    /**
     * 
     * @param session
     */
    protected void removeSession(ISession session) {
        sessions.remove(session.getSessionId());
        postEvent(session);
    }

    /**
     * 
     * @return
     */
    public Collection<IVenueInfo> getVenueInfo() {
        // Check to see if the container has been connected.
        Collection<IVenueInfo> info = new ArrayList<IVenueInfo>();
        if (isConnected()) {
            IPresenceContainerAdapter presenceAdapter = Tools
                    .getPresenceContainerAdapter(container,
                            IPresenceContainerAdapter.class);
            IChatRoomManager venueManager = presenceAdapter
                    .getChatRoomManager();
            if (venueManager != null) {
                IChatRoomInfo[] roomInfo = venueManager.getChatRoomInfos();
                for (IChatRoomInfo rInfo : roomInfo) {
                    IVenueInfo vi = new VenueInfo(rInfo);
                    info.add(vi);
                }
            }
        }

        return info;
    }

    // ***************************
    // Connection listener
    // ***************************

    /**
     * 
     */
    private void setupInternalConnectionListeners() {

        presenceAdapter.getRosterManager().addPresenceListener(
                new IPresenceListener() {

                    @Override
                    public void handlePresence(ID fromId,
                            org.eclipse.ecf.presence.IPresence presence) {

                        if (rosterManager != null) {
                            if (contactsMgr != null) {
                                IUser u = contactsMgr.getUser(fromId);
                                if (u != null) {
                                    IRosterEntry entry = contactsMgr
                                            .getRosterEntry(u);
                                    eventBus.post(entry);
                                }
                            }
                        }
                    }
                });

        presenceAdapter.getRosterManager().addRosterListener(
                new IRosterListener() {

                    @Override
                    public void handleRosterEntryAdd(IRosterEntry entry) {
                        IRosterChangeEvent event = new RosterChangeEvent(
                                RosterChangeType.ADD, entry);
                        eventBus.post(event);
                    }

                    @Override
                    public void handleRosterUpdate(IRoster roster,
                            IRosterItem item) {
                        IRosterChangeEvent event = new RosterChangeEvent(
                                RosterChangeType.MODIFY, item);
                        eventBus.post(event);
                    }

                    @Override
                    public void handleRosterEntryRemove(IRosterEntry entry) {
                        IRosterChangeEvent event = new RosterChangeEvent(
                                RosterChangeType.DELETE, entry);
                        eventBus.post(event);
                    }
                });
    }

    public ISession getSession(String sessionId) {
        return sessions.get(sessionId);
    }

    private void setupP2PComm(IPresenceContainerAdapter presenceAdapter) {
        if (isConnected() && (presenceAdapter != null)) {
            PeerToPeerCommHelper helper = new PeerToPeerCommHelper(this);
            presenceAdapter.getChatManager().addMessageListener(helper);
        }
    }

    // ***************************
    // Venue invitation listener management
    // ***************************

    /**
     * 
     */
    private void setupInternalVenueInvitationListener() {
        if (isConnected() && (presenceAdapter != null)) {
            IChatRoomManager venueManager = presenceAdapter
                    .getChatRoomManager();
            if (venueManager != null) {
                intInvitationListener = new IChatRoomInvitationListener() {
                    @Override
                    public void handleInvitationReceived(ID roomID, ID from,
                            String subject, String body) {

                        IQualifiedID venueId = null;
                        if (roomID instanceof XMPPRoomID) {
                            XMPPRoomID room = (XMPPRoomID) roomID;
                            venueId = new VenueId();
                            venueId.setName(room.getLongName());

                        }
                        if (venueId != null) {
                            IQualifiedID id = IDConverter.convertFrom(from);

                            UserId invitor = new UserId(id.getName(),
                                    id.getHost(), id.getResource());

                            VenueInvite received;
                            try {
                                received = (VenueInvite) Tools
                                        .unMarshallData(body);

                                if (subject == null) {
                                    subject = received.getSubject();
                                    if (subject == null) {
                                        subject = presenceAdapter
                                                .getChatRoomManager()
                                                .getChatRoomInfo(
                                                        roomID.getName())
                                                .getSubject();
                                    }
                                }

                                IVenueInvitationEvent invite = new VenueInvitationEvent(
                                        venueId, invitor, subject, received);
                                eventBus.post(invite);
                            } catch (CollaborationException e) {
                                statusHandler
                                        .handle(Priority.PROBLEM,
                                                "Error handling received invite message",
                                                e);
                            }

                        }
                    }
                };
                venueManager.addInvitationListener(intInvitationListener);
            }
        }
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

    /**
     * 
     * @param name
     * @return
     */
    public ID createID(UserId name) throws CollaborationException {
        ID id = null;
        try {
            if (connectionNamespace != null) {
                id = IDFactory.getDefault().createID(connectionNamespace,
                        name.getFQName());
            }
        } catch (IDCreateException idce) {
            throw new CollaborationException("Could not create id");
        }
        return id;
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

}
