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
import org.eclipse.ecf.core.IContainerListener;
import org.eclipse.ecf.core.events.IContainerEvent;
import org.eclipse.ecf.core.identity.ID;
import org.eclipse.ecf.core.identity.IDFactory;
import org.eclipse.ecf.core.identity.Namespace;
import org.eclipse.ecf.core.security.ConnectContextFactory;
import org.eclipse.ecf.presence.IPresenceContainerAdapter;
import org.eclipse.ecf.presence.chatroom.IChatRoomInfo;
import org.eclipse.ecf.presence.chatroom.IChatRoomInvitationListener;
import org.eclipse.ecf.presence.chatroom.IChatRoomManager;
import org.eclipse.ecf.presence.roster.IRoster;
import org.eclipse.ecf.provider.xmpp.identity.XMPPRoomID;
import org.jivesoftware.smack.XMPPConnection;

import com.google.common.eventbus.EventBus;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.IAccountManager;
import com.raytheon.uf.viz.collaboration.comm.identity.IPresence;
import com.raytheon.uf.viz.collaboration.comm.identity.ISession;
import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IEventPublisher;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IVenueInvitationEvent;
import com.raytheon.uf.viz.collaboration.comm.identity.info.IVenueInfo;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterManager;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IChatID;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID;
import com.raytheon.uf.viz.collaboration.comm.provider.Presence;
import com.raytheon.uf.viz.collaboration.comm.provider.Tools;
import com.raytheon.uf.viz.collaboration.comm.provider.event.VenueInvitationEvent;
import com.raytheon.uf.viz.collaboration.comm.provider.info.InfoAdapter;
import com.raytheon.uf.viz.collaboration.comm.provider.roster.RosterManager;
import com.raytheon.uf.viz.collaboration.comm.provider.user.IDConverter;
import com.raytheon.uf.viz.collaboration.comm.provider.user.VenueId;
import com.raytheon.uf.viz.collaboration.comm.provider.user.VenueUserId;

/**
 * 
 * <ul>
 * <li>EventBus subscription events.</li>
 * <ul>
 * <li><strong>IVenueInvitationEvent</strong> : This event is posted when a
 * venue participant enters, leaves a venue, or updates their status in the
 * venue.</li>
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
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 * @see com.raytheon.uf.viz.collaboration.comm.identity.event.IVenueInvitationEvent
 */

public class SessionManager implements IEventPublisher {

    private enum SessionType {
        SESSION_P2P, SESSION_CHAT_ONLY, SESSION_COLLABORATION;
    }

    private static final String PROVIDER = "ecf.xmpp.smack";

    private Map<String, ISession> sessions;

    private Map<String, ISession> following;

    private String account;

    private String password;

    private IChatRoomInvitationListener intInvitationListener;

    private IPresenceContainerAdapter presenceAdapter;

    private PeerToPeerChat chatInstance = null;

    private IAccountManager accountManager = null;

    private IRosterManager rosterManager = null;

    private IContainer container = null;

    private EventBus eventBus;

    /**
     * @throws ContainerCreateException
     * 
     */
    public SessionManager(String account, String password) throws Exception {
        XMPPConnection.DEBUG_ENABLED = true;

        try {
            container = ContainerFactory.getDefault().createContainer(PROVIDER);
        } catch (ContainerCreateException cce) {
            throw new CollaborationException(String.format(
                    "Could not create container for provider [%s]", PROVIDER));
        }
        this.account = account;
        this.password = password;
        try {
            connectToContainer();
        } catch (Exception e) {
            throw new Exception("Error creating SessionManager", e);
        }
        setupAccountManager();

        eventBus = new EventBus();

        sessions = new HashMap<String, ISession>();
        following = new HashMap<String, ISession>();

        setupInternalConnectionListeners();
        setupInternalVenueInvitationListener();
        setupP2PComm(presenceAdapter);
        getPeerToPeerSession();
    }

    /**
     * 
     * @param account
     *            The account name to connect to.
     * @param password
     *            The password to use for connection.
     * @param initialPresence
     *            The initial presence for the account name.
     * @throws ContainerCreateException
     * 
     */
    public SessionManager(String account, String password,
            IPresence initialPresence) throws Exception {
        this(account, password);
        if (accountManager != null) {
            accountManager.sendPresence(initialPresence);
        }
    }

    /**
     * 
     */
    private void connectToContainer() {
        if (container.getConnectedID() == null) {
            Namespace namespace = container.getConnectNamespace();

            ID targetID = IDFactory.getDefault().createID(namespace, account);
            // Now connect
            try {
                container.connect(targetID, ConnectContextFactory
                        .createPasswordConnectContext(password));

                presenceAdapter = Tools.getPresenceContainerAdapter(container,
                        IPresenceContainerAdapter.class);
            } catch (ContainerConnectException e) {
                System.out.println("Error attempting to connect");
                e.printStackTrace();
            }
        }
    }

    /**
     * 
     */
    private void setupAccountManager() {
        if (accountManager == null) {
            if (isConnected() && (presenceAdapter != null)) {
                accountManager = new AccountManager(presenceAdapter);
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
        IRoster roster = null;
        IPresenceContainerAdapter presenceAdapter = Tools
                .getPresenceContainerAdapter(container,
                        IPresenceContainerAdapter.class);
        if (presenceAdapter != null) {
            roster = presenceAdapter.getRosterManager().getRoster();
            if (roster != null) {
                rosterManager = new RosterManager(roster);
            }
        }
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

    /**
     *  
     */
    public void closeManager() {
        if (container != null) {

            // Close any created sessions.
            for (ISession session : sessions.values()) {
                if ((chatInstance != null) && chatInstance.equals(session)) {
                    chatInstance.close();
                    chatInstance = null;
                } else {
                    session.close();
                }
            }
            chatInstance = null;
            // Get rid of the account and roster managers
            container.dispose();
            container = null;
        }
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
        }
        return chatInstance;
    }

    public IVenueSession joinCollaborationVenue(IVenueInvitationEvent invitation)
            throws CollaborationException {
        VenueSession session = null;
        try {
            String venueName = invitation.getRoomId().getName();
            String sessionId = invitation.getSessionId();
            session = new VenueSession(container, eventBus, this, sessionId);
            if (session != null) {
                session.joinVenue(venueName);
                sessions.put(session.getSessionId(), session);
            }
        } catch (Exception e) {

        }
        return session;
    }

    /**
     * 
     * @param venueName
     * @return
     * @throws CollaborationException
     */
    public IVenueSession joinCollaborationVenue(String venueName)
            throws CollaborationException {
        VenueSession session = null;
        try {
            session = new VenueSession(container, eventBus, this);
            if (session != null) {
                session.joinVenue(venueName);
                sessions.put(session.getSessionId(), session);
            }

        } catch (Exception e) {

        }
        return session;
    }

    /**
     * 
     * @param venueName
     * @return
     * @throws CollaborationException
     */
    public IVenueSession createCollaborationVenue(String venueName,
            String subject) throws CollaborationException {
        VenueSession session = null;
        try {
            session = new VenueSession(container, eventBus, this);
            if (session != null) {
                session.createVenue(venueName, subject);

                IPresence presence = new Presence();
                presence.setMode(IPresence.Mode.AVAILABLE);
                presence.setType(IPresence.Type.AVAILABLE);
                presence.setProperty("DATA_PROVIDER", "");
                presence.setProperty("SESSION_LEADER", "");

                sessions.put(session.getSessionId(), session);
            }
        } catch (Exception e) {
        }
        return session;
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
            }

        } catch (Exception e) {

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
            }

        } catch (Exception e) {

        }
        return session;
    }

    /**
     * 
     * @param session
     */
    void removeSession(ISession session) {
        sessions.remove(session);
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
                    IVenueInfo vi = InfoAdapter.createVenueInfo(rInfo);
                    if (vi != null) {
                        info.add(vi);
                    }
                }
            } else {
                // Could not create venueManager
            }
        } else {
            // not currently connected
        }

        return info;
    }

    // ***************************
    // Connection listener
    // ***************************

    private void setupInternalConnectionListeners() {

        if (container != null) {
            container.addListener(new IContainerListener() {

                @Override
                public void handleEvent(IContainerEvent event) {

                }
            });
        }
    }

    // ***************************
    // Peer to Peer communications
    // ***************************

    /**
     * 
     */
    ISession getSession(String sessionId) {
        return sessions.get(sessionId);
    }

    private void setupP2PComm(IPresenceContainerAdapter presenceAdapter) {
        if (isConnected() && (presenceAdapter != null)) {
            PeerToPeerCommHelper helper = new PeerToPeerCommHelper(this,
                    presenceAdapter);
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
                        if(roomID instanceof XMPPRoomID) {
                            XMPPRoomID room = (XMPPRoomID) roomID;
                            venueId = new VenueId();
                            venueId.setName(room.getLongName());
                            
                        }
                        if(venueId != null) {
                            IQualifiedID id = IDConverter.convertFrom(from);

                            IChatID invitor = new VenueUserId(id.getName(),
                                    id.getHost(), id.getResource());

                            IVenueInvitationEvent invite = new VenueInvitationEvent(
                                    venueId, invitor, subject, body);
                            System.out.println("Posting invitation using eventBus:"
                                    + eventBus.hashCode());
                            eventBus.post(invite);
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
     * @see com.raytheon.uf.viz.collaboration.comm.identity.event.IEventPublisher#unRegisterEventHandler(java.lang.Object)
     */
    @Override
    public void unRegisterEventHandler(Object handler) {
        eventBus.unregister(handler);
    }

    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.event.IEventPublisher#getEventPublisher()
     */
    @Override
    public EventBus getEventPublisher() {
        return eventBus;
    }

}
