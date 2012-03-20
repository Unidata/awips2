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
import java.util.HashSet;
import java.util.Set;

import org.eclipse.ecf.core.ContainerConnectException;
import org.eclipse.ecf.core.ContainerCreateException;
import org.eclipse.ecf.core.ContainerFactory;
import org.eclipse.ecf.core.IContainer;
import org.eclipse.ecf.core.identity.ID;
import org.eclipse.ecf.core.identity.IDFactory;
import org.eclipse.ecf.core.identity.Namespace;
import org.eclipse.ecf.core.security.ConnectContextFactory;
import org.eclipse.ecf.presence.IPresenceContainerAdapter;
import org.eclipse.ecf.presence.chatroom.IChatRoomInfo;
import org.eclipse.ecf.presence.chatroom.IChatRoomInvitationListener;
import org.eclipse.ecf.presence.chatroom.IChatRoomManager;
import org.eclipse.ecf.presence.roster.IRoster;

import com.google.common.eventbus.EventBus;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.IAccountManager;
import com.raytheon.uf.viz.collaboration.comm.identity.ISession;
import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.identity.info.IVenueInfo;
import com.raytheon.uf.viz.collaboration.comm.identity.listener.IVenueInvitationListener;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterManager;
import com.raytheon.uf.viz.collaboration.comm.provider.info.InfoAdapter;
import com.raytheon.uf.viz.collaboration.comm.provider.roster.RosterManager;

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

public class SessionManager {

    public static final String SESSION_P2P = "P2P";

    public static final String SESSION_COLLABORATION = "collaboration";
    
    public static final String SESSION_CHAT_ONLY = "chatOnly";
    
    private static final String PROVIDER = "ecf.xmpp.smack";
    
    private Set<ISession> sessions = null;
    
    private String account;
    
    private String password;
    
    private IChatRoomInvitationListener intInvitationListener;
    
    private IVenueInvitationListener invitationListener;
    
    private IContainer container = null;
    
    private EventBus eventBus;
    
    /**
     * @throws ContainerCreateException 
     * 
     */
    public SessionManager(String account, String password) throws Exception {
        try {
            container = ContainerFactory.getDefault().createContainer(PROVIDER);
        } catch(ContainerCreateException cce) {
            throw new CollaborationException(String.format("Could not create container for provider [%s]", PROVIDER));
        }
        this.account = account;
        this.password = password;
        try {
            connectToContainer();
        } catch(Exception e) {
            
        }
        eventBus = new EventBus();
        
        sessions = new HashSet<ISession>();
    }

    /**
     * 
     */
    private void connectToContainer() {
        if(container.getConnectedID() == null) {
            Namespace namespace = container.getConnectNamespace();
            
            ID targetID = IDFactory.getDefault().createID(namespace, account);
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

//    if(session != null) {
//        int errorCode = session.connect(account, password);
//        if(errorCode == Errors.BAD_NAME) {
//            throw new CollaborationException(String.format("Bad name [%s]", account));
//        } else if (errorCode == Errors.CANNOT_CONNECT) {
//            throw new CollaborationException(String.format("Count not connect using name [%s]", account));
//        }
//    } else {
//        throw new CollaborationException(String.format("Count not connect using name [%s]", account));
//    }

    
    /**
     * Get the account manager for this connection.
     * @return The account manager for this connection.
     */
    public IAccountManager getAccountManager() {
        IAccountManager manager = null;
        IPresenceContainerAdapter presence = Tools.getPresenceContainerAdapter(container, IPresenceContainerAdapter.class);
        if(presence != null) {
            manager = new AccountManager(presence);
        }
        return manager;
    }

    /**
     * 
     * @return
     */
    public IRosterManager getRosterManager() {
        IRoster roster = null;
        IRosterManager manager = null;
        IPresenceContainerAdapter presence = Tools.getPresenceContainerAdapter(container, IPresenceContainerAdapter.class);
        if(presence != null) {
            roster = presence.getRosterManager().getRoster();
            if(roster != null) {
                manager = new RosterManager(roster); 
            }
        }
        return manager;
    }

    
    /**
     * Is this SessionManager currently connected?
     * @return Is this SessionManager currently connected?
     */
    public boolean isConnected() {
        return ((container != null) && (container.getConnectedID() != null));
    }
    
    /**
     *  
     */
    public void closeManager() {
        if(container != null) {
            // Close any created sessions.
            for(ISession session : sessions) {
                session.close();
            }
            
            // Get rid of the account and roster managers
            container.dispose();
            container = null;
        }
    }

    /**
     * 
     * @return
     */
    public ISession createPeerToPeerSession() throws CollaborationException {
        return (ISession) createSession(SESSION_P2P);
    }
    
    /**
     * 
     * @return
     */
    @Deprecated
    public IVenueSession createChatOnlySession() throws CollaborationException {
        return (IVenueSession) createSession(SESSION_CHAT_ONLY);
    }
    
    /**
     * 
     * @return
     */
    @Deprecated
    public IVenueSession createCollaborationSession() throws CollaborationException {
        return (IVenueSession) createSession(SESSION_COLLABORATION);
    }

    /**
     * 
     * @param venueName
     * @return
     * @throws CollaborationException
     */
    public IVenueSession joinCollaborationVenue(String venueName) throws CollaborationException {
        CollaborationSession session = null;
        try {
            session = new CollaborationSession(container, eventBus);
            if(session != null) {
                session.joinVenue(venueName);
            }
            
        } catch(Exception e) {
            
        }
        return session;
    }
    
    /**
     * 
     * @param venueName
     * @return
     * @throws CollaborationException
     */
    public IVenueSession createCollaborationVenue(String venueName, String subject) throws CollaborationException {
        CollaborationSession session = null;
        try {
            session = new CollaborationSession(container, eventBus);
            if(session != null) {
                session.createVenue(venueName, subject);
            }
            
        } catch(Exception e) {
            
        }
        return session;
    }
    
    /**
     * 
     * @param venueName
     * @return
     * @throws CollaborationException
     */
    public IVenueSession joinTextVenue(String venueName) throws CollaborationException {
        CollaborationSession session = null;
        try {
            session = new CollaborationSession(container, eventBus);
            if(session != null) {
                session.joinVenue(venueName);
            }
            
        } catch(Exception e) {
            
        }
        return session;
    }
    
    /**
     * 
     * @param venueName
     * @return
     * @throws CollaborationException
     */
    public IVenueSession createTextVenue(String venueName, String subject) throws CollaborationException {
        CollaborationSession session = null;
        try {
            session = new CollaborationSession(container, eventBus);
            if(session != null) {
                session.createVenue(venueName, subject);
            }
            
        } catch(Exception e) {
            
        }
        return session;
    }
    
    
    /**
     * 
     * @param sessionKind
     * @return
     */
    private ISession createSession(String sessionKind) throws CollaborationException {

        ISession session = null;
        if(sessionKind != null) {
            
            if(SESSION_P2P.equals(sessionKind)) {
                throw new CollaborationException(String.format("Session kind [%s] not currently implemented", sessionKind));
            } else if(SESSION_COLLABORATION.equals(sessionKind)) {
                session = new CollaborationSession(container, eventBus);
            } else if(SESSION_CHAT_ONLY.equals(sessionKind)) {
                throw new CollaborationException(String.format("Session kind [%s] not currently implemented", sessionKind));
            } else {
                throw new CollaborationException(String.format("[%s] is not a valid session kind.", sessionKind));
            }
        }
        if(session != null) {
            sessions.add(session);
        }
        return session;
    }
    
    /**
     * 
     * @return
     */
    public Collection<IVenueInfo> getVenueInfo() {
        // Check to see if the container has been connected.
        Collection<IVenueInfo> info = new ArrayList<IVenueInfo>();
        if(isConnected()) {
            IPresenceContainerAdapter presence = Tools.getPresenceContainerAdapter(container, IPresenceContainerAdapter.class);
            IChatRoomManager venueManager = presence.getChatRoomManager();
            if(venueManager != null) {
                IChatRoomInfo [] roomInfo = venueManager.getChatRoomInfos();
                for(IChatRoomInfo rInfo : roomInfo) {
                    IVenueInfo vi = InfoAdapter.createVenueInfo(rInfo);
                    if(vi != null) {
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
    
    /**
     * 
     * @param listener
     * @return
     */
    public IVenueInvitationListener setVenueInvitationListener(IVenueInvitationListener listener) {
        if(isConnected()) {
            IPresenceContainerAdapter presence = Tools.getPresenceContainerAdapter(container, IPresenceContainerAdapter.class);
            IChatRoomManager venueManager = presence.getChatRoomManager();
            if(venueManager != null) {
                invitationListener = listener;
                if(invitationListener != null) {
                    // Do we already have one set?
                    if(intInvitationListener != null) {
                        venueManager.removeInvitationListener(intInvitationListener);
                    }
                    intInvitationListener = new IChatRoomInvitationListener() {
                        @Override
                        public void handleInvitationReceived(ID roomID, ID from,
                                String subject, String body) {
                            invitationListener.handleInvitation(null, null, subject, body);
                        }
                    };
                    venueManager.addInvitationListener(intInvitationListener);
                }
            } else {
                // Could not create venueManager
            }
        } else {
            // not currently connected
        }
        return listener;
    }
    
    public IVenueInvitationListener removeVenueInvitationListener(IVenueInvitationListener listener) {
        connectToContainer();
        IPresenceContainerAdapter presence = (IPresenceContainerAdapter) container.getAdapter(IPresenceContainerAdapter.class);
        IChatRoomManager venueManager = presence.getChatRoomManager();
        
        invitationListener = listener;
        if(invitationListener != null) {
            venueManager.removeInvitationListener(intInvitationListener);
        }
        return listener;
    }
    
    
    
    
    
}
