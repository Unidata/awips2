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
package com.raytheon.uf.viz.collaboration.comm;

import java.util.ArrayList;
import java.util.Collection;

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
import org.eclipse.ecf.presence.chatroom.IChatRoomManager;

import com.raytheon.uf.viz.collaboration.comm.identity.ISession;
import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.identity.info.IVenueInfo;
import com.raytheon.uf.viz.collaboration.comm.provider.CollaborationSession;
import com.raytheon.uf.viz.collaboration.comm.provider.info.InfoAdapter;

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

    public static final String SESSION_P2P = "peertopeer";

    public static final String SESSION_COLLABORATION = "collaboration";
    
    public static final String SESSION_CHAT_ONLY = "chatOnly";
    
    public static final String PROVIDER = "ecf.xmpp.smack";
    
    private String account;
    
    private String password;
    
    private IContainer container = null;
    
    
    /**
     * @throws ContainerCreateException 
     * 
     */
    public SessionManager(String account, String password) throws Exception {
        container = ContainerFactory.getDefault().createContainer(PROVIDER);
        this.account = account;
        this.password = password;
    }

    

    
    /**
     * 
     * @return
     */
    public ISession createPeerToPeerSession() {
        return (ISession) createSession(SESSION_P2P);
    }
    
    /**
     * 
     * @return
     */
    public IVenueSession createChatOnlySession() {
        return (IVenueSession) createSession(SESSION_CHAT_ONLY);
    }
    
    
    /**
     * 
     * @return
     */
    public IVenueSession createCollaborationSession() {
        return (IVenueSession) createSession(SESSION_COLLABORATION);
    }

    /**
     * 
     * @param sessionKind
     * @return
     */
    public ISession createSession(String sessionKind) {

        ISession session = null;
        if(sessionKind != null) {
            
            if(SESSION_P2P.equals(sessionKind)) {
                System.out.println(sessionKind + " Not currently implemented");
            } else if(SESSION_COLLABORATION.equals(sessionKind)) {
                session = new CollaborationSession(container);
            } else if(SESSION_CHAT_ONLY.equals(sessionKind)) {
                System.out.println(sessionKind + " Not currently implemented");
            } else {
                System.out.println(sessionKind + " is not a valid session kind.");
            }
        }
        if(session != null) {
            session.connect(account, password);
        } else {
            System.out.println("Could not connect session");
        }
        return session;
    }
    
    /**
     * 
     * @return
     */
    public Collection<IVenueInfo> getVenueInfo() {
        // Check to see if the container has been connected. If no, do so
        connectToContainer();
        IPresenceContainerAdapter presence = (IPresenceContainerAdapter) container.getAdapter(IPresenceContainerAdapter.class);
        IChatRoomManager venueManager = presence.getChatRoomManager();
        
        Collection<IVenueInfo> info = new ArrayList<IVenueInfo>();
        if(venueManager != null) {
            IChatRoomInfo [] roomInfo = venueManager.getChatRoomInfos();
            for(IChatRoomInfo rInfo : roomInfo) {
                IVenueInfo vi = InfoAdapter.createVenueInfo(rInfo);
                if(vi != null) {
                    info.add(vi);
                }
            }
        }
        
        return info;
    }

    
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
    
    /**
     * 
     */
    public void closeManager() {
        if(container != null) {
            container.dispose();
        }
    }
    
    
    
    
    
}
