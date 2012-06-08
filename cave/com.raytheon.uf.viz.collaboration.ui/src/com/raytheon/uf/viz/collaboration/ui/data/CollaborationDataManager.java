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
package com.raytheon.uf.viz.collaboration.ui.data;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.Assert;
import org.eclipse.ecf.presence.IPresence;
import org.eclipse.ecf.presence.IPresence.Mode;
import org.eclipse.ecf.presence.IPresence.Type;
import org.eclipse.ecf.presence.Presence;
import org.eclipse.ecf.presence.roster.IRosterManager;
import org.eclipse.ecf.presence.roster.RosterEntry;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchListener;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.identity.user.SharedDisplayRole;
import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;

/**
 * A single class that contains all data information.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 1, 2012            rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class CollaborationDataManager {
    private static CollaborationDataManager instance;

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CollaborationDataManager.class);

    /**
     * The connection to the server.
     */
    private CollaborationConnection connection;

    Shell shell;

    /**
     * Created when connection made. Used to clean up connection when CAVE shuts
     * down.
     */
    private IWorkbenchListener wbListener;

    /**
     * Mapping for all active chat sessions.
     */
    Map<String, IVenueSession> sessionsMap;

    public static CollaborationDataManager getInstance() {
        if (instance == null) {
            instance = new CollaborationDataManager();
        }
        return instance;
    }

    /**
     * Private constructor to for singleton class.
     */
    private CollaborationDataManager() {
        sessionsMap = new HashMap<String, IVenueSession>();
    }

    /**
     * Get the Venue session associated with the key or any session when key is
     * null.
     * 
     * @param sessionId
     *            - key to fetch session
     * @return session - The venue session or null if none found
     */
    public IVenueSession getSession(String sessionId) {
        Assert.isNotNull(sessionId,
                "getSession should never be passed a null sessionId");
        // IVenueSession session = null;
        // if (sessionId == null) {
        // if (sessionsMap.size() > 0) {
        // session = sessionsMap.get(sessionsMap.keySet().toArray()[0]);
        // }
        // } else {
        // session = sessionsMap.get(sessionId);
        // }
        // return session;
        return sessionsMap.get(sessionId);
    }

    public Map<String, IVenueSession> getSessions() {
        return sessionsMap;
    }

    /**
     * Closes connection to the session.
     * 
     * @param sessionId
     */
    public void closeSession(String sessionId) {
        IVenueSession session = sessionsMap.get(sessionId);
        if (session != null) {
            sessionsMap.remove(sessionId);
            session.close();
        }
    }

    /**
     * Generate a new session.
     * 
     * @param venue
     *            - Session name
     * @param subject
     *            - Session topic
     * @return sessionId - the key to use to retrieve the sesson or null if
     *         unable to create the session
     */
    public String createCollaborationSession(String venue, String subject)
            throws CollaborationException {
        CollaborationConnection connection = CollaborationConnection
                .getConnection();
        IVenueSession session = null;
        String sessionId = null;
        // try {
        session = connection.createCollaborationVenue(venue, subject);
        // sessionId = venueIdToSessionId(session.getVenue().getInfo()
        // .getVenueID());
        sessionId = session.getSessionId();
        // TODO throw an exception if unable to make connection?
        if (session.isConnected()) {
            ISharedDisplaySession displaySession = (ISharedDisplaySession) session;
            sessionsMap.put(sessionId, session);
            SharedDisplaySessionMgr.joinSession(displaySession,
                    SharedDisplayRole.DATA_PROVIDER, null);
        }

        return sessionId;
    }

    public String createTextOnlySession(String venueName, String subject)
            throws CollaborationException {
        CollaborationConnection connection = CollaborationConnection
                .getConnection();
        IVenueSession session = null;
        String sessionId = null;
        session = connection.createTextOnlyVenue(venueName, subject);
        if (session.isConnected()) {
            sessionId = session.getSessionId();
            sessionsMap.put(sessionId, session);
        }
        return sessionId;
    }

    public boolean isConnected() {
        return connection != null && connection.isConnected();
    }

    public void fireModifiedPresence(Mode mode, String msg) {
        IRosterManager manager = connection.getRosterManager();
        IPresence presence = new Presence(Type.AVAILABLE, msg, mode);

        try {
            connection.getAccountManager().sendPresence(presence);
            UserId id = connection.getUser();
            RosterEntry rosterEntry = new RosterEntry(manager.getRoster(), id,
                    presence);
            rosterEntry.setPresence(presence);
            connection.getEventPublisher().post(rosterEntry);
        } catch (CollaborationException e) {
            // TODO Auto-generated catch block. Please revise as
            // appropriate.
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    // TODO remove as this is temporary to fix a problem and get in a good state
    public void addSession(String sessionId, IVenueSession session) {
        sessionsMap.put(sessionId, session);
    }

}
