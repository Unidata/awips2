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
package com.raytheon.uf.viz.collaboration.display.data;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.comm.identity.user.SharedDisplayRole;
import com.raytheon.uf.viz.collaboration.display.roles.DataProviderEventController;
import com.raytheon.uf.viz.collaboration.display.roles.IRoleEventController;
import com.raytheon.uf.viz.collaboration.display.roles.ParticipantEventController;

/**
 * Tracks all of the active sessions that are SharedDisplaySessions. Provides
 * SessionContainers that contain the data related to those sessions.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 16, 2012            njensen     Initial creation
 * Jan 28, 2014 2698       bclement    removed false throws statement
 * Feb 12, 2014 2751       njensen     Register session containers to session event bus
 * Mar 07, 2014 2848       bclement    split event handler registration from joinSession() to registerSession()
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class SharedDisplaySessionMgr {

    private static Map<String, SessionContainer> sharedDisplaySessionMap = new HashMap<String, SessionContainer>();

    public static SessionContainer getSessionContainer(String sessionId) {
        return sharedDisplaySessionMap.get(sessionId);
    }

    public static Set<String> getActiveSessionIds() {
        return sharedDisplaySessionMap.keySet();
    }

    /**
     * Add a session to the manager and register listeners with session event
     * bus.
     * 
     * @param session
     * @param initialRole
     */
    public static void registerSession(ISharedDisplaySession session,
            SharedDisplayRole initialRole) {
        SessionContainer container = new SessionContainer();
        container.setSessionId(session.getSessionId());
        container.setSession(session);

        IRoleEventController rec = null;
        switch (initialRole) {
        case DATA_PROVIDER:
            rec = new DataProviderEventController(session);
            break;
        case PARTICIPANT:
            rec = new ParticipantEventController(session);
            break;
        default:
            throw new IllegalArgumentException(
                    "ParticipantRole must be DataProvider or Participant for initialization");
        }
        container.setRoleEventController(rec);
        sharedDisplaySessionMap.put(session.getSessionId(), container);
        session.registerEventHandler(container);
    }

    /**
     * Join a session registered in the manager
     * 
     * @param sessionId
     */
    public static void joinSession(String sessionId) {
        SessionContainer container = sharedDisplaySessionMap.get(sessionId);
        if (container != null) {
            container.getRoleEventController().startup();
        }
    }

    /**
     * Removes a session from the manager.
     * 
     * @param sessionId
     */
    public static void exitSession(String sessionId) {
        SessionContainer container = sharedDisplaySessionMap.get(sessionId);
        if (container != null) {
            container.getSession().unregisterEventHandler(container);
            container.getRoleEventController().shutdown();
        }

        // remove after shutting down event controller
        sharedDisplaySessionMap.remove(sessionId);
    }
}
