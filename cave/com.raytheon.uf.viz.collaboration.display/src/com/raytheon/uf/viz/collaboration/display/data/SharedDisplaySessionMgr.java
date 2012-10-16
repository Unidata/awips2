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

import org.eclipse.ui.IEditorPart;

import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
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

    public static void joinSession(ISharedDisplaySession session,
            SharedDisplayRole initialRole, SessionColorManager colors)
            throws CollaborationException {
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
        if (colors != null) {
            container.setColorManager(colors);
        }
        sharedDisplaySessionMap.put(session.getSessionId(), container);

        rec.startup();
    }

    /**
     * Removes a session from the manager.
     * 
     * @param sessionId
     */
    public static void exitSession(String sessionId) {
        SessionContainer container = sharedDisplaySessionMap.get(sessionId);
        if (container != null) {
            container.getRoleEventController().shutdown();

            // remove after shutting down event controller
            sharedDisplaySessionMap.remove(sessionId);
        }
    }
}
