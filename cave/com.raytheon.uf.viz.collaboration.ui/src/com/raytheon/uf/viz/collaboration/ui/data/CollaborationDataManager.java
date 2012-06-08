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

import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.identity.user.SharedDisplayRole;
import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;

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
        }
        return sessionId;
    }

}
