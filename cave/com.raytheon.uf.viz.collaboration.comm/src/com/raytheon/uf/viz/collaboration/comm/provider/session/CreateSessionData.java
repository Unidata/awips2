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

import com.raytheon.uf.viz.collaboration.comm.provider.user.VenueId;

/**
 * Configuration used to create a new session
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 7, 2012            rferrel     Initial creation
 * Jan 30, 2014 2698       bclement    moved to collaboration.comm project from collaboration.ui
 *                                     added handle
 * Mar 10, 2014 2848       bclement    added constructor with required fields
 * Jun 16, 2014 3288       bclement    changed String venueName to VenueId venueId
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class CreateSessionData {
    private VenueId venueId;

    private String handle;

    private String subject;

    private boolean inviteUsers;

    private boolean collaborationSession;

    private String inviteMessage;

    private String sessionId;

    /**
     * @param id
     *            id of session venue
     * @param handle
     *            name user is known by in venue
     */
    public CreateSessionData(VenueId venueId, String handle) {
        this.venueId = venueId;
        this.handle = handle;
        this.collaborationSession = false;
    }

    public String getSessionId() {
        return sessionId;
    }

    public void setSessionId(String sessionId) {
        this.sessionId = sessionId;
    }

    /**
     * @return
     */
    public VenueId getVenueId() {
        return venueId;
    }

    /**
     * @param venueId
     */
    public void setVenueId(VenueId venueId) {
        this.venueId = venueId;
    }

    /**
     * @return the handle
     */
    public String getHandle() {
        return handle;
    }

    /**
     * @param handle
     *            the handle to set
     */
    public void setHandle(String handle) {
        this.handle = handle;
    }

    public String getSubject() {
        return subject;
    }

    public void setSubject(String subject) {
        this.subject = subject;
    }

    public boolean isCollaborationSession() {
        return collaborationSession;
    }

    public boolean isInviteUsers() {
        return inviteUsers;
    }

    public void setCollaborationSessioh(boolean collaborationSession) {
        this.collaborationSession = collaborationSession;
    }

    public void setInviteUsers(boolean inviteUsers) {
        this.inviteUsers = inviteUsers;
    }

    public String getInviteMessage() {
        return inviteMessage;
    }

    public void setInviteMessage(String inviteMessage) {
        this.inviteMessage = inviteMessage;
    }
}
