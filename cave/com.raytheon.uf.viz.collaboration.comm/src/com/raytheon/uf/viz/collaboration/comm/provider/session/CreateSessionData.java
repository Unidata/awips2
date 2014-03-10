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
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class CreateSessionData {
    private String name;

    private String handle;

    private String subject;

    private boolean inviteUsers;

    private boolean collaborationSession;

    private String inviteMessage;

    private String sessionId;

    /**
     * @param name
     *            name of session venue
     * @param handle
     *            name user is known by in venue
     */
    public CreateSessionData(String name, String handle) {
        this.name = name;
        this.handle = handle;
        this.collaborationSession = false;
    }

    public String getSessionId() {
        return sessionId;
    }

    public void setSessionId(String sessionId) {
        this.sessionId = sessionId;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
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
