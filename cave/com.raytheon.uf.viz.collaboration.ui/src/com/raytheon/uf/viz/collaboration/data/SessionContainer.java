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
package com.raytheon.uf.viz.collaboration.data;

import java.util.List;

import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.display.editor.CollaborationEditor;
import com.raytheon.uf.viz.collaboration.ui.SessionColorManager;
import com.raytheon.uf.viz.collaboration.ui.role.IRoleEventController;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * A container holding an underlying session and associated data for a shared
 * display session.
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

public class SessionContainer {

    /** the session id **/
    private String sessionId;

    /** the underlying session object **/
    private ISharedDisplaySession session;

    /** subscribes to events related to the session based on role **/
    private IRoleEventController roleEventController;

    private SessionColorManager colorManager;

    /**
     * the editor associated with a session, only valid if not fulfilling the
     * Data Provider role
     **/
    private CollaborationEditor collaborationEditor;

    /**
     * the editors shared with a session, only valid if fulfilling the Data
     * Provider role
     **/
    private List<AbstractEditor> sharedEditors;

    public ISharedDisplaySession getSession() {
        return session;
    }

    public void setSession(ISharedDisplaySession session) {
        this.session = session;
    }

    public IRoleEventController getRoleEventController() {
        return roleEventController;
    }

    public void setRoleEventController(IRoleEventController roleEventController) {
        this.roleEventController = roleEventController;
    }

    public CollaborationEditor getCollaborationEditor() {
        return collaborationEditor;
    }

    public void setCollaborationEditor(CollaborationEditor collaborationEditor) {
        if (this.sharedEditors != null) {
            throw new IllegalStateException(
                    "Cannot have both a "
                            + "CollaborationEditor and shared editors on the same session at the same time");
        }
        this.collaborationEditor = collaborationEditor;
    }

    public List<AbstractEditor> getSharedEditors() {
        return sharedEditors;
    }

    public void setSharedEditors(List<AbstractEditor> sharedEditors) {
        if (this.collaborationEditor != null) {
            throw new IllegalStateException(
                    "Cannot have both a "
                            + "CollaborationEditor and shared editors on the same session at the same time");
        }
        this.sharedEditors = sharedEditors;
    }

    public String getSessionId() {
        return sessionId;
    }

    public void setSessionId(String sessionId) {
        this.sessionId = sessionId;
    }

    /**
     * @return the colorManager
     */
    public SessionColorManager getColorManager() {
        if (colorManager == null) {
            colorManager = new SessionColorManager();
        }
        return colorManager;
    }

    /**
     * @param colorManager
     *            the colorManager to set
     */
    public void setColorManager(SessionColorManager colorManager) {
        this.colorManager = colorManager;
    }
}
