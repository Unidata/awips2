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

import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.display.IRemoteDisplayContainer;
import com.raytheon.uf.viz.collaboration.display.roles.IRoleEventController;

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

    private IRemoteDisplayContainer displayContainer;

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

    /**
     * @return the displayContainer
     */
    public IRemoteDisplayContainer getDisplayContainer() {
        return displayContainer;
    }

    /**
     * @param displayContainer
     *            the displayContainer to set
     */
    public void setDisplayContainer(IRemoteDisplayContainer displayContainer) {
        this.displayContainer = displayContainer;
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
