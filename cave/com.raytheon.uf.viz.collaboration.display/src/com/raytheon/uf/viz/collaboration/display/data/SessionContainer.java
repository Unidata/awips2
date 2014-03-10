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

import java.util.ArrayList;
import java.util.List;

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.comm.identity.invite.ColorPopulator;
import com.raytheon.uf.viz.collaboration.comm.provider.event.LeaderChangeEvent;
import com.raytheon.uf.viz.collaboration.comm.provider.user.VenueParticipant;
import com.raytheon.uf.viz.collaboration.display.IRemoteDisplayContainer;
import com.raytheon.uf.viz.collaboration.display.roles.DataProviderEventController;
import com.raytheon.uf.viz.collaboration.display.roles.IRoleEventController;
import com.raytheon.uf.viz.collaboration.display.roles.ParticipantEventController;
import com.raytheon.uf.viz.core.VizApp;

/**
 * A container holding an underlying session and associated data for a shared
 * display session.
 * 
 * The container is also capable of listening for leader change events and if
 * the remote display is changed.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 16, 2012            njensen     Initial creation
 * Feb 11, 2014 2751       njensen     Added leaderChanged() and listeners
 * Mar 07, 2014 2848       bclement    made colorManager final, added modifyColors() listeners
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

    private final SessionColorManager colorManager = new SessionColorManager();

    private IRemoteDisplayContainer displayContainer;

    private List<IDisplayContainerChangedListener> listeners = new ArrayList<IDisplayContainerChangedListener>();

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
        IRemoteDisplayContainer old = this.displayContainer;
        this.displayContainer = displayContainer;
        fireDisplayContainerChanged(old, displayContainer);
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
        return colorManager;
    }


    @Subscribe
    public void leaderChanged(LeaderChangeEvent event) {
        // for now (and possibly forever) we are not allowing capabilities to be
        // transferred separately
        VenueParticipant newLeader = event.getNewLeader();
        VenueParticipant oldLeader = event.getOldLeader();

        if (session.getUserID().isSameUser(oldLeader)) {
            // just gave up our leadership
            final IRoleEventController formerRole = getRoleEventController();
            if (!(formerRole instanceof DataProviderEventController)) {
                throw new IllegalStateException(
                        "Shared Display Session "
                                + session.getVenueName()
                                + " attempted to surrender leadership when it's not the leader!");
            }
            VizApp.runSync(new Runnable() {
                @Override
                public void run() {
                    formerRole.shutdown();
                    IRoleEventController newRole = new ParticipantEventController(
                            session);
                    setRoleEventController(newRole);
                    newRole.startup();
                }
            });
        }

        if (session.getUserID().isSameUser(newLeader)) {
            // just received leadership
            final IRoleEventController formerRole = getRoleEventController();
            if (!(formerRole instanceof ParticipantEventController)) {
                throw new IllegalStateException(
                        "Shared Display Session "
                                + session.getVenueName()
                                + " attempted to acquire leadership when it wasn't a participant!");
            }
            VizApp.runSync(new Runnable() {
                @Override
                public void run() {
                    formerRole.shutdown();
                    IRoleEventController newRole = new DataProviderEventController(
                            session);
                    setRoleEventController(newRole);
                    newRole.startup();
                }
            });
        }
    }

    @Subscribe
    public void modifyColors(ColorPopulator populator) {
        colorManager.setColors(populator.getColors());
    }

    @Subscribe
    public void modifyColors(ColorChangeEvent event) {
        colorManager.setColorForUser(event.getUserName(), event.getColor());
    }

    /**
     * Interface for listening if a remote display container associated with a
     * session changed. This only applies if the reference to a remote display
     * container changed, not if the remote display container's attributes
     * changed. This should only happen if leadership changes.
     */
    public static interface IDisplayContainerChangedListener {

        /**
         * Notifies that the display container has changed. This only applies if
         * a reference associated with the session changed, not if an attribute
         * on the same instance changed.
         * 
         * @param oldDisplayContainer
         *            the old display container, possibly null
         * @param newDisplayContainer
         *            the new display container, possibly null
         */
        public void displayContainerChanged(
                IRemoteDisplayContainer oldDisplayContainer,
                IRemoteDisplayContainer newDisplayContainer);
    }

    public void addDisplayContainerChangedListener(
            IDisplayContainerChangedListener listener) {
        synchronized (listeners) {
            listeners.add(listener);
        }
    }

    public void removeDisplayContainerChangedListener(
            IDisplayContainerChangedListener listener) {
        synchronized (listeners) {
            listeners.remove(listener);
        }
    }

    private void fireDisplayContainerChanged(
            IRemoteDisplayContainer oldDisplay,
            IRemoteDisplayContainer newDisplay) {
        synchronized (listeners) {
            for (IDisplayContainerChangedListener listener : listeners) {
                listener.displayContainerChanged(oldDisplay, newDisplay);
            }
        }
    }

}
