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
package com.raytheon.uf.viz.collaboration.ui.role.event;

import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.core.jobs.JobPool;
import com.raytheon.uf.viz.remote.graphics.AbstractRemoteGraphicsEvent;
import com.raytheon.uf.viz.remote.graphics.Dispatcher;
import com.raytheon.uf.viz.remote.graphics.events.AbstractDispatchingObjectEvent;
import com.raytheon.uf.viz.remote.graphics.events.ICreationEvent;
import com.raytheon.uf.viz.remote.graphics.events.IRenderEvent;

/**
 * Dispatches graphics objects to participants in the collaboration session
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 19, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class CollaborationDispatcher extends Dispatcher {

    private static JobPool persistPool = new JobPool("Persister", 1, true);

    private ISharedDisplaySession session;

    private IObjectEventPersistance persistance;

    public CollaborationDispatcher(ISharedDisplaySession session)
            throws CollaborationException {
        this.session = session;
        this.persistance = CollaborationObjectEventStorage
                .createPersistanceObject(session);
    }

    private static final boolean PERSISTENCE = false;

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.remote.graphics.Dispatcher#dispatch(com.raytheon.
     * uf.viz.remote.graphics.AbstractRemoteGraphicsEvent)
     */
    @Override
    public void dispatch(AbstractRemoteGraphicsEvent eventObject) {
        // Set PERSISTENCE to true if testing persisting capabilities
        if (PERSISTENCE
                && eventObject instanceof AbstractDispatchingObjectEvent
                && eventObject instanceof IRenderEvent == false) {
            final AbstractDispatchingObjectEvent toPersist = (AbstractDispatchingObjectEvent) eventObject;
            persistPool.schedule(new Runnable() {
                @Override
                public void run() {
                    try {
                        IPersistedEvent event = persistance
                                .persistEvent(toPersist);
                        // Creation events are sent immediately to avoid false
                        // negatives
                        if (toPersist instanceof ICreationEvent == false) {
                            send(event);
                        }
                    } catch (CollaborationException e) {
                        Activator.statusHandler.handle(Priority.PROBLEM,
                                e.getLocalizedMessage(), e);
                    }
                }
            });
            if (eventObject instanceof ICreationEvent) {
                send(eventObject);
            }
        } else {
            send(eventObject);
        }
    }

    private void send(Object obj) {
        try {
            session.sendObjectToVenue(obj);
        } catch (CollaborationException e) {
            Activator.statusHandler.handle(Priority.PROBLEM,
                    e.getLocalizedMessage(), e);
        }
    }

    public void dispose() {
        try {
            persistance.dispose();
        } catch (CollaborationException e) {
            Activator.statusHandler.handle(Priority.PROBLEM,
                    e.getLocalizedMessage(), e);
        }
    }
}
