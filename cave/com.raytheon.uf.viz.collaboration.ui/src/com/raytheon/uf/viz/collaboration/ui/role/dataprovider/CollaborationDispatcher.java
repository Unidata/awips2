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
package com.raytheon.uf.viz.collaboration.ui.role.dataprovider;

import java.util.ArrayList;

import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.comm.provider.Tools;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.collaboration.ui.role.dataprovider.event.IPersistedEvent;
import com.raytheon.uf.viz.collaboration.ui.role.dataprovider.event.RenderFrameEvent;
import com.raytheon.uf.viz.core.jobs.JobPool;
import com.raytheon.uf.viz.remote.graphics.AbstractRemoteGraphicsEvent;
import com.raytheon.uf.viz.remote.graphics.Dispatcher;
import com.raytheon.uf.viz.remote.graphics.DispatchingObject;
import com.raytheon.uf.viz.remote.graphics.events.AbstractDispatchingObjectEvent;
import com.raytheon.uf.viz.remote.graphics.events.BeginFrameEvent;
import com.raytheon.uf.viz.remote.graphics.events.EndFrameEvent;
import com.raytheon.uf.viz.remote.graphics.events.ICreationEvent;
import com.raytheon.uf.viz.remote.graphics.events.IRenderEvent;
import com.raytheon.uf.viz.remote.graphics.events.RemoteGraphicsEventFactory;

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

    private static final boolean PERSISTENCE = true;

    private static final int IMMEDIATE_SEND_SIZE = 1024;

    private ISharedDisplaySession session;

    private IObjectEventPersistance persistance;

    private DispatchingObject<?> frameObject;

    private RenderFrameEvent previousFrame;

    private RenderFrameEvent currentFrame;

    public CollaborationDispatcher(ISharedDisplaySession session)
            throws CollaborationException {
        this.session = session;
        this.persistance = CollaborationObjectEventStorage
                .createPersistanceObject(session);
    }

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
            boolean immediateSend = true;
            if (eventObject instanceof ICreationEvent == false) {
                // Not a creation event, check event size. All creation events
                // are sent immediately to avoid false negatives
                try {
                    byte[] data = Tools.compress(SerializationUtil
                            .transformToThrift(eventObject));
                    if (data.length > IMMEDIATE_SEND_SIZE) {
                        System.err.println("Object: "
                                + eventObject.getClass().getSimpleName()
                                + " is too large to send immediately, size is "
                                + data.length + " bytes");
                        immediateSend = false;
                    } else {
                        System.out
                                .println("Object: "
                                        + eventObject.getClass()
                                                .getSimpleName()
                                        + " is small enough to send immediately, size is "
                                        + data.length + " bytes");
                    }
                } catch (Exception e) {
                    Activator.statusHandler.handle(Priority.PROBLEM,
                            "Error determing size of eventObject: "
                                    + eventObject, e);
                }
            }
            final AbstractDispatchingObjectEvent toPersist = (AbstractDispatchingObjectEvent) eventObject;
            final boolean[] sendPersisted = new boolean[] { !immediateSend };
            persistPool.schedule(new Runnable() {
                @Override
                public void run() {
                    try {
                        IPersistedEvent event = persistance
                                .persistEvent(toPersist);
                        // If was no immediateSend, send now
                        if (sendPersisted[0]) {
                            send(event);
                        }
                    } catch (CollaborationException e) {
                        Activator.statusHandler.handle(Priority.PROBLEM,
                                e.getLocalizedMessage(), e);
                    }
                }
            });
            // Need to immediately send eventObject
            if (immediateSend) {
                send(eventObject);
            }
        } else if (eventObject instanceof IRenderEvent) {
            if (eventObject instanceof BeginFrameEvent && currentFrame == null) {
                frameObject = new DispatchingObject<Object>(null, this);
                currentFrame = RemoteGraphicsEventFactory.createEvent(
                        RenderFrameEvent.class, frameObject);
            }

            currentFrame.addEvent((IRenderEvent) eventObject);
            if (eventObject instanceof EndFrameEvent) {
                if (currentFrame.equals(previousFrame)) {
                    // No change in render, send event to render
                    send(createRenderFrameEvent());
                    currentFrame.getRenderEvents().clear();
                } else {
                    // New info, send current frame with new render events as
                    // new object to avoid unwanted data pointer manipulation
                    RenderFrameEvent frameEvent = RemoteGraphicsEventFactory
                            .createEvent(RenderFrameEvent.class, frameObject);
                    frameEvent.setRenderEvents(new ArrayList<IRenderEvent>(
                            currentFrame.getRenderEvents()));
                    send(frameEvent);
                    previousFrame = currentFrame;
                    currentFrame = createRenderFrameEvent();
                }
            }
        } else {
            Activator.statusHandler.handle(Priority.PROBLEM,
                    "Unable to handle event type: " + eventObject);
        }
    }

    private RenderFrameEvent createRenderFrameEvent() {
        return RemoteGraphicsEventFactory.createEvent(RenderFrameEvent.class,
                frameObject);
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
