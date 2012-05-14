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

import java.util.ArrayDeque;
import java.util.Arrays;
import java.util.Deque;

import org.eclipse.swt.widgets.Event;

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.compression.CompressionUtil;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.collaboration.ui.role.dataprovider.event.IPersistedEvent;
import com.raytheon.uf.viz.collaboration.ui.role.dataprovider.event.IRenderFrameEvent;
import com.raytheon.uf.viz.collaboration.ui.role.dataprovider.event.MouseLocationEvent;
import com.raytheon.uf.viz.collaboration.ui.role.dataprovider.event.RenderFrame;
import com.raytheon.uf.viz.collaboration.ui.role.dataprovider.event.RenderFrameNeededEvent;
import com.raytheon.uf.viz.collaboration.ui.rsc.ParticipantInitializedEvent;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.jobs.JobPool;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.remote.graphics.Dispatcher;
import com.raytheon.uf.viz.remote.graphics.events.AbstractDispatchingObjectEvent;
import com.raytheon.uf.viz.remote.graphics.events.AbstractRemoteGraphicsEvent;
import com.raytheon.uf.viz.remote.graphics.events.ICreationEvent;
import com.raytheon.uf.viz.remote.graphics.events.rendering.BeginFrameEvent;
import com.raytheon.uf.viz.remote.graphics.events.rendering.EndFrameEvent;
import com.raytheon.uf.viz.remote.graphics.events.rendering.IRenderEvent;
import com.raytheon.viz.ui.input.InputAdapter;

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

    private static final int IMMEDIATE_SEND_SIZE = 1024;

    private static final int MAX_PREV_FRAMES = 10;

    private ISharedDisplaySession session;

    private IObjectEventPersistance persistance;

    private Deque<RenderFrame> previousFrames = new ArrayDeque<RenderFrame>();

    private RenderFrame currentFrame;

    private IInputHandler mouseCapturer = new InputAdapter() {

        private double[] mouseLocation;

        @Override
        public boolean handleMouseMove(int x, int y) {
            IDisplayPaneContainer container = display.getContainer();
            for (IDisplayPane pane : container.getDisplayPanes()) {
                if (pane.getRenderableDisplay() == display) {
                    update(display.screenToGrid(x, y, 1.0, pane.getTarget()));
                    break;
                }
            }
            return false;
        }

        @Override
        public boolean handleMouseExit(Event event) {
            update(null);
            return false;
        }

        @Override
        public boolean handleMouseEnter(Event event) {
            handleMouseMove(event.x, event.y);
            return false;
        }

        private void update(double[] mouseLocation) {
            if (!Arrays.equals(mouseLocation, this.mouseLocation)) {
                this.mouseLocation = mouseLocation;
                MouseLocationEvent event = new MouseLocationEvent();
                event.setDisplayId(getDispatcherId());
                event.setMouseLocation(mouseLocation);
                send(event);
            }
        }
    };

    private IRenderableDisplay display;

    public CollaborationDispatcher(ISharedDisplaySession session,
            IRenderableDisplay display) throws CollaborationException {
        this.session = session;
        this.display = display;
        this.persistance = CollaborationObjectEventStorage
                .createPersistanceObject(session);
        session.registerEventHandler(this);
        // TODO:
        // Not sure if we should show mouse location or not due to
        // bandwidth, messages are very small but frequent
        // display.getContainer().registerMouseHandler(mouseCapturer);
    }

    @Subscribe
    public void handleNeedsRenderFrameEvent(RenderFrameNeededEvent event) {
        synchronized (previousFrames) {
            RenderFrame needed = null;
            for (RenderFrame prevFrame : previousFrames) {
                if (prevFrame.getObjectId() == event.getObjectId()) {
                    needed = prevFrame;
                }
            }
            if (needed != null) {
                // Remove and dispose to reset frame
                previousFrames.remove(needed);
                needed.dispose();
                display.refresh();
            }
        }
    }

    @Subscribe
    public void handleParticipantInitialized(ParticipantInitializedEvent event) {
        // Force repaint when participant initializes
        display.refresh();
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
        if (eventObject instanceof IRenderFrameEvent) {
            send(eventObject);
            return;
        }
        // Set PERSISTENCE to true if testing persisting capabilities
        if (eventObject instanceof AbstractDispatchingObjectEvent
                && eventObject instanceof IRenderEvent == false) {
            boolean immediateSend = true;
            if (eventObject instanceof ICreationEvent == false) {
                // Not a creation event, check event size. All creation events
                // are sent immediately to avoid false negatives
                try {
                    byte[] data = CompressionUtil.compress(SerializationUtil
                            .transformToThrift(eventObject));
                    if (data.length > IMMEDIATE_SEND_SIZE) {
                        immediateSend = false;
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
                currentFrame = new RenderFrame(this);
            }

            currentFrame.addEvent((IRenderEvent) eventObject);
            if (eventObject instanceof EndFrameEvent) {
                synchronized (previousFrames) {
                    // End frame, do some smart rendering
                    RenderFrame mergeWith = null;
                    for (RenderFrame prevFrame : previousFrames) {
                        // Try and find a previous frame with same number of
                        // frames
                        if (prevFrame.size() == currentFrame.size()) {
                            mergeWith = prevFrame;
                            break;
                        }
                    }

                    boolean merged = false;
                    if (mergeWith != null) {
                        // Move merge frame to front of queue
                        previousFrames.remove(mergeWith);
                        previousFrames.addFirst(mergeWith);

                        if (mergeWith.merge(currentFrame)) {
                            // Mark successfull merge
                            merged = true;
                            // Reset current frame
                            currentFrame.clear();
                        }
                    }

                    if (!merged) {
                        // Send full frame if no merge happened
                        if (mergeWith != null) {
                            // Notify we tried to merge but failed
                            Activator.statusHandler.handle(Priority.PROBLEM,
                                    "Attempted merge did not succeed");
                        }

                        // Send current frame
                        currentFrame.sendFrame();
                        if (previousFrames.size() == MAX_PREV_FRAMES) {
                            // Too many frames, remove and dispose last
                            previousFrames.removeLast().dispose();
                        }
                        previousFrames.addFirst(currentFrame);
                        currentFrame = new RenderFrame(this);
                    }
                }
            }
        } else {
            Activator.statusHandler.handle(Priority.PROBLEM,
                    "Unable to handle event type: " + eventObject);
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
        for (RenderFrame frame : previousFrames) {
            frame.dispose();
        }
        previousFrames.clear();
        session.unRegisterEventHandler(this);
    }
}
