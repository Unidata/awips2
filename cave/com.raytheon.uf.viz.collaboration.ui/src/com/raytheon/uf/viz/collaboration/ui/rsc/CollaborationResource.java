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
package com.raytheon.uf.viz.collaboration.ui.rsc;

import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuManager;

import com.google.common.eventbus.EventBus;
import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.collaboration.ui.role.dataprovider.event.FrameDisposed;
import com.raytheon.uf.viz.collaboration.ui.role.dataprovider.event.IPersistedEvent;
import com.raytheon.uf.viz.collaboration.ui.role.dataprovider.event.IRenderFrameEvent;
import com.raytheon.uf.viz.collaboration.ui.role.dataprovider.event.MouseLocationEvent;
import com.raytheon.uf.viz.collaboration.ui.role.dataprovider.event.RenderFrameEvent;
import com.raytheon.uf.viz.collaboration.ui.role.dataprovider.event.RenderFrameNeededEvent;
import com.raytheon.uf.viz.collaboration.ui.role.dataprovider.event.UpdateRenderFrameEvent;
import com.raytheon.uf.viz.collaboration.ui.rsc.rendering.CollaborationRenderingDataManager;
import com.raytheon.uf.viz.collaboration.ui.rsc.rendering.CollaborationRenderingHandler;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.jobs.JobPool;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.RenderingOrderFactory.ResourceOrder;
import com.raytheon.uf.viz.remote.graphics.events.AbstractDispatchingObjectEvent;
import com.raytheon.uf.viz.remote.graphics.events.AbstractRemoteGraphicsEvent;
import com.raytheon.uf.viz.remote.graphics.events.rendering.BeginFrameEvent;
import com.raytheon.uf.viz.remote.graphics.events.rendering.EndFrameEvent;
import com.raytheon.uf.viz.remote.graphics.events.rendering.IRenderEvent;
import com.raytheon.viz.ui.cmenu.IContextMenuProvider;

/**
 * A resource for displaying rendered data that is received from the Data
 * Provider.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 3, 2012            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class CollaborationResource extends
        AbstractVizResource<CollaborationResourceData, IDescriptor> implements
        IContextMenuProvider {

    private static JobPool retrievePool = new JobPool("Retriever", 1, true);

    /** List of objects rendered in paint */
    private List<IRenderEvent> currentRenderables;

    /** Active list of renderable events to add to */
    private List<IRenderEvent> activeRenderables;

    /** Queue of graphics data update events to process in paint */
    private List<AbstractDispatchingObjectEvent> dataChangeEvents;

    /** Internal rendering event object router */
    private EventBus renderingRouter;

    private CollaborationRenderingDataManager dataManager;

    private MouseLocationEvent latestMouseLocation;

    private Set<Integer> waitingOnObjects = new HashSet<Integer>();

    private Set<Integer> waitingOnFrames = new HashSet<Integer>();

    public CollaborationResource(CollaborationResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        dataChangeEvents = new LinkedList<AbstractDispatchingObjectEvent>();
        currentRenderables = new LinkedList<IRenderEvent>();
        activeRenderables = new LinkedList<IRenderEvent>();
    }

    @Override
    protected void disposeInternal() {
        dataManager.dispose();
        renderingRouter = null;
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        List<AbstractDispatchingObjectEvent> currentDataChangeEvents = new LinkedList<AbstractDispatchingObjectEvent>();
        synchronized (dataChangeEvents) {
            if (waitingOnObjects.size() == 0) {
                currentDataChangeEvents.addAll(dataChangeEvents);
                dataChangeEvents.clear();
            } else {
                for (AbstractDispatchingObjectEvent dcEvent : dataChangeEvents) {
                    if (waitingOnObjects.contains(dcEvent.getObjectId()) == false) {
                        currentDataChangeEvents.add(dcEvent);
                    }
                }
                dataChangeEvents.removeAll(currentDataChangeEvents);
            }
        }

        dataManager.beginRender(target, paintProps);

        List<IRenderEvent> toRender = new LinkedList<IRenderEvent>();
        synchronized (currentRenderables) {
            toRender.addAll(currentRenderables);
        }

        synchronized (renderingRouter) {
            for (AbstractRemoteGraphicsEvent event : currentDataChangeEvents) {
                renderingRouter.post(event);
            }

            for (IRenderEvent event : toRender) {
                renderingRouter.post(event);
            }
            if (latestMouseLocation != null) {
                renderingRouter.post(latestMouseLocation);
            }
        }
    }

    public void lockObject(int objectId) {
        synchronized (dataChangeEvents) {
            waitingOnObjects.add(objectId);
        }
    }

    public void unlockObject(int objectId) {
        synchronized (dataChangeEvents) {
            waitingOnObjects.remove(objectId);
        }
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        renderingRouter = new EventBus();
        dataManager = new CollaborationRenderingDataManager(
                resourceData.getSession(), this);
        for (CollaborationRenderingHandler handler : CollaborationRenderingDataManager
                .createRenderingHandlers(dataManager)) {
            renderingRouter.register(handler);
        }
    }

    @Subscribe
    public void updateRenderFrameEvent(UpdateRenderFrameEvent event) {
        int objectId = event.getObjectId();
        RenderFrameEvent frame = dataManager.getRenderableObject(objectId,
                RenderFrameEvent.class, false);
        if (frame == null) {
            if (waitingOnFrames.contains(objectId) == false) {
                RenderFrameNeededEvent needEvent = new RenderFrameNeededEvent();
                needEvent.setDisplayId(event.getDisplayId());
                needEvent.setObjectId(objectId);
                ISharedDisplaySession session = resourceData.getSession();
                try {
                    session.sendObjectToPeer(session.getCurrentDataProvider(),
                            needEvent);
                    waitingOnFrames.add(objectId);
                } catch (CollaborationException e) {
                    Activator.statusHandler.handle(Priority.PROBLEM,
                            "Error sending message to data provider", e);
                }
            }
        } else {
            // We have the frame, apply update
            RenderFrameEvent updated = null;
            if (event.getRenderEvents().size() > 0) {
                updated = new RenderFrameEvent();
                updated.setDisplayId(frame.getDisplayId());
                updated.setObjectId(objectId);
                List<IRenderEvent> events = new LinkedList<IRenderEvent>();
                Iterator<IRenderEvent> currIter = frame.getRenderEvents()
                        .iterator();
                Iterator<IRenderEvent> updateIter = event.getRenderEvents()
                        .iterator();
                while (currIter.hasNext() && updateIter.hasNext()) {
                    IRenderEvent curr = currIter.next();
                    IRenderEvent update = updateIter.next();
                    IRenderEvent toUse = null;
                    if (update != null) {
                        if (update.getClass().equals(curr.getClass())) {
                            curr.applyDiffObject(update);
                            toUse = curr;
                        } else {
                            toUse = update;
                        }
                    } else {
                        toUse = curr;
                    }
                    events.add(toUse);
                }
                updated.setRenderEvents(events);
            } else {
                updated = frame;
            }
            // Render updated data
            renderFrameEvent(updated);
        }
    }

    @Subscribe
    public void renderFrameEvent(RenderFrameEvent event) {
        if (event instanceof UpdateRenderFrameEvent == false) {
            // Not an update event, new frame
            int objectId = event.getObjectId();
            for (IRenderEvent re : event.getRenderEvents()) {
                renderableArrived((AbstractRemoteGraphicsEvent) re);
            }
            dataManager.putRenderableObject(objectId, event);
        }
    }

    @Subscribe
    public void disposeRenderFrame(FrameDisposed event) {
        waitingOnFrames.remove(event.getObjectId());
        dataManager.dispose(event.getObjectId());
    }

    @Subscribe
    public void persitableArrived(final IPersistedEvent event) {
        retrievePool.schedule(new Runnable() {
            @Override
            public void run() {
                try {
                    renderableArrived(dataManager.retrieveEvent(event));
                } catch (CollaborationException e) {
                    Activator.statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }
        });
    }

    public void postObjectEvent(AbstractDispatchingObjectEvent event) {
        synchronized (renderingRouter) {
            renderingRouter.post(event);
        }
    }

    @Subscribe
    public void renderableArrived(AbstractRemoteGraphicsEvent event) {
        // TODO: May need to allow for adding objects to the data manager for an
        // objectId and creation events should put a flag that a creation event
        // was received for the object
        if (event instanceof IRenderFrameEvent) {
            // Skip IRenderFrameEvents, not applicable here
            return;
        }
        if (event instanceof IRenderEvent) {
            // Render based event
            if (event instanceof BeginFrameEvent) {
                // If begin frame event, clear current active renderables
                activeRenderables.clear();
                activeRenderables.add((IRenderEvent) event);
            } else if (event instanceof EndFrameEvent) {
                synchronized (currentRenderables) {
                    currentRenderables.clear();
                    // Frame over, process BeginFrameEvent now to keep in sync
                    for (IRenderEvent renderable : activeRenderables) {
                        if (renderable instanceof BeginFrameEvent) {
                            IRenderableDisplay display = descriptor
                                    .getRenderableDisplay();
                            BeginFrameEvent bfe = (BeginFrameEvent) renderable;
                            IExtent extent = display.getView().getExtent();
                            double[] center = bfe.getExtentCenter();
                            double[] currCenter = extent.getCenter();
                            extent.shift(center[0] - currCenter[0], center[1]
                                    - currCenter[1]);
                            extent.scaleAndBias(
                                    bfe.getExtentFactor() / extent.getScale(),
                                    center[0], center[1]);
                            display.setBackgroundColor(bfe.getColor());
                        } else {
                            // Add to list for processing in paintInternal
                            currentRenderables.add(renderable);
                        }
                    }
                    activeRenderables.clear();
                }
                issueRefresh();
            } else if (event instanceof MouseLocationEvent) {
                latestMouseLocation = (MouseLocationEvent) event;
                issueRefresh();
            } else {
                activeRenderables.add((IRenderEvent) event);
            }
        } else if (event instanceof AbstractDispatchingObjectEvent) {
            // If not IRenderEvent, event modifies data object
            synchronized (dataChangeEvents) {
                dataChangeEvents.add((AbstractDispatchingObjectEvent) event);
            }
            issueRefresh();
        } else {
            Activator.statusHandler.handle(Priority.PROBLEM,
                    "Could not handle event type: "
                            + event.getClass().getSimpleName());
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#getResourceOrder()
     */
    @Override
    public ResourceOrder getResourceOrder() {
        return ResourceOrder.LOWEST;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.cmenu.IContextMenuProvider#provideContextMenuItems
     * (org.eclipse.jface.action.IMenuManager, int, int)
     */
    @Override
    public void provideContextMenuItems(IMenuManager menuManager, int x, int y) {
        menuManager.add(new Action("Quit Session") {
            @Override
            public void run() {
                System.out.println("TODO: Quit session");
            }
        });
    }

}
