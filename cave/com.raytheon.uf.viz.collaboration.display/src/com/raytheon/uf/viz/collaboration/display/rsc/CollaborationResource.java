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
package com.raytheon.uf.viz.collaboration.display.rsc;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.swt.graphics.Rectangle;

import com.google.common.eventbus.EventBus;
import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.display.Activator;
import com.raytheon.uf.viz.collaboration.display.roles.dataprovider.event.FrameDisposed;
import com.raytheon.uf.viz.collaboration.display.roles.dataprovider.event.IRenderFrameEvent;
import com.raytheon.uf.viz.collaboration.display.roles.dataprovider.event.MouseLocationEvent;
import com.raytheon.uf.viz.collaboration.display.roles.dataprovider.event.RenderFrameEvent;
import com.raytheon.uf.viz.collaboration.display.roles.dataprovider.event.RenderFrameNeededEvent;
import com.raytheon.uf.viz.collaboration.display.roles.dataprovider.event.UpdateRenderFrameEvent;
import com.raytheon.uf.viz.collaboration.display.rsc.event.ParticipantInitializedEvent;
import com.raytheon.uf.viz.collaboration.display.rsc.event.ReentrantEventHandler;
import com.raytheon.uf.viz.collaboration.display.rsc.rendering.CollaborationRenderingDataManager;
import com.raytheon.uf.viz.collaboration.display.rsc.rendering.CollaborationRenderingHandler;
import com.raytheon.uf.viz.collaboration.display.storage.IPersistedEvent;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.VizApp;
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
 * Provider. Only participants (non-leaders) will have this resource.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 03, 2012            njensen     Initial creation
 * Mar 06, 2014 2826       njensen     Fix spelling mistake
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

    private static class DisplayData {
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

        private Map<IExtent, List<IRenderEvent>> renderableMap = new LinkedHashMap<IExtent, List<IRenderEvent>>() {

            private static final long serialVersionUID = 1L;

            /*
             * (non-Javadoc)
             * 
             * @see
             * java.util.LinkedHashMap#removeEldestEntry(java.util.Map.Entry)
             */
            @Override
            protected boolean removeEldestEntry(
                    Entry<IExtent, List<IRenderEvent>> eldest) {
                if (size() > 10) {
                    return true;
                }
                return false;
            }
        };

        private DisplayData(int displayId, ISharedDisplaySession session,
                CollaborationResource resource) {
            dataChangeEvents = new LinkedList<AbstractDispatchingObjectEvent>();
            currentRenderables = new LinkedList<IRenderEvent>();
            activeRenderables = new LinkedList<IRenderEvent>();
            renderingRouter = new ReentrantEventHandler();

            dataManager = new CollaborationRenderingDataManager(session,
                    resource, displayId);
            for (CollaborationRenderingHandler handler : CollaborationRenderingDataManager
                    .createRenderingHandlers(dataManager)) {
                renderingRouter.register(handler);
            }
        }

        private void dispose() {
            dataManager.dispose();
            renderingRouter = null;
        }
    }

    private Rectangle previousBounds = null;

    private DisplayData displayData;

    public CollaborationResource(CollaborationResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
    }

    @Override
    protected void disposeInternal() {
        resourceData.getSession().unregisterEventHandler(this);
        displayData.dispose();
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        if (displayData == null) {
            return;
        }

        Map<IExtent, List<IRenderEvent>> renderableMap = displayData.renderableMap;
        List<AbstractDispatchingObjectEvent> dataChangeEvents = displayData.dataChangeEvents;
        Set<Integer> waitingOnObjects = displayData.waitingOnObjects;
        EventBus renderingRouter = displayData.renderingRouter;

        // Get the renderables for my extent
        List<IRenderEvent> currentRenderables = null;
        synchronized (renderableMap) {
            currentRenderables = renderableMap.get(paintProps.getView()
                    .getExtent());
            if (currentRenderables == null) {
                synchronized (displayData.currentRenderables) {
                    currentRenderables = new ArrayList<IRenderEvent>(
                            displayData.currentRenderables);
                }
            }
        }

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

        displayData.dataManager.beginRender(target, paintProps);

        synchronized (renderingRouter) {
            for (AbstractRemoteGraphicsEvent event : currentDataChangeEvents) {
                renderingRouter.post(event);
            }

            for (IRenderEvent event : currentRenderables) {
                renderingRouter.post(event);
            }
            if (displayData.latestMouseLocation != null) {
                renderingRouter.post(displayData.latestMouseLocation);
            }
        }
    }

    public void lockObject(int objectId) {
        synchronized (displayData.dataChangeEvents) {
            displayData.waitingOnObjects.add(objectId);
        }
    }

    public void unlockObject(int objectId) {
        synchronized (displayData.dataChangeEvents) {
            displayData.waitingOnObjects.remove(objectId);
        }
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        ISharedDisplaySession session = resourceData.getSession();
        displayData = new DisplayData(resourceData.getDisplayId(), session,
                this);
        ParticipantInitializedEvent event = new ParticipantInitializedEvent();
        event.setUserId(session.getUserID().getFQName());
        try {
            session.registerEventHandler(this);
            session.sendObjectToPeer(session.getCurrentDataProvider(), event);
        } catch (CollaborationException e) {
            Activator.statusHandler.handle(Priority.PROBLEM,
                    e.getLocalizedMessage(), e);
        }
    }

    @Subscribe
    public void updateRenderFrameEvent(UpdateRenderFrameEvent event) {
        if (event.getDisplayId() != resourceData.getDisplayId()) {
            return;
        }
        int objectId = event.getObjectId();
        RenderFrameEvent frame = displayData.dataManager.getRenderableObject(
                objectId, RenderFrameEvent.class, false);
        if (frame == null) {
            if (displayData.waitingOnFrames.contains(objectId) == false) {
                RenderFrameNeededEvent needEvent = new RenderFrameNeededEvent();
                needEvent.setDisplayId(event.getDisplayId());
                needEvent.setObjectId(objectId);
                ISharedDisplaySession session = resourceData.getSession();
                try {
                    session.sendObjectToPeer(session.getCurrentDataProvider(),
                            needEvent);
                    displayData.waitingOnFrames.add(objectId);
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
        if (event.getDisplayId() != resourceData.getDisplayId()) {
            return;
        }
        if (event instanceof UpdateRenderFrameEvent == false) {
            // Not an update event, new frame
            int objectId = event.getObjectId();
            for (IRenderEvent re : event.getRenderEvents()) {
                renderableArrived((AbstractRemoteGraphicsEvent) re.clone());
            }
            displayData.dataManager.putRenderableObject(objectId, event);
        }
        issueRefresh();
    }

    @Subscribe
    public void disposeRenderFrame(FrameDisposed event) {
        if (event.getDisplayId() != resourceData.getDisplayId()) {
            return;
        }
        displayData.waitingOnFrames.remove(event.getObjectId());
        displayData.dataManager.dispose(event.getObjectId());
    }

    @Subscribe
    public void persistableArrived(final IPersistedEvent event) {
        if (event.getDisplayId() != resourceData.getDisplayId()) {
            return;
        }
        retrievePool.schedule(new Runnable() {
            @Override
            public void run() {
                try {
                    AbstractDispatchingObjectEvent objectEvent = displayData.dataManager
                            .retrieveEvent(event);
                    if (objectEvent != null) {
                        renderableArrived(objectEvent);
                    }
                } catch (CollaborationException e) {
                    Activator.statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }
        });
    }

    public void postObjectEvent(AbstractDispatchingObjectEvent event) {
        synchronized (displayData.renderingRouter) {
            displayData.renderingRouter.post(event);
        }
    }

    @Subscribe
    public void renderableArrived(AbstractRemoteGraphicsEvent event) {
        if (event.getDisplayId() != resourceData.getDisplayId()
                || event instanceof IRenderFrameEvent) {
            // Skip IRenderFrameEvents, not applicable here
            return;
        }
        if (event instanceof IRenderEvent) {
            // Render based event
            if (event instanceof BeginFrameEvent) {
                // If begin frame event, clear current active renderables
                displayData.activeRenderables.clear();
                displayData.activeRenderables.add((IRenderEvent) event);
            } else if (event instanceof EndFrameEvent) {
                synchronized (displayData.currentRenderables) {
                    IExtent current = null;
                    displayData.currentRenderables.clear();
                    // Frame over, process BeginFrameEvent now to keep in sync
                    for (IRenderEvent renderable : displayData.activeRenderables) {
                        if (renderable instanceof BeginFrameEvent) {
                            // Handle begin frame event immediately before next
                            // paint occurs
                            final IRenderableDisplay display = descriptor
                                    .getRenderableDisplay();
                            final BeginFrameEvent bfe = (BeginFrameEvent) renderable;
                            display.setBackgroundColor(bfe.getColor());
                            if (previousBounds == null
                                    || previousBounds.equals(bfe.getBounds()) == false) {
                                previousBounds = bfe.getBounds();
                                VizApp.runAsync(new Runnable() {
                                    @Override
                                    public void run() {
                                        resourceData.getEditor()
                                                .setCanvasBounds(
                                                        bfe.getDisplayId(),
                                                        bfe.getBounds());
                                        display.getView().setExtent(
                                                bfe.getExtent());
                                    }
                                });
                            } else {
                                display.getView().setExtent(bfe.getExtent());
                            }
                            current = bfe.getExtent();
                        } else {
                            // Add to list for processing in paintInternal
                            displayData.currentRenderables.add(renderable);
                        }
                    }
                    displayData.activeRenderables.clear();
                    displayData.renderableMap.put(current,
                            new ArrayList<IRenderEvent>(
                                    displayData.currentRenderables));
                }
                issueRefresh();
            } else if (event instanceof MouseLocationEvent) {
                displayData.latestMouseLocation = (MouseLocationEvent) event;
                issueRefresh();
            } else {
                displayData.activeRenderables.add((IRenderEvent) event);
            }
        } else if (event instanceof AbstractDispatchingObjectEvent) {
            // If not IRenderEvent, event modifies data object
            synchronized (displayData.dataChangeEvents) {
                displayData.dataChangeEvents
                        .add((AbstractDispatchingObjectEvent) event);
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
