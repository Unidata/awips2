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

import java.util.LinkedList;
import java.util.List;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuManager;

import com.google.common.eventbus.EventBus;
import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.collaboration.ui.role.event.IPersistedEvent;
import com.raytheon.uf.viz.collaboration.ui.rsc.rendering.CollaborationRenderingDataManager;
import com.raytheon.uf.viz.collaboration.ui.rsc.rendering.CollaborationRenderingHandler;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.jobs.JobPool;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.RenderingOrderFactory.ResourceOrder;
import com.raytheon.uf.viz.remote.graphics.AbstractRemoteGraphicsEvent;
import com.raytheon.uf.viz.remote.graphics.events.BeginFrameEvent;
import com.raytheon.uf.viz.remote.graphics.events.EndFrameEvent;
import com.raytheon.uf.viz.remote.graphics.events.IRenderEvent;
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
    private List<AbstractRemoteGraphicsEvent> dataChangeEvents;

    /** Internal rendering event object router */
    private EventBus renderingRouter;

    private CollaborationRenderingDataManager dataManager;

    private BeginFrameEvent latestBeginFrameEvent;

    public CollaborationResource(CollaborationResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        dataChangeEvents = new LinkedList<AbstractRemoteGraphicsEvent>();
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
        List<IRenderEvent> toRender = new LinkedList<IRenderEvent>();
        synchronized (currentRenderables) {
            toRender.addAll(currentRenderables);
        }

        List<AbstractRemoteGraphicsEvent> currentDataChangeEvents = new LinkedList<AbstractRemoteGraphicsEvent>();
        synchronized (dataChangeEvents) {
            currentDataChangeEvents.addAll(dataChangeEvents);
            dataChangeEvents.clear();
        }

        dataManager.beginRender(target, paintProps);

        // Handle begin frame
        if (latestBeginFrameEvent != null) {
            renderingRouter.post(latestBeginFrameEvent);
            latestBeginFrameEvent = null;
        }
        for (AbstractRemoteGraphicsEvent event : currentDataChangeEvents) {
            renderingRouter.post(event);
        }

        for (IRenderEvent event : toRender) {
            renderingRouter.post(event);
        }
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        renderingRouter = new EventBus();
        dataManager = new CollaborationRenderingDataManager(
                resourceData.getSession());
        for (CollaborationRenderingHandler handler : CollaborationRenderingDataManager
                .createRenderingHandlers(dataManager)) {
            renderingRouter.register(handler);
        }
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

    @Subscribe
    public void renderableArrived(AbstractRemoteGraphicsEvent event) {
        if (event instanceof IRenderEvent) {
            // Render based event
            if (event instanceof BeginFrameEvent) {
                // If begin frame event, clear current active renderables
                activeRenderables.clear();
                BeginFrameEvent beginEvent = (BeginFrameEvent) event;
                if (beginEvent.getExtentCenter() != null) {
                    // Event modifies extent
                    latestBeginFrameEvent = beginEvent;
                    issueRefresh();
                }
            } else if (event instanceof EndFrameEvent) {
                synchronized (currentRenderables) {
                    currentRenderables.clear();
                    currentRenderables.addAll(activeRenderables);
                    activeRenderables.clear();
                }
                activeRenderables.clear();
                issueRefresh();
            } else {
                activeRenderables.add((IRenderEvent) event);
            }
        } else {
            // If not IRenderEvent, event modifies data object
            synchronized (dataChangeEvents) {
                dataChangeEvents.add(event);
            }
            issueRefresh();
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
