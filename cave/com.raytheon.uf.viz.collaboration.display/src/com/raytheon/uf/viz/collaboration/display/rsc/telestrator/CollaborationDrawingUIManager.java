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
package com.raytheon.uf.viz.collaboration.display.rsc.telestrator;

import org.eclipse.swt.widgets.Shell;

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.viz.collaboration.display.IRemoteDisplayContainer;
import com.raytheon.uf.viz.collaboration.display.editor.ActivateRemoteDisplay;
import com.raytheon.uf.viz.collaboration.display.editor.DisposeRemoteDisplay;
import com.raytheon.uf.viz.collaboration.display.editor.ReprojectRemoteDisplay;
import com.raytheon.uf.viz.collaboration.display.roles.dataprovider.event.RenderFrameEvent;
import com.raytheon.uf.viz.collaboration.display.rsc.telestrator.CollaborationDrawingEvent.CollaborationEventType;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.capabilities.EditableCapability;
import com.raytheon.uf.viz.drawing.DrawingToolLayer;
import com.raytheon.uf.viz.drawing.DrawingToolLayer.DrawMode;
import com.raytheon.uf.viz.drawing.DrawingToolUIManager;
import com.raytheon.uf.viz.remote.graphics.events.rendering.BeginFrameEvent;
import com.raytheon.uf.viz.remote.graphics.events.rendering.IRenderEvent;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * UI Manager for the CollaborationDrawingResource, handles mouse input and
 * opens the toolbar. Registers and listens for events on the event bus so that
 * drawing can be stopped if zooming, panning, dispose, swap, or scale change
 * occurs (to prevent skewing of the drawing).
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 23, 2012            mschenke    Initial creation
 * Feb 13, 2014 2751       njensen     Fix null error on shutdown
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class CollaborationDrawingUIManager extends DrawingToolUIManager
        implements IResourceDataChanged {

    private CollaborationDrawingResource resource;

    private static final int NO_ACTIVE_DISPLAY = -1;

    public CollaborationDrawingUIManager(CollaborationDrawingResource resource) {
        super(resource.getDrawingLayerFor(resource.getMyUser()), resource
                .getResourceContainer());
        this.resource = resource;
        resource.getResourceData().addChangeListener(this);
        resource.getContainer().getSession().registerEventHandler(this);
    }

    /**
     * Drawing event, used for when the leader locks the collaborators from
     * drawing
     * 
     * @param event
     */
    @Subscribe
    public void handleDrawingEvent(CollaborationDrawingEvent event) {
        int displayId = getDisplayId();
        if (event.getDisplayId() == displayId) {
            if (event.getType() == CollaborationEventType.LOCK_USERS) {
                VizApp.runAsync(new Runnable() {
                    @Override
                    public void run() {
                        Shell shell = getCurrentShell();
                        if (shell != null) {
                            clearCursor(shell);
                        }
                    }
                });
                finishDrawing();
            } else if (event.getType() == CollaborationEventType.UNLOCK_USERS) {
                VizApp.runAsync(new Runnable() {
                    @Override
                    public void run() {
                        if (EditorUtil.getActiveEditor() instanceof AbstractEditor == false) {
                            Shell shell = getCurrentShell();
                            if (shell != null) {
                                updateCursor(getCurrentShell());
                            }
                        }
                    }
                });
            }
        }
    }

    /**
     * Dispose event, used for when the leader unshares the map
     * 
     * @param event
     */
    @Subscribe
    public void handleDisposeEvent(DisposeRemoteDisplay event) {
        int displayId = getDisplayId();
        if (event.getDisplayId() == displayId) {
            finishDrawing();
        }
    }

    /**
     * Reproject event, used when the map scale changes
     * 
     * @param event
     */
    @Subscribe
    public void handleReprojectEvent(ReprojectRemoteDisplay event) {
        int displayId = getDisplayId();
        if (event.getDisplayId() == displayId) {
            finishDrawing();
        }
    }

    /**
     * Render frame event, used for zoom/pan
     * 
     * @param event
     */
    @Subscribe
    public void handleRenderFrameEvent(RenderFrameEvent event) {
        int displayId = getDisplayId();
        for (IRenderEvent ev : event.getRenderEvents()) {
            if (ev instanceof BeginFrameEvent) {
                BeginFrameEvent bfe = (BeginFrameEvent) ev;
                if (bfe.getExtent() != null || bfe.getBounds() != null
                        && bfe.getDisplayId() == displayId
                        || displayId == NO_ACTIVE_DISPLAY) {
                    finishDrawing();
                }
            }
        }
    }

    @Subscribe
    public void handleActivateDisplayEvent(ActivateRemoteDisplay event) {
        int displayId = getDisplayId();
        if (event.getDisplayId() != displayId) {
            finishDrawing();
        }
    }

    /**
     * Gets the display id of the current display. This will make sure that the
     * event occurred on the display that we are currently drawing on.
     * 
     * @return
     */
    private int getDisplayId() {
        IRemoteDisplayContainer displayContainer = resource.getContainer()
                .getDisplayContainer();
        if (displayContainer != null
                && displayContainer.getActiveDisplay() != null) {
            return displayContainer.getActiveDisplay().getDisplayId();
        } else {
            // event to unshared editor, so that active display doesn't
            // exist
            return NO_ACTIVE_DISPLAY;
        }
    }

    /**
     * Emulate the fact that drawing should be finished. This will be called
     * from numerous places for when the user "should" stop drawing to prevent
     * drawing errors
     */
    private void finishDrawing() {
        // 0 for all values is fine since it will just return immediately
        handleMouseUp(0, 0, 0);
        setHandlingInput(false);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.drawing.DrawingToolUIManager#dispose()
     */
    @Override
    public void dispose() {
        resource.getResourceData().removeChangeListener(this);
        resource.getContainer().getSession().unregisterEventHandler(this);
        super.dispose();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.drawing.DrawingToolUIManager#canTellestrate(int)
     */
    @Override
    protected boolean canTellestrate(int mouseButton) {
        return super.canTellestrate(mouseButton) && resource.canTellestrate();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.IResourceDataChanged#resourceChanged(com
     * .raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType,
     * java.lang.Object)
     */
    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (type == ChangeType.CAPABILITY
                && object instanceof EditableCapability) {
            EditableCapability editable = (EditableCapability) object;
            if (editable.isEditable() == false) {
                DrawingToolLayer layer = resource.getDrawingLayerFor(resource
                        .getMyUser());
                layer.setDrawMode(DrawMode.NONE);
                VizApp.runAsync(new Runnable() {
                    @Override
                    public void run() {
                        updateCursor(getCurrentShell());
                    }
                });
            }
        }
        resource.issueRefresh();
    }

}
