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
package com.raytheon.uf.viz.collaboration.ui.editor;

import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.ISaveablePart2;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.part.EditorPart;

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.display.data.SharedDisplaySessionMgr;
import com.raytheon.uf.viz.collaboration.display.editor.ActivateRemoteDisplay;
import com.raytheon.uf.viz.collaboration.display.editor.CollaborationEditorInput;
import com.raytheon.uf.viz.collaboration.display.editor.CreateRemoteDisplay;
import com.raytheon.uf.viz.collaboration.display.editor.DisposeRemoteDisplay;
import com.raytheon.uf.viz.collaboration.display.editor.ICollaborationEditor;
import com.raytheon.uf.viz.collaboration.display.editor.RemoteDisplayRequested;
import com.raytheon.uf.viz.collaboration.display.editor.ReprojectRemoteDisplay;
import com.raytheon.uf.viz.collaboration.display.rsc.CollaborationResourceData;
import com.raytheon.uf.viz.collaboration.display.rsc.event.ResourceCapabilityChanged;
import com.raytheon.uf.viz.collaboration.display.rsc.event.ResourcePropertiesChanged;
import com.raytheon.uf.viz.collaboration.display.rsc.event.SharedResource;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.AbstractCapability;
import com.raytheon.viz.ui.input.InputManager;

/**
 * A collaboration editor that displays the display of an editor shared by the
 * Data Provider.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 16, 2012            njensen     Initial creation
 * Mar 25, 2014 2935       njensen     Fix refresh issue
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class CollaborationEditor extends EditorPart implements
        ICollaborationEditor, ISaveablePart2 {

    private static final int NO_ACTIVE_DISPLAY = -1;

    private ISharedDisplaySession session;

    private CollaborationPaneManager paneManager;

    private Map<Integer, IRenderableDisplay> displayMap = new LinkedHashMap<Integer, IRenderableDisplay>(
            5, 1.25f, true);

    private int currentActiveDisplay = NO_ACTIVE_DISPLAY;

    private Set<IRemoteDisplayChangedListener> listeners = new LinkedHashSet<IRemoteDisplayChangedListener>();

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.EditorPart#init(org.eclipse.ui.IEditorSite,
     * org.eclipse.ui.IEditorInput)
     */
    @Override
    public void init(IEditorSite site, IEditorInput input)
            throws PartInitException {
        setInput(input);
        setSite(site);
        CollaborationEditorInput cei = (CollaborationEditorInput) input;
        setPartName(cei.getName());
        session = SharedDisplaySessionMgr.getSessionContainer(
                cei.getSessionId()).getSession();
        session.registerEventHandler(this);
        paneManager = new CollaborationPaneManager();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets
     * .Composite)
     */
    @Override
    public void createPartControl(Composite parent) {
        paneManager.initializeComponents(paneManager, parent);

        RemoteDisplayRequested request = new RemoteDisplayRequested();
        request.setDisplayId(NO_ACTIVE_DISPLAY);
        request.setUserId(session.getUserID().getFQName());
        try {
            session.sendObjectToPeer(session.getCurrentDataProvider(), request);
        } catch (CollaborationException e) {
            Activator.statusHandler.handle(Priority.PROBLEM,
                    e.getLocalizedMessage(), e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.WorkbenchPart#dispose()
     */
    @Override
    public void dispose() {
        super.dispose();
        session.unregisterEventHandler(this);
    }

    @Subscribe
    public void createRemoteDisplay(CreateRemoteDisplay event) {
        int displayId = event.getDisplayId();
        IRenderableDisplay display = displayMap.get(displayId);
        if (display == null) {
            display = event.getDisplay();
            for (ResourcePair rp : display.getDescriptor().getResourceList()) {
                rp.getProperties().setSystemResource(true);
            }
            CollaborationResourceData crd = new CollaborationResourceData(this,
                    session, displayId);
            ResourcePair rp = ResourcePair.constructSystemResourcePair(crd);
            display.getDescriptor().getResourceList().add(rp);
            display.getDescriptor().getResourceList()
                    .instantiateResources(display.getDescriptor(), true);
            displayMap.put(displayId, display);

            fireListeners(displayId, display, RemoteDisplayChangeType.CREATED);
            if (currentActiveDisplay == displayId || currentActiveDisplay == -1) {
                /*
                 * when transferring leader, it's possible the leader told us to
                 * activate a display that we don't have so we request it (see
                 * activateRemoteDisplay when display==null), so the leader
                 * responds with this create event, but then the display had
                 * missed the activate event so we still have to activate it,
                 * hence the == -1 check
                 */
                ActivateRemoteDisplay activate = new ActivateRemoteDisplay();
                activate.setDisplayId(displayId);
                activateRemoteDisplay(activate);
            }
        }
    }

    @Subscribe
    public void activateRemoteDisplay(ActivateRemoteDisplay event) {
        currentActiveDisplay = event.getDisplayId();
        final IRenderableDisplay display = displayMap.get(currentActiveDisplay);
        if (currentActiveDisplay < 0) {
            fireListeners(currentActiveDisplay, display,
                    RemoteDisplayChangeType.ACTIVATED);
            VizApp.runAsync(new Runnable() {
                @Override
                public void run() {
                    paneManager.deactivateDisplays();
                }
            });
        } else if (display == null) {
            RemoteDisplayRequested request = new RemoteDisplayRequested();
            request.setDisplayId(currentActiveDisplay);
            request.setUserId(session.getUserID().getFQName());
            try {
                session.sendObjectToPeer(session.getCurrentDataProvider(),
                        request);
            } catch (CollaborationException e) {
                Activator.statusHandler.handle(Priority.PROBLEM,
                        e.getLocalizedMessage(), e);
            }
        } else {
            fireListeners(currentActiveDisplay, display,
                    RemoteDisplayChangeType.ACTIVATED);
            VizApp.runAsync(new Runnable() {
                @Override
                public void run() {
                    paneManager.activateDisplay(currentActiveDisplay, display);
                }
            });
        }
    }

    @Subscribe
    public void disposeRemoteDisplay(DisposeRemoteDisplay event) {
        final int displayId = event.getDisplayId();
        final IRenderableDisplay display = displayMap.remove(displayId);
        if (display != null) {
            fireListeners(event.getDisplayId(), display,
                    RemoteDisplayChangeType.DISPOSED);
            VizApp.runAsync(new Runnable() {
                @Override
                public void run() {
                    paneManager.dispose(display);
                    if (currentActiveDisplay == displayId) {
                        currentActiveDisplay = NO_ACTIVE_DISPLAY;
                    }
                }
            });
        }
    }

    @Subscribe
    public void reprojectRemoteDisplay(ReprojectRemoteDisplay event) {
        IRenderableDisplay display = displayMap.get(event.getDisplayId());
        if (display != null) {
            IDescriptor descriptor = display.getDescriptor();
            try {
                descriptor.setGridGeometry(event.getTargetGeometry());
                Rectangle bounds = paneManager.getCanvasSize(display);
                if (bounds != null) {
                    display.scaleToClientArea(bounds);
                    display.refresh();
                }
            } catch (VizException e) {
                Activator.statusHandler.handle(Priority.PROBLEM,
                        e.getLocalizedMessage(), e);
            }
        }
    }

    @Subscribe
    public void sharedResourceEvent(SharedResource event) {
        IRenderableDisplay display = displayMap.get(event.getDisplayId());
        if (display != null) {
            IDescriptor descriptor = display.getDescriptor();
            event.getResource().getProperties().setSystemResource(true);
            if (event.isRemoveResource()) {
                descriptor.getResourceList().remove(event.getResource());
            } else {
                descriptor.getResourceList().add(event.getResource());
                descriptor.getResourceList().instantiateResources(descriptor,
                        true);
            }
        }
    }

    @Subscribe
    public void sharedResourceCapabilityChanged(ResourceCapabilityChanged event) {
        IRenderableDisplay display = displayMap.get(event.getDisplayId());
        if (display != null) {
            AbstractResourceData lookFor = event.getResourceData();
            AbstractCapability capability = event.getCapability();
            for (ResourcePair rp : display.getDescriptor().getResourceList()) {
                if (lookFor.equals(rp.getResourceData())) {
                    AbstractCapability rscCapability = capability.clone();
                    rscCapability.setResourceData(rp.getResourceData());
                    rp.getLoadProperties().getCapabilities()
                            .addCapability(rscCapability);
                }
            }
        }
    }

    @Subscribe
    public void sharedResourcePropertiesChanged(ResourcePropertiesChanged event) {
        IRenderableDisplay display = displayMap.get(event.getDisplayId());
        if (display != null) {
            AbstractResourceData lookFor = event.getResourceData();
            ResourceProperties props = event.getProperties();
            props.setSystemResource(true);
            for (ResourcePair rp : display.getDescriptor().getResourceList()) {
                if (lookFor.equals(rp.getResourceData())) {
                    rp.setProperties(props);
                }
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.display.editor.ICollaborationEditor
     * #setCanvasBounds(org.eclipse.swt.graphics.Rectangle)
     */
    @Override
    public void setCanvasBounds(int displayId, Rectangle canvasBounds) {
        IRenderableDisplay display = displayMap.get(displayId);
        if (display != null) {
            paneManager.setCanvasSize(display, canvasBounds);
        }
    }

    public InputManager getInputManager() {
        return paneManager.getMouseManager();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.display.editor.ICollaborationEditor
     * #getSessionId()
     */
    @Override
    public String getSessionId() {
        return session.getSessionId();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.display.editor.ICollaborationEditor
     * #getActiveDisplayPane()
     */
    @Override
    public IDisplayPane getActiveDisplayPane() {
        return paneManager.getActiveDisplayPane();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.EditorPart#isDirty()
     */
    @Override
    public boolean isDirty() {
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.EditorPart#doSave(org.eclipse.core.runtime.
     * IProgressMonitor)
     */
    @Override
    public void doSave(IProgressMonitor monitor) {

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.EditorPart#doSaveAs()
     */
    @Override
    public void doSaveAs() {

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.EditorPart#isSaveAsAllowed()
     */
    @Override
    public boolean isSaveAsAllowed() {
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
     */
    @Override
    public void setFocus() {
        paneManager.setFocus();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.ISaveablePart2#promptToSaveOnClose()
     */
    @Override
    public int promptToSaveOnClose() {
        // Let the user know why we refuse to close the editor
        MessageDialog.openError(getSite().getShell(), "Closing Disabled",
                "Please close the \"" + getPartName()
                        + "\" chat to exit the session.");
        // Cancel the clsoe
        return ISaveablePart2.CANCEL;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.collaboration.display.IRemoteDisplayContainer#
     * addRemoteDisplayChangedListener
     * (com.raytheon.uf.viz.collaboration.display.
     * IRemoteDisplayContainer.IRemoteDisplayChangedListener)
     */
    @Override
    public void addRemoteDisplayChangedListener(
            IRemoteDisplayChangedListener listener) {
        listeners.add(listener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.collaboration.display.IRemoteDisplayContainer#
     * removeRemoteDisplayChangedListener
     * (com.raytheon.uf.viz.collaboration.display
     * .IRemoteDisplayContainer.IRemoteDisplayChangedListener)
     */
    @Override
    public void removeRemoteDisplayChangedListener(
            IRemoteDisplayChangedListener listener) {
        listeners.remove(listener);
    }

    private void fireListeners(int displayId, IRenderableDisplay display,
            RemoteDisplayChangeType changeType) {
        RemoteDisplay rd = null;
        if (display != null) {
            rd = new RemoteDisplay(displayId, display);
        }
        for (IRemoteDisplayChangedListener listener : listeners) {
            listener.remoteDisplayChanged(rd, changeType);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.collaboration.display.IRemoteDisplayContainer#
     * getActiveDisplay()
     */
    @Override
    public RemoteDisplay getActiveDisplay() {
        IRenderableDisplay display = displayMap.get(currentActiveDisplay);
        if (display == null) {
            return null;
        }
        return new RemoteDisplay(currentActiveDisplay, display);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.collaboration.display.IRemoteDisplayContainer#
     * getActiveDisplayEditor()
     */
    @Override
    public IEditorPart getActiveDisplayEditor() {
        return this;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.collaboration.display.IRemoteDisplayContainer#
     * disposeContainer()
     */
    @Override
    public void disposeContainer() {
        getSite().getPage().closeEditor(this, false);
    }
}
