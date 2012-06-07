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
package com.raytheon.uf.viz.collaboration.ui.role;

import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IWorkbenchPart;

import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.collaboration.ui.telestrator.CollaborationDrawingResourceData;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceList.RemoveListener;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * Abstract role event controller that shares fields and methods that are common
 * to other event controllers.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 26, 2012            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public abstract class AbstractRoleEventController implements
        IRoleEventController, IPartListener, RemoveListener {

    protected ISharedDisplaySession session;

    protected List<ResourcePair> resourcesAdded = new ArrayList<ResourcePair>();

    private List<AbstractEditor> resourceEditors = new CopyOnWriteArrayList<AbstractEditor>();

    protected AbstractRoleEventController(ISharedDisplaySession session) {
        this.session = session;
    }

    @Override
    public void startup() {
        session.registerEventHandler(this);
    }

    @Override
    public void shutdown() {
        session.unRegisterEventHandler(this);

        // Orphaned tellestrators, not sure what to do yet about clear
        for (AbstractEditor editor : resourceEditors) {
            deactivateResources(editor);
        }
        for (ResourcePair rp : resourcesAdded) {
            AbstractVizResource<?, ?> resource = rp.getResource();
            if (resource != null) {
                resource.getDescriptor().getResourceList()
                        .removePostRemoveListener(this);
                resource.unload();
            }
        }
        resourcesAdded.clear();
        resourceEditors.clear();
    }

    protected void activateResources(AbstractEditor editor) {
        for (IDisplayPane pane : editor.getDisplayPanes()) {
            activateResources(pane.getRenderableDisplay());
        }
        resourceEditors.add(editor);
        editor.getSite().getPage().addPartListener(this);
    }

    protected void activateResources(IRenderableDisplay display) {
        try {
            IDescriptor descriptor = display.getDescriptor();
            for (ResourcePair resource : getResourcesToAdd()) {
                if (resource.getResource() == null) {
                    resource.setResource(resource.getResourceData().construct(
                            resource.getLoadProperties(), descriptor));
                }
                descriptor.getResourceList().add(resource);
                descriptor.getResourceList().addPostRemoveListener(this);
                resourcesAdded.add(resource);
            }
        } catch (VizException e) {
            Activator.statusHandler.handle(Priority.PROBLEM,
                    "Error adding drawing resource to pane", e);
        }
    }

    protected void deactivateResources(AbstractEditor editor) {
        partClosed(editor);
        editor.getSite().getPage().removePartListener(this);
    }

    protected List<ResourcePair> getResourcesToAdd() {
        List<ResourcePair> resources = new ArrayList<ResourcePair>();
        CollaborationDrawingResourceData resourceData = new CollaborationDrawingResourceData();
        resourceData.setSessionId(session.getSessionId());
        ResourcePair resource = new ResourcePair();
        resource.setResourceData(resourceData);
        resource.setProperties(new ResourceProperties());
        resource.setLoadProperties(new LoadProperties());
        resources.add(resource);
        return resources;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.ResourceList.RemoveListener#notifyRemove
     * (com.raytheon.uf.viz.core.drawables.ResourcePair)
     */
    @Override
    public void notifyRemove(ResourcePair rp) throws VizException {
        if (resourcesAdded.contains(rp)) {
            try {
                Class<?> clazz = rp.getResource().getClass();
                Constructor<?> constructor = clazz.getConstructor(clazz);
                ResourcePair newPair = new ResourcePair();
                newPair.setLoadProperties(rp.getLoadProperties());
                newPair.setProperties(rp.getProperties());
                newPair.setResourceData(rp.getResourceData());
                newPair.setResource((AbstractVizResource<?, ?>) constructor
                        .newInstance(rp.getResource()));
                rp.getResource().getDescriptor().getResourceList().add(newPair);
            } catch (Exception e) {
                Activator.statusHandler
                        .handle(Priority.PROBLEM,
                                "Cannot manage resources from being unloaded that do not have copy constructor",
                                e);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.IPartListener#partClosed(org.eclipse.ui.IWorkbenchPart)
     */
    @Override
    public void partClosed(IWorkbenchPart part) {
        for (AbstractEditor editor : resourceEditors) {
            if (editor == part) {
                for (IDisplayPane pane : editor.getDisplayPanes()) {
                    ResourceList list = pane.getDescriptor().getResourceList();
                    list.removePostRemoveListener(this);
                    for (ResourcePair rp : list) {
                        if (resourcesAdded.contains(rp)) {
                            resourcesAdded.remove(rp);
                            list.remove(rp);
                        }
                    }
                }
                resourceEditors.remove(editor);
            }
        }
    }

    // Unneeded part events
    @Override
    public void partActivated(IWorkbenchPart part) {
    }

    @Override
    public void partBroughtToTop(IWorkbenchPart part) {
    }

    @Override
    public void partDeactivated(IWorkbenchPart part) {
    }

    @Override
    public void partOpened(IWorkbenchPart part) {
    }

}
