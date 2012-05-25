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
package com.raytheon.uf.viz.collaboration.ui.telestrator;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.core.ContextManager;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IVizEditorChangedListener;
import com.raytheon.uf.viz.core.icon.IconUtil;
import com.raytheon.uf.viz.drawing.DrawingToolbar;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.VizWorkbenchManager;

/**
 * Collaboration drawing toolbar, adds locking of collaborators
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 23, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class CollaborationDrawingToolbar extends DrawingToolbar implements
        IVizEditorChangedListener {

    private static CollaborationDrawingToolbar instance;

    private Set<CollaborationDrawingResource> resources = new HashSet<CollaborationDrawingResource>();

    public static CollaborationDrawingToolbar openToolbar(
            CollaborationDrawingResource resource) {
        if (instance == null) {
            instance = new CollaborationDrawingToolbar(VizWorkbenchManager
                    .getInstance().getCurrentWindow());
            instance.open();
        }
        instance.resources.add(resource);
        return instance;
    }

    /**
     * Get the current instance of the toolbar, will not create if null
     * 
     * @return
     */
    public static CollaborationDrawingToolbar getInstance() {
        return instance;
    }

    private ToolItem leaderOnly;

    private List<CollaborationDrawingResource> activeEditorResources = new ArrayList<CollaborationDrawingResource>();

    private IWorkbenchWindow window;

    /**
     * @param parentShell
     */
    private CollaborationDrawingToolbar(IWorkbenchWindow window) {
        super(window.getShell());
        this.window = window;
        setText("Collaboration Drawing");
        VizWorkbenchManager.getInstance().addListener(this);
        IDisplayPaneContainer active = EditorUtil.getActiveVizContainer(window);
        if (active != null) {
            editorChanged(active);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#opened()
     */
    @Override
    protected void opened() {
        super.opened();
        ContextManager.getInstance(window).activateContexts(this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.drawing.DrawingToolbar#initializeComponents(org.eclipse
     * .swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {
        super.initializeComponents(shell);
        Listener activateDeactivate = new Listener() {
            @Override
            public void handleEvent(Event event) {
                switch (event.type) {
                case SWT.Activate:
                    ContextManager.getInstance(PlatformUI.getWorkbench())
                            .activateContexts(CollaborationDrawingToolbar.this);
                    break;
                case SWT.Deactivate:
                    ContextManager.getInstance(PlatformUI.getWorkbench())
                            .deactivateContexts(
                                    CollaborationDrawingToolbar.this);
                    break;
                }
            }
        };
        shell.addListener(SWT.Activate, activateDeactivate);
        shell.addListener(SWT.Deactivate, activateDeactivate);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.drawing.DrawingToolbar#addToolsToToolBar(org.eclipse
     * .swt.widgets.ToolBar)
     */
    @Override
    protected void addToolsToToolBar(ToolBar toolbar) {
        super.addToolsToToolBar(toolbar);
        undoItem.setToolTipText("Ctrl+Z to Undo");
        redoItem.setToolTipText("Ctrl+Y to Undo");

        leaderOnly = new ToolItem(toolbar, SWT.CHECK);
        leaderOnly.setText("Lock Collaborators");
        leaderOnly.setImage(IconUtil.getImageDescriptor(
                Activator.getDefault().getBundle(), "multiple_draw.gif")
                .createImage());
        leaderOnly.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                for (CollaborationDrawingResource resource : activeEditorResources) {
                    resource.setLockingDrawing(leaderOnly.getSelection());
                }
            }
        });
    }

    /**
     * Fuction for notifying the toolbar a resource has changed, will update the
     * UI if the resource is currently being managed by the toolbar
     * 
     * @param resource
     */
    public void resourceChanged(CollaborationDrawingResource resource) {
        if (activeEditorResources.contains(resource)) {
            updateItemState();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.drawing.DrawingToolbar#nullLayer()
     */
    @Override
    protected void nullLayer() {
        super.nullLayer();
        leaderOnly.setSelection(false);
        leaderOnly.setEnabled(false);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.drawing.DrawingToolbar#validLayer()
     */
    @Override
    protected void validLayer() {
        super.validLayer();
        for (CollaborationDrawingResource resource : activeEditorResources) {
            leaderOnly.setEnabled(resource.isSessionLeader());
            leaderOnly.setSelection(resource.isLockingDrawing());
            break;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#disposed()
     */
    @Override
    protected void disposed() {
        super.disposed();
        resources.clear();
        VizWorkbenchManager.getInstance().removeListener(this);
        ContextManager.getInstance(window).deactivateContexts(this);
    }

    /**
     * Function for notifying the toolbar a resource has been disposed, toolbar
     * will close when no resources are left being managed
     * 
     * @param resource
     */
    public synchronized void disposed(CollaborationDrawingResource resource) {
        resources.remove(resource);
        activeEditorResources.remove(resource);
        if (resources.size() == 0) {
            close();
            instance = null;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IVizEditorChangedListener#editorChanged(com.
     * raytheon.uf.viz.core.IDisplayPaneContainer)
     */
    @Override
    public void editorChanged(IDisplayPaneContainer container) {
        List<CollaborationDrawingResource> activeResources = new ArrayList<CollaborationDrawingResource>();
        for (IDisplayPane pane : container.getDisplayPanes()) {
            List<CollaborationDrawingResource> paneResources = pane
                    .getDescriptor()
                    .getResourceList()
                    .getResourcesByTypeAsType(
                            CollaborationDrawingResource.class);
            activeResources.addAll(paneResources);
        }
        activeEditorResources = activeResources;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.drawing.DrawingToolbar#undo()
     */
    @Override
    public void undo() {
        super.undo();
        refresh();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.drawing.DrawingToolbar#redo()
     */
    @Override
    public void redo() {
        super.redo();
        refresh();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.drawing.DrawingToolbar#clear()
     */
    @Override
    protected void clear() {
        super.clear();
        refresh();
    }

    /**
     * Refresh the resource to repaint
     */
    private void refresh() {
        for (CollaborationDrawingResource resource : activeEditorResources) {
            resource.issueRefresh();
        }
    }
}
