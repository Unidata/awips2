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
package com.raytheon.uf.viz.d2d.nsharp.display;

import org.eclipse.e4.ui.model.application.ui.MElementContainer;
import org.eclipse.e4.ui.model.application.ui.MUIElement;
import org.eclipse.e4.ui.model.application.ui.advanced.MPlaceholder;
import org.eclipse.e4.ui.model.application.ui.basic.MPart;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IPartListener2;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartReference;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.viz.ui.VizWorkbenchManager;

import gov.noaa.nws.ncep.ui.nsharp.display.NsharpEditor;

/**
 * 
 * Listener which ensures that the {@link D2DNSharpPaletteWindow} is open if and
 * only if an nsharp editor is currently open.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 26, 2011            bsteffen     Initial creation
 * Jan 28, 2016 5193       bsteffen     Update for eclipse 4.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class D2DNSharpPartListener implements IPartListener2 {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(D2DNSharpPartListener.class);

    private static final String VIEW_ID = D2DNSharpPaletteWindow.class
            .getCanonicalName();

    private final AbstractVizResource<?, ?> resource;

    public D2DNSharpPartListener(AbstractVizResource<?, ?> resource) {
        this.resource = resource;
    }

    public void enable() {
        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                IWorkbenchPage page = VizWorkbenchManager.getInstance()
                        .getCurrentWindow().getActivePage();
                page.addPartListener(D2DNSharpPartListener.this);
                for (IEditorReference editor : page.getEditorReferences()) {
                    if (page.isPartVisible(editor.getEditor(false))) {
                        partVisible(editor);
                    }
                }
            }
        });
    }

    public void disable() {
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                IWorkbenchWindow window = VizWorkbenchManager.getInstance()
                        .getCurrentWindow();
                if (window == null) {
                    // window will be null when CAVE is shutting down
                    return;
                }
                IWorkbenchPage page = window.getActivePage();
                page.removePartListener(D2DNSharpPartListener.this);
            }
        });
    }

    private boolean isD2DNSharpPart(IWorkbenchPartReference partRef) {
        IWorkbenchPart part = partRef.getPart(false);
        if (part instanceof NsharpEditor) {
            NsharpEditor editor = (NsharpEditor) part;
            for (IDisplayPane pane : editor.getDisplayPanes()) {
                if (pane.getRenderableDisplay().getDescriptor() == resource
                        .getDescriptor()) {
                    return true;
                }
            }
        }
        return false;
    }

    @Override
    public void partActivated(IWorkbenchPartReference partRef) {

    }

    @Override
    public void partBroughtToTop(IWorkbenchPartReference partRef) {

    }

    @Override
    public void partClosed(IWorkbenchPartReference partRef) {

    }

    @Override
    public void partDeactivated(IWorkbenchPartReference partRef) {

    }

    @Override
    public void partOpened(IWorkbenchPartReference partRef) {

    }

    @Override
    public void partHidden(IWorkbenchPartReference partRef) {
        scheduleUpdate(partRef);
    }

    @Override
    public void partVisible(IWorkbenchPartReference partRef) {
        scheduleUpdate(partRef);
    }

    @Override
    public void partInputChanged(IWorkbenchPartReference partRef) {
        // Don't Care
    }

    /**
     * Update the visibility of the nsharp view if the provided part is a nsharp
     * editor that is the responsibility of this listener.
     * 
     * @param partRef
     */
    protected void scheduleUpdate(IWorkbenchPartReference partRef) {
        if (!isD2DNSharpPart(partRef)) {
            return;
        }
        final IWorkbenchPage page = partRef.getPage();
        Display display = page.getWorkbenchWindow().getShell().getDisplay();
        display.asyncExec(new Runnable() {

            @Override
            public void run() {
                updateViewVisibility(page);
            }

        });
    }

    protected void updateViewVisibility(IWorkbenchPage page) {
        IViewPart view = page.findView(VIEW_ID);
        boolean visible = false;
        for (IEditorReference editorRef : page.getEditorReferences()) {
            IEditorPart editor = editorRef.getEditor(false);
            if (editor instanceof NsharpEditor) {
                if (page.isPartVisible(editor)) {
                    visible = true;
                    break;
                } else if (view != null && checkSharedStack(editor, view)) {
                    visible = true;
                    break;
                }
            }
        }
        if (visible && view == null) {
            try {
                page.showView(VIEW_ID);
            } catch (PartInitException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Cannot open NSharp View.", e);
            }
        } else if (!visible && view != null) {
            page.hideView(view);
        }
    }

    /**
     * When a view and an editor are in the same stack then errors will occur if
     * you link their visibility.
     */
    protected boolean checkSharedStack(IEditorPart editor, IViewPart view) {
        MPart modelEditor = editor.getSite().getService(MPart.class);
        MPart modelView = view.getSite().getService(MPart.class);
        MElementContainer<MUIElement> editorParent = modelEditor.getParent();
        MElementContainer<MUIElement> viewParent = modelView.getParent();
        if (editorParent == null) {
            MPlaceholder placeholder = modelEditor.getCurSharedRef();
            if (placeholder != null) {
                editorParent = placeholder.getParent();
            }
        }
        if (viewParent == null) {
            MPlaceholder placeholder = modelView.getCurSharedRef();
            if (placeholder != null) {
                viewParent = placeholder.getParent();
            }
        }
        return viewParent == editorParent;
    }

}
