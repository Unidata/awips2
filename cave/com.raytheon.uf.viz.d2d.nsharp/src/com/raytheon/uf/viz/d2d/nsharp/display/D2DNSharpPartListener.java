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

import gov.noaa.nws.ncep.ui.nsharp.display.NsharpEditor;

import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IPartListener2;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IViewReference;
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

/**
 * 
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 26, 2011            bsteffen     Initial creation
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
        if (isD2DNSharpPart(partRef)) {
            IWorkbenchPage page = partRef.getPage();
            IViewPart view = page.findView(VIEW_ID);
            if (view != null) {
                boolean hide = true;
                // set visible for any other visible editors, for when there are
                // two nsharp editors visible and one goes invisible.
                for (IEditorReference editorRef : page.getEditorReferences()) {
                    IEditorPart editor = editorRef.getEditor(false);
                    if (page.isPartVisible(editor)
                            && editor instanceof NsharpEditor) {
                        hide = false;
                    }
                }
                if (hide) {
                    if (page.getActivePart() == view) {
                        // TODO find a better solution to this problem.
                        // It seems like when switching perspectives with the
                        // view we want to hide active hideView causes an
                        // exception.
                        // Making another view active fixes this.
                        for (IViewReference viewRef : page.getViewReferences()) {
                            if (viewRef.getView(false) != view) {
                                page.activate(viewRef.getView(false));
                                break;
                            }
                        }
                    }
                    page.hideView(view);
                }

            }
        }

    }

    @Override
    public void partVisible(IWorkbenchPartReference partRef) {
        if (isD2DNSharpPart(partRef)) {
            IWorkbenchPage page = partRef.getPage();
            try {
                page.showView(VIEW_ID);
            } catch (PartInitException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
    }

    @Override
    public void partInputChanged(IWorkbenchPartReference partRef) {
        // Don't Care
    }

}
