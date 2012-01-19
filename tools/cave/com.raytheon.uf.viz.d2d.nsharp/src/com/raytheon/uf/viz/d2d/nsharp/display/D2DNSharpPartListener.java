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

import gov.noaa.nws.ncep.ui.nsharp.skewt.NsharpSkewTEditor;

import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IPartListener2;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPartReference;
import org.eclipse.ui.PartInitException;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;

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

    private static D2DNSharpPartListener instance;

    private D2DNSharpPartListener() {
    }

    public static D2DNSharpPartListener getInstance() {
        if (instance != null) {
            return instance;
        }
        synchronized (D2DNSharpPartListener.class) {
            if (instance == null) {
                instance = new D2DNSharpPartListener();
            }
            return instance;
        }
    }

    private boolean isD2DNSharpPart(IWorkbenchPartReference partRef) {
        if (partRef.getPart(false) instanceof NsharpSkewTEditor) {
            NsharpSkewTEditor editor = (NsharpSkewTEditor) partRef
                    .getPart(false);
            IRenderableDisplay display = editor.getActiveDisplayPane()
                    .getRenderableDisplay();
            return display instanceof D2DNSharpDisplay;
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
                if (page.getActivePart() == view) {
                    // TODO find a better solution to this problem.
                    // It seems like when switching perspectives with the view
                    // we want to hide active hideView causes an exception.
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
            // set visible for any other visible editors, for when there are two
            // nsharp editors visible and one goes invisible.
            for (IEditorReference editor : page.getEditorReferences()) {
                if (page.isPartVisible(editor.getEditor(false))) {
                    partVisible(editor);
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
