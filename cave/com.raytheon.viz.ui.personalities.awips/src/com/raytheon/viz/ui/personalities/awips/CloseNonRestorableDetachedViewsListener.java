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
package com.raytheon.viz.ui.personalities.awips;

import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.ui.IPerspectiveDescriptor;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.internal.Perspective;
import org.eclipse.ui.internal.WorkbenchPage;
import org.eclipse.ui.views.IViewDescriptor;
import org.eclipse.ui.views.IViewRegistry;

/**
 * Listener to close all non-restorable detached views on CAVE close. This
 * prevents an Eclipse bug from showing up, where an empty dialog appears and if
 * that dialog is closed the view is not allowed to open again.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 8, 2013            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class CloseNonRestorableDetachedViewsListener implements Listener {

    /**
     * 
     */
    public CloseNonRestorableDetachedViewsListener() {
    }

    @Override
    @SuppressWarnings("restriction")
    public void handleEvent(Event event) {
        IViewRegistry reg = PlatformUI.getWorkbench().getViewRegistry();
        for (IWorkbenchWindow win : PlatformUI.getWorkbench()
                .getWorkbenchWindows()) {
            for (IWorkbenchPage page : win.getPages()) {
                IPerspectiveDescriptor[] perspectives = page
                        .getOpenPerspectives();
                for (IPerspectiveDescriptor desc : perspectives) {
                    WorkbenchPage wPage = (WorkbenchPage) page;
                    Perspective persp = wPage.findPerspective(desc);
                    for (IViewReference ref : persp.getViewReferences()) {
                        IViewDescriptor descr = reg.find(ref.getId());
                        if (descr != null && descr.isRestorable() == false) {
                            if (wPage.getActivePerspective() != persp) {
                                persp.hideView(ref);
                            } else {
                                page.hideView(ref);
                            }
                        }
                    }
                }

            }
        }
    }

}
