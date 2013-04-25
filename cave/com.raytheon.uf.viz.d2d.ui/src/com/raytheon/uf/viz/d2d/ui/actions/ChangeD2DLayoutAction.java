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
package com.raytheon.uf.viz.d2d.ui.actions;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.IPerspectiveRegistry;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.maps.actions.NewMapEditor;
import com.raytheon.viz.ui.EditorUtil;

/**
 * This action class sets the number of views for the current window for D2D and
 * resets the perspective so the new layout takes effect
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 28, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class ChangeD2DLayoutAction extends AbstractHandler {

    private static Map<IWorkbenchWindow, Integer> viewMap = new HashMap<IWorkbenchWindow, Integer>();

    public static int getViewCount() {
        Integer views = viewMap.get(PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow());
        return views != null ? views : 4;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.IHandler#execute(org.eclipse.core.commands.
     * ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        String sideViews = event.getParameter("sideViews");
        if (sideViews != null) {
            int numViews = Integer.parseInt(sideViews);
            IWorkbenchWindow window = PlatformUI.getWorkbench()
                    .getActiveWorkbenchWindow();
            if (window != null) {
                viewMap.put(window, numViews);
                IWorkbenchPage page = window.getActivePage();
                if (page != null) {
                    // If the perspective has been saved since it was created
                    // the D2DPerspective will not get to adjust the layout, so
                    // we need to revert the perspective
                    IPerspectiveRegistry pReg = window.getWorkbench()
                            .getPerspectiveRegistry();
                    pReg.revertPerspective(page.getPerspective());
                    // now reset it which allows the D2DPerspective to lay it
                    // out.
                    page.resetPerspective();
                }
            }

        }

        // get current map editor, if non existent create one.
        IDisplayPaneContainer part = EditorUtil.getActiveVizContainer();
        if (part == null) {
            new NewMapEditor().execute(null);
        }

        return null;
    }
}
