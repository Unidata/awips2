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
import org.eclipse.e4.ui.model.application.ui.MElementContainer;
import org.eclipse.e4.ui.model.application.ui.MUIElement;
import org.eclipse.e4.ui.model.application.ui.basic.MWindow;
import org.eclipse.e4.ui.workbench.modeling.EModelService;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.maps.actions.NewMapEditor;
import com.raytheon.uf.viz.d2d.ui.map.SideView;
import com.raytheon.viz.ui.EditorUtil;

/**
 * This action class sets the number of views for the current window for D2D and
 * re-layouts the perspective so the new layout takes effect
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 28, 2010            mschenke    Initial creation
 * Jan 23, 2018 7082       njensen     Rewrote execute() to use Eclipse 4 MUIElements
 *                                     to resize the display for differing number of
 *                                     side views
 * 
 * </pre>
 * 
 * @author mschenke
 */

public class ChangeD2DLayoutAction extends AbstractHandler {

    private static Map<IWorkbenchWindow, Integer> viewMap = new HashMap<>();

    public static int getViewCount() {
        Integer views = viewMap
                .get(PlatformUI.getWorkbench().getActiveWorkbenchWindow());
        return views != null ? views : 4;
    }

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
                    EModelService modelService = window
                            .getService(EModelService.class);
                    MWindow mwindow = window.getService(MWindow.class);
                    MUIElement view = modelService
                            .find(SideView.ID + ":sideView1", mwindow);
                    if (view != null) {
                        MElementContainer<MUIElement> sideViewsContainer = view
                                .getParent();
                        sizeWidth(sideViewsContainer, numViews);
                        sizeHeight(sideViewsContainer, numViews);
                    }
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

    /**
     * Proportionally sizes each side view height to the same height based on
     * the number of side views. Each container is made up of two children which
     * are percentages based on 10000. We must carve out an appropriately sized
     * side view out of the entire height of the container (the left column that
     * holds side views). Each time this recurses, it shrinks the area available
     * in the container so we recalculate the height based on how many have yet
     * to be carved out.
     * 
     * For example, say we want 4 side views. Remember, every container has two
     * children. So the first child of the container should be 25% (1/4) and the
     * second child should be 75% (3/4). Now the second child takes up the space
     * of 3 side views and has two children of its own. Therefore, in that area
     * of 3 side views we want the first child to have 33% (1/3) and the second
     * child to have 67% (2/3). By now we've sized the top two side views, but
     * underneath them is the area containing two more side views. Now that
     * we're down to a container holding 2 side views, we want them equally
     * spaced, so we give each of the two children 50% (1/2).
     * 
     * 
     * @param sideViewsContainer
     *            the container made up of two children
     * @param nSideViews
     *            the number of side views to size for
     */
    @SuppressWarnings("unchecked")
    private void sizeHeight(MElementContainer<MUIElement> sideViewsContainer,
            int nSideViews) {
        // top is always a side view
        MUIElement top = sideViewsContainer.getChildren().get(0);
        MUIElement bottom = sideViewsContainer.getChildren().get(1);
        if (nSideViews > 1) {
            int diff = 10000 / nSideViews;
            top.setContainerData(Integer.toString(diff));
            bottom.setContainerData(Integer.toString(10000 - diff));
            if (bottom instanceof MElementContainer) {
                sizeHeight((MElementContainer<MUIElement>) bottom,
                        nSideViews - 1);
            }
        } else if (nSideViews == 1) {
            // hide lower side views
            top.setContainerData("9999");
            bottom.setContainerData("1");
        }
    }

    /**
     * Proportionally sizes the side view width based on the number of size
     * views. The sideViewsContainer's parent will be the application's screen.
     * At most the width will max out at 50% of the screen.
     * 
     * This algorithm to determine width was derived from the legacy behavior
     * with 2 and 4 side panes.
     * 
     * @param sideViewsContainer
     *            the container made up of two children
     * @param nSideViews
     *            the number of side views to size for
     */
    private void sizeWidth(MElementContainer<MUIElement> sideViewsContainer,
            int nSideViews) {
        if (nSideViews > 5) {
            nSideViews = 5;
        }
        int width = (6 - nSideViews) * 1000;
        int diff = 10000 - width;
        MElementContainer<MUIElement> screenContainer = sideViewsContainer
                .getParent();
        /*
         * sideViewsContainer is the same as
         * screenContainer.getChildren().get(0)
         */
        sideViewsContainer.setContainerData(Integer.toString(width));
        screenContainer.getChildren().get(1)
                .setContainerData(Integer.toString(diff));
    }
}
