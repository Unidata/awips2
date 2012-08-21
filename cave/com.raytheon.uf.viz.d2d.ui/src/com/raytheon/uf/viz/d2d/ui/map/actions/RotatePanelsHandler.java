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
package com.raytheon.uf.viz.d2d.ui.map.actions;

import java.util.List;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.capabilities.BlendableCapability;
import com.raytheon.uf.viz.d2d.core.legend.D2DLegendResource;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.HistoryList;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;
import com.raytheon.viz.ui.tools.AbstractTool;

public class RotatePanelsHandler extends AbstractTool {

    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        IDisplayPaneContainer container = EditorUtil.getActiveVizContainer();
        if (container == null) {
            return null;
        }
        // direction is usually +1 or -1 to specify which direction to rotate
        String dirStr = arg0.getParameter("direction");
        // start index is the index to start rotating from, for example if you
        // want to display pane 3 then you set startIndex to 2 and direction to
        // +1, this is done so that if pane 3 has no data it will rotate past
        // pane 3 and the next available pane with data.
        String startStr = arg0.getParameter("startIndex");
        // hideIndex can be set to 0 or 1 to specify which half of a blended
        // images should be hidden.
        String hideIndexStr = arg0.getParameter("hideIndex");
        boolean toggle = false;
        int dir = Integer.parseInt(dirStr);
        if (startStr == null) {
            // If there is no startIndex rotate from the currently displayed
            // pane
            if (container instanceof IMultiPaneEditor) {
                // If it is going from multiple panes to a single pain, toggle
                // the blended image
                toggle = ((IMultiPaneEditor) container).displayedPaneCount() > 1;
            }
            if (rotateCurrent(container, dir)) {
                // if it wraps around when we rotate, toggle the blended image.
                toggle = true;
            }
        } else {
            int start = Integer.parseInt(startStr);
            rotate(container, start, dir);
        }

        Integer hideIndex = null;
        if (hideIndexStr != null) {
            hideIndex = Integer.parseInt(hideIndexStr);
        }
        if (toggle || hideIndex != null) {
            for (IDisplayPane pane : container.getDisplayPanes()) {
                for (ResourcePair rp : pane.getDescriptor().getResourceList()) {
                    if (rp.getResource() != null
                            && rp.getResource().hasCapability(
                                    BlendableCapability.class)) {
                        BlendableCapability cap = rp.getResource()
                                .getCapability(BlendableCapability.class);
                        if (hideIndex != null) {
                            cap.toggle(hideIndex);
                        } else {
                            cap.toggle();
                        }
                    }
                }
            }
        }

        if (container instanceof IMultiPaneEditor) {
            ((IMultiPaneEditor) container).setSelectedPane(
                    IMultiPaneEditor.IMAGE_ACTION, null);
        }

        return null;
    }

    /**
     * rotate starting from the activeDisplayPane in direction
     * 
     * @param direction
     *            should be either 1, or -1
     * @return true if the data wrapped to the other side of the pane array.
     */
    public boolean rotateCurrent(IDisplayPaneContainer container, int direction) {
        if (container instanceof IMultiPaneEditor) {
            IMultiPaneEditor mEditor = (IMultiPaneEditor) container;
            int index = getIndex(container, mEditor.getActiveDisplayPane());
            return rotate(container, index, direction);
        }
        return false;
    }

    public void rotate(IDisplayPaneContainer container, IDisplayPane pane,
            int direction) {
        if (container instanceof IMultiPaneEditor) {
            rotate(container, getIndex(container, pane), direction);
        }
    }

    /**
     * rotate starting from a specific index in direction
     * 
     * @param index
     *            the index to start rotating from
     * @param direction
     *            should be either 1, or -1
     * @return true if the data wrapped to the other side of the pane array.
     */
    private boolean rotate(IDisplayPaneContainer container, int index,
            int direction) {
        boolean wrapped = false;
        IMultiPaneEditor mEditor = (IMultiPaneEditor) container;
        IDisplayPane[] panes = mEditor.getDisplayPanes();
        if (panes.length == 4) {
            // Pretend the panels are in the order 0, 1, 3, 2 because
            // AWIPS I rotates the panes in a weird order = ul, ur, lr, ll
            IDisplayPane[] reorderedPanes = new IDisplayPane[4];
            reorderedPanes[0] = panes[0];
            reorderedPanes[1] = panes[1];
            reorderedPanes[2] = panes[3];
            reorderedPanes[3] = panes[2];
            panes = reorderedPanes;
        }

        IDisplayPane paneToShow = null;

        if (panes != null && index < panes.length && panes.length != 1) {
            boolean from4To1 = mEditor.displayedPaneCount() > 1;
            boolean hasProducts = false;
            if (panes[index] != null) {
                List<D2DLegendResource> rscs = panes[index].getDescriptor()
                        .getResourceList()
                        .getResourcesByTypeAsType(D2DLegendResource.class);
                for (D2DLegendResource rsc : rscs) {
                    hasProducts = rsc.hasProducts();
                    if (hasProducts) {
                        break;
                    }
                }
            }

            if (from4To1 && hasProducts) {
                paneToShow = panes[index];
            } else {
                IDisplayPane displayedPane = null;
                boolean done = false;
                for (int i = index + direction; !done; i = i + direction) {
                    if (i < 0) {
                        i += panes.length;
                        wrapped = true;
                    } else if (i >= panes.length) {
                        wrapped = true;
                        i -= panes.length;
                    }
                    IDisplayPane pane = panes[i];
                    if (i == index) {
                        done = true;
                    }

                    if (pane != panes[index] && pane != null) {
                        List<D2DLegendResource> rscs = pane
                                .getDescriptor()
                                .getResourceList()
                                .getResourcesByTypeAsType(
                                        D2DLegendResource.class);
                        for (D2DLegendResource rsc : rscs) {
                            if (rsc.hasProducts()) {
                                displayedPane = pane;
                                done = true;
                                break;
                            }
                        }
                    }
                }
                paneToShow = displayedPane != null ? displayedPane
                        : panes[index];
            }

            for (IDisplayPane displayPane : panes) {
                if (displayPane != paneToShow) {
                    mEditor.hidePane(displayPane);
                }
            }
            mEditor.showPane(paneToShow);
            mEditor.setSelectedPane(IMultiPaneEditor.VISIBLE_PANE, paneToShow);

            container.refresh();
        }
        try {
            HistoryList.getInstance().refreshLatestBundle();
        } catch (VizException e) {
            e.printStackTrace();
        }
        return wrapped;
    }

    private int getIndex(IDisplayPaneContainer container, IDisplayPane pane) {
        IMultiPaneEditor mEditor = (IMultiPaneEditor) container;
        IDisplayPane[] panes = mEditor.getDisplayPanes();
        int currentIndex = -1;
        for (int i = 0; i < panes.length; i++) {
            if (panes[i] == pane) {
                currentIndex = i;
            }
        }
        // Pretend the panels are in the order 0, 1, 3, 2 because
        // AWIPS I rotates the panes in a wierd order = ul, ur, lr, ll
        if (panes.length == 4 && currentIndex == 3) {
            currentIndex = 2;
        } else if (panes.length == 4 && currentIndex == 2) {
            currentIndex = 3;
        }
        return currentIndex;
    }
}
