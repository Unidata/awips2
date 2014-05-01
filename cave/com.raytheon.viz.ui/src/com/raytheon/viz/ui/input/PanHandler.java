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
package com.raytheon.viz.ui.input;

import java.io.IOException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Event;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IView;
import com.raytheon.uf.viz.core.localization.HierarchicalPreferenceStore;
import com.raytheon.viz.ui.UiPlugin;
import com.raytheon.viz.ui.input.preferences.MouseEvent;
import com.raytheon.viz.ui.input.preferences.MousePreferenceManager;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * PanHandler implements the pan capability's mouse interactions
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Dec 28, 2007             chammack    Refactored out of PanTool
 *    Oct 27, 2009 #2354       bsteffen    Configured input handler to use mouse preferences
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class PanHandler extends InputAdapter {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PanHandler.class);

    private static final String PAN_PERCENTAGE = "panningPercentage";

    private static final String PAN_PREF = "com.raytheon.viz.ui.input.pan";

    public static final String ZOOMIN_PREF = "com.raytheon.viz.ui.input.zoomin";

    public static final String ZOOMOUT_PREF = "com.raytheon.viz.ui.input.zoomout";

    private static final String CLEAR_PREF = "com.raytheon.viz.ui.clear";

    private MousePreferenceManager prefManager = MousePreferenceManager
            .getInstance();

    protected IDisplayPaneContainer container;

    protected float theLastMouseX = 0;

    protected float theLastMouseY = 0;

    protected int[] downPosition;

    protected Job job;

    protected int zoomDir = 0;

    protected Double panningPercentage = null;

    /**
     * Constructor
     * 
     * @param container
     *            the container associated with the tool
     */
    public PanHandler(IDisplayPaneContainer container) {
        this.container = container;
    }

    /**
     * Retarget the pan handler to a specific container
     * 
     * @param container
     */
    public void setContainer(IDisplayPaneContainer container) {
        this.container = container;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDown(int, int,
     * int)
     */
    @Override
    public boolean handleMouseDown(int x, int y, int button) {

        // Dont do anything if it is right button or if it is another button and
        // we have no preference set for that button
        if (prefManager.handleLongClick(ZOOMIN_PREF, button)
                || prefManager.handleLongClick(ZOOMOUT_PREF, button)) {
            theLastMouseX = x;
            theLastMouseY = y;
            if (prefManager.handleLongClick(ZOOMIN_PREF, button)) {
                zoomDir = 1;
            } else {
                zoomDir = -1;
            }
            if (job == null) {
                job = new Job("ZoomHandler") {

                    @Override
                    protected IStatus run(IProgressMonitor monitor) {
                        if (zoomDir != 0) {
                            for (IDisplayPane pane : container
                                    .getDisplayPanes()) {

                                pane.zoom(zoomDir, (int) theLastMouseX,
                                        (int) theLastMouseY);

                            }
                            container.refresh();
                            job.schedule(50);
                        }
                        return Status.OK_STATUS;
                    }

                };
            }
            if (job.getState() != Job.RUNNING) {
                job.schedule(500);
            }
        }
        if (!prefManager.handleDrag(PAN_PREF, button)
                && !prefManager.handleClick(ZOOMIN_PREF, button)
                && !prefManager.handleClick(ZOOMOUT_PREF, button))
            return false;
        downPosition = new int[] { x, y };
        theLastMouseX = x;
        theLastMouseY = y;
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDownMove(int,
     * int, int)
     */
    @Override
    public boolean handleMouseDownMove(int aX, int aY, int button) {
        if (prefManager.handleLongClick(ZOOMIN_PREF, button)
                || prefManager.handleLongClick(ZOOMOUT_PREF, button)) {
            theLastMouseX = aX;
            theLastMouseY = aY;
        }
        if ((!prefManager.handleDrag(PAN_PREF, button)) || container == null)
            return false;
        double[] grid = container.getActiveDisplayPane()
                .screenToGrid(aX, aY, 0);
        double[] theLastGrid = container.getActiveDisplayPane().screenToGrid(
                theLastMouseX, theLastMouseY, 0);

        IDisplayPane[] panes = container.getDisplayPanes();
        for (IDisplayPane p : panes) {
            // translate grid coordinates to screen coordinates
            double[] screen = p.gridToScreen(grid);
            double[] theLastScreen = p.gridToScreen(theLastGrid);

            IView tmpView = (IView) p.getRenderableDisplay().getView().clone();
            tmpView.shiftExtent(screen, theLastScreen, p.getTarget());
            IExtent tmpExtent = tmpView.getExtent();
            double percentage = getPanningPercentage();
            double xMinThreshold = tmpExtent.getMinX()
                    + (tmpExtent.getMaxX() - tmpExtent.getMinX()) * percentage;
            double xMaxThreshold = tmpExtent.getMinX()
                    + (tmpExtent.getMaxX() - tmpExtent.getMinX())
                    * (1.0 - percentage);
            double yMinThreshold = tmpExtent.getMinY()
                    + (tmpExtent.getMaxY() - tmpExtent.getMinY()) * percentage;
            double yMaxThreshold = tmpExtent.getMinY()
                    + (tmpExtent.getMaxY() - tmpExtent.getMinY())
                    * (1.0 - percentage);

            double height = p.getRenderableDisplay().getWorldHeight();
            double width = p.getRenderableDisplay().getWorldWidth();

            double aX2 = screen[0], aY2 = screen[1];

            if ((0 <= xMinThreshold && width >= xMaxThreshold) == false) {
                if (((width < xMaxThreshold && theLastScreen[0] < aX) || (0 > xMinThreshold && theLastMouseX > aX)) == false) {
                    aX2 = theLastScreen[0];
                }
            }

            if ((0 <= yMinThreshold && height >= yMaxThreshold) == false) {
                if (((height < yMaxThreshold && theLastScreen[1] < aY) || (0 > yMinThreshold && theLastMouseY > aY)) == false) {
                    aY2 = theLastScreen[1];
                }
            }

            if (aX2 != theLastScreen[0] || aY2 != theLastScreen[1]) {
                p.shiftExtent(new double[] { aX2, aY2 }, theLastScreen);
            }
        }
        theLastMouseX = aX;
        theLastMouseY = aY;

        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseUp(int, int, int)
     */
    public boolean handleMouseUp(int x, int y, int button) {
        zoomDir = 0;

        if (prefManager.handleClick(CLEAR_PREF, button)) {
            for (IDisplayPane pane : container.getDisplayPanes()) {
                pane.clear();
            }
            return true;
        }

        if (this.downPosition == null || downPosition[0] != x
                || downPosition[1] != y)
            return false;

        if (prefManager.handleClick(ZOOMIN_PREF, button)
                || prefManager.handleClick(ZOOMOUT_PREF, button)) {
            IDisplayPane[] panes = container.getDisplayPanes();

            Coordinate world = container.translateClick(x, y);
            if (world == null) {
                return false;
            }
            for (IDisplayPane pane : panes) {
                if (prefManager.handleClick(ZOOMIN_PREF, button)) {
                    pane.zoom(15, x, y);
                } else {
                    pane.zoom(-15, x, y);
                }
            }
            container.refresh();
            return false;
        }

        container.refresh();
        return false;
    }

    private double getPanningPercentage() {
        if (panningPercentage == null) {
            HierarchicalPreferenceStore store = UiPlugin.getDefault()
                    .getPreferenceStore();
            panningPercentage = UiPlugin.getDefault().getPreferenceStore()
                    .getDouble(PAN_PERCENTAGE);

            if (panningPercentage < 0.0 || panningPercentage > 1.0) {
                // bad value set, reset and store
                panningPercentage = panningPercentage < 0.0 ? 0.0 : 1.0;

                store.setValue(PAN_PERCENTAGE, panningPercentage.doubleValue());
                try {
                    store.save();
                } catch (IOException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error saving panning percentage preference", e);
                }
            }
        }
        return panningPercentage.doubleValue();
    }

    @Override
    public boolean handleMouseWheel(Event event, int x, int y) {
        if ((event.stateMask & SWT.SHIFT) == 0
                && container.translateClick(x, y) != null) {
            MouseEvent mouseEvent = MouseEvent.SCROLL_FORWARD;
            if (event.count == 0) {
                // Dont scroll zero, if that is even possible
                return false;
            } else if (event.count < 0) {
                mouseEvent = MouseEvent.SCROLL_BACK;
            }
            IDisplayPane[] panes = container.getDisplayPanes();
            double[] grid = container.getActiveDisplayPane().screenToGrid(x, y,
                    0);
            if (prefManager.handleEvent(PanHandler.ZOOMOUT_PREF, mouseEvent)) {
                for (IDisplayPane pane : panes) {
                    double[] screen = pane.gridToScreen(grid);
                    pane.zoom(-Math.abs(event.count), (int) screen[0],
                            (int) screen[1]);
                }
                return true;
            } else if (prefManager.handleEvent(PanHandler.ZOOMIN_PREF,
                    mouseEvent)) {
                for (IDisplayPane pane : panes) {
                    double[] screen = pane.gridToScreen(grid);
                    pane.zoom(Math.abs(event.count), (int) screen[0],
                            (int) screen[1]);
                }
                return true;
            } else if (prefManager.handleEvent(CLEAR_PREF, mouseEvent)) {
                for (IDisplayPane pane : panes) {
                    pane.clear();
                    pane.refresh();
                }
                return true;
            }
        }
        return false;
    }

}
