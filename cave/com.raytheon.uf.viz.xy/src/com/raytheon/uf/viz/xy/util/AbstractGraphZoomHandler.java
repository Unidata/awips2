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
package com.raytheon.uf.viz.xy.util;

import org.eclipse.swt.widgets.Event;
import org.geotools.geometry.DirectPosition2D;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.xy.AbstractGraphInputHandler;
import com.raytheon.uf.viz.xy.graph.IGraph;
import com.raytheon.uf.viz.xy.graph.XyGraphDescriptor;
import com.raytheon.viz.ui.input.PanHandler;
import com.raytheon.viz.ui.input.preferences.MouseEvent;
import com.raytheon.viz.ui.input.preferences.MousePreferenceManager;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * Handles mouse events in order to zoom graphs.
 * 
 * Note: the initial creation was abstracted out from TimeSeriesZoomHandler and
 * VarHeightZoomHandler.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Jul 16, 2015 4220        mapeters    Initial creation.
 * 
 * </pre>
 * 
 * @author mapeters
 * @version 1.0
 */
public abstract class AbstractGraphZoomHandler extends
        AbstractGraphInputHandler {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractGraphZoomHandler.class);

    protected MousePreferenceManager prefManager = MousePreferenceManager
            .getInstance();

    protected int zoomIndex = 0;

    public AbstractGraphZoomHandler(IRenderableDisplay display) {
        super(display);
    }

    @Override
    public boolean handleMouseWheel(Event event, int x, int y) {
        IDisplayPaneContainer editor = display.getContainer();
        if (editor.getActiveDisplayPane().getDescriptor() instanceof XyGraphDescriptor == false) {
            return super.handleMouseWheel(event, x, y);
        } else {
            MouseEvent mouseEvent = MouseEvent.SCROLL_FORWARD;
            if (event.count == 0) {
                return false;
            } else if (event.count < 0) {
                mouseEvent = MouseEvent.SCROLL_BACK;
            }
            if (prefManager.handleEvent(PanHandler.ZOOMOUT_PREF, mouseEvent)) {
                return zoom(PanHandler.ZOOMOUT_PREF, x, y);
            } else if (prefManager.handleEvent(PanHandler.ZOOMIN_PREF,
                    mouseEvent)) {
                return zoom(PanHandler.ZOOMIN_PREF, x, y);
            }
        }
        return false;
    }

    @Override
    public boolean handleMouseUp(int x, int y, int mouseButton) {
        IDisplayPaneContainer editor = display.getContainer();
        if (editor.getActiveDisplayPane().getDescriptor() instanceof XyGraphDescriptor == false) {
            return super.handleMouseUp(x, y, mouseButton);
        } else {
            if (prefManager.handleClick(PanHandler.ZOOMIN_PREF, mouseButton)) {
                return zoom(PanHandler.ZOOMIN_PREF, x, y);
            } else if (prefManager.handleClick(PanHandler.ZOOMOUT_PREF,
                    mouseButton)) {
                return zoom(PanHandler.ZOOMOUT_PREF, x, y);
            }
        }
        return false;
    }

    protected Coordinate translateClick(int x, int y) {
        IDisplayPaneContainer editor = display.getContainer();
        XyGraphDescriptor desc = (XyGraphDescriptor) editor
                .getActiveDisplayPane().getDescriptor();
        Coordinate grid = editor.translateClick(x, y);
        if (grid == null) {
            return null;
        }
        /*
         * Convert from the overall display coordinate space to the coordinate
         * space for our resource.
         */
        DirectPosition2D dp = new DirectPosition2D(grid.x, grid.y);
        try {
            desc.getGridGeometry().getGridToCRS().transform(dp, dp);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error converting coordinate", e);
        }
        grid.x = dp.x;
        grid.y = dp.y;
        grid.z = 0;
        return grid;
    }

    protected abstract boolean zoom(String pref, int x, int y);

    public void reset() {
        this.zoomIndex = 0;
    }

    /**
     * Performs the zoom on an individual graph, given the index and grid
     * coordinate to zoom to. Also sets the graph's zoomHandler to this if it is
     * not already set.
     * 
     * @param graph
     * @param index
     * @param grid
     */
    protected void performZoom(IGraph graph, int index, Coordinate grid) {
        graph.zoom(index, grid);
        if (graph.getZoomHandler() == null) {
            graph.setZoomHandler(this);
        }
    }
}
