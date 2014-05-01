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
package com.raytheon.uf.viz.xy.timeseries.util;

import java.util.Stack;

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
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 16, 2009            mschenke     Initial creation
 * Dec 11, 2013 DR 16795   D. Friedman  Transform pixel coordinate for zoom
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class TimeSeriesZoomHandler extends AbstractGraphInputHandler {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(TimeSeriesZoomHandler.class);

    private MousePreferenceManager prefManager = MousePreferenceManager
            .getInstance();

    protected Stack<IGraph> hiddenGraphs = new Stack<IGraph>();

    protected int zoomIndex = 0;

    public TimeSeriesZoomHandler(IRenderableDisplay display) {
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
                return zoomOut(x, y);
            } else if (prefManager.handleEvent(PanHandler.ZOOMIN_PREF,
                    mouseEvent)) {
                return zoomIn(x, y);
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
                return zoomIn(x, y);
            } else if (prefManager.handleClick(PanHandler.ZOOMOUT_PREF,
                    mouseButton)) {
                return zoomOut(x, y);
            }
        }
        return false;

    }

    private boolean zoomIn(int x, int y) {
        IDisplayPaneContainer editor = display.getContainer();
        Coordinate grid = translateClick(x, y);
        if (grid == null) {
            return false;
        }
        XyGraphDescriptor desc = (XyGraphDescriptor) editor
                .getActiveDisplayPane().getDescriptor();
        int visibleGraphs = desc.getGraphResource().visibleGraphCount();
        int totalGraphs = desc.getGraphResource().totalGraphCount();
        ++zoomIndex;
        if (visibleGraphs > 1) {
            IGraph graphToHide = desc.getGraphResource().getFurthestGraph(grid);
            desc.getGraphResource().hideGraph(graphToHide);
            hiddenGraphs.push(graphToHide);
        } else {
            IGraph graph = desc.getGraphResource().getClosestGraph(grid);
            if (graph.getExtent().contains(new double[] { grid.x, grid.y })) {
                graph.zoom((int) Math.pow(2, zoomIndex - totalGraphs + 1), grid);
            } else {
                zoomIndex--;
            }
        }
        return true;
    }

    private boolean zoomOut(int x, int y) {
        IDisplayPaneContainer editor = display.getContainer();
        Coordinate grid = translateClick(x, y);
        if (grid == null) {
            return false;
        }
        XyGraphDescriptor desc = (XyGraphDescriptor) editor
                .getActiveDisplayPane().getDescriptor();
        int visibleGraphs = desc.getGraphResource().visibleGraphCount();
        int totalGraphs = desc.getGraphResource().totalGraphCount();
        if (zoomIndex > 0) {
            if (zoomIndex >= totalGraphs) {
                IGraph graph = desc.getGraphResource().getClosestGraph(grid);
                if (graph.getExtent().contains(new double[] { grid.x, grid.y })) {
                    graph.zoom((int) Math.pow(2, zoomIndex - totalGraphs), grid);
                } else {
                    zoomIndex++;
                }
            } else if (visibleGraphs < totalGraphs) {
                desc.getGraphResource().showGraph(hiddenGraphs.pop());
            }
            zoomIndex--;
        }
        return true;
    }

    private Coordinate translateClick(int x, int y) {
        IDisplayPaneContainer editor = display.getContainer();
        XyGraphDescriptor desc = (XyGraphDescriptor) editor
                .getActiveDisplayPane().getDescriptor();
        Coordinate grid = editor.translateClick(x, y);
        if (grid == null) {
            return null;
        }
        /* Convert from the overall display coordinate space to the coordinate
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

}
