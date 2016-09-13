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

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.xy.graph.IGraph;
import com.raytheon.uf.viz.xy.graph.XyGraphDescriptor;
import com.raytheon.uf.viz.xy.util.AbstractGraphZoomHandler;
import com.raytheon.viz.ui.input.PanHandler;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * A zoom handler for time series. Time series needs a special zoom handler
 * since the axes will redraw as the user zooms in and out.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 16, 2009            mschenke     Initial creation
 * Dec 11, 2013 DR 16795   D. Friedman  Transform pixel coordinate for zoom
 * Jun 18, 2014 3242       njensen      Null safety checks
 * Jul 16, 2015 4220       mapeters     Abstract out functionality to AbstractGraphZoomHandler,
 *                                      set this as each zoomed graph's zoomHandler
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class TimeSeriesZoomHandler extends AbstractGraphZoomHandler {

    protected Stack<IGraph> hiddenGraphs = new Stack<IGraph>();

    public TimeSeriesZoomHandler(IRenderableDisplay display) {
        super(display);
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
            if (graphContainsCoordinate(graph, grid)) {
                performZoom(graph,
                        (int) Math.pow(2, zoomIndex - totalGraphs + 1), grid);
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
                if (graphContainsCoordinate(graph, grid)) {
                    performZoom(graph,
                            (int) Math.pow(2, zoomIndex - totalGraphs), grid);
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

    @Override
    protected boolean zoom(String pref, int x, int y) {
        if (pref.equals(PanHandler.ZOOMOUT_PREF)) {
            return zoomOut(x, y);
        } else if (pref.equals(PanHandler.ZOOMIN_PREF)) {
            return zoomIn(x, y);
        }
        return false;
    }
}
