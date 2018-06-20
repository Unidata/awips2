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
package com.raytheon.uf.viz.xy.varheight.util;

import java.util.HashSet;
import java.util.Set;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.xy.graph.IGraph;
import com.raytheon.uf.viz.xy.graph.XyGraphDescriptor;
import com.raytheon.uf.viz.xy.map.rsc.IGraphableResource;
import com.raytheon.uf.viz.xy.util.AbstractGraphZoomHandler;
import com.raytheon.viz.ui.cmenu.ZoomMenuAction;
import com.raytheon.viz.ui.input.PanHandler;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * Zoom handler for var height displays
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 3, 2010             bsteffen    Initial creation
 * Dec 11, 2013 DR 16795   D. Friedman Transform pixel coordinate for zoom
 * Jun 18, 2014 3242       njensen     Null safety checks
 * Jul 16, 2015 4220       mapeters    Abstract out functionality to AbstractGraphZoomHandler,
 *                                     set this as each zoomed graph's zoomHandler
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class VarHeightZoomHandler extends AbstractGraphZoomHandler {

    public VarHeightZoomHandler(IRenderableDisplay display) {
        super(display);
    }

    @Override
    protected boolean zoom(String pref, int x, int y) {
        Coordinate grid = translateClick(x, y);

        if (grid == null) {
            return false;
        }

        if (pref.equals(PanHandler.ZOOMOUT_PREF) && zoomIndex > 0) {
            zoomIndex -= 1;
        } else if (pref.equals(PanHandler.ZOOMIN_PREF)
                && zoomIndex < ZoomMenuAction.ZOOM_LEVELS.length - 1) {
            zoomIndex += 1;
        } else {
            return true;
        }

        for (IGraph graph : getGraphs()) {
            if (graphContainsCoordinate(graph, grid)) {
                int zoomLevel = (int) Math.pow(2, zoomIndex);
                performZoom(graph, zoomLevel, grid);
                graph.setZoomHandler(this);
            }

        }
        return true;
    }

    private Set<IGraph> getGraphs() {
        Set<IGraph> graphs = new HashSet<>();

        IDisplayPaneContainer editor = display.getContainer();
        XyGraphDescriptor desc = (XyGraphDescriptor) editor
                .getActiveDisplayPane().getDescriptor();
        for (ResourcePair rsc : desc.getResourceList()) {
            if (rsc.getResource() instanceof IGraphableResource<?, ?>) {
                IGraph graph = desc.getGraph((IGraphableResource<?, ?>) rsc
                        .getResource());
                if (graph != null) {
                    graphs.add(graph);
                }
            }
        }

        return graphs;
    }
}
