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
import com.raytheon.uf.viz.xy.util.AbstractGraphPanHandler;

/**
 * 
 * Handles mouse events in order to pan VarHeightGraphs.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 3, 2010            bsteffen     Initial creation
 * Mar 3, 2015  4189       nabowle     Handle null coordinate in handleMouseDown
 * Jul 16, 2015 4220       mapeters    Abstract out functionality to AbstractGraphPanHandler
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class VarHeightPanHandler extends AbstractGraphPanHandler {

    public VarHeightPanHandler(IRenderableDisplay display) {
        super(display);
    }

    @Override
    protected void setGraph(IGraph graph) {
        /*
         * Do nothing. A set of graphs will always be gotten when needed as
         * shown in getGraphs().
         */
    }

    @Override
    protected Set<IGraph> getGraphs() {
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
