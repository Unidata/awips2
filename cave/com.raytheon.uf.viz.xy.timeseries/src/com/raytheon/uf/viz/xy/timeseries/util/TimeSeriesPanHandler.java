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

import java.util.HashSet;
import java.util.Set;

import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.xy.graph.IGraph;
import com.raytheon.uf.viz.xy.util.AbstractGraphPanHandler;

/**
 * A pan handler for time series.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 16, 2009            mschenke     Initial creation
 * Jun 14, 2014 3242       njensen      Null safety checks
 * Jul 16, 2015 4220       mapeters     Abstract out functionality to AbstractGraphPanHandler
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class TimeSeriesPanHandler extends AbstractGraphPanHandler {

    protected IGraph graph;

    public TimeSeriesPanHandler(IRenderableDisplay display) {
        super(display);
    }

    @Override
    protected void setGraph(IGraph graph) {
        this.graph = graph;
    }

    @Override
    protected Set<IGraph> getGraphs() {
        Set<IGraph> graphs = new HashSet<>();
        graphs.add(graph);
        return graphs;
    }
}
