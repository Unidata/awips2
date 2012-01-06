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

package com.raytheon.viz.core.graphing;

import com.raytheon.uf.viz.core.IView;
import com.raytheon.uf.viz.core.drawables.PaintProperties;

/**
 * PaintProperties that include the graph to paint on
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 22, 2007            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 */
public class GraphProperties extends PaintProperties {

    protected IGraph graph;

    public GraphProperties(PaintProperties aProps) {
        super(aProps, (IView) aProps.getView().clone());
    }

    /**
     * @return the graph
     */
    public IGraph getGraph() {
        return graph;
    }

    /**
     * @param graph
     *            the graph to set
     */
    public void setGraph(IGraph graph) {
        this.graph = graph;
    }

}
