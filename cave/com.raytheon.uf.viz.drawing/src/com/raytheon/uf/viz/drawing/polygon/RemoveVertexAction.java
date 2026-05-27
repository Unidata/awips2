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
package com.raytheon.uf.viz.drawing.polygon;

import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * Removes a vertex from a polygon in a {@code PolygonLayer}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 15, 2015  #4375     dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class RemoveVertexAction extends AbstractRightClickAction {

    private final int polygonIndex;

    private final int vertexIndex;

    private final PolygonInputAdapter inputAdapter;

    /**
     * @param polygonIndex
     *            Index of the polygon to remove the vertex from.
     * @param vertexIndex
     *            Index of the vertex to remove.
     * @param inputAdapter
     *            {@code PolygonInputAdapter} instance to interact with the
     *            {@code PolygonLayer}.
     */
    public RemoveVertexAction(int polygonIndex, int vertexIndex,
            PolygonInputAdapter inputAdapter) {
        super("Remove Vertex");
        this.polygonIndex = polygonIndex;
        this.vertexIndex = vertexIndex;
        this.inputAdapter = inputAdapter;
    }

    @Override
    public void run() {
        inputAdapter.removeVertex(polygonIndex, vertexIndex);
    }
}
