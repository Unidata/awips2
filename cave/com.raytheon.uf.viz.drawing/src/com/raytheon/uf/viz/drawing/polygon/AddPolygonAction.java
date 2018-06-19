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
 * Adds a new "default" polygon to a {@code PolygonLayer} centered at the
 * specified point.
 * 
 * @see PolygonUtil#makePolygon()
 * 
 *      <pre>
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

public class AddPolygonAction extends AbstractRightClickAction {

    private final int x;

    private final int y;

    private final PolygonInputAdapter inputAdapter;

    /**
     * @param x
     *            Screen x-coordinate for the new polygon's center.
     * @param y
     *            Screen y-coordinate for the new polygon's center.
     * @param inputAdapter
     *            {@code PolygonInputAdapter} instance to interact with the
     *            {@code PolygonLayer}.
     */
    public AddPolygonAction(int x, int y, PolygonInputAdapter inputAdapter) {
        super("Add Polygon");
        this.x = x;
        this.y = y;
        this.inputAdapter = inputAdapter;
    }

    @Override
    public void run() {
        inputAdapter.addPolygon(x, y);
    }
}
