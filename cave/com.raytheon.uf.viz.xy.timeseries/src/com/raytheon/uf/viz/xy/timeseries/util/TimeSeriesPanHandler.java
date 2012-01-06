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

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.xy.AbstractGraphInputHandler;
import com.raytheon.uf.viz.xy.graph.IGraph;
import com.raytheon.uf.viz.xy.graph.XyGraphDescriptor;
import com.raytheon.viz.ui.input.PanHandler;
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
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class TimeSeriesPanHandler extends AbstractGraphInputHandler {

    protected IGraph graph;

    protected float theLastMouseX = 0;

    protected float theLastMouseY = 0;

    protected int[] downPosition;

    protected PanHandler defaultHandler;

    protected boolean panHandling = false;

    protected boolean active = false;

    public TimeSeriesPanHandler(IRenderableDisplay display) {
        super(display);
        defaultHandler = new PanHandler(display.getContainer());
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDown(int, int,
     * int)
     */
    @Override
    public boolean handleMouseDown(int x, int y, int button) {
        IDisplayPaneContainer editor = display.getContainer();
        if (button != 1) {
            active = false;
            return false;
        } else {
            active = true;
        }
        if (editor.getActiveDisplayPane().getDescriptor() instanceof XyGraphDescriptor == false) {
            defaultHandler.setContainer(editor);
            panHandling = true;
            return defaultHandler.handleMouseDown(x, y, button);
        } else {

            Coordinate grid = editor.translateClick(x, y);
            if (grid == null) {
                return false;
            }
            XyGraphDescriptor desc = (XyGraphDescriptor) editor
                    .getActiveDisplayPane().getDescriptor();
            IGraph graphToUse = desc.getGraphResource().getClosestGraph(grid);

            if (graphToUse != null
                    && graphToUse.getExtent().contains(
                            new double[] { grid.x, grid.y, grid.z })) {
                this.graph = graphToUse;
                downPosition = new int[] { x, y };
                theLastMouseX = x;
                theLastMouseY = y;
            } else {
                defaultHandler.setContainer(editor);
                panHandling = true;
                return defaultHandler.handleMouseDown(x, y, button);
            }
        }

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
        IDisplayPaneContainer editor = display.getContainer();
        if (!active) {
            return false;
        }
        if (!panHandling && graph != null) {
            Coordinate lastLoc = editor.translateClick(downPosition[0],
                    downPosition[1]);
            Coordinate curLoc = editor.translateClick(aX, aY);

            if (lastLoc != null && curLoc != null) {
                graph.pan(lastLoc.x - curLoc.x, lastLoc.y - curLoc.y, true);
                downPosition[0] = aX;
                downPosition[1] = aY;
            }
        } else if (panHandling) {
            return defaultHandler.handleMouseDownMove(aX, aY, button);
        }

        if (button != 1 || graph == null)
            return false;

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
        IDisplayPaneContainer editor = display.getContainer();
        if (button != 1) {
            return false;
        }
        if (!panHandling && graph != null) {
            Coordinate lastLoc = editor.translateClick(downPosition[0],
                    downPosition[1]);
            Coordinate curLoc = editor.translateClick(x, y);

            if (lastLoc != null && curLoc != null) {
                graph.pan(lastLoc.x - curLoc.x, lastLoc.y - curLoc.y, false);
            }

        }
        graph = null;
        active = false;
        if (panHandling) {
            panHandling = false;
            return defaultHandler.handleMouseUp(x, y, button);
        }
        return false;
    }
}
