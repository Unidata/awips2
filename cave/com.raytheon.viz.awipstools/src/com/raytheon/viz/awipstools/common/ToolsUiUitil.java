/*****************************************************************************************
 * COPYRIGHT (c), 2007, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/
package com.raytheon.viz.awipstools.common;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;

/**
 * Class for providing utility functions to awips tools
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class ToolsUiUitil {

    private static final GeometryFactory gf = new GeometryFactory();

    /**
     * Give the coordinates treated as points in a line, determine if the mouse
     * location denoted by mouseX and mouseY are within the threshold in screen
     * pixels
     * 
     * @param container
     *            display container to use
     * @param coords
     *            points in the line
     * @param refX
     *            x location of reference point in screen pixels
     * @param refY
     *            y location of reference point in screen pixels
     * @param threshold
     *            threshold to be within in screen pixels
     * @return true if line is within threshold, false otherwise
     */
    public static boolean closeToLine(IDisplayPaneContainer container,
            Coordinate[] coords, int refX, int refY, double threshold) {
        if (coords.length < 2) {
            return false;
        }

        Coordinate refCoord = new Coordinate(refX, refY);

        List<Coordinate> inverted = new ArrayList<Coordinate>(coords.length);

        for (int i = 0; i < coords.length; ++i) {
            double[] vals = container.translateInverseClick(coords[i]);
            if (vals != null) {
                inverted.add(new Coordinate(vals[0], vals[1]));
            }
        }

        if (inverted.size() > 1) {
            return (gf.createLineString(
                    inverted.toArray(new Coordinate[inverted.size()]))
                    .distance(gf.createPoint(refCoord)) <= threshold);
        }

        return false;
    }

    /**
     * Give the array of coordinates, this function the first one within the
     * threshold value from the location denoted by mouseX and mouseY
     * 
     * @param container
     *            display container to use
     * @param coords
     *            points to check
     * @param refX
     *            x location in screen pixels
     * @param refY
     *            y location in screen pixels
     * @param threshold
     *            threshold to be within in screen pixels
     * @return The index of the coordinate within threshold or -1 if none found
     */
    public static int closeToCoordinate(IDisplayPaneContainer container,
            Coordinate[] coords, int refX, int refY, double threshold) {
        Coordinate refCoord = new Coordinate(refX, refY);
        int rval = -1;
        int i = 0;
        for (Coordinate c : coords) {
            double[] screen = container.translateInverseClick(c);
            if (screen != null) {
                c = new Coordinate(screen[0], screen[1]);
                if (c.distance(refCoord) <= threshold) {
                    rval = i;
                    break;
                }
            }
            ++i;
        }
        return rval;
    }

    /**
     * Translate a screen click into a descriptor world coordinate. This method
     * will not return null if outside the grid range of the descriptor unlike
     * IDisplayPaneCainer
     * 
     * @param container
     * @param descriptor
     * @param refX
     * @param refY
     * @return
     */
    public static Coordinate translateClick(IDisplayPaneContainer container,
            IDescriptor descriptor, double refX, double refY) {
        IDisplayPane ourPane = null;
        for (IDisplayPane pane : container.getDisplayPanes()) {
            if (pane.getDescriptor() == descriptor) {
                ourPane = pane;
                break;
            }
        }

        double[] gridLoc = ourPane.screenToGrid(refX, refY, 0);
        if (gridLoc != null) {
            double[] world = descriptor.pixelToWorld(gridLoc);
            if (world != null) {
                return new Coordinate(world[0], world[1], world[2]);
            }
        }
        return null;
    }
}
