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
package com.raytheon.viz.gfe.edittool;

import java.awt.Point;
import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.List;

import org.locationtech.jts.geom.Coordinate;

/**
 * The base implementation for drawing-based tools
 *
 * This is intended to be subclassed by tools that require free form drawing
 * tool, and then perform an action.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Apr 14, 2008           chammack  Initial Creation
 * Apr 14, 2009  2058     rjpeter   Ensured nulls couldn't be added to
 *                                  currentCoordinates.
 * Jan 29, 2018  7153     randerso  Code cleanup
 *
 * </pre>
 *
 * @author chammack
 */
public abstract class AbstractFreeformTool extends AbstractGFEEditTool {

    protected List<Coordinate> currentCoordinates;

    @Override
    protected void handleEvent(int button, EventType type, Point point2D,
            Coordinate coordinate) {
        if (isEditStateOK(false)) {
            this.processDrawEvent(type, coordinate);

            switch (type) {
            case START_DRAG:
                if (this.currentCoordinates == null) {
                    this.currentCoordinates = new ArrayList<>();
                }
                this.currentCoordinates.clear();

                if (coordinate != null) {
                    this.currentCoordinates.add(coordinate);
                }

                refresh();
                break;

            case IN_DRAG:
                if (coordinate != null) {
                    this.currentCoordinates.add(coordinate);
                }

                refresh();
                break;
            case END_DRAG:

                handleEndDrag(button, point2D, coordinate);
                this.currentCoordinates.clear();
                break;

            case MOUSE_CLICK:
                handleMouseClick(button, point2D, coordinate);
            }

        }
    }

    protected void handleMouseClick(int button, Point point2D,
            Coordinate coordinate) {
        // do nothing default method
    }

    protected abstract void handleEndDrag(int button, Point2D point2D,
            Coordinate coordinate);

}
