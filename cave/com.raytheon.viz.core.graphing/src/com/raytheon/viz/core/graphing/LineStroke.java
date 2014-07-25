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

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Implement a simple stroke graphics package.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06 Nov 2006             jkorman     Initial Coding
 * 28 Jul 2014  3429       mapeters    Removed unused render() method.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class LineStroke {
    private enum MovementType {
        MOVETO, DRAWTO;

        @Override
        public String toString() {
            String retValue = null;
            if (this.equals(MOVETO)) {
                retValue = "M";
            } else if (this.equals(DRAWTO)) {
                retValue = "D";
            }
            return retValue;
        }
    }

    private MovementType type = null;

    private Coordinate point = null;

    /**
     * 
     * @param x
     * @param y
     * @param type
     */
    private LineStroke(double x, double y, MovementType type) {
        point = new Coordinate(x, y);
        this.type = type;
    }

    /**
     * Create a moveTo instance.
     * 
     * @param x
     *            The x position.
     * @param y
     *            The y position.
     * @return The stroke instance.
     */
    public static LineStroke moveTo(double x, double y) {
        return new LineStroke(x, y, MovementType.MOVETO);
    }

    /**
     * Create a drawTo instance.
     * 
     * @param x
     *            The x position.
     * @param y
     *            The y position.
     * @return The stroke instance.
     */
    public static LineStroke drawTo(double x, double y) {
        return new LineStroke(x, y, MovementType.DRAWTO);
    }

    /**
     * Rotate this stroke point by an angle. Rotation angles are relative to
     * position {0,0}
     * 
     * @param anAngle
     */
    public void rotate(double anAngle) {
        anAngle = Math.toRadians(anAngle);
        double x = (point.x * Math.cos(anAngle))
                - (point.y * Math.sin(anAngle));
        double y = (point.x * Math.sin(anAngle))
                + (point.y * Math.cos(anAngle));
        point.x = x;
        point.y = y;
    }

    /**
     * Scale the stroke by separate x and y factors. Scale factors are relative
     * to position {0,0}
     * 
     * @param scaleX
     *            The x axis scale factor.
     * @param scaleY
     *            The y axis scale factor.
     */
    public void scale(double scaleX, double scaleY) {
        point.x = point.x * scaleX;
        point.y = point.y * scaleY;
    }

    /**
     * Scale the stoke by a single scale factor. Scale factor is relative to
     * position {0,0}
     * 
     * @param scaleFactor
     */
    public void scale(double scaleFactor) {
        scale(scaleFactor, scaleFactor);
    }

    /**
     * Translate the point to a different location. Translation distances are
     * relative to the current position of the point.
     * 
     * @param distX
     *            The x axis distance.
     * @param distY
     *            The y axis distance.
     */
    public void translate(double distX, double distY) {
        point.x = point.x + distX;
        point.y = point.y + distY;
    }

    /**
     * Render the stroke.
     * 
     * @param world
     *            The world graphics system.
     * @param drawColor
     *            The color to draw with.
     */
    public void render(org.eclipse.swt.graphics.GC gc, int relativeX,
            int relativeY) {

        int lastMoveX = relativeX;
        int lastMoveY = relativeY;

        switch (type) {
        case MOVETO: {
            lastMoveX = (int) point.x;
            lastMoveY = (int) point.y;
            // gc.drawLine(relativeX, relativeY, (int) point.x, (int) point.y);
            // gc.drawPoint((int) point.x + relativeX, (int) point.y +
            // relativeY);
            break;
        }
        case DRAWTO: {
            gc.drawLine(lastMoveX, lastMoveY, (int) point.x + relativeX,
                    (int) point.y + relativeY);
            break;
        }
        }
    }

    /**
     * Create a string representation of this stroke.
     * 
     * @param buffer
     *            A buffer to receive the data. If null, a new StringBuffer is
     *            created.
     * @return The populated buffer.
     */
    public StringBuffer toStringBuilder(StringBuffer buffer) {
        if (buffer == null) {
            buffer = new StringBuffer();
        }
        buffer.append(type);
        buffer.append(":x[");
        buffer.append(point.x);
        buffer.append("]y[");
        buffer.append(point.y);
        buffer.append("]");
        return buffer;
    }

    /**
     * Create a string representation of this stroke.
     * 
     * @return The string representation of this stroke.
     */
    @Override
    public String toString() {
        return toStringBuilder(null).toString();
    }

    public String getType() {
        return type.toString();
    }

    public Coordinate getPoint() {
        return point;
    }
}
