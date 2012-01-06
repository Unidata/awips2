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

import java.util.ArrayList;
import java.util.List;

/**
 * WindBarbFactory creates a list of line segments for a given wind speed and
 * direction. The list origin is at position {0,0} so after creation, the list
 * may be translated to some arbitrary position for display.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06 Nov 2006             jkorman     Initial Coding
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class WindBarbFactory {

    /**
     * Construct a wind barb for a given wind speed. The LineStroke segments are
     * created for a North wind direction. The created List will then be rotated
     * at a later time to the correct wind direction.
     * 
     * @param windSpeed
     *            Wind speed to plot. Wind speeds are rounded to the nearest
     *            integer value.
     */
    private static List<LineStroke> createBarb(double windSpeed) {
        List<LineStroke> barb = new ArrayList<LineStroke>();
        int fiftyKtBarbs = 0;
        int tenKtBarbs = 0;
        boolean fiveKtBarb = false;
        if (windSpeed >= 0) {
            
            int windMagnitude = (int) Math.round(windSpeed);
            
            fiftyKtBarbs = windMagnitude / 50;
            windMagnitude %= 50;

            tenKtBarbs = windMagnitude / 10;
            windMagnitude %= 10;
            // The following will give us a five knot barb for winds > 2 but
            // less than 8
            // and add an extra ten knot barb for winds > 7
            if (windMagnitude > 7) {
                if(tenKtBarbs < 4) {
                    tenKtBarbs++;
                } else {
                    fiftyKtBarbs++;
                    tenKtBarbs = 0;
                }
            } else if (windMagnitude > 2) {
                fiveKtBarb = true;
            }
        }
        if (windSpeed > 0) {
            // index of the end point of the staff.
            double shaftPos = 0.5 + ((tenKtBarbs - 1) * 0.1)
                    + (fiftyKtBarbs * 0.1);
            if (fiftyKtBarbs > 0) {
                shaftPos += 0.1;
            }
            // add an empty staff for winds less than 3 knots
            barb.add(LineStroke.moveTo(0, 0));
            barb.add(LineStroke.drawTo(0, shaftPos));
            // add the five knot barb to the staff if needed.
            if (fiveKtBarb) {
                barb.add(LineStroke.moveTo(0, 0.4));
                barb.add(LineStroke.drawTo(0.1, 0.5));
            }
            shaftPos = 0.5;
            for (int i = 0; i < tenKtBarbs; i++) {
                barb.add(LineStroke.moveTo(0, shaftPos));
                barb.add(LineStroke.drawTo(0.2, shaftPos + 0.2));
                shaftPos += 0.1;
            }
            if (fiftyKtBarbs > 0) {
                barb.add(LineStroke.moveTo(0, shaftPos));
                for (int i = 0; i < fiftyKtBarbs; i++) {
                    barb.add(LineStroke.drawTo(0.2, shaftPos + 0.2));
                    shaftPos += 0.1;
                    barb.add(LineStroke.drawTo(0.0, shaftPos));
                }
            }
        } else {
            // nothing for now. Probably want to plot a circle around the
            // plot point - something like the following
            // barb.add(LineStroke.circle(0,0,radius));
        }

        return barb;
    }

    /**
     * Rotate all members of the list by a specified angle.
     * 
     * @param windBarb
     *            The wind barb line segment list.
     * @param anAngle
     *            Wind direction in degrees.
     * @return The transformed line segment list.
     */
    public static List<LineStroke> rotateBarb(List<LineStroke> windBarb,
            double anAngle) {
        for (LineStroke stroke : windBarb) {
            stroke.rotate(anAngle);
        }
        return windBarb;
    }

    /**
     * Translate all members of the list by a specified distance.
     * 
     * @param windBarb
     *            The wind barb line segment list.
     * @param distX
     *            The x axis distance.
     * @param distY
     *            The y axis distance.
     * @return The transformed line segment list.
     */
    public static List<LineStroke> translateBarb(List<LineStroke> windBarb,
            double distX, double distY) {
        for (LineStroke stroke : windBarb) {
            stroke.translate(distX, distY);
        }
        return windBarb;
    }

    /**
     * Scale all members of the list by a specified factor.
     * 
     * @param windBarb
     *            The wind barb line segment list.
     * @param scaleFactor
     *            The scale factor.
     * @return The transformed line segment list.
     */
    public static List<LineStroke> scaleBarb(List<LineStroke> windBarb,
            double scaleFactor) {
        for (LineStroke stroke : windBarb) {
            stroke.scale(scaleFactor);
        }
        return windBarb;
    }

    /**
     * Create a line segment list for a a wind barb for a given speed and
     * direction.
     * 
     * @param windSpeed
     *            The wind speed in knots.
     * @param windDirection
     *            Wind direction in degrees.
     * @return The created line segment list.
     */
    public static List<LineStroke> getWindGraphics(Double windSpeed,
            Double windDirection) {
        List<LineStroke> retList = null;
        if ((windSpeed != null) && (windDirection != null)) {
            // In the event that the wind direction is greater than 360
            // normalize to 0..360
            while (windDirection > 360) {
                windDirection -= 360;
            }

            // now create the normalized wind barb
            retList = scaleBarb(createBarb(windSpeed), 4);
            // and rotate to the proper orientation
            rotateBarb(retList, -windDirection);
        }
        return retList;
    }
}
