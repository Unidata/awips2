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
package com.raytheon.viz.gfe.rsc;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.viz.core.contours.util.VectorGraphicsRenderable;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * GFE version of VectorGraphicsRenderable. Subclassed to better match A1 GFE
 * behavior
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 22, 2013     #2287  randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class GFEVectorGraphicsRenderable extends VectorGraphicsRenderable {
    double minLog = 0.0;

    double maxLog = 0.0;

    private double maxLimit;

    /**
     * @param descriptor
     * @param target
     * @param size
     * @param logFactor
     * @param maxLimit
     */
    public GFEVectorGraphicsRenderable(IDescriptor descriptor,
            IGraphicsTarget target, double size, double logFactor,
            double maxLimit) {
        super(descriptor, target, size, logFactor);

        this.maxLimit = maxLimit;
        if (logFactor > 0.0) {
            minLog = Math.log(logFactor);
            maxLog = Math.log(logFactor + 1.0);
        }
    }

    /**
     * 
     * @param plotLoc
     * @param adjSize
     * @param spd
     * @param dir
     *            barb direction in radians
     */
    @Override
    public void paintBarb(Coordinate plotLoc, double adjSize, double spd,
            double dir) {
        paintPoint(plotLoc, adjSize);

        int speed = (int) (spd + 2.5);
        double staff = adjSize * .4;
        double barb = staff * 0.30;
        double add = staff * 0.105;
        // DIRECTIONS
        double uudd = -spd * Math.sin(dir);
        double vvff = -spd * Math.cos(dir);
        double dix = -uudd / spd;
        double djy = -vvff / spd;
        double dix1 = Math.cos(Math.toRadians(75)) * dix
                + Math.sin(Math.toRadians(75)) * djy;
        double djy1 = (-1) * Math.sin(Math.toRadians(75)) * dix
                + Math.cos(Math.toRadians(75)) * djy;

        // SPEED AND COUNTERS:
        int n50 = speed / 50;
        int calcSpd = speed - 50 * n50;
        int n10 = calcSpd / 10;
        calcSpd = calcSpd - 10 * n10;
        int n5 = calcSpd / 5;
        double sx = ((n50 + n50 + n10 + n5 + 2)) * add;
        staff = Math.max(adjSize * .4, sx);

        // DRAW STAFF
        double ix2 = plotLoc.x;
        double jy2 = plotLoc.y;
        double ix1 = ix2 + dix * staff;
        double jy1 = jy2 - djy * staff;
        lastShape.addLineSegment(new double[][] { { ix2, jy2 }, { ix1, jy1 } });

        // PLOT LONE HALF-BARB, IF NECESSARY
        if (n50 == 0 && n10 == 0) {
            ix2 = ix1 - dix * add;
            jy2 = jy1 + djy * add;
            ix1 = ix2 + dix1 * barb / 2.0;
            jy1 = jy2 - djy1 * barb / 2.0;
            lastShape.addLineSegment(new double[][] { { ix2, jy2 },
                    { ix1, jy1 } });
            return;
        }

        // PLOT FLAGS, IF NECESSARY
        for (int i = 0; i < n50; i++) {
            ix2 = ix1 + dix1 * barb;
            jy2 = jy1 - djy1 * barb;
            lastShape.addLineSegment(new double[][] { { ix2, jy2 },
                    { ix1, jy1 } });
            ix1 = ix1 - dix * add * 2;
            jy1 = jy1 + djy * add * 2;
            lastShape.addLineSegment(new double[][] { { ix2, jy2 },
                    { ix1, jy1 } });
        }
        if (n50 > 0) {
            ix1 = ix1 - dix * add / 2.0;
            jy1 = jy1 + djy * add / 2.0;
        }

        // PLOT BARB, IF NECESSARY
        for (int i = 0; i < n10; i++) {
            ix2 = ix1 + dix1 * barb;
            jy2 = jy1 - djy1 * barb;
            lastShape.addLineSegment(new double[][] { { ix2, jy2 },
                    { ix1, jy1 } });
            ix1 = ix1 - dix * add;
            jy1 = jy1 + djy * add;
        }

        // PLOT HALF-BARB, IF NECESSARY
        if (n5 != 0) {
            ix2 = ix1 + dix1 * barb / 2.0;
            jy2 = jy1 - djy1 * barb / 2.0;
            lastShape.addLineSegment(new double[][] { { ix2, jy2 },
                    { ix1, jy1 } });
        }
    }

    @Override
    public void paintArrow(Coordinate plotLoc, double adjSize, double mag,
            double dir) {
        paintPoint(plotLoc, adjSize);

        double staff = 0.0;

        double logFactor = this.scale;

        // linear scaling
        if (logFactor == 0.00) {
            staff = mag * size / maxLimit;
        } else {
            double pcentRange = mag / maxLimit;
            double lg = Math.log(logFactor + pcentRange);
            double pcentLog = (lg - minLog) / (maxLog - minLog);
            staff = pcentLog * size;
        }

        double barb = staff / 7.0;

        // if (staff < barb) {
        // return;
        // }

        double ratio = adjSize / size;
        staff *= ratio;
        barb *= ratio;

        // DIRECTIONS
        double uudd = -mag * Math.sin(dir);
        double vvff = -mag * Math.cos(dir);
        double dix = uudd / mag;
        double djy = vvff / mag;
        double dix1 = -dix - djy;
        double djy1 = dix - djy;
        double dix2 = -dix + djy;
        double djy2 = -dix - djy;

        // DRAW BODY OF ARROW
        double ix2 = plotLoc.x;
        double jy2 = plotLoc.y;
        double ix1 = ix2 + dix * staff;
        double jy1 = jy2 - djy * staff;
        lastShape.addLineSegment(new double[][] { { ix2, jy2 }, { ix1, jy1 } });
        // DRAW HEAD OF ARROW.
        ix2 = ix1 + dix1 * barb;
        jy2 = jy1 - djy1 * barb;
        double ix3 = ix1 + dix2 * barb;
        double jy3 = jy1 - djy2 * barb;
        lastShape.addLineSegment(new double[][] { { ix2, jy2 }, { ix1, jy1 },
                { ix3, jy3 } });
    }
}
