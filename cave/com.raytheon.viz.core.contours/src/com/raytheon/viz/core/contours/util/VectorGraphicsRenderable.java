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
package com.raytheon.viz.core.contours.util;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 27, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class VectorGraphicsRenderable {

    private IWireframeShape lastShape;

    private double size = 80;

    private double scale = 1.0;

    private RGB color;

    private float lineWidth = 1.0f;

    private LineStyle lineStyle;

    public VectorGraphicsRenderable(IDescriptor descriptor,
            IGraphicsTarget target, double size, double scale) {
        this.lastShape = target.createWireframeShape(true, descriptor);
        this.size = size;
        this.scale = scale;
    }

    public void setColor(RGB color) {
        this.color = color;
    }

    public void setLineWidth(float lineWidth) {
        this.lineWidth = lineWidth;
    }

    public void setLineStyle(LineStyle lineStyle) {
        this.lineStyle = lineStyle;
    }

    /**
     * 
     * @param plotLoc
     * @param adjSize
     * @param spd
     * @param dir
     *            barb direction in radians
     */
    public void paintBarb(Coordinate plotLoc, double adjSize, double spd,
            double dir) {
        if (spd < 2.5) {
            double[][] line = new double[9][2];

            double aa = adjSize * .030;
            double saa = aa * 0.707;

            line[8][0] = line[0][0] = plotLoc.x + aa;
            line[8][1] = line[0][1] = plotLoc.y;
            line[1][0] = plotLoc.x + saa;
            line[1][1] = plotLoc.y + saa;

            line[2][0] = plotLoc.x;
            line[2][1] = plotLoc.y + aa;

            line[3][0] = plotLoc.x - saa;
            line[3][1] = plotLoc.y + saa;

            line[4][0] = plotLoc.x - aa;
            line[4][1] = plotLoc.y;

            line[5][0] = plotLoc.x - saa;
            line[5][1] = plotLoc.y - saa;

            line[6][0] = plotLoc.x;
            line[6][1] = plotLoc.y - aa;

            line[7][0] = plotLoc.x + saa;
            line[7][1] = plotLoc.y - saa;

            lastShape.addLineSegment(line);

            return;
        }

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

    public void paintDualArrow(Coordinate plotLoc, double adjSize, double spd,
            double dir) {
        if (spd < 4.0) {
            return;
        }
        double staff = 0.0;
        if (this.scale > 0.0) {
            staff = spd * this.scale;
        } else {
            staff = Math.log10(spd * -this.scale) * 10 + 10;
        }

        double barb = 4.0;

        if (staff < barb) {
            return;
        }

        double ratio = adjSize / size;
        staff *= ratio;
        barb *= ratio;

        // DIRECTIONS
        double uudd = -spd * Math.sin(dir);
        double vvff = -spd * Math.cos(dir);
        double dix = uudd / spd;
        double djy = vvff / spd;
        double dix1 = -dix - djy;
        double djy1 = dix - djy;

        // DRAW BODY OF ARROW
        double ix2 = plotLoc.x;
        double jy2 = plotLoc.y;
        double ix1 = ix2 + dix * staff;
        double jy1 = jy2 - djy * staff;

        double ix3 = ix1 + dix1 * barb;
        double jy3 = jy1 - djy1 * barb;
        double ix4 = ix2 - dix1 * barb;
        double jy4 = jy2 + djy1 * barb;
        lastShape.addLineSegment(new double[][] { { ix4, jy4 }, { ix2, jy2 },
                { ix1, jy1 }, { ix3, jy3 } });

    }

    public void paintArrow(Coordinate plotLoc, double adjSize, double spd,
            double dir) {
        if (spd == 0.0) {
            return;
        }
        double staff = 0.0;
        if (this.scale > 0.0) {
            staff = spd * this.scale;
        } else {
            staff = Math.log10(spd * -this.scale) * 10 + 10;
        }

        double barb = 4.0;

        if (staff < barb) {
            return;
        }

        double ratio = adjSize / size;
        staff *= ratio;
        barb *= ratio;

        // DIRECTIONS
        double uudd = -spd * Math.sin(dir);
        double vvff = -spd * Math.cos(dir);
        double dix = uudd / spd;
        double djy = vvff / spd;
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

    public void paint(IGraphicsTarget target) throws VizException {
        if (lastShape != null) {
            target.drawWireframeShape(lastShape, color, lineWidth, lineStyle);
        }
    }

    public void dispose() {
        if (lastShape != null) {
            lastShape.dispose();
            lastShape = null;
        }
    }

}
