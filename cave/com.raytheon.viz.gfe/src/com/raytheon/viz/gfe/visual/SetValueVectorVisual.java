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
package com.raytheon.viz.gfe.visual;

import java.util.ArrayList;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.util.Geometry;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Canvas;

import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.wxvalue.VectorWxValue;
import com.raytheon.viz.gfe.gridmanager.MouseHandler;
import com.raytheon.viz.gfe.rsc.GFEFonts;

/**
 * Visual for Vector set value dialog
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 22, 2009 #1318      randerso    Initial creation
 * Mar 10, 2016 #5479      randerso    Use improved GFEFonts API
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class SetValueVectorVisual {

    private Canvas canvas;

    private Parm parm;

    private double logFactor, minLog, maxLog;

    private Font pickupFont;

    public SetValueVectorVisual(Canvas canvas, Parm parm) {
        this.canvas = canvas;
        this.parm = parm;

        this.canvas.addMouseListener(new MouseHandler() {

            /*
             * (non-Javadoc)
             * 
             * @see
             * com.raytheon.viz.gfe.gridmanager.MouseHandler#dragMove(org.eclipse
             * .swt.events.MouseEvent)
             */
            @Override
            public void dragMove(MouseEvent e) {
                super.dragMove(e);

                handleMouseEvent(e);
            }

        });

        // set the pickup font
        pickupFont = GFEFonts.makeGFEFont(canvas.getDisplay(),
                "SetValuePickUp_font", SWT.BOLD, 3);

        // get the logFactor
        IPreferenceStore prefs = Activator.getDefault().getPreferenceStore();
        logFactor = prefs.getDouble(parm.getParmID().compositeNameUI()
                + "_arrowScaling");
        if (logFactor < 0.0) {
            logFactor = 0.0;
        }

        // Calculate the log scalings once
        if (logFactor > 0.0) {
            minLog = Math.log(logFactor);
            maxLog = Math.log(logFactor + 1.0);
        } else {
            minLog = maxLog = 0.0;
        }
    }

    protected void handleMouseEvent(MouseEvent e) {
        VectorWxValue pickupValue = (VectorWxValue) parm.getParmState()
                .getPickUpValue();

        Rectangle area = canvas.getClientArea();
        Point center = Geometry.centerPoint(area);
        int maxRadius = (int) (Math.min(area.width, area.height) * 0.45);

        int dx = e.x - center.x;
        int dy = e.y - center.y;

        float magMax = parm.getGridInfo().getMaxValue();
        float newDir = (float) (Math.toDegrees(Math.atan2(-dx, dy)));
        if (newDir < 0) {
            newDir += 360;
        }
        float newMag = (float) ((Math.hypot(dx, dy) / maxRadius) * magMax);
        newMag = Math.max(newMag, parm.getGridInfo().getMinValue());
        newMag = Math.min(newMag, parm.getGridInfo().getMaxValue());

        // determine whether the direction is "to" or "from"
        float diff = (Math.abs(pickupValue.getDir() - newDir));
        if (diff > 180.0) {
            diff -= 360.0;
            diff = (Math.abs(diff));
        }

        // if location is > 90 from the arrow, then we are in from mode
        if (diff > 90.0) {
            newDir -= 180.0;
            if (newDir < 0.0) {
                newDir += 360.0;
            }
        }

        // depending upon certain vector modes, we want to not change
        // a component of the pickupvalue
        switch (parm.getParmState().getVectorMode()) {
        case DIRECTION:
            newMag = pickupValue.getMag();
            break;
        case MAGNITUDE:
            newDir = pickupValue.getDir();
            break;
        default:
            break;
        }

        pickupValue = new VectorWxValue(newMag, newDir, parm);
        parm.getParmState().setPickUpValue(pickupValue);
    }

    public void render(PaintEvent e) {
        VectorWxValue pickupValue = (VectorWxValue) parm.getParmState()
                .getPickUpValue();
        canvas.setBackground(canvas.getDisplay()
                .getSystemColor(SWT.COLOR_BLACK));
        float mag = pickupValue.getMag();
        float dir = pickupValue.getDir();
        float magMax = parm.getGridInfo().getMaxValue();

        // determine the center coordinate for this drawing area
        Rectangle area = canvas.getClientArea();
        Point center = Geometry.centerPoint(area);
        int maxRadius = (int) (Math.min(area.width, area.height) * 0.45);

        // calculate the circle radius
        int radius = 0;
        if (logFactor != 0.0) {
            double lg = Math.log(logFactor + (mag / magMax));
            radius = (int) ((maxRadius * (lg - minLog)) / (maxLog - minLog));
        } else {
            radius = (int) ((mag * maxRadius) / magMax);
        }

        // paint the circle
        GC gc = e.gc;
        gc.setForeground(gc.getDevice().getSystemColor(SWT.COLOR_WHITE));
        gc.drawOval(center.x - radius, center.y - radius, radius * 2,
                radius * 2);

        // paint the wind arrows and wind barbs, if in BOTH or DIRECTION mode
        // arrows and barbs are always drawn full radius

        drawArrow(gc, mag, dir, maxRadius, center);
        drawBarb(gc, mag, dir, maxRadius, center);

        gc.setFont(pickupFont);
        String s = pickupValue.toString();
        Point extent = gc.textExtent(s);
        gc.drawText(s, center.x - (extent.x / 2), center.y - (extent.y / 2),
                true);

    }

    private void drawArrow(GC gc, float mag, float dir, int size, Point location) {
        double tipAngle = 150.0;
        double tipLength = size / 5.0;

        double arrowDir = dir + 180; // arrows point down wind
        double dirSin = Math.sin(Math.toRadians(arrowDir));
        double dirCos = Math.cos(Math.toRadians(arrowDir));
        double tip1Sin = Math.sin(Math.toRadians(arrowDir + tipAngle));
        double tip1Cos = Math.cos(Math.toRadians(arrowDir + tipAngle));
        double tip2Sin = Math.sin(Math.toRadians(arrowDir - tipAngle));
        double tip2Cos = Math.cos(Math.toRadians(arrowDir - tipAngle));

        ArrayList<Point> segments = new ArrayList<Point>();

        // Some handy vectors.
        Point v1 = new Point((int) ((dirSin * size) + 0.5),
                -(int) ((dirCos * size) + 0.5));
        Point v2 = new Point((int) ((tip1Sin * tipLength) + 0.5),
                -(int) ((tip1Cos * tipLength) + 0.5));
        Point v3 = new Point((int) ((tip2Sin * tipLength) + 0.5),
                -(int) ((tip2Cos * tipLength) + 0.5));

        // calculate the little rectangle
        // genLittleRectangle(segments, location);

        // Calculate the arrow shaft
        v1.x += location.x;
        v1.y += location.y;
        segments.add(location);
        segments.add(v1);

        // And now the tips
        v2.x += v1.x;
        v2.y += v1.y;
        segments.add(v1);
        segments.add(v2);

        v3.x += v1.x;
        v3.y += v1.y;
        segments.add(v1);
        segments.add(v3);

        paintLines(gc, segments);
    }

    private void drawBarb(GC gc, float mag, float dir, int size, Point location) {
        double fletchAngle = 60.0; // in degrees from the barb shaft

        // round mag to nearest 5 knots
        float speed = mag + 2.5f;

        // calculate some stuff based on the barbSize
        // these can be fiddled with to modify the appearance of the barb
        double staffSize = size / 2.0; // base length of the staff
        double fletchSize = staffSize / 2.0; // length of each fletch
        double fletchDelta = size * 0.1; // distance between fletches

        double dirSin = Math.sin(Math.toRadians(dir));
        double dirCos = Math.cos(Math.toRadians(dir));
        double fletchSin = Math.sin(Math.toRadians(dir + fletchAngle));
        double fletchCos = Math.cos(Math.toRadians(dir + fletchAngle));

        // calculate some terms once and use them many times
        double xStaff = dirSin * staffSize;
        double yStaff = dirCos * staffSize;
        double xSpace = dirSin * fletchDelta;
        double ySpace = dirCos * fletchDelta;
        double xFletch = fletchSin * fletchSize;
        double yFletch = fletchCos * fletchSize;

        // Now figure out how many flags, fletches, and halfFletches we need.
        int flagCount = 0;
        int fletchCount = 0;
        int halfFletchCount = 0;

        // count the number of flags
        while (speed >= 50.0) {
            flagCount++;
            speed -= 50.0;
        }

        // count the number of fletches
        while (speed >= 10.0) {
            fletchCount++;
            speed -= 10.0;
        }

        // count the number of half fletches
        while (speed >= 5.0) {
            halfFletchCount++;
            speed -= 5.0;
        }

        // Now start calculating all of the segments that make up the wind barb
        ArrayList<Point> segments = new ArrayList<Point>();

        // calculate the little rectangle
        // genLittleRectangle(segments, location);

        // if the speed is less than 2.5, we're done.
        // We could put the rest of the code in an IF block, too.
        if (mag < 2.5f) {
            paintLines(gc, segments);
            return;
        }

        // Figure out many fletch and flag units are required
        int fletchDeltaCount = fletchCount;
        int flagDeltaCount = 0;
        if (mag < 10.0) {
            fletchDeltaCount = 1;
        }
        if (flagCount > 0) {
            flagDeltaCount = flagCount + 1;
        }

        // calculate the staffSize length based on the number of units
        segments.add(location);
        Point p = new Point(
                (int) (0.5 + (dirSin * (staffSize + (fletchDelta * ((flagDeltaCount + fletchDeltaCount) - 1))))),
                -(int) ((-0.5 + (dirCos * (staffSize + (fletchDelta * ((flagDeltaCount + fletchDeltaCount) - 1)))))));
        p.x += location.x;
        p.y += location.y;
        segments.add(p);

        // Now calculate the locations for the fletches and flags
        // Half fletches first
        if (halfFletchCount > 0) {
            Point v1 = new Point(
                    (int) (0.5 + (dirSin * (staffSize - fletchDelta))),
                    (int) (0.5 - (dirCos * (staffSize - fletchDelta))));
            Point v2 = new Point((int) ((xFletch * 0.6) + 0.5),
                    -(int) ((yFletch * 0.6) + 0.5));

            v1.x += location.x;
            v1.y += location.y;
            segments.add(v1);

            v2.x += v1.x;
            v2.y += v1.y;
            segments.add(v2);
        }

        // On to the full-size fletches
        int i;
        int elementCount = 0;
        for (i = 0; i < fletchCount; i++) {
            Point v1 = new Point(
                    (int) (xStaff + (elementCount * xSpace) + 0.5),
                    (int) ((-yStaff - (elementCount * ySpace)) + 0.5));
            Point v2 = new Point((int) (xFletch + 0.5), -(int) (yFletch + 0.5));

            v1.x += location.x;
            v1.y += location.y;
            segments.add(v1);

            v2.x += v1.x;
            v2.y += v1.y;
            segments.add(v2);

            elementCount++;
        }

        // and finally the flags
        for (i = 0; i < flagCount; i++) {
            Point v1 = new Point(
                    (int) (xStaff + (elementCount * xSpace) + 0.5),
                    (int) ((-yStaff - (elementCount * ySpace)) + 0.5));

            v1.x += location.x;
            v1.y += location.y;
            segments.add(v1);

            // the first side is just like a regular fletch
            Point v2 = new Point((int) (xFletch + 0.5), -(int) (yFletch + 0.5));
            v2.x += v1.x;
            v2.y += v1.y;
            segments.add(v2);

            // the second side connects back to the staffSize
            elementCount++;
            Point v3 = new Point(
                    (int) (xStaff + ((elementCount) * xSpace) + 0.5),
                    (int) ((-yStaff - ((elementCount) * ySpace)) + 0.5));

            v3.x += location.x;
            v3.y += location.y;
            segments.add(v2); // previous point
            segments.add(v3);
        }

        // Draw the segments!
        paintLines(gc, segments);
    }

    private void paintLines(GC gc, ArrayList<Point> segments) {
        for (int i = 0; i < segments.size(); i += 2) {
            Point p1 = segments.get(i);
            Point p2 = segments.get(i + 1);
            gc.drawLine(p1.x, p1.y, p2.x, p2.y);
        }
    }

    /**
     * pickupFont contains system resources which must be freed.
     * 
     * @see java.lang.Object#finalize()
     */
    public void dispose() {
        if ((pickupFont != null) && !pickupFont.isDisposed()) {
            pickupFont.dispose();
            pickupFont = null;
        }
    }
}
