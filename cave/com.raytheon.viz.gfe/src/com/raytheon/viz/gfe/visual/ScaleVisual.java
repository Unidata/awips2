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

import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo.GridType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.constants.StatusConstants;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.wxvalue.ScalarWxValue;
import com.raytheon.viz.gfe.core.wxvalue.VectorWxValue;

/**
 * Displays a scale
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/30/2009   #2159      rjpeter     Initial creation.
 * 06/08/2009   #2159      rjpeter     Fixed pan min/maxExtent to consider zoom.
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class ScaleVisual {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(ScaleVisual.class);
    protected static final Color SCALE_COLOR = Display.getDefault()
            .getSystemColor(SWT.COLOR_WHITE);

    private static int multStep[] = { 1, 2, 5 }; // possible freq of major tick

    // marks

    private static int divider[] = { 5, 4, 5 }; // freq of minor tick marks

    private static int minPixelsPerMajorTick = 24;

    private static int majorTickSize = 9;

    private static int minorTickSize = 5;

    private static float maxZoom = 20f;

    private Canvas scaleCanvas;

    private float minValue = Float.MAX_VALUE;

    private float maxValue = -Float.MAX_VALUE;

    private int scaleFactor = Integer.MIN_VALUE;

    private int maxPrecision = Integer.MIN_VALUE;

    private Parm maxPrecisionParm;

    private int height = -1;

    private int majorInterval;

    private int minorInterval;

    private int majorStart;

    private int minorStart;

    private float maxExtent = -Float.MAX_VALUE;

    private float minExtent = Float.MAX_VALUE;

    private float curTopExtent = maxExtent;

    private float curBottomExtent = minExtent;

    private int rhsOffset;

    private float margin;

    private boolean forceTickRecompute = true;

    private float currentZoom = 1f;

    /**
     * @param rhsOffset
     */
    public ScaleVisual(int rhsOffset, float margin) {
        this.rhsOffset = rhsOffset;
        this.margin = margin;
    }

    public void setCanvas(Canvas scaleCanvas) {
        this.scaleCanvas = scaleCanvas;
    }

    /**
     * 
     * @param parmSet
     */
    public void setParms(List<Parm> parmSet) {
        minValue = Float.MAX_VALUE;
        maxValue = -Float.MAX_VALUE;
        maxPrecision = Integer.MIN_VALUE;
        currentZoom = 1;

        if (parmSet.size() > 0) {
            for (Parm parm : parmSet) {
                GridParmInfo gridInfo = parm.getGridInfo();
                minValue = Math.min(minValue, gridInfo.getMinValue());
                maxValue = Math.max(maxValue, gridInfo.getMaxValue());

                if (gridInfo.getPrecision() > maxPrecision) {
                    maxPrecision = gridInfo.getPrecision();
                    maxPrecisionParm = parm;
                }
            }

            float parmExtent = maxValue - minValue;
            if (parmExtent < 1.0) {
                scaleFactor = 1000000;
            } else if (parmExtent < 10.0) {
                scaleFactor = 100000;
            } else if (parmExtent < 100.0) {
                scaleFactor = 10000;
            } else if (parmExtent < 1000.0) {
                scaleFactor = 1000;
            } else if (parmExtent < 10000.0) {
                scaleFactor = 100;
            } else if (parmExtent < 100000.0) {
                scaleFactor = 10;
            } else {
                scaleFactor = 1;
            }

            resetExtent();
        } else {
            minExtent = Float.MAX_VALUE;
            maxExtent = -Float.MAX_VALUE;
        }

        curBottomExtent = minExtent;
        curTopExtent = maxExtent;
    }

    protected void resetExtent() {
        // Add a margin in each direction based on the zoom factor
        float extraSpace = (maxValue - minValue) * margin * currentZoom;

        minExtent = minValue - extraSpace;
        maxExtent = maxValue + extraSpace;
    }

    // -- protected
    // --------------------------------------------------------------
    // ContScaleVisual::renderScale()
    // Rouine to render the scale. The graphics context has already been set.
    // This routine simply performs the drawing.
    // -- implementation
    // ---------------------------------------------------------
    // The graphics is used to determine the scaling. If the Graphics's
    // scaling or windowsize has changed (by comparing the
    // Visualization::mapping() with _lastMapping, we must recompute the
    // interval
    // between tick marks. Then we begin at the starting minvalue (passed in
    // from the base class' paint routine) and step up by the minor interval,
    // drawing a minor tick mark at each step, and checking if the next tick
    // mark is also a major tick mark. For each major tick mark we encounter,
    // we also draw a label, using the WxValue class to turn the value of the
    // major tick mark into a text string to paint.
    //
    // Once the distance between tick marks is figured, we must extend the
    // maximum and minimum marks by one in each direction, so that we make
    // sure to paint the next larger and next smaller label. This is just in
    // case the paint domain extends far enough for the edges of the label
    // to be seen, but not far enough for the tick marks causing those labels
    // to be painted within the calculated range. This is the "adjust paint
    // domain" calculation.
    //
    // Note that all calculations are done as integers: we multiply the
    // values by _scaleFactor for our calculations, then divide by _scaleFactor
    // again just before recording the final tick mark values.
    //
    // The lines coordinates are converted to screen coordinates in this
    // routine.
    // ---------------------------------------------------------------------------
    /**
     * Paints the scale on the canvas
     * 
     * @param event
     */
    public void renderScale(PaintEvent event) {
        GC gc = event.gc;
        gc.setLineWidth(1);
        gc.setLineStyle(SWT.LINE_SOLID);
        gc.setForeground(SCALE_COLOR);

        Rectangle area = scaleCanvas.getClientArea();

        if (forceTickRecompute || height != area.height) {
            computeTickInterval();
        }

        // calculate the x location for the axis in screen coordinates
        Point axisMin = getPointForValue(minValue);
        Point axisMax = getPointForValue(maxValue);

        // ensure line is drawn in visible area
        if (axisMin.y > area.height) {
            axisMin.y = area.height;
        }

        if (axisMax.y < 0) {
            axisMax.y = 0;
        }

        // vertical axis
        event.gc.drawLine(axisMin.x, axisMin.y, axisMax.x, axisMax.y);
        // set up an array of lines for us to draw the tick marks
        // into for our scale. Add the vertical axis to it first.

        // find the first major and minor tick mark to start painting.
        // first adjust range Min and Max to be a minor tick mark larger on
        // both sides, to account for labels that may be painted outside of
        // the paintArea but will still be partly visible inside the paint area.
        int iRangeMin = (int) (curBottomExtent * scaleFactor) - minorInterval
                / 2 - 1;

        int minScale = (int) (minValue * scaleFactor);
        // but make sure that we don't expand the minimum range lower than
        // the actual scale value.
        if (iRangeMin < minScale) {
            iRangeMin = minScale;
        }

        int iRangeMax = (int) (curTopExtent * scaleFactor) + majorInterval / 2
                + 1;
        int maxScale = (int) (maxValue * scaleFactor);
        // but make sure that we don't expand the maximum range greater than
        // the actual scale value.
        if (iRangeMax > maxScale) {
            iRangeMax = maxScale;
        }

        // establish exactly where, within our current minimum and maximum
        // range, the first minor tick mark and first major tick mark will be
        // drawn.
        int minor = minorStart; // minor is first minor - for this range.
        while (minor < iRangeMin) {
            minor += minorInterval;
        }
        int major = majorStart; // major is first major - for this range.
        while (major < iRangeMin) {
            major += majorInterval;
        }

        // now, increment through the tick marks and paint each one by
        // adding the line to the mono line set.
        for (int i = minor; i <= iRangeMax; i += minorInterval) {
            float yPosition = ((float) i) / scaleFactor;
            Point pt = getPointForValue(yPosition);

            if (i == major) {
                // then append a major tick mark to the line set.
                int leftTick = pt.x - majorTickSize / 2;
                gc.drawLine(leftTick, pt.y, pt.x + majorTickSize / 2, pt.y);

                String txt;
                // ... and paint a text label next to the tick mark.
                if (GridType.SCALAR.equals(maxPrecisionParm.getGridInfo()
                        .getGridType())) {
                    ScalarWxValue wx = new ScalarWxValue(
                            (i / (float) scaleFactor), maxPrecisionParm);
                    txt = wx.toString();

                } else {
                    VectorWxValue wx = new VectorWxValue(
                            (i / (float) scaleFactor), 0.0f, maxPrecisionParm);
                    txt = wx.magToString();
                }

                Point textExtent = gc.stringExtent(txt);
                gc.drawText(txt, leftTick - textExtent.x, pt.y - textExtent.y
                        / 2, SWT.DRAW_TRANSPARENT);
                major += majorInterval;
            }

            else {
                // it's a minor tick mark
                gc.drawLine(pt.x - minorTickSize / 2, pt.y, pt.x
                        + minorTickSize / 2, pt.y);
            }
        }

        return;
    }

    // -- private
    // ----------------------------------------------------------------
    // ContScaleVisual::computeTickInterval()
    // Utility function to recompute the tick intervals. Calculates and sets
    // _majorInterval, _minorInterval, _majorStart, and _minorStart.
    // -- implementation
    // ---------------------------------------------------------
    // Once the new ranges are calculated, the maximum number of tick-marks
    // per window is calculated based on the scale location's Y extent. Then
    // the closest tick-interval for this window is fit to the number
    // of ticks-per-window.
    // ---------------------------------------------------------------------------
    /**
     * Utility function to recompute the tick intervals.
     */
    private void computeTickInterval() {
        forceTickRecompute = false;
        height = scaleCanvas.getSize().y;
        float scaling = (curTopExtent - curBottomExtent) / height;

        // Convert our min and max values to integers
        // so we can do this whole deal using integer arithmetic.
        int iRangeMin = (int) minValue * scaleFactor;
        int iRangeMax = (int) maxValue * scaleFactor;

        // check that the range minimum/maximums make sense. Otherwise
        // this is an indicator of bad values coming in from the database.
        // NOTE that this is a temporary check: bad database values were
        // causing a core dump (floating point excp) in this routine. In
        // a perfect world, these bad values would have been caught before
        // getting this far.
        if (iRangeMin >= iRangeMax) {
            statusHandler.handle(Priority.PROBLEM,
                    "Min/Max database values illegal.");
        }

        // get the number of major tick marks allowed in the current
        // scale range and in the current data area sizing. Must
        // map world DISTANCE into screen DISTANCE.
        int numMajorTicks = (int) (((maxValue - minValue) / scaling) / minPixelsPerMajorTick) + 1;

        long decade = 1; // used to step up multipliers by factors of 10
        boolean done = false; // boolean value - to determine if done
        // multiplying
        long majorTicks = 0; // how many major tick marks in the range?
        long majorTickVal = 0; // interval between major tick marks
        int i = 0; // loop variable, also needed outside of loop.

        while (!done) {
            for (i = 0; i < 2; i++) {
                majorTickVal = decade * multStep[i];
                majorTicks = (iRangeMax - iRangeMin) / majorTickVal;
                if (majorTicks <= numMajorTicks) {
                    done = true;
                    break;
                }
            }
            if (done) {
                break;
            } else {
                decade *= 10; // increase by factor of 10 and try again.
            }
        }

        // after finishing the loop, majorTickInterval is set to the value
        // between each major tick mark.
        majorInterval = (int) majorTickVal;
        minorInterval = (int) (majorTickVal / divider[i]);

        // set the starting locations for the first major tick mark and
        // minor tick mark. It's possible, if the interval is larger than
        // the minimum value in the scale location, that the first tick
        // marks will be truncated down to a SMALLER number than the first
        // number in the location range. If this happens, increase the
        // starting location by one interval value.
        majorStart = (iRangeMin / majorInterval) * majorInterval;
        if (majorStart < iRangeMin) {
            majorStart += majorInterval;
        }
        minorStart = (iRangeMin / minorInterval) * minorInterval;
        if (minorStart < iRangeMin) {
            minorStart += minorInterval;
        }

        return;
    }

    /**
     * Zooms the scale in/out by the given factor and moves the pixel height to
     * the center of the scale.
     * 
     * @param zoomFactor
     * @param yZoomCenterPixel
     */
    public void zoom(float zoomFactor, int yZoomCenterPixel) {
        forceTickRecompute = true;
        float centerVal = getValueForHeight(yZoomCenterPixel);

        // check max zoom
        if (currentZoom * zoomFactor > 1) {
            currentZoom = 1;
            resetExtent();
            curTopExtent = maxExtent;
            curBottomExtent = minExtent;
        } else {
            if ((currentZoom * zoomFactor) < (1 / maxZoom)) {
                currentZoom = (1 / maxZoom);
                resetExtent();
                curTopExtent = maxExtent * currentZoom;
                curBottomExtent = minExtent * currentZoom;
            } else {
                currentZoom *= zoomFactor;
                resetExtent();
                curTopExtent *= zoomFactor;
                curBottomExtent *= zoomFactor;
            }

            // adjust center
            float extentRange = curTopExtent - curBottomExtent;
            curTopExtent = centerVal + extentRange / 2;
            curBottomExtent = centerVal - extentRange / 2;

            verifyBoundaries();
        }

        scaleCanvas.redraw();
    }

    /**
     * Scrolls the scale by the given number of pixels.
     * 
     * @param pixels
     */
    public void pan(int pixels) {
        // only pan if zoomed in
        if (currentZoom < 1) {
            int curHeight = getPointForValue(curBottomExtent).y;
            float newBottomExtent = getValueForHeight(curHeight + pixels);
            curTopExtent += (newBottomExtent - curBottomExtent);
            curBottomExtent = newBottomExtent;
            verifyBoundaries();
            scaleCanvas.redraw();
        }
    }

    /**
     * Forces the current top/bottom to the passed values.
     */
    public void setVisibleRange(float bottomValue, float topValue) {
        curBottomExtent = bottomValue;
        curTopExtent = topValue;
        currentZoom = 1;
        resetExtent(); // force back to full boundary to figure out true zoom
        // value

        currentZoom = (curTopExtent - curBottomExtent)
                / (maxExtent - minExtent);

        if (currentZoom < 1 / maxZoom) {
            currentZoom = 1 / maxZoom;
        }

        resetExtent();
        verifyBoundaries();
    }

    /**
     * Resets the extents to the min/max extent.
     */
    public void fullView() {
        currentZoom = 1;
        forceTickRecompute = true;
        resetExtent();
        curTopExtent = maxExtent;
        curBottomExtent = minExtent;
    }

    /**
     * @return The y point for the given value.
     */
    public Point getPointForValue(float val) {
        Rectangle area = scaleCanvas.getClientArea();
        int yMax = area.height;
        float range = curTopExtent - curBottomExtent;
        float rangePerPixel = range / yMax;
        int y = yMax + (int) ((curBottomExtent - val) / rangePerPixel);
        return new Point(area.width - rhsOffset, y);
    }

    /**
     * @return The value for the given height.
     */
    public float getValueForHeight(int y) {
        Rectangle area = scaleCanvas.getClientArea();
        int yMax = area.height;
        float range = curTopExtent - curBottomExtent;
        float rangePerPixel = range / yMax;
        float val = curBottomExtent - (y - yMax) * rangePerPixel;
        return val;
    }

    /**
     * @return The direction for the given height.
     */
    public float getDirectionForHeight(int y) {
        Rectangle area = scaleCanvas.getClientArea();
        int yMax = area.height;
        float spacing = yMax * 0.2f; // drop 20% off top and bottom

        if (y < spacing || y > yMax - spacing) {
            return 0;
        }

        float rangePerPixel = 360.0f / (yMax - spacing * 2);

        float val = (yMax - spacing - y) * rangePerPixel;
        return val;
    }

    /**
     * Verifies the current extents are valid and adjusts them to be correct if
     * they are invalid.
     */
    private void verifyBoundaries() {
        // handle equal extent
        if (curTopExtent == curBottomExtent) {
            float spacing = (maxExtent - minExtent) * 0.2f;
            curTopExtent += spacing;
            curBottomExtent -= spacing;
        }

        // readjust bounds if centered too close to a boundary
        if (curTopExtent > maxExtent) {
            float diff = curTopExtent - maxExtent;
            curTopExtent -= diff;
            curBottomExtent -= diff;
        }
        if (curBottomExtent < minExtent) {
            float diff = minExtent - curBottomExtent;
            curTopExtent += diff;
            curBottomExtent += diff;
        }

        // force clip if still outside bounds
        if (curTopExtent > maxExtent) {
            curTopExtent = maxExtent;
        }
        if (curBottomExtent < minExtent) {
            curBottomExtent = minExtent;
        }
    }
}
