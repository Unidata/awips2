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
package com.raytheon.uf.viz.datadelivery.bandwidth.ui;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.Map;
import java.util.SortedSet;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Composite;

import com.raytheon.uf.common.datadelivery.bandwidth.data.BandwidthBucketDescription;
import com.raytheon.uf.common.datadelivery.bandwidth.data.BandwidthGraphData;
import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.datadelivery.bandwidth.ui.BandwidthImageMgr.GraphSection;
import com.raytheon.uf.viz.datadelivery.bandwidth.ui.BandwidthImageMgr.GraphType;

/**
 * Percent utilized graph image.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 20, 2013   2430     mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class UtilizationGraphImage extends AbstractCanvasImage {
    /** Lower percent limit line */
    private int lowerLimitLine;

    /** Upper percent limit line */
    private int upperLimitLine;

    private Color nonPriorityColor;

    /**
     * Constructor.
     * 
     * @param parentComp
     *            Parent composite
     * @param cs
     *            Canvas settings
     * @param graphData
     *            Graph data
     * @param imageMgr
     *            The BandwidthImageManager
     */
    public UtilizationGraphImage(Composite parentComp, CanvasSettings cs,
            BandwidthGraphData graphData, BandwidthImageMgr imageMgr) {
        super(parentComp, cs, graphData, null);
        bgColor = display.getSystemColor(SWT.COLOR_WHITE);
        this.imageMgr = imageMgr;
        init();
    }

    /**
     * Initialize
     */
    private void init() {
        millisPerPix = millis48Hrs
                / (cs.getImageWidth() - cs.getXSpaceBuffer() * 2);
        bgColor = new Color(display, 230, 230, 230);
        nonPriorityColor = new Color(display, new RGB(6, 122, 255));
    }

    /**
     * Draw the vertical time lines.
     * 
     * @param gc
     *            Graphics Context
     */
    private void drawTimeLines(GC gc) {
        Calendar cal = TimeUtil.newGmtCalendar();
        cal.set(Calendar.SECOND, 0);
        cal.set(Calendar.MILLISECOND, 0);
        long currentTimeMillis = cal.getTimeInMillis();
        long endTimeMillis = currentTimeMillis + millis48Hrs;

        cal.set(Calendar.MINUTE, 0);
        cal.add(Calendar.HOUR, 1);
        long hourInMillis = cal.getTimeInMillis();

        // Current time vertical line
        gc.setLineWidth(2);
        gc.setForeground(display.getSystemColor(SWT.COLOR_BLACK));
        gc.drawLine(cs.getXSpaceBuffer(), 0, cs.getXSpaceBuffer(),
                cs.getImageHeight());

        gc.setLineWidth(1);

        // Draw hour lines
        while (hourInMillis < endTimeMillis) {
            int xCoord = (int) ((hourInMillis - currentTimeMillis) / millisPerPix);
            xCoord += cs.getXSpaceBuffer();

            // Draw a thicker line at 00Z
            if (cal.get(Calendar.HOUR_OF_DAY) == 0) {
                gc.setLineWidth(3);
                gc.drawLine(xCoord, 0, xCoord, cs.getImageHeight());
                gc.setLineWidth(1);
            } else {
                gc.drawLine(xCoord, 0, xCoord, cs.getImageHeight());
            }

            cal.add(Calendar.HOUR, 1);
            hourInMillis = cal.getTimeInMillis();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.datadelivery.bandwidth.ui.AbstractCanvasImage#
     * disposeResources()
     */
    @Override
    public void disposeResources() {
        if (bgColor != null) {
            bgColor.dispose();
        }

        if (nonPriorityColor != null) {
            nonPriorityColor.dispose();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.datadelivery.bandwidth.ui.AbstractCanvasImage#drawImage
     * ()
     */
    @Override
    public void drawImage() {
        GC gc = new GC(image);
        gc.setAntialias(SWT.ON);

        gc.setBackground(bgColor);
        gc.fillRectangle(0, 0, cs.getImageWidth(), cs.getImageHeight());

        gc.setBackground(display.getSystemColor(SWT.COLOR_WHITE));
        gc.fillRectangle(0, 0, this.cs.getXSpaceBuffer(), cs.getImageHeight());

        // Draw vertical time lines
        drawTimeLines(gc);
        drawPercentLines(gc);
        drawData(gc);
    }

    /**
     * Draw the data.
     * 
     * @param gc
     */
    private void drawData(GC gc) {
        gc.setForeground(display.getSystemColor(SWT.COLOR_BLACK));
        gc.setBackground(display.getSystemColor(SWT.COLOR_BLACK));

        Map<Network, SortedSet<BandwidthBucketDescription>> dataMap = this.graphData
                .getNetworkBucketMap();
        SortedSet<BandwidthBucketDescription> data = dataMap.get(imageMgr
                .getNetwork());
        if (data == null || data.isEmpty()) {
            return;
        }

        long currentTimeMillis = imageMgr.getCurrentTimeMillis();

        int height = cs.getCanvasHeight();
        List<GraphPoint> points = new ArrayList<GraphPoint>();
        for (BandwidthBucketDescription bucket : data) {
            long bucketSize = bucket.getBucketSize();
            long startTime = bucket.getBucketStartTime();
            long usedBytes = bucket.getUsedBytes();

            if (startTime < currentTimeMillis) {
                continue;
            }

            int x = Math.round(((startTime - currentTimeMillis) / millisPerPix)
                    + 1 + cs.getXSpaceBuffer());

            double percent = usedBytes / (double) bucketSize;

            int y = (int) Math.round(height * percent);
            y = 60 - y;
            GraphPoint point = new GraphPoint(x, y);
            points.add(point);
        }

        if (imageMgr.getBandwidthGraphType() == GraphType.LINE) {
            drawLineGraph(points, gc);
        } else {
            drawBarGraph(points, gc);
        }
    }

    /**
     * Draw the bar graph.
     * 
     * @param points
     * @param gc
     */
    private void drawBarGraph(List<GraphPoint> points, GC gc) {
        int height = cs.getCanvasHeight();
        if (!imageMgr.isColorByPriority()) {
            gc.setBackground(nonPriorityColor);
        }
        for (GraphPoint point : points) {
            if (imageMgr.isColorByPriority()) {
                Color c = getColor(point.getY());
                gc.setForeground(c);
                gc.setBackground(c);
                c.dispose();
            }

            if (height - point.getY() != 0) {
                gc.fillRectangle(point.getX() - 1, point.getY() - 1, 3, height
                        - point.getY());
            }
        }
    }

    /**
     * Draw the line graph.
     * 
     * @param points
     * @param gc
     */
    private void drawLineGraph(List<GraphPoint> points, GC gc) {
        // Draw the line
        GraphPoint prevPoint = points.get(0);
        for (int i = 1; i < points.size(); i++) {
            GraphPoint p = points.get(i);
            gc.setForeground(display.getSystemColor(SWT.COLOR_DARK_GRAY));
            gc.drawLine(p.getX(), p.getY(), prevPoint.getX(), prevPoint.getY());
            prevPoint = p;
        }

        // Draw the points
        for (GraphPoint p : points) {
            int x = p.getX();
            int y = p.getY();

            Color c = null;
            if (imageMgr.isColorByPriority()) {
                c = getColor(y);
                gc.setForeground(c);
                gc.setBackground(c);
                c.dispose();
            }
            gc.drawRectangle(x - 1, (Math.round(y)) - 1, 3, 3);
            gc.fillRectangle(x, Math.round(y), 3, 3);
            gc.setForeground(display.getSystemColor(SWT.COLOR_BLACK));
            gc.setBackground(display.getSystemColor(SWT.COLOR_BLACK));
        }
        gc.setBackground(display.getSystemColor(SWT.COLOR_WHITE));
    }

    /**
     * Get the color for the point y.
     * 
     * @param y
     *            The point
     * @return The color for the point
     */
    private Color getColor(int y) {
        Color c = null;
        if (y > lowerLimitLine) {
            c = new Color(display, imageMgr.getPercentColor(GraphSection.LOWER));
        } else if (y > upperLimitLine) {
            c = new Color(display,
                    imageMgr.getPercentColor(GraphSection.MIDDLE));
        } else {
            c = new Color(display, imageMgr.getPercentColor(GraphSection.UPPER));
        }
        return c;
    }

    /**
     * Draw the percent threshold lines.
     * 
     * @param gc
     */
    private void drawPercentLines(GC gc) {
        int[] threshValues = imageMgr.getBandwidthThreholdValues();
        int height = cs.getCanvasHeight();
        lowerLimitLine = 60 - ((int) Math.round(height
                * (threshValues[0] / 100.0)));
        upperLimitLine = 60 - ((int) Math.round(height
                * (threshValues[1] / 100.0)));

        gc.setLineStyle(SWT.LINE_DASH);
        gc.setLineWidth(1);
        gc.setForeground(display.getSystemColor(SWT.COLOR_GRAY));
        gc.drawLine(cs.getXSpaceBuffer(), lowerLimitLine, cs.getImageWidth(),
                lowerLimitLine);
        gc.drawLine(cs.getXSpaceBuffer(), upperLimitLine, cs.getImageWidth(),
                upperLimitLine);
        gc.setLineStyle(SWT.LINE_SOLID);
    }
}
