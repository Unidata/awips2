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

import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Composite;

import com.raytheon.uf.common.datadelivery.bandwidth.data.BandwidthGraphData;
import com.raytheon.uf.common.datadelivery.bandwidth.data.TimeWindowData;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.datadelivery.bandwidth.ui.BandwidthImageMgr.SortBy;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryGUIUtils.SubscriptionPriority;

/**
 * The graph image class.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 28, 2012    1269    lvenable    Initial creation.
 * Dec 13, 2012   1269     lvenable    Fixes and updates.
 * Jan 07, 2013   1451     djohnson    Use TimeUtil.newGmtCalendar().
 * Jan 04, 2013   1420     mpduff      Change default priority to normal priority.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */

public class GraphImage extends AbstractCanvasImage {
    /**
     * A map of rectangles keys for the time window and String values that will
     * be the tool tip text.
     */
    private Map<Rectangle, String> windowTimeInfoMap;

    /** Rectangle selected flag */
    private boolean rectangleSelected;

    /** The TimeWindowData object */
    private TimeWindowData windowData;

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
     *            The bandwidth image manager
     */
    public GraphImage(Composite parentComp, CanvasSettings cs,
            BandwidthGraphData graphData, BandwidthImageMgr imageMgr) {
        super(parentComp, cs, graphData, imageMgr);

        init();
    }

    /**
     * Initialize
     */
    private void init() {
        windowTimeInfoMap = new HashMap<Rectangle, String>();
        millisPerPix = millis48Hrs
                / (cs.getImageWidth() - cs.getXSpaceBuffer() * 2);
        bgColor = new Color(display, 230, 230, 230);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void disposeResources() {
        bgColor.dispose();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getToolTipText(Point mouseCoord) {
        for (Rectangle rect : this.windowTimeInfoMap.keySet()) {
            if (rect.contains(mouseCoord)) {
                return windowTimeInfoMap.get(rect);
            }
        }

        return null;
    }

    /**
     * {@inheritDoc}
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

        // Draw the bars
        drawData(gc);
    }

    /**
     * Draw the data.
     * 
     * @param gc
     *            Graphics Context
     */
    private void drawData(GC gc) {
        windowTimeInfoMap.clear();

        int offset = 22;
        int yCoord = cs.getImageHeight() - offset;
        Color c = null;
        gc.setForeground(display.getSystemColor(SWT.COLOR_GRAY));

        if (graphData == null) {

            return;
        }

        long currentTimeMillis = imageMgr.getCurrentTimeMillis();

        // Loop over data and draw
        Map<String, List<TimeWindowData>> dataMap = graphData.getDataMap();

        List<String> subscriptionList = getSortedData();

        for (String subName : subscriptionList) {
            if (imageMgr.isColorByPriority()) {
                if (graphData.getPriority(subName) == SubscriptionPriority.NORMAL
                        .ordinal()) {
                    c = new Color(
                            display,
                            imageMgr.getPriorityColor(SubscriptionPriority.NORMAL));
                    gc.setBackground(c);
                } else if (graphData.getPriority(subName) == SubscriptionPriority.HIGH
                        .ordinal()) {
                    c = new Color(
                            display,
                            imageMgr.getPriorityColor(SubscriptionPriority.HIGH));
                    gc.setBackground(c);
                } else if (graphData.getPriority(subName) == SubscriptionPriority.LOW
                        .ordinal()) {
                    c = new Color(display,
                            imageMgr.getPriorityColor(SubscriptionPriority.LOW));
                    gc.setBackground(c);
                }
            } else {
                c = new Color(display, new RGB(6, 122, 255));
                gc.setBackground(c);
            }

            // Draw the dashed subscription line
            gc.setLineStyle(SWT.LINE_DASH);
            if (imageMgr.isShowSubscriptionLines()) {
                gc.setLineWidth(1);
                gc.setForeground(display.getSystemColor(SWT.COLOR_GRAY));
                gc.drawLine(0, yCoord + 4, cs.getImageWidth(), yCoord + 4);
            }

            gc.setLineStyle(SWT.LINE_SOLID);

            long startTime = 0;
            List<TimeWindowData> timeWindows = dataMap.get(subName);
            for (TimeWindowData data : timeWindows) {
                startTime = data.getTimeWindowStartTime();
                if (data.getTimeWindowEndTime() < currentTimeMillis) {
                    continue;
                }

                if (startTime < currentTimeMillis) {
                    startTime = currentTimeMillis;
                }
                int xCoord = (int) ((startTime - currentTimeMillis) / millisPerPix) + 1;

                xCoord += cs.getXSpaceBuffer();

                int xCoord2 = 0;
                long endTime = data.getTimeWindowEndTime();
                if (endTime == startTime) {
                    endTime = startTime + graphData.getBinTimeInMins()
                            * TimeUtil.MILLIS_PER_MINUTE;

                }

                xCoord2 = (int) ((endTime - startTime) / millisPerPix);

                Rectangle r = new Rectangle(xCoord, yCoord, xCoord2, 9);
                gc.fillRectangle(r);
                // if (selectionPoint != null && r.contains(selectionPoint)) {
                if (windowData != null && subName != null) {
                    if (windowData.getTimeWindowEndTime() > currentTimeMillis) {

                        if (data.equals(windowData) && subName.equals(subName)) {
                            // Highlight the time bar
                            gc.setForeground(display
                                    .getSystemColor(SWT.COLOR_MAGENTA));
                            gc.setLineWidth(2);
                            gc.drawRectangle(r.x, r.y - 4, r.width + 4,
                                    r.height + 8);

                            if (imageMgr.getSortBy() == SortBy.SELECTED_START
                                    || imageMgr.getSortBy() == SortBy.SELECTED_INTERSECT) {
                                gc.drawLine(xCoord, 0, xCoord,
                                        cs.getImageHeight());
                            }
                        }
                    } else {
                        clearCanvasSelection();
                    }
                }

                windowTimeInfoMap.put(r, data.toString());
            }

            if (c != null) {
                c.dispose();
            }
            yCoord -= 25;
        }
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

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean hasSelection() {
        return rectangleSelected;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void performAction(Point point) {
        if (graphData == null) {
            return;
        }

        Map<String, List<TimeWindowData>> dataMap = graphData.getDataMap();
        long startTime = 0;
        long currentTimeMillis = imageMgr.getCurrentTimeMillis();
        List<String> subscriptionList = getSortedData();
        int offset = 22;
        int yCoord = cs.getImageHeight() - offset;

        for (String subName : subscriptionList) {
            List<TimeWindowData> timeWindows = dataMap.get(subName);
            for (TimeWindowData data : timeWindows) {
                startTime = data.getTimeWindowStartTime();
                if (data.getTimeWindowEndTime() < currentTimeMillis) {
                    continue;
                }

                if (startTime < currentTimeMillis) {
                    startTime = currentTimeMillis;
                }
                int xCoord = (int) ((startTime - currentTimeMillis) / millisPerPix) + 1;

                xCoord += cs.getXSpaceBuffer();

                int xCoord2 = 0;
                long endTime = data.getTimeWindowEndTime();
                if (endTime == startTime) {
                    endTime = startTime + graphData.getBinTimeInMins()
                            * TimeUtil.MILLIS_PER_MINUTE;

                }

                xCoord2 = (int) ((endTime - startTime) / millisPerPix);

                Rectangle r = new Rectangle(xCoord, yCoord, xCoord2, 9);
                if (r.contains(point)) {
                    rectangleSelected = true;
                    this.windowData = data;
                    imageMgr.setSortTimeMillis(startTime);
                    return;
                }
            }
            yCoord -= 25;
        }
    }

    @Override
    public void clearCanvasSelection() {
        rectangleSelected = false;
        windowData = null;
    }
}
