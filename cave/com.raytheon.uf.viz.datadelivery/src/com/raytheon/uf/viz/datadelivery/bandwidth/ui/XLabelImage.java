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

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;

import com.raytheon.uf.common.datadelivery.bandwidth.data.BandwidthGraphData;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * X axis label image
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 28, 2012            lvenable     Initial creation
 *
 * </pre>
 *
 * @author lvenable
 * @version 1.0
 */

public class XLabelImage extends AbstractCanvasImage {
    /** Zero digit string constant */
    private final String ZERO = "0";

    /** Date format */
    private final SimpleDateFormat sdf;

    /** Current time date format */
    private final SimpleDateFormat currentTimeFormat;

    /** Current time hash mark size */
    private final int currentTimeHash = 17;

    /** Hour hash mark size */
    private final int hourHash = 10;

    /**
     * Constructor.
     *
     * @param parentComp
     *            Parent composite
     * @param cs
     * @param graphData
     */
    public XLabelImage(Composite parentComp, CanvasSettings cs,
            BandwidthGraphData graphData) {
        super(parentComp, cs, graphData, null);
        sdf = new SimpleDateFormat("MM/dd/yyyy");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));

        currentTimeFormat = new SimpleDateFormat("HH:mm");
        currentTimeFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
        init();
    }

    /**
     * Initialize values.
     */
    private void init() {
        millisPerPix = millis48Hrs
                / (cs.getImageWidth() - cs.getXSpaceBuffer() * 2);
        bgColor = display.getSystemColor(SWT.COLOR_WHITE);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void disposeResources() {
        // TODO dispose resources here as needed
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

        // Draw time lines
        drawTimeLines(gc);
    }

    /**
     * Draw the time hash marks.
     *
     * @param gc
     *            The GC
     */
    private void drawTimeLines(GC gc) {
        Calendar cal = TimeUtil.newCalendar(timeZone);
        cal.set(Calendar.SECOND, 0);
        cal.set(Calendar.MILLISECOND, 0);

        long currentTimeMillis = cal.getTimeInMillis();
        long endTimeMillis = currentTimeMillis + millis48Hrs;

        cal.set(Calendar.MINUTE, 0);
        cal.add(Calendar.HOUR, 1);
        long hourInMillis = cal.getTimeInMillis();

        Calendar dateCal = TimeUtil.newCalendar(timeZone);
        dateCal.setTimeInMillis(currentTimeMillis);

        // Current time vertical line
        gc.setLineWidth(2);
        gc.setForeground(display.getSystemColor(SWT.COLOR_BLACK));
        gc.drawLine(cs.getXSpaceBuffer(), 0, cs.getXSpaceBuffer(),
                currentTimeHash);

        // Current time value
        gc.drawText(currentTimeFormat.format(dateCal.getTime()), 1,
                currentTimeHash + 12, true);

        // Draw hour lines
        int xCoord = 0;
        int zeroHourXCoord = 0;
        gc.setLineWidth(1);
        while (hourInMillis < endTimeMillis) {
            int hour = cal.get(Calendar.HOUR_OF_DAY);
            xCoord = (int) ((hourInMillis - currentTimeMillis) / millisPerPix);
            xCoord += cs.getXSpaceBuffer();

            if (hour == 0) {
                // Thicker 00Z line
                gc.setLineWidth(3);
                gc.drawLine(xCoord, 0, xCoord, hourHash);
                gc.setLineWidth(1);

                // Draw the date String
                dateCal.setTimeInMillis(cal.getTimeInMillis());
                dateCal.add(Calendar.DAY_OF_MONTH, -1);
                String currentDateStr = sdf.format(dateCal.getTime());
                Point p = gc.stringExtent(currentDateStr);
                int dateStartPix = ((xCoord - zeroHourXCoord) / 2) - (p.x / 2)
                        + zeroHourXCoord;
                gc.drawText(currentDateStr, dateStartPix, cs.getCanvasHeight()
                        - p.y - 2, true);
                zeroHourXCoord = xCoord;
            } else {
                gc.drawLine(xCoord, 0, xCoord, hourHash);
            }

            String hrStr = String.valueOf(hour);
            if (hour < 10) {
                hrStr = ZERO.concat(hrStr);
            }

            Point strPoint = gc.stringExtent(hrStr);
            gc.drawText(hrStr, xCoord - strPoint.x / 2, hourHash + 4, true);

            cal.add(Calendar.HOUR, 1);
            hourInMillis = cal.getTimeInMillis();
        }

        // Draw the date label if more than 2 hours are visible on the graph for
        // that date
        if (cal.get(Calendar.HOUR_OF_DAY) >= 2) {
            dateCal.setTimeInMillis(cal.getTimeInMillis());
            String currentDateStr = sdf.format(dateCal.getTime());
            Point p = gc.stringExtent(currentDateStr);
            int dateStartPix = ((xCoord - zeroHourXCoord) / 2) - (p.x / 2)
                    + zeroHourXCoord;
            gc.drawText(currentDateStr, dateStartPix, cs.getCanvasHeight()
                    - p.y - 2, true);
        }
    }
}