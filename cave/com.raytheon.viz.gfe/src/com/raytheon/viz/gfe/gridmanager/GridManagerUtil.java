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
package com.raytheon.viz.gfe.gridmanager;

import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Pattern;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.drawables.FillPatterns;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.PreferenceInitializer;
import com.raytheon.viz.gfe.rsc.GFELinePatterns;

/**
 * Utility to store common tasks of the GridManager
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03/04/2008              dfitch      Initial creation.
 * Apr 7, 2009  #2212      randerso    Reimplemented
 * May 18, 2009 #2159      rjpeter     Added support for temporal editor.
 * 06/21/2011   9897       ryu         reload preferences
 * 
 * </pre>
 * 
 * @author dfitch
 * @version 1.0
 */

public class GridManagerUtil {
    public static final long MILLIS_PER_SECOND = 1000;

    public static final long SECONDS_PER_HOUR = 3600;

    public static final long MILLIS_PER_HOUR = SECONDS_PER_HOUR
            * MILLIS_PER_SECOND;

    public static final int SECONDS_PER_PIXEL[] = { 400, 300, 240, 180, 144,
            100, 80, 60, 48, 36, 25, 20, 16, 12, 8, 4 };

    public final int HOUR_INCREMENT[] = { 6, 3, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1,
            1, 1, 1, 1 };

    public static Pattern Selected_pattern;

    public static Color TimeScaleLines_color;

    public static int TimeScaleLines_pattern;

    public static boolean showTimeScaleLines;

    public static Color EditorTimeLine_color;

    public static int EditorTimeLine_width;

    public static int EditorTimeLine_pattern;

    public static Color EditorBackground_color;

    static {
        new PreferenceInitializer() {
            @Override
            public void init() {
                IPreferenceStore prefs = Activator.getDefault()
                        .getPreferenceStore();
                String color, pattern;

                if ((color = prefs.getString("Selected_color")).isEmpty()) {
                    color = "LightSkyBlue";
                }
                if ((pattern = prefs.getString("Selected_fillPattern"))
                        .isEmpty()) {
                    pattern = "TRANS_25PC_45DEG";
                }
                if (Selected_pattern != null) {
                    Selected_pattern.dispose();
                }
                Selected_pattern = FillPatterns.getSWTPattern(
                        RGBColors.getRGBColor(color), pattern);

                if ((color = prefs.getString("TimeScaleLines_color")).isEmpty()) {
                    color = "Blue";
                }
                if (TimeScaleLines_color != null) {
                    TimeScaleLines_color.dispose();
                }
                TimeScaleLines_color = new Color(Display.getDefault(),
                        RGBColors.getRGBColor(color));

                if ((pattern = prefs.getString("TimeScaleLines_pattern"))
                        .isEmpty()) {
                    pattern = "DOTTED";
                }
                TimeScaleLines_pattern = GFELinePatterns.getSWTPattern(pattern);

                showTimeScaleLines = true;
                if (prefs.contains("TimeScaleLines")) {
                    showTimeScaleLines = prefs.getBoolean("TimeScaleLines");
                }

                if ((color = prefs.getString("EditorTimeLine_color")).isEmpty()) {
                    color = "Yellow";
                }
                if (EditorTimeLine_color != null) {
                    EditorTimeLine_color.dispose();
                }
                EditorTimeLine_color = new Color(Display.getDefault(),
                        RGBColors.getRGBColor(color));

                if ((EditorTimeLine_width = prefs
                        .getInt("EditorTimeLine_width")) == 0) {
                    EditorTimeLine_width = 2;
                }

                if ((pattern = prefs.getString("EditorTimeLine_pattern"))
                        .isEmpty()) {
                    pattern = "DOTTED";
                }
                EditorTimeLine_pattern = GFELinePatterns.getSWTPattern(pattern);

                if ((color = prefs.getString("bgColor")).isEmpty()) {
                    color = "Black";
                }
                if (EditorBackground_color != null) {
                    EditorBackground_color.dispose();
                }
                EditorBackground_color = new Color(Display.getDefault(),
                        RGBColors.getRGBColor(color));
            }
        }.run();
    }

    protected GridManager gridManager;

    public GridManagerUtil(GridManager gridManager) {
        this.gridManager = gridManager;
    }

    /**
     * Returns the pixel that represents the Date's value.
     * 
     * @param date
     * @return
     */
    public int dateToPixel(Date date) {
        Date startTime = getVisibleTimeRange().getStart();

        int retVal = (int) ((date.getTime() - startTime.getTime()) / MILLIS_PER_SECOND)
                / SECONDS_PER_PIXEL[gridManager.getWidthIncrement()];
        return retVal;
    }

    public TimeRange dateToHour(Date date) {
        long l = (date.getTime() / MILLIS_PER_HOUR) * MILLIS_PER_HOUR;
        return new TimeRange(l, l + MILLIS_PER_HOUR);
    }

    public Date pixelToDate(int pixel) {
        long duration = pixelsToDuration(pixel);
        return new Date(getVisibleTimeRange().getStart().getTime() + duration);
    }

    public TimeRange pixelToHour(int pixel) {
        return dateToHour(pixelToDate(pixel));
    }

    public int durationToPixels(long duration) {
        return (int) (duration / MILLIS_PER_SECOND / SECONDS_PER_PIXEL[gridManager
                .getWidthIncrement()]);
    }

    public long pixelsToDuration(int pixels) {
        return (long) pixels
                * SECONDS_PER_PIXEL[gridManager.getWidthIncrement()]
                * MILLIS_PER_SECOND;
    }

    /**
     * Convert a range of selected pixels to a selected TimeRange. Start and end
     * will be rounded to the next hour.
     * 
     * @param x0
     * @param x1
     * @return
     */
    public TimeRange pixelsToSelection(int x0, int x1) {
        return pixelToHour(x0).span(pixelToHour(x1));
    }

    /**
     * Turns the given time range into a pixel range.
     * 
     * @param tr
     *            The range of time to calculate the pixel range.
     * @return The pixels representing the time range.
     */
    public Rectangle timeRangeToPixels(TimeRange tr) {
        int x0 = 0;
        int x1 = 0;

        if ((tr != null) && tr.isValid()) {
            x0 = dateToPixel(tr.getStart());
            x1 = dateToPixel(tr.getEnd());
        }
        return new Rectangle(Math.min(x0, x1), 0, Math.abs(x1 - x0),
                Integer.MAX_VALUE);
    }

    public void paintBackground(PaintEvent event, Rectangle bounds) {
        event.gc.setBackground(EditorBackground_color);
        event.gc.fillRectangle(bounds);

        // if (showTimeScaleLines) {
        // paintTimeScaleLines(event, bounds);
        // }
    }

    public void paintSelectionTimeRange(PaintEvent event, Rectangle selection) {
        event.gc.setBackgroundPattern(Selected_pattern);
        event.gc.fillRectangle(selection);
        event.gc.setBackgroundPattern(null);
    }

    /**
     * Paints the blue lines
     * 
     * @param gc
     * @param bounds
     */
    public void paintTimeScaleLines(PaintEvent event, Rectangle bounds) {
        if (showTimeScaleLines) {
            TimeRange timeRange = getVisibleTimeRange();
            Date startTime = timeRange.getStart();
            Date stopTime = timeRange.getEnd();

            Calendar currentTickCal = Calendar.getInstance(TimeZone
                    .getTimeZone("GMT"));
            currentTickCal.setTime(startTime);
            currentTickCal.set(Calendar.MINUTE, 0);
            currentTickCal.set(Calendar.SECOND, 0);
            currentTickCal.set(Calendar.MILLISECOND, 0);
            currentTickCal.set(Calendar.HOUR_OF_DAY, 0);
            Date currentTickDate = currentTickCal.getTime();

            GC gc = event.gc;
            gc.setLineWidth(0);
            gc.setForeground(TimeScaleLines_color);
            int y = bounds.y + bounds.height - 1;
            while (currentTickDate.before(stopTime)) {
                int x = dateToPixel(currentTickDate);

                int hour = currentTickCal.get(Calendar.HOUR_OF_DAY);

                if (hour != 0) {
                    gc.setLineStyle(TimeScaleLines_pattern);
                } else {
                    gc.setLineStyle(SWT.LINE_SOLID);
                }
                gc.drawLine(x, bounds.y, x, y);

                currentTickCal.add(Calendar.HOUR_OF_DAY, getHourIncrement());
                currentTickDate = currentTickCal.getTime();
            }
        }
    }

    public int getHourIncrement() {
        return HOUR_INCREMENT[gridManager.getWidthIncrement()];
    }

    /**
     * Paints the selected line in yellow from the time bar through all the
     * GridBars.
     * 
     * @param gc
     */
    public void paintSelected(PaintEvent event, Rectangle rect) {
        GC gc = event.gc;
        gc.setForeground(EditorTimeLine_color);
        gc.setLineStyle(EditorTimeLine_pattern);
        gc.setLineWidth(EditorTimeLine_width);
        int x = dateToPixel(gridManager.getSelectedTime());
        gc.drawLine(x, rect.y, x, rect.y + rect.height);
    }

    /**
     * Returns the visible time range. Allows for different implementations of
     * visible time range.
     * 
     * @return
     */
    protected TimeRange getVisibleTimeRange() {
        return gridManager.getVisibleTimeRange();
    }
}
