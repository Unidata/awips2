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
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Pattern;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.drawables.FillPatterns;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.PreferenceInitializer;
import com.raytheon.viz.gfe.rsc.GFEFonts;
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

    private static final int QUANTUM = TimeUtil.SECONDS_PER_HOUR;

    private static final int MAX_PIXELS_PER_QUANTUM = 450;

    private static final int MIN_SCALING = QUANTUM / MAX_PIXELS_PER_QUANTUM;

    private static final double TIME_SCALING_FACTOR = 0.8;

    private static Pattern Selected_pattern;

    private static Color TimeScaleLines_color;

    private static int TimeScaleLines_pattern;

    private static boolean showTimeScaleLines;

    private static Color EditorTimeLine_color;

    private static int EditorTimeLine_width;

    private static int EditorTimeLine_pattern;

    private static Color EditorBackground_color;

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

    private int secondsPerPixel;

    private int maxScaling;

    private int hourIncrement;

    private int hourLabelWidth;

    /**
     * Constructor
     * 
     * @param gridManager
     */
    public GridManagerUtil(GridManager gridManager) {
        this.gridManager = gridManager;

        // compute minimumBlockWidth based on TimeBlockSource_font width
        Display display = Display.getCurrent();
        GC gc = new GC(display);
        Font font = GFEFonts.makeGFEFont(display, "TimeBlockSource_font",
                SWT.NORMAL, 1);
        gc.setFont(font);
        int minBlockWidth = (gc.textExtent("W").x + (GridBar.DATA_BLOCK_HORIZONTAL_MARGIN * 2)) - 1;
        font.dispose();

        int fontNum = Math.max(GFEFonts.getFontNum("TimeScale_font", 2) - 1, 0);
        font = GFEFonts.getFont(display, Math.max(fontNum - 1, 0));
        hourLabelWidth = gc.textExtent("00").x;
        gc.dispose();

        // compute initial time scaling for minimumBlockWidth
        maxScaling = adjustScaling(QUANTUM / minBlockWidth, -1);
        secondsPerPixel = maxScaling;

        computeHourIncrement();
    }

    /**
     * Returns the pixel offset that represents the Date's value.
     * 
     * @param date
     * @return pixel offset of date
     */
    public int dateToPixel(Date date) {
        Date startTime = getVisibleTimeRange().getStart();

        int retVal = (int) ((date.getTime() - startTime.getTime()) / TimeUtil.MILLIS_PER_SECOND)
                / secondsPerPixel;
        return retVal;
    }

    /**
     * Determine hour containing date
     * 
     * @param date
     * @return the one hour time range containing date
     */
    public TimeRange dateToHour(Date date) {
        long l = (date.getTime() / TimeUtil.MILLIS_PER_HOUR)
                * TimeUtil.MILLIS_PER_HOUR;
        return new TimeRange(l, l + TimeUtil.MILLIS_PER_HOUR);
    }

    /**
     * Compute date/time corresponding to time scale pixel offset
     * 
     * @param pixel
     *            pixel offset
     * @return date corresponding to pixel offset
     */
    public Date pixelToDate(int pixel) {
        long duration = pixelsToDuration(pixel);
        return new Date(getVisibleTimeRange().getStart().getTime() + duration);
    }

    /**
     * Determine hour containing pixel offset
     * 
     * @param pixel
     *            pixel offset
     * @return one hour time range containing pixel offset
     */
    public TimeRange pixelToHour(int pixel) {
        return dateToHour(pixelToDate(pixel));
    }

    /**
     * Convert duration in seconds to width in pixels
     * 
     * @param duration
     *            in seconds
     * @return width in pixels
     */
    public int durationToPixels(long duration) {
        return (int) (duration / TimeUtil.MILLIS_PER_SECOND / secondsPerPixel);
    }

    /**
     * Convert width in pixels to duration in seconds
     * 
     * @param pixels
     *            width in pixels
     * @return duration in seconds
     */
    public long pixelsToDuration(int pixels) {
        return (long) pixels * secondsPerPixel * TimeUtil.MILLIS_PER_SECOND;
    }

    /**
     * Convert a range of selected pixels to a selected TimeRange. Start and end
     * will be rounded to the next hour.
     * 
     * @param x0
     *            start of pixel range
     * @param x1
     *            end of pixel range
     * @return TimeRange corresponding to pixel range
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

    /**
     * Paint the background of a grid bar
     * 
     * @param event
     * @param bounds
     */
    public void paintBackground(PaintEvent event, Rectangle bounds) {
        event.gc.setBackground(EditorBackground_color);
        event.gc.fillRectangle(bounds);
    }

    /**
     * Paint the selected time range of a grid bar
     * 
     * @param event
     * @param selection
     */
    public void paintSelectionTimeRange(PaintEvent event, Rectangle selection) {
        event.gc.setBackgroundPattern(Selected_pattern);
        event.gc.fillRectangle(selection);
        event.gc.setBackgroundPattern(null);
    }

    /**
     * Paints the blue lines
     * 
     * @param event
     *            PaintEvent
     * @param bounds
     *            area needing repainting
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
            int y = (bounds.y + bounds.height) - 1;
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

    /**
     * Get number of hours between hour labels
     * 
     * @return hour increment
     */
    public int getHourIncrement() {
        return hourIncrement;
    }

    /**
     * Paints the selected line in yellow from the time bar through all the
     * GridBars.
     * 
     * @param event
     *            PaintEvent
     * @param rect
     *            area needing repainting
     * 
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

    private int adjustScaling(int scaling, int increment) {
        while ((QUANTUM % scaling) != 0) {
            scaling += increment;
        }

        return scaling;
    }

    /**
     * Expand the Grid Manager time scale
     * 
     * @return true if time scale expanded, false if scaling limit would be
     *         exceeded.
     */
    public boolean expandTimeScale() {
        int currentScaling = secondsPerPixel;
        int newScaling = (int) (currentScaling * TIME_SCALING_FACTOR);

        newScaling = adjustScaling(newScaling, -1);
        newScaling = Math.max(newScaling, MIN_SCALING);

        secondsPerPixel = newScaling;

        if (newScaling != currentScaling) {
            computeHourIncrement();
            return true;
        }
        return false;
    }

    /**
     * Contract the Grid Manager time scale
     * 
     * @return true if time scale expanded, false if scaling limit would be
     *         exceeded.
     */
    public boolean contractTimeScale() {
        int currentScaling = secondsPerPixel;
        int newScaling = (int) (currentScaling / TIME_SCALING_FACTOR);

        // Need to insure that there will be an increase
        if (newScaling == currentScaling) {
            newScaling = currentScaling + 1;
        }

        newScaling = adjustScaling(newScaling, 1);
        newScaling = Math.min(newScaling, maxScaling);

        secondsPerPixel = newScaling;

        if (newScaling != currentScaling) {
            computeHourIncrement();
            return true;
        }
        return false;
    }

    private void computeHourIncrement() {
        int pixelsPerHour = TimeUtil.SECONDS_PER_HOUR / secondsPerPixel;

        int h = 12;
        while ((h > 2) && ((h * pixelsPerHour) > (hourLabelWidth * 7))) {
            h /= 2;
        }
        hourIncrement = h;
    }
}
