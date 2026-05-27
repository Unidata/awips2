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

import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Pattern;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.drawables.FillPatterns;
import com.raytheon.viz.gfe.GFEPreference;
import com.raytheon.viz.gfe.rsc.GFELinePatterns;

/**
 * GridBar preference container
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Jan 23, 2018  7153     randerso  Initial creation
 *
 * </pre>
 *
 * @author randerso
 */

public class GridBarPreferences {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(GridBarPreferences.class);

    private static class TimeBasedColors {
        private static final Date BASE_DATE = new Date(0);

        private static final Color DEFAULT_COLOR = new Color(
                Display.getDefault(), RGBColors.getRGBColor("gray75"));

        private int[] minutes;

        private Color[] colors;

        /**
         * @param string
         */
        private TimeBasedColors(String modifier) {
            minutes = GFEPreference.getIntArray(modifier + "_minutes");
            String colorStrings[] = GFEPreference
                    .getStringArray(modifier + "_colors");

            if (minutes.length != colorStrings.length) {
                String msg = String.format(
                        "%1$s_minutes and %1$s_colors not same size", modifier);
                statusHandler.error(msg);

                minutes = new int[] { Integer.MAX_VALUE };
                colors = new Color[] { new Color(Display.getDefault(),
                        RGBColors.getRGBColor("Black")) };
            }

            colors = new Color[colorStrings.length];
            int i = 0;
            for (String s : colorStrings) {
                colors[i] = new Color(null, RGBColors.getRGBColor(s));
                i++;
            }

        }

        public void dispose() {
            for (Color c : colors) {
                c.dispose();
            }
        }

        public Color getColor(Date time) {
            // calculate the time, and the thresholds, determine the color.
            // only look at the "lateTime"
            Color color = DEFAULT_COLOR;
            if (!time.equals(BASE_DATE)) {
                long ago = (SimulatedTime.getSystemTime().getTime().getTime()
                        - time.getTime()) / TimeUtil.MILLIS_PER_MINUTE;
                for (int i = 0; i < minutes.length; i++) {
                    if (ago < minutes[i]) {
                        color = colors[i];
                        break;
                    }
                }
                // if fell off the end, then use the very last color
                if ((color == DEFAULT_COLOR) && (colors.length > 0)) {
                    color = colors[colors.length - 1];
                }
            }

            return color;
        }
    }

    private Pattern lockedByMe;

    private Pattern lockedByOther;

    private Color timeBlockVisibleColor;

    private Color timeBlockActiveColor;

    private Color timeBlockInvisibleColor;

    private boolean showEditorTimeLines;

    private boolean showSplitBoundaries;

    private Pattern selectedPattern;

    private Color timeScaleLinesColor;

    private int timeScaleLinesPattern;

    private boolean showTimeScaleLines;

    private Color editorTimeLineColor;

    private int editorTimeLineWidth;

    private int editorTimeLinePattern;

    private Color editorBackgroundColor;

    private Map<String, Color> historyColors = new HashMap<>();

    private Map<GridMode, TimeBasedColors> timeBasedColors = new HashMap<>();

    private Map<RGB, Pattern> modifiedByMe = new HashMap<>();

    private Map<RGB, Pattern> modifiedByOther = new HashMap<>();

    /**
     * Constructor
     */
    public GridBarPreferences() {
        String color;
        String pattern;

        color = GFEPreference.getString("LockedByMe_color", "forestgreen");
        pattern = GFEPreference.getString("LockedByMe_pattern", "WHOLE");
        if (lockedByMe != null) {
            lockedByMe.dispose();
        }
        lockedByMe = FillPatterns.getSWTPattern(RGBColors.getRGBColor(color),
                pattern);

        color = GFEPreference.getString("LockedByOther_color", "tomato2");
        pattern = GFEPreference.getString("LockedByOther_pattern", "WHOLE");
        if (lockedByOther != null) {
            lockedByOther.dispose();
        }
        lockedByOther = FillPatterns.getSWTPattern(RGBColors.getRGBColor(color),
                pattern);

        for (RGB key : modifiedByMe.keySet()) {
            Pattern pat = modifiedByMe.remove(key);
            pat.dispose();
        }

        for (RGB key : modifiedByOther.keySet()) {
            Pattern pat = modifiedByOther.remove(key);
            pat.dispose();
        }

        color = GFEPreference.getString("TimeBlockVisible_color", "White");
        if (timeBlockVisibleColor != null) {
            timeBlockVisibleColor.dispose();
        }
        timeBlockVisibleColor = new Color(Display.getDefault(),
                RGBColors.getRGBColor(color));

        color = GFEPreference.getString("TimeBlockActive_color", "Yellow");
        if (timeBlockActiveColor != null) {
            timeBlockActiveColor.dispose();
        }
        timeBlockActiveColor = new Color(Display.getDefault(),
                RGBColors.getRGBColor(color));

        color = GFEPreference.getString("TimeBlockInvisible_color", "Gray50");
        if (timeBlockInvisibleColor != null) {
            timeBlockInvisibleColor.dispose();
        }
        timeBlockInvisibleColor = new Color(Display.getDefault(),
                RGBColors.getRGBColor(color));

        showEditorTimeLines = GFEPreference.getBoolean("EditorTimeLines", true);

        showSplitBoundaries = GFEPreference.getBoolean("SplitBoundaryDisplay",
                true);

        color = GFEPreference.getString("Selected_color", "LightSkyBlue");
        pattern = GFEPreference.getString("Selected_fillPattern",
                "TRANS_25PC_45DEG");
        if (selectedPattern != null) {
            selectedPattern.dispose();
        }
        selectedPattern = FillPatterns
                .getSWTPattern(RGBColors.getRGBColor(color), pattern);

        color = GFEPreference.getString("TimeScaleLines_color", "Blue");
        if (timeScaleLinesColor != null) {
            timeScaleLinesColor.dispose();
        }
        timeScaleLinesColor = new Color(Display.getDefault(),
                RGBColors.getRGBColor(color));

        pattern = GFEPreference.getString("TimeScaleLines_pattern", "DOTTED");
        timeScaleLinesPattern = GFELinePatterns.getSWTPattern(pattern);

        showTimeScaleLines = GFEPreference.getBoolean("TimeScaleLines", true);

        color = GFEPreference.getString("EditorTimeLine_color", "Yellow");
        if (editorTimeLineColor != null) {
            editorTimeLineColor.dispose();
        }
        editorTimeLineColor = new Color(Display.getDefault(),
                RGBColors.getRGBColor(color));

        editorTimeLineWidth = GFEPreference.getInt("EditorTimeLine_width", 2);

        pattern = GFEPreference.getString("EditorTimeLine_pattern", "DOTTED");
        editorTimeLinePattern = GFELinePatterns.getSWTPattern(pattern);

        color = GFEPreference.getString("bgColor", "Black");
        if (editorBackgroundColor != null) {
            editorBackgroundColor.dispose();
        }
        editorBackgroundColor = new Color(Display.getDefault(),
                RGBColors.getRGBColor(color));

    }

    /**
     * dispose all preference resources
     */
    public void dispose() {
        editorBackgroundColor.dispose();
        editorTimeLineColor.dispose();
        lockedByMe.dispose();
        lockedByOther.dispose();
        selectedPattern.dispose();
        timeBlockVisibleColor.dispose();
        timeBlockActiveColor.dispose();
        timeBlockInvisibleColor.dispose();
        timeScaleLinesColor.dispose();

        for (Color c : historyColors.values()) {
            c.dispose();
        }
        historyColors.clear();

        for (TimeBasedColors tbc : timeBasedColors.values()) {
            tbc.dispose();
        }
        timeBasedColors.clear();

        for (Pattern p : modifiedByMe.values()) {
            p.dispose();
        }
        modifiedByMe.clear();

        for (Pattern p : modifiedByOther.values()) {
            p.dispose();
        }
        modifiedByOther.clear();
    }

    /**
     * @return the lockedByMe pattern
     */
    public Pattern getLockedByMe() {
        return lockedByMe;
    }

    /**
     * @return the lockedByOther pattern
     */
    public Pattern getLockedByOther() {
        return lockedByOther;
    }

    /**
     * @param color
     * @return the modifiedByMe pattern
     */
    public Pattern getModifiedByMe(Color color) {
        String patt = GFEPreference.getString("HistoryUserModPattern_Me");

        Pattern fp = null;
        if (!patt.isEmpty()) {
            fp = modifiedByMe.get(color.getRGB());
            if (fp == null) {
                fp = FillPatterns.getSWTPattern(color.getRGB(), patt);
                modifiedByMe.put(color.getRGB(), fp);
            }
        }
        return fp;
    }

    /**
     * @param color
     * @return the modifiedByOther pattern
     */
    public Pattern getModifiedByOther(Color color) {
        String patt = GFEPreference.getString("HistoryUserModPattern_Other");
        Pattern fp = null;
        if (!patt.isEmpty()) {
            fp = modifiedByOther.get(color.getRGB());
            if (fp == null) {
                fp = FillPatterns.getSWTPattern(color.getRGB(), patt);
                modifiedByOther.put(color.getRGB(), fp);
            }
        }
        return fp;
    }

    /**
     * @return the timeBlockVisibleColor
     */
    public Color getTimeBlockVisibleColor() {
        return timeBlockVisibleColor;
    }

    /**
     * @return the timeBlockActiveColor
     */
    public Color getTimeBlockActiveColor() {
        return timeBlockActiveColor;
    }

    /**
     * @return the timeBlockInvisibleColor
     */
    public Color getTimeBlockInvisibleColor() {
        return timeBlockInvisibleColor;
    }

    /**
     * @return showEditorTimeLines
     */
    public boolean isShowEditorTimeLines() {
        return showEditorTimeLines;
    }

    /**
     * @return showSplitBoundaries
     */
    public boolean isShowSplitBoundaries() {
        return showSplitBoundaries;
    }

    /**
     * @return the selected time pattern
     */
    public Pattern getSelectedPattern() {
        return selectedPattern;
    }

    /**
     * @return the timeScaleLinesColr
     */
    public Color getTimeScaleLinesColor() {
        return timeScaleLinesColor;
    }

    /**
     * @return the timeScaleLinesPattern
     */
    public int getTimeScaleLinesPattern() {
        return timeScaleLinesPattern;
    }

    /**
     * @return showTimeScaleLines
     */
    public boolean isShowTimeScaleLines() {
        return showTimeScaleLines;
    }

    /**
     * @return the editorTimeLineColor
     */
    public Color getEditorTimeLineColor() {
        return editorTimeLineColor;
    }

    /**
     * @return the editorTimeLineWidth
     */
    public int getEditorTimeLineWidth() {
        return editorTimeLineWidth;
    }

    /**
     * @return the editorTimeLinePattern
     */
    public int getEditorTimeLinePattern() {
        return editorTimeLinePattern;
    }

    /**
     * @return the editorBackgroundColor
     */
    public Color getEditorBackgroundColor() {
        return editorBackgroundColor;
    }

    /**
     * @param id
     *            preference id to retrieve
     * @return the historyColor
     */
    public Color getHistoryColor(String id) {
        Color color = historyColors.get(id);
        if (color == null) {
            String colorString = GFEPreference.getString(id);
            if (!colorString.isEmpty()) {
                RGB rgbColor = RGBColors.getRGBColor(colorString);
                color = new Color(Display.getCurrent(), rgbColor);
                historyColors.put(id, color);
            }
        }

        return color;
    }

    /**
     * Retrieve the appropriate color based on how long since last modified,
     * saved, sent, published...
     *
     * @param mode
     *            the grid mode
     * @param time
     *            time last saved, sent, ...
     * @return the time based color
     */
    public Color getTimeBasedColor(GridMode mode, Date time) {
        TimeBasedColors tbc = timeBasedColors.get(mode);
        if (tbc == null) {
            tbc = new TimeBasedColors(mode.toString());
            timeBasedColors.put(mode, tbc);
        }

        return tbc.getColor(time);
    }

}
