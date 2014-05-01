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

package com.raytheon.viz.core.graphing.axis;

import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Calendar;
import java.util.HashMap;

import javax.measure.unit.Unit;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.style.graph.AxisScale;
import com.raytheon.uf.common.style.graph.GraphPreferences;
import com.raytheon.uf.common.style.level.SingleLevel;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.ColorUtil;
import com.raytheon.viz.core.graphing.DataAxisInfo;
import com.raytheon.viz.core.graphing.GraphUtil;
import com.raytheon.viz.core.graphing.util.GraphPrefsFactory;
import com.raytheon.viz.core.graphing.util.GraphUtilPorted;
import com.raytheon.viz.core.slice.request.HeightScale;
import com.raytheon.viz.core.slice.request.HeightScale.ScaleType;

/**
 * Factory methods for generating axes.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 4, 2007             njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 */
public class AxisFactory {

    protected static final SimpleDateFormat TIME_FORMAT = new SimpleDateFormat(
            "HH:mm'Z'");

    protected static final SimpleDateFormat DAY_OF_WEEK_FORMAT = new SimpleDateFormat(
            "EEE");

    protected static final SimpleDateFormat DATE_FORMAT = new SimpleDateFormat(
            "ddMMMyy");

    protected static final SimpleDateFormat REF_TIME_FORMAT = new SimpleDateFormat(
            "dd.HH");

    protected static final SimpleDateFormat FCST_TIME_FORMAT = new SimpleDateFormat(
            "HH'Z' EEE");

    // never really figured out what absMin should be from the
    // ported code
    private static float ABS_MIN = Float.NEGATIVE_INFINITY;

    private AxisFactory() {

    }

    /**
     * Builds a number axis from style rules
     * 
     * @param parameter
     *            the parameter of the data
     * @param level
     *            the level of the parameter
     * @param orientation
     *            the orientation of the axis
     * @param dataMinValue
     *            the minimum value of the data to be graphed
     * @param dataMaxValue
     *            the maximum value of the data to be graphed
     * @param zeroYes
     *            if the data has a point at zero
     * @return
     * @throws VizException
     */
    public static NumberAxis buildNumberAxis(GraphPreferences preferences,
            IAxis.Orientation orientation, double dataMinValue,
            double dataMaxValue, boolean zeroYes, int numberOfGraphs,
            Unit<?> fallbackUnits) throws VizException {
        NumberAxis axis = null;
        String units = "";
        if (preferences.getDisplayUnits() != null) {
            units = preferences.getDisplayUnits().toString();
        } else if (fallbackUnits != null) {
            units = fallbackUnits.toString();
        }

        DataAxisInfo info = GraphUtilPorted.initDataAxisInfo(
                (float) dataMinValue, zeroYes, ABS_MIN, (float) dataMaxValue,
                units, preferences);

        AxisLabeling labeling = AxisUtil.makeYaxis(info, numberOfGraphs, false);

        axis = new NumberAxis(orientation, units);
        axis.setLabeling(labeling);
        axis.setRange((double) info.getDivMin(), (double) info.getDivMax());
        if (preferences.getAxisScale().getScaleType() == AxisScale.Type.LOG) {
            axis.setLogarithmic(true);
        }
        axis.setInfo(info);

        return axis;
    }

    /**
     * Builds a linear number axis
     * 
     * @param orientation
     *            the orientation of the axis
     * @param min
     *            the min value of the axis
     * @param max
     *            the max value of the axis
     * @param units
     *            the units of the axis
     * @return the initialized number axis
     */
    public static NumberAxis buildNumberAxis(IAxis.Orientation orientation,
            double min, double max, String units) {
        NumberAxis axis = new NumberAxis(orientation, units);
        axis.setRange(min, max);

        return axis;
    }

    public static boolean axisIsCompatible(DataAxisInfo firstInfo,
            boolean dataAtZero, String units, DataAxisInfo secondInfo) {
        boolean compatible = false;
        if (firstInfo == null && secondInfo == null) {
            // primarily axes that hold images (e.g. wind barbs)
            compatible = true;
        } else if (firstInfo == null || secondInfo == null) {
            compatible = false;
        } else {
            compatible = GraphUtilPorted.verifyDataAxisInfo(
                    firstInfo.getDataMin(), dataAtZero, ABS_MIN,
                    firstInfo.getDataMax(), units, firstInfo.getStyle(),
                    secondInfo, true);
        }

        return compatible;
    }

    /**
     * Builds a date axis
     * 
     * @param orientation
     *            the orientation
     * @param times
     *            the times to label
     * @param minTime
     *            the start time in milliseconds
     * @param maxTime
     *            the end time in milliseconds
     * @return the date axis
     */
    public static DateAxis buildDateAxis(IAxis.Orientation orientation,
            Object[] times, double minTime, double maxTime) {
        DateAxis axis = new DateAxis(orientation);
        AxisLabeling labeling = new AxisLabeling();
        HashMap<Double, String> labels = new HashMap<Double, String>();
        if (times[0] instanceof Calendar) {
            for (Object time : times) {
                Calendar cTime = (Calendar) time;
                double millis = cTime.getTimeInMillis();
                String label = TIME_FORMAT.format(cTime.getTime());
                if (millis == minTime || millis == maxTime) {
                    label += DateAxis.LABEL_LINEBREAK
                            + DAY_OF_WEEK_FORMAT.format(cTime.getTime());
                    label += DateAxis.LABEL_LINEBREAK
                            + DATE_FORMAT.format(cTime.getTime());
                }
                labels.put(millis, label);
            }
        } else if (times[0] instanceof DataTime) {
            for (Object time : times) {
                DataTime dTime = (DataTime) time;
                double millis = GraphUtil.getNumberRepresentation(dTime);
                String label = REF_TIME_FORMAT.format(dTime.getRefTime()
                        .getTime());
                label += DateAxis.LABEL_LINEBREAK
                        + (dTime.getFcstTime() / 3600) + "HR";
                label += DateAxis.LABEL_LINEBREAK
                        + FCST_TIME_FORMAT.format(dTime.getValidTime()
                                .getTime());
                if (millis == minTime || millis == maxTime) {
                    label += DateAxis.LABEL_LINEBREAK
                            + DATE_FORMAT
                                    .format(dTime.getValidTime().getTime());
                }
                labels.put(millis, label);
            }
        }
        labeling.setLabels(labels);
        axis.setLabeling(labeling);
        axis.setRange(minTime, maxTime);
        return axis;
    }

    public static NumberAxis buildVerticalHeightAxis(RGB color,
            SingleLevel[] levels, HeightScale scale) {
        if (color == null) {
            color = ColorUtil.DEFAULT_ITEM_COLOR;
        }

        // TODO support more level types
        NumberAxis rangeAxis = new NumberAxis(IAxis.Orientation.VERTICAL, "mb");
        rangeAxis.addTitle("Millibars", color);
        rangeAxis.setShowTitle(true);
        Arrays.sort(levels);
        double minLevel = levels[0].getValue();
        double maxLevel = levels[levels.length - 1].getValue();

        rangeAxis.setRange(minLevel, maxLevel);

        // TODO should be configurable
        GraphPreferences style = GraphPrefsFactory.buildSimplePreferences(
                scale.getScale() == ScaleType.LOG, minLevel, maxLevel);
        style.getAxisScale().setMaxValue(maxLevel);
        style.getAxisScale().setMinValue(minLevel);
        DataAxisInfo info = GraphUtilPorted.initDataAxisInfo((float) minLevel,
                false, (float) minLevel, (float) maxLevel, "mb", style);
        rangeAxis.setInfo(info);
        rangeAxis.setLogarithmic(scale.getScale() == ScaleType.LOG);
        rangeAxis.setLabeling(generateVerticalLabels(levels, scale));
        rangeAxis.setDrawLinesAtLabels(true);
        rangeAxis.setLabelLineStyle(IGraphicsTarget.LineStyle.SOLID);

        return rangeAxis;
    }

    private static AxisLabeling generateVerticalLabels(SingleLevel[] levels,
            HeightScale scale) {
        AxisLabeling labeling = new AxisLabeling();
        HashMap<Double, String> labelMap = new HashMap<Double, String>();
        if (levels.length < 11) {
            for (SingleLevel level : levels) {
                double val = level.getValue();
                labelMap.put(val, Double.toString(val));
            }
        } else {
            if (scale.getScale() == ScaleType.LOG && levels.length > 20) {
                for (int i = 0; i < levels.length / 2; i += 4) {
                    double val = levels[i].getValue();
                    labelMap.put(val, Double.toString(val));
                }
                for (int i = levels.length / 2; i < levels.length; i += 2) {
                    double val = levels[i].getValue();
                    labelMap.put(val, Double.toString(val));
                }
                double val = levels[levels.length - 1].getValue();
                labelMap.put(val, Double.toString(val));
            } else {
                for (int i = 0; i < levels.length; i += 2) {
                    double val = levels[i].getValue();
                    labelMap.put(val, Double.toString(val));
                }
            }
        }
        labeling.setLabels(labelMap);

        return labeling;
    }

}
