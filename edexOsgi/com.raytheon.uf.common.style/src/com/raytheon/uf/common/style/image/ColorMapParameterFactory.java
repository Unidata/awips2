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
package com.raytheon.uf.common.style.image;

import java.util.ArrayList;
import java.util.List;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.Unit;

import org.apache.commons.lang.ArrayUtils;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.colormap.prefs.DataMappingPreferences;
import com.raytheon.uf.common.colormap.prefs.DataMappingPreferences.DataMappingEntry;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.style.LabelingPreferences;
import com.raytheon.uf.common.style.ParamLevelMatchCriteria;
import com.raytheon.uf.common.style.StyleException;
import com.raytheon.uf.common.style.StyleManager;
import com.raytheon.uf.common.style.StyleRule;
import com.raytheon.uf.common.style.image.DataScale.Type;
import com.raytheon.uf.common.style.level.Level;
import com.raytheon.uf.common.style.level.RangeLevel;
import com.raytheon.uf.common.style.level.SingleLevel;
import com.raytheon.uf.common.util.GridUtil;

/**
 * ColorMapParameterFactory
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Jul 25, 2007             chammack    Initial Creation.
 *    Mar 26, 2009 2086        jsanchez    Added a entityList to the match criteria.
 *    Feb 15, 2013 1638        mschenke    Moved GRID_FILL_VALUE from edex.common Util into GridUtil
 *    Jun 24, 2013 2122        mschenke    Added method for constructing {@link ColorMapParameters} from {@link StyleRule}
 *    Sep 24, 2013 2404        bclement    moved to common.style from viz.core, added build method that takes ParamLevelMatchCriteria, removed unused methods
 *    Nov 13, 2013 2492        mschenke    Create build that does not take data for adaptive building
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class ColorMapParameterFactory {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ColorMapParameterFactory.class);

    private ColorMapParameterFactory() {

    }

    public static ColorMapParameters build(Object data, String parameter,
            Unit<?> parameterUnits, SingleLevel level, String entity)
            throws StyleException {

        // StyleRule sr = StyleLoader.getInstance()
        // .getStyleRule(parameter, levels);
        ParamLevelMatchCriteria match = new ParamLevelMatchCriteria();
        match.setLevel(level);
        ArrayList<String> paramList = new ArrayList<String>();
        paramList.add(parameter);
        match.setParameterName(paramList);
        if (entity != null) {
            ArrayList<String> entityList = new ArrayList<String>();
            entityList.add(entity);
            match.setCreatingEntityNames(entityList);
        }
        StyleRule sr = StyleManager.getInstance().getStyleRule(
                StyleManager.StyleType.IMAGERY, match);

        return build(sr, data, level, parameterUnits);
    }

    /**
     * Find style rules for match and construct colormap parameters for data
     * 
     * @param data
     *            primitive number array
     * @param parameterUnits
     * @param match
     * @return
     * @throws StyleException
     */
    public static ColorMapParameters build(Object data, Unit<?> parameterUnits,
            ParamLevelMatchCriteria match) throws StyleException {
        StyleRule sr = StyleManager.getInstance().getStyleRule(
                StyleManager.StyleType.IMAGERY, match);
        SingleLevel level = null;
        if (match.getLevels() != null && !match.getLevels().isEmpty()) {
            Level styleLevel = match.getLevels().get(0);
            if (styleLevel instanceof SingleLevel) {
                level = (SingleLevel) styleLevel;
            }
        }
        return build(sr, data, level, parameterUnits);
    }

    public static ColorMapParameters build(StyleRule sr, Object data,
            SingleLevel level, Unit<?> parameterUnits) {

        ColorMapParameters params = new ColorMapParameters();

        ImagePreferences preferences = null;
        if (sr != null) {
            preferences = (ImagePreferences) sr.getPreferences();
        } else {
            preferences = new ImagePreferences();
        }

        // If a record to convert units exists, use it
        Unit<?> colorMapUnit = null;
        try {
            colorMapUnit = preferences.getColorMapUnitsObject();
        } catch (StyleException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
        if (colorMapUnit == null) {
            params.setColorMapUnit(parameterUnits);
        } else {
            params.setColorMapUnit(colorMapUnit);
        }
        params.setDisplayUnit(preferences.getDisplayUnits());
        params.setDataMapping(preferences.getDataMapping());

        // If no data scale style rules exist, determine the range of the
        // data and add a 25% buffer to it.
        UnitConverter dataToDisplay = params.getDataToDisplayConverter();
        UnitConverter displayToData = params.getDisplayToDataConverter();

        DataScale scale = null;
        Double definedMin = null;
        Double definedMax = null;
        Type dataScaleType = Type.LINEAR;

        if (sr != null) {
            scale = preferences.getDataScale();
            if (scale != null) {
                dataScaleType = scale.getScaleType();
                definedMin = scale.getMinValue();
                definedMax = scale.getMaxValue();
            }
        }

        if ((definedMin == null || definedMax == null || preferences
                .getDataScale().isAdaptive())) {
            float max = 0, min = 0, colormapMin = 0, colormapMax = 0;
            if (data != null) {
                // Repack the data
                Number[] numArray = repackData(data);

                min = Float.POSITIVE_INFINITY;
                max = Float.NEGATIVE_INFINITY;
                for (Number num : numArray) {
                    if (num.floatValue() != GridUtil.GRID_FILL_VALUE
                            && !Float.isNaN(num.floatValue())) {
                        colormapMin = min = Math.min(min, num.floatValue());
                        colormapMax = max = Math.max(max, num.floatValue());
                    }
                }

                if (min == Float.POSITIVE_INFINITY) {
                    colormapMin = min = colormapMax = max = GridUtil.GRID_FILL_VALUE;
                }

                // Add 25% buffer (same strategy as D2D)
                double diff = 0;
                if (dataScaleType == Type.LINEAR) {
                    diff = (max - min) / 4.0;
                    if (definedMax != null) {
                        double definedDataMax = definedMax;
                        if (displayToData != null) {
                            definedDataMax = displayToData.convert(definedMax);
                        }
                        if (definedDataMax > max) {
                            max = (float) definedDataMax;
                        } else {
                            max += diff;
                        }
                    } else {
                        max += diff;
                    }
                    if (definedMin != null) {
                        double definedDataMin = definedMin;
                        if (displayToData != null) {
                            definedDataMin = displayToData.convert(definedMin);
                        }
                        if (definedDataMin < min) {
                            min = (float) definedDataMin;
                        } else {
                            min -= diff;
                        }
                    } else {
                        min -= diff;
                    }
                } else if (dataScaleType == Type.LOG) {
                    final double VERY_SMALL = 1e-30f;
                    // log scaling...redefine min and max to all positive
                    if (scale != null && scale.isMirror() && -min > max)
                        max = -min;
                    if (min < max / 1e4)
                        min = (float) (max / 1e4);
                    if (min < VERY_SMALL)
                        min = (float) VERY_SMALL;

                    double interval = Math.log(max / min) / 4.0;
                    min *= Math.exp(-interval);
                    max *= Math.exp(interval);

                    if (min < max / 1e4)
                        min = max / 1e4f;
                    if (min < VERY_SMALL)
                        min = (float) VERY_SMALL;
                }
                colormapMin = min;
                colormapMax = max;

                params.setColorMapMax(colormapMax);
                params.setColorMapMin(colormapMin);
                params.setDataMax(max);
                params.setDataMin(min);
            }

            float displayMin = dataToDisplay != null ? (float) dataToDisplay
                    .convert(colormapMin) : colormapMin;
            float displayMax = dataToDisplay != null ? (float) dataToDisplay
                    .convert(colormapMax) : colormapMax;

            if (preferences.getColorbarLabeling() != null) {
                extractLabelValues(preferences, displayMax, displayMin, params);
            } else if (preferences.getDataMapping() == null) {
                calculateLabelValues(preferences, displayMax, displayMin,
                        params);
                if (scale != null) {
                    params.setMirror(scale.isMirror());
                }
            }

            if (sr != null) {
                params.setColorMapName(preferences.getDefaultColormap());
            }
        } else {
            double displayMin = definedMin;
            double displayMax = definedMax;

            if (scale.getMinValue2() != null || scale.getMaxValue2() != null) {
                Double min2 = scale.getMinValue2();
                Double max2 = scale.getMaxValue2();
                if (min2 == null)
                    min2 = scale.getMinValue();
                if (max2 == null)
                    max2 = scale.getMaxValue();

                Type scaleType = preferences.getDataScale().getLevelScale();
                if (scaleType == Type.LINEAR || scaleType == Type.LOG) {
                    RangeLevel ruleLevel = null;
                    try {
                        ruleLevel = (RangeLevel) ((ParamLevelMatchCriteria) sr
                                .getMatchCriteria()).getLevels().get(0);
                    } catch (RuntimeException e) {
                        String message = "Style rule not defined correctly.  Requested min/max "
                                + "interpolation based on range of levels, but matching criteria does "
                                + "not specify a range.";
                        statusHandler.handle(Priority.PROBLEM, message, e);
                    }

                    if (ruleLevel != null) {
                        double[] vals = StyleManager.calculateMinMax(
                                level.getValue(), ruleLevel.getLowerValue(),
                                ruleLevel.getUpperValue(), scale.getMinValue(),
                                min2, scale.getMaxValue(), max2,
                                scaleType == Type.LOG);
                        displayMin = vals[0];
                        displayMax = vals[1];
                    }
                }
            }

            if (dataScaleType == Type.LINEAR) {
                if (scale != null && scale.isMirror()) {
                    if (-displayMin > displayMax)
                        displayMax = -displayMin;
                    else
                        displayMin = -displayMax;
                }
            } else if (dataScaleType == Type.LOG) {
                if (displayMin > displayMax) {
                    double t = displayMax;
                    displayMax = displayMin;
                    displayMin = t;
                }

                if (scale != null && scale.isMirror()
                        && -displayMin > displayMax)
                    displayMax = -displayMin;
                if (displayMin < displayMax / 1e4)
                    displayMin = displayMax / 1e4;
            }

            double dataMin;
            double dataMax;
            if (displayToData != null) {
                dataMin = displayToData.convert(displayMin);
                dataMax = displayToData.convert(displayMax);
                /*
                 * ColorMapParameters handles the case in which the conversion
                 * negates the values, making max < min
                 */
            } else {
                dataMin = displayMin;
                dataMax = displayMax;
            }

            params.setColorMapMax((float) dataMax);
            params.setDataMax((float) dataMax);
            params.setColorMapMin((float) dataMin);
            params.setDataMin((float) dataMin);

            extractLabelValues(preferences, (float) displayMax,
                    (float) displayMin, params);
            params.setColorMapName(preferences.getDefaultColormap());
        }
        if (preferences.getDataScale() != null) {
            if (dataScaleType == Type.LOG) {
                params.setLogarithmic(true);
            }
        }
        return params;

    }

    /**
     * Does the same thing as {@link #build(ImagePreferences, Unit)} but first
     * unpacks the ImagePreference from a {@link StyleRule}.
     * 
     * @param sr
     * @param defaultColorMapUnit
     * @return
     * @throws StyleException
     */
    public static ColorMapParameters build(StyleRule sr,
            Unit<?> defaultColorMapUnit) throws StyleException {
        if (sr == null) {
            throw new IllegalArgumentException(
                    "StyleRule must not be null when building ColorMapParameters");
        }
        if (sr.getPreferences() instanceof ImagePreferences == false) {
            throw new IllegalArgumentException(
                    "ImagePreferences are only supported for building ColorMapParameters");
        }
        return build((ImagePreferences) sr.getPreferences(),
                defaultColorMapUnit);
    }

    /**
     * Builds a {@link ColorMapParameters} given {@link ImagePreferences} and a
     * unit to colormap in. This method provides for the simplest color mapping
     * case. No data or level information is input so the
     * {@link ImagePreferences} must specify an exact range or data mapping and
     * also no lableing is added unless the colorbar labeling is specifically
     * set in the style rule.
     * 
     * @param preferences
     * @param defaultColorMapUnit
     * @return
     * @throws StyleException
     */
    public static ColorMapParameters build(ImagePreferences preferences,
            Unit<?> defaultColorMapUnit) throws StyleException {
        ColorMapParameters params = new ColorMapParameters();

        Unit<?> colorMapUnit = preferences.getColorMapUnitsObject();
        if (colorMapUnit == null) {
            colorMapUnit = defaultColorMapUnit;
        }

        Unit<?> displayUnit = preferences.getDisplayUnits();
        if (displayUnit == null) {
            displayUnit = colorMapUnit;
        } else if (colorMapUnit == null) {
            colorMapUnit = displayUnit;
        }

        params.setColorMapUnit(colorMapUnit);
        params.setDisplayUnit(displayUnit);
        params.setDataMapping(preferences.getDataMapping());
        params.setColorMapName(preferences.getDefaultColormap());

        Float displayMin = null, displayMax = null;
        DataScale scale = preferences.getDataScale();
        boolean mirrored = false;
        Type scaleType = Type.LINEAR;
        if (scale != null) {
            if (scale.getMinValue() != null) {
                displayMin = scale.getMinValue().floatValue();
            }
            if (scale.getMaxValue() != null) {
                displayMax = scale.getMaxValue().floatValue();
            }

            mirrored = scale.isMirror();
            scaleType = scale.getScaleType();

        }

        if (displayMin == null || displayMax == null) {
            // Could not find min/max in DataScale, attempt to get from
            // DataMappingPreferences if set
            if (params.getDataMapping() != null) {
                DataMappingPreferences mapping = params.getDataMapping();
                List<DataMappingEntry> entries = mapping.getEntries();
                if (entries != null && entries.isEmpty() == false) {
                    if (displayMin == null) {
                        DataMappingEntry min = entries.get(0);
                        if (min.getPixelValue() != null
                                && min.getDisplayValue() != null) {
                            displayMin = min.getDisplayValue().floatValue();
                        }
                    }
                    if (displayMax == null) {
                        DataMappingEntry max = entries.get(entries.size() - 1);
                        if (max.getPixelValue() != null
                                && max.getDisplayValue() != null) {
                            displayMax = max.getDisplayValue().floatValue();
                        }
                    }

                }
            } else if (mirrored && (displayMin != null || displayMax != null)) {
                // Mirror value set
                if (displayMin == null) {
                    displayMin = -displayMax;
                } else {
                    displayMax = -displayMin;
                }
            }
        }

        if (displayMin == null || displayMax == null) {
            throw new StyleException("Unable to determine colormap min/max.");
        }

        params.setMirror(mirrored);
        params.setLogarithmic(scaleType == Type.LOG);

        // Convert to colormap min/max
        float colorMapMin = displayMin;
        float colorMapMax = displayMax;

        UnitConverter displayToColorMap = params
                .getDisplayToColorMapConverter();
        if (displayToColorMap != null) {
            colorMapMin = (float) displayToColorMap.convert(displayMin);
            colorMapMax = (float) displayToColorMap.convert(displayMax);
        }

        params.setColorMapMin(colorMapMin);
        params.setColorMapMax(colorMapMax);

        LabelingPreferences labeling = preferences.getColorbarLabeling();
        if (labeling != null) {
            if (labeling.getValues() != null) {
                params.setColorBarIntervals(labeling.getValues());
            } else if (labeling.getIncrement() != 0) {
                float increment = labeling.getIncrement();
                float initialPoint = (float) (Math
                        .ceil(colorMapMin / increment) * increment);
                float finalPoint = (float) (Math.floor(colorMapMin / increment) * increment);
                int count = (int) ((finalPoint - initialPoint) / increment) + 1;
                float[] vals = new float[count];
                for (int i = 0; i < count; i += 1) {
                    vals[i] = initialPoint + increment * i;
                }
            }
        }

        return params;
    }

    private static Number[] repackData(Object data) {
        Number[] numArray = null;
        if (data instanceof byte[]) {
            byte[] byteData = (byte[]) data;
            numArray = new Number[byteData.length];
            for (int i = 0; i < byteData.length; i++) {
                numArray[i] = byteData[i];
            }
        } else if (data instanceof float[]) {
            float[] floatData = (float[]) data;
            numArray = new Number[floatData.length];
            for (int i = 0; i < floatData.length; i++) {
                numArray[i] = floatData[i];
            }
        } else if (data instanceof float[][]) {
            float[][] floatData = (float[][]) data;
            numArray = new Number[floatData.length * floatData[0].length];
            for (int i = 0; i < floatData.length; i++) {
                for (int j = 0; j < floatData[0].length; ++j) {
                    numArray[i + j * floatData.length] = floatData[i][j];
                }
            }
        } else if (data instanceof int[]) {
            int[] intData = (int[]) data;
            numArray = new Number[intData.length];
            for (int i = 0; i < intData.length; i++) {
                numArray[i] = intData[i];
            }
        }
        return numArray;
    }

    public static ColorMapParameters build(Object data, String parameter,
            Unit<?> parameterUnits, SingleLevel level) throws StyleException {
        return build(data, parameter, parameterUnits, level, null);
    }

    private static void extractLabelValues(ImagePreferences preferences,
            float max, float min, ColorMapParameters parameters) {
        if (preferences.getColorbarLabeling() != null) {
            DataScale dataScale = preferences.getDataScale();
            if (dataScale != null && dataScale.isMirror()
                    && !dataScale.isAdaptive()
                    && preferences.getColorbarLabeling().getValues() != null) {
                // if its mirror and not adaptive and the upper half labels
                // have been provided.
                createMirror(preferences, parameters);
                parameters.setMirror(dataScale.isMirror());
            } else if (preferences.getColorbarLabeling().getValues() != null) {
                // If a list is provided, use it
                float[] vals = preferences.getColorbarLabeling().getValues();

                parameters.setColorBarIntervals(vals);
            } else {
                // Populate the list using min and max
                calculateLabelValues(preferences, max, min, parameters);
                if (dataScale != null) {
                    parameters.setMirror(dataScale.isMirror());
                }
            }
        }
    }

    /**
     * Copy positive values to negative side.
     * 
     * @param preferences
     * @param parameters
     */
    private static void createMirror(ImagePreferences preferences,
            ColorMapParameters parameters) {
        float[] vals = preferences.getColorbarLabeling().getValues();
        int startZero = vals[0] == 0 ? 1 : 0;
        float[] mirroredVals = new float[vals.length * 2 - startZero];
        int upperPos = vals.length;
        int lowerPos = vals.length - 1 - startZero;
        for (int i = 0; i < vals.length - startZero; i++) {
            mirroredVals[upperPos] = vals[i + startZero];
            mirroredVals[lowerPos] = -vals[i + startZero];
            upperPos++;
            lowerPos--;
        }
        parameters.setColorBarIntervals(mirroredVals);
    }

    /**
     * Derived from ImageDepictable.C::adaptGridImageStyle
     */
    private static void calculateLabelValues(ImagePreferences preferences,
            float max, float min, ColorMapParameters parameters) {

        boolean haveIncrement = false;
        float increment = 0.1f;
        Type dataScaleType = Type.LINEAR;
        boolean isMirror = false;
        LabelingPreferences prefs = preferences.getColorbarLabeling();
        if (prefs != null) {
            increment = prefs.getIncrement();
            haveIncrement = true;
        }

        DataScale dataScale = preferences.getDataScale();
        if (dataScale != null) {
            dataScaleType = dataScale.getScaleType();
            isMirror = dataScale.isMirror();
        }
        if (Math.abs(max - min) / increment > 16) {
            increment *= 2;
        }

        final float VERY_SMALL = 1e-16f;

        List<Float> labellingPositions;

        if (dataScaleType == Type.LINEAR) {
            if (!haveIncrement) {
                increment = Math.abs((max - min) / 10);
                increment = newDataIntervalFromZoom(increment, 1, 3);
                if (increment < VERY_SMALL)
                    increment = VERY_SMALL;
                if (min + increment == min) {
                    // This can be the case if the numbers are very close
                    // together, essentially just give up and use min and max as
                    // labels
                    increment = max - min;
                }
            } else {
                if (Math.abs(max - min) / increment > 16) {
                    increment *= 2;
                }
            }

            float initialPoint = ((int) (min / increment)) * increment;
            int n = ((int) Math.abs(Math.abs(max - min) / increment)) + 1;
            labellingPositions = new ArrayList<Float>(Math.min(100,
                    Math.max(0, n)));
            if (min < max) {
                if (initialPoint <= min)
                    initialPoint += increment;

                float currentPoint = initialPoint;
                while (currentPoint < max) {
                    labellingPositions.add(currentPoint);
                    currentPoint += increment;
                }
            } else {
                if (initialPoint >= min)
                    initialPoint -= increment;

                float currentPoint = initialPoint;
                while (currentPoint > max) {
                    labellingPositions.add(currentPoint);
                    currentPoint -= increment;
                }
            }
        } else if (dataScaleType == Type.LOG) {
            if (isMirror && -min > max) {
                max = -min;
            }

            // NOTE: This ignores any increment specified in the style rule

            increment = (float) (Math.log(max / min) / 10);
            if (isMirror)
                increment *= 2;
            if (increment < Math.log(1.2))
                increment = (float) Math.log(1.2);
            int perDecade = (int) (Math.log(10) / increment + 0.5);
            increment = (float) Math.exp(increment);
            float value = newDataIntervalFromZoom(max, 1, perDecade);
            if (value > max)
                value = newDataIntervalFromZoom(max / 2, 1, perDecade);

            List<Float> tempval = new ArrayList<Float>(20);

            while (value > min) {
                tempval.add(value);
                value /= increment;
                value = newDataIntervalFromZoom(value, 1, perDecade);
            }

            labellingPositions = new ArrayList<Float>(tempval.size()
                    + (isMirror ? tempval.size() + 1 : 0));
            if (isMirror) {
                for (float v : tempval)
                    labellingPositions.add(-v);
                labellingPositions.add(0f);
            }
            for (int i = tempval.size() - 1; i >= 0; i--)
                labellingPositions.add(tempval.get(i));

        } else {
            labellingPositions = new ArrayList<Float>();
        }

        float[] valsArr = ArrayUtils.toPrimitive(labellingPositions
                .toArray(new Float[labellingPositions.size()]));
        parameters.setColorBarIntervals(valsArr);
    }

    /**
     * Derived from XformFunctions.C newDataIntervalFromZoom
     */
    static float newDataIntervalFromZoom(float defaultInterval, float dataZoom,
            int perDecade) {
        // find mantissa and exponent of scaled contour interval, check if
        // integer labels mean we must use a contour interval of one.
        float temp = defaultInterval / dataZoom;
        float e = (float) Math.pow(10, (int) (Math.log10(temp) + 10.0) - 10);
        float m = temp / e;
        if (m > 9.99) {
            m = 1;
            e *= 10;
        }
        // All other intervals have specific mantissa based on number of values
        // per decade.
        switch (perDecade) {
        case 0:
        case 1:
            m = m <= 3 ? 1 : 10;
            break;
        case 2:
            m = m < 2 ? 1 : (m < 7 ? 3 : 10);
            break;
        case 3:
        case 4:
            m = m <= 3 ? (m < 1.5 ? 1 : 2) : (m < 7.5 ? 5 : 10);
            break;
        case 5:
        case 6:
        case 7:
            if (m >= 4)
                m = m < 6 ? 5 : (m < 8.5 ? 7 : 10);
            else
                m = (float) (m < 1.75 ? (m < 1.25 ? 1 : 1.5)
                        : (m < 2.5 ? 2 : 3));
            break;
        default:
            if (m >= 9.5)
                m = 10;
            else if (m >= 5.5)
                m = m < 7.5 ? (m < 6.5 ? 6 : 7) : (m < 8.5 ? 8 : 9);
            else if (m >= 2.25)
                m = (float) (m < 3.5 ? (m < 2.75 ? 2.5 : 3) : (m < 4.5 ? 4 : 5));
            else
                m = (float) (m < 1.35 ? (m < 1.1 ? 1 : 1.2) : (m < 1.75 ? 1.5
                        : 2));
        }
        return m * e;
    }
}
