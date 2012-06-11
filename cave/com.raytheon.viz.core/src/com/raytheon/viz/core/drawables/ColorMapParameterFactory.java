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
package com.raytheon.viz.core.drawables;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.Unit;

import org.apache.commons.lang.ArrayUtils;

import com.raytheon.edex.util.Util;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.style.LabelingPreferences;
import com.raytheon.uf.viz.core.style.ParamLevelMatchCriteria;
import com.raytheon.uf.viz.core.style.StyleManager;
import com.raytheon.uf.viz.core.style.StyleRule;
import com.raytheon.uf.viz.core.style.level.RangeLevel;
import com.raytheon.uf.viz.core.style.level.SingleLevel;
import com.raytheon.viz.core.style.image.DataScale;
import com.raytheon.viz.core.style.image.DataScale.Type;
import com.raytheon.viz.core.style.image.ImagePreferences;

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
 *    Mar 26, 2009     2086    jsanchez    Added a entityList to the match criteria.
 * 
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
            throws VizException {

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

        ColorMapParameters params = new ColorMapParameters();

        ImagePreferences preferences = null;
        if (sr != null) {
            preferences = (ImagePreferences) sr.getPreferences();
        } else {
            preferences = new ImagePreferences();
        }

        // If a record to convert units exists, use it
        params.setDataUnit(parameterUnits);
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
                    if (num.floatValue() != Util.GRID_FILL_VALUE
                            && !Float.isNaN(num.floatValue())) {
                        colormapMin = min = Math.min(min, num.floatValue());
                        colormapMax = max = Math.max(max, num.floatValue());
                    }
                }

                if (min == Float.POSITIVE_INFINITY) {
                    colormapMin = min = colormapMax = max = Util.GRID_FILL_VALUE;
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
                extractLabelValues(sr, displayMax, displayMin, params);
            } else if (preferences.getDataMapping() == null) {
                calculateLabelValues(sr, displayMax, displayMin, params);
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

            extractLabelValues(sr, (float) displayMax, (float) displayMin,
                    params);
            params.setColorMapName(preferences.getDefaultColormap());
        }
        if (preferences.getDataScale() != null) {
            if (dataScaleType == Type.LOG) {
                params.setLogarithmic(true);
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
            Unit<?> parameterUnits, SingleLevel level) throws VizException {
        return build(data, parameter, parameterUnits, level, null);
    }

    public static ColorMapParameters build(File file, String dataURI,
            String parameter, Unit<?> parameterUnits, SingleLevel level,
            String entity) throws VizException {
        ColorMapParameters params = null;
        if (file != null) {
            IDataRecord rec = null;
            try {
                IDataStore ds = DataStoreFactory.getDataStore(file);
                rec = ds.retrieve("", dataURI, Request.ALL);
            } catch (Exception e) {
                throw new VizException("Unable to read file (File: " + file
                        + ") (DataSet: " + dataURI + ")", e);
            }

            if (rec != null) {
                Object data = rec.getDataObject();
                params = build(data, parameter, parameterUnits, level, entity);
            }
        } else {
            params = build(null, parameter, parameterUnits, level, entity);
        }

        return params;
    }

    public static ColorMapParameters build(File file, String dataURI,
            String parameter, Unit<?> parameterUnits, SingleLevel level)
            throws VizException {
        return build(file, dataURI, parameter, parameterUnits, level, null);
    }

    public static ColorMapParameters build(PluginDataObject record,
            String parameter, Unit<?> parameterUnits, SingleLevel level,
            String entity) throws VizException {
        ColorMapParameters params = null;
        if (record != null) {
            IDataRecord rec = null;
            IDataRecord[] records = DataCubeContainer.getDataRecord(record);
            if (records != null && records.length > 0) {
                rec = records[0];
            }
            if (rec != null) {
                Object data = rec.getDataObject();
                params = build(data, parameter, parameterUnits, level, entity);
            }
        } else {
            params = build((Object) null, parameter, parameterUnits, level,
                    entity);
        }

        return params;
    }

    public static ColorMapParameters build(PluginDataObject record,
            String parameter, Unit<?> parameterUnits, SingleLevel level)
            throws VizException {
        return build(record, parameter, parameterUnits, level, null);
    }

    private static void extractLabelValues(StyleRule sr, float max, float min,
            ColorMapParameters parameters) {
        if (sr != null) {
            ImagePreferences preferences = (ImagePreferences) sr
                    .getPreferences();
            if (preferences.getColorbarLabeling() != null) {
                DataScale dataScale = preferences.getDataScale();
                if (dataScale != null
                        && dataScale.isMirror()
                        && !dataScale.isAdaptive()
                        && preferences.getColorbarLabeling().getValues() != null) {
                    // if its mirror and not adaptive and the upper half labels
                    // have been provided.
                    createMirror(preferences, parameters);
                    parameters.setMirror(dataScale.isMirror());
                } else if (preferences.getColorbarLabeling().getValues() != null) {
                    // If a list is provided, use it
                    float[] vals = preferences.getColorbarLabeling()
                            .getValues();

                    parameters.setColorBarIntervals(vals);
                } else {
                    // Populate the list using min and max
                    calculateLabelValues(sr, max, min, parameters);
                    if (dataScale != null) {
                        parameters.setMirror(dataScale.isMirror());
                    }
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
    private static void calculateLabelValues(StyleRule sr, float max,
            float min, ColorMapParameters parameters) {

        boolean haveIncrement = false;
        float increment = 0.1f;
        Type dataScaleType = Type.LINEAR;
        boolean isMirror = false;
        if (sr != null) {
            ImagePreferences preferences = (ImagePreferences) sr
                    .getPreferences();
            if (preferences != null) {
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
            }
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
