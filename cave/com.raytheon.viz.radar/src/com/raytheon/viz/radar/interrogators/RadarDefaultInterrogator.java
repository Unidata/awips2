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
package com.raytheon.viz.radar.interrogators;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import javax.measure.Quantity;
import javax.measure.Unit;
import javax.measure.UnitConverter;

import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.GeometryFactory;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.colormap.prefs.DataMappingPreferences.DataMappingEntry;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.geospatial.CRSCache;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.style.ParamLevelMatchCriteria;
import com.raytheon.uf.common.style.StyleException;
import com.raytheon.uf.common.style.StyleManager;
import com.raytheon.uf.common.style.StyleRule;
import com.raytheon.uf.common.style.image.ImagePreferences;
import com.raytheon.uf.common.style.image.NumericFormat;
import com.raytheon.uf.common.style.image.SampleFormat;
import com.raytheon.uf.common.units.PiecewisePixel;
import com.raytheon.uf.common.units.UnitConv;
import com.raytheon.uf.viz.core.rsc.interrogation.InterrogateMap;
import com.raytheon.uf.viz.core.rsc.interrogation.InterrogationKey;
import com.raytheon.uf.viz.core.rsc.interrogation.Interrogator;

import tec.uom.se.AbstractConverter;
import tec.uom.se.AbstractUnit;
import tec.uom.se.format.SimpleUnitFormat;
import tec.uom.se.quantity.Quantities;

/**
 * Default interrogator for radar resource.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer     Description
 * ------------- -------- ------------ -----------------------------------------
 * Aug 04, 2010           mnash        Initial creation
 * May 02, 2013  14587    D. Friedman  Refactor to store multiple types.
 * Aug 22, 2013  2278     bsteffen     Allow radar interrogation to work without
 *                                     ColorMapParameters.
 * Jun 22, 2015  17486    Zihou Wang   Use displayUnits in style rules
 * Sep 13, 2016  3239     nabowle      Use the Interrogatable API.
 * Apr 15, 2019 7596       lsingh      Updated units framework to JSR-363.
 *                                     Handled unit conversion.
 * Apr 20, 2020  8145     randerso     Replace SamplePreferences with
 *                                     SampleFormat
 *
 * </pre>
 *
 * @author mnash
 */

public class RadarDefaultInterrogator implements IRadarInterrogator {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RadarDefaultInterrogator.class);

    @Override
    public InterrogateMap sample(RadarRecord radarRecord, Coordinate latLon,
            ColorMapParameters params, Set<InterrogationKey<?>> keys) {

        InterrogateMap dataMap = new InterrogateMap();
        int dataValue = sample(radarRecord, latLon, keys, dataMap);

        addValueToMap(dataMap, keys, PRIMAY_ELEVATION_ANGLE,
                radarRecord.getPrimaryElevationAngle());

        if (keys.contains(Interrogator.VALUE) || keys.contains(VALUE_STRING)) {
            addValueToMap(dataValue, Interrogator.VALUE, VALUE_STRING,
                    radarRecord, params, dataMap, keys);
        }

        computeValues(radarRecord, dataMap, dataValue, params, keys);
        return dataMap;
    }

    /**
     * @param radarRecord
     * @param latLon
     * @param keys
     * @param dataMap
     * @return
     */
    protected int sample(RadarRecord radarRecord, Coordinate latLon,
            Set<InterrogationKey<?>> keys, InterrogateMap dataMap) {
        if (latLon == null || radarRecord == null || keys == null
                || keys.isEmpty()) {
            return 0;
        }
        addCrsLocation(radarRecord, latLon, keys, dataMap);

        addValueToMap(dataMap, keys, IRadarInterrogator.ICAO,
                radarRecord.getIcao());
        addValueToMap(dataMap, keys, MNEMONIC, radarRecord.getMnemonic());

        int dataValue = addParameters(radarRecord, latLon, dataMap, keys);
        return dataValue;
    }

    /**
     * @param radarRecord
     * @param latLon
     * @param keys
     * @param dataMap
     */
    protected void addCrsLocation(RadarRecord radarRecord, Coordinate latLon,
            Set<InterrogationKey<?>> keys, InterrogateMap dataMap) {
        boolean includeCrs = keys.contains(CRS_LOCATION);
        boolean includeGeom = keys.contains(Interrogator.GEOMETRY);
        if (includeCrs || includeGeom) {
            double[] input = { latLon.x, latLon.y }; // rr
            double[] output = new double[2]; // rr
            try {
                MathTransform mt = CRSCache.getInstance()
                        .getTransformFromLatLon(radarRecord.getCRS());

                mt.transform(input, 0, output, 0, 1);
                if (includeCrs) {
                    dataMap.put(CRS_LOCATION,
                            output == null ? new double[] { -1D, -1D }
                                    : output);
                }
                if (includeGeom && output != null) {
                    dataMap.put(Interrogator.GEOMETRY, new GeometryFactory()
                            .createPoint(new Coordinate(output[0], output[1])));
                }
            } catch (FactoryException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to get transform from the radar record CRS.",
                        e);
            } catch (TransformException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to transform lat/lon to grid.", e);
            }
        }
    }

    /**
     * @param dataValue
     * @param stringKey
     * @param baseName
     * @param dataMap
     */
    protected void addValueToMap(int dataValue,
            InterrogationKey<Quantity<?>> valueKey,
            InterrogationKey<String> stringKey, RadarRecord radarRecord,
            ColorMapParameters params, InterrogateMap dataMap,
            Set<InterrogationKey<?>> keys) {
        String stringValue = null;
        Number numericValue = dataValue == 0 ? null : dataValue;

        if (radarRecord.getNumLevels() <= 16 && radarRecord.getNumLevels() != 0
                && params != null) {
            // /////////////////////////////////////////////////////////
            // Handle cases where there are ranges and not actual values
            // /////////////////////////////////////////////////////////
            Object th0 = radarRecord.getDecodedThreshold(dataValue);
            Unit<?> dispUnit = params.getDisplayUnit();
            String units = null;

            // handle PiecewisePixel issue with toString()
            if (dispUnit instanceof PiecewisePixel<?>) {
                dispUnit = dispUnit.getSystemUnit();
                units = dispUnit.toString();
            } else {
                units = SimpleUnitFormat.getInstance(SimpleUnitFormat.Flavor.ASCII).format(dispUnit);
            }
            if (dataValue == 0) {
                stringValue = "NO DATA";
            } else if (th0 instanceof Float) {
                double f0 = (Float) th0;
                UnitConverter converter = getConverter(params, radarRecord);
                if (converter != null) {
                    f0 = converter.convert(dataValue);
                }
                numericValue = f0;
                if (dataValue < 15) {
                    Object th1 = radarRecord.getDecodedThreshold(dataValue + 1);
                    if (th1 instanceof Float) {
                        double f1 = (Float) th1;
                        if (converter != null) {
                            f1 = converter.convert(dataValue + 1);
                        }

                        stringValue = String.format("%.1f to %.1f%s", f0, f1,
                                units);
                    } else {
                        stringValue = String.format(">%.1f%s", f0, units);
                    }
                } else {
                    stringValue = String.format(">%.1f%s", f0, units);
                }
                if (params.getDataMapping() != null) {
                    for (DataMappingEntry entry : params.getDataMapping()
                            .getEntries()) {
                        if (entry.getSample() != null
                                && entry.getDisplayValue() != null
                                && entry.getDisplayValue() == f0) {
                            stringValue = entry.getSample();
                        }
                    }
                }
            } else {
                stringValue = th0.toString();
                if (stringValue.isEmpty() && params.getDataMapping() != null) {
                    for (DataMappingEntry entry : params.getDataMapping()
                            .getEntries()) {
                        if (entry.getSample() != null
                                && entry.getDisplayValue() != null
                                && entry.getDisplayValue() == dataValue) {
                            stringValue = entry.getSample();
                        }
                    }
                }
            }
            if (stringValue != null) {
                addValueToMap(dataMap, keys, stringKey, stringValue);
            }
            if (numericValue != null) {
                addValueToMap(dataMap, keys, valueKey,
                        Quantities.getQuantity(numericValue, dispUnit));
            }
        } else {
            // /////////////////////////////////////////////////////////
            // Handle cases where the the actual value is used
            // /////////////////////////////////////////////////////////
            if (dataValue == 0 && stringKey != null) {
                addValueToMap(dataMap, keys, stringKey, "NO DATA");
            } else {
                decodeValues(dataValue, valueKey, stringKey, dataMap,
                        radarRecord, params, keys);
            }
        }
    }

    public void computeValues(RadarRecord radarRecord, InterrogateMap dataMap,
            int dataValue, ColorMapParameters params,
            Set<InterrogationKey<?>> keys) {
    }

    /**
     * Gives the ability to handle product specific sampling
     *
     * @param dataValue
     * @param dataMap
     * @return
     */
    public void decodeValues(int dataValue,
            InterrogationKey<Quantity<?>> valueKey,
            InterrogationKey<String> stringKey, InterrogateMap dataMap,
            RadarRecord radarRecord, ColorMapParameters params,
            Set<InterrogationKey<?>> keys) {
        UnitConverter converter = getConverter(params, radarRecord);
        double dispVal = converter.convert(dataValue);
        String unitString = "";
        Unit<?> unit = AbstractUnit.ONE;
        if (params != null && params.getDisplayUnit() != AbstractUnit.ONE) {
            unit = params.getDisplayUnit();
            unitString = SimpleUnitFormat.getInstance(SimpleUnitFormat.Flavor.ASCII).format(unit);
        }

        addValueToMap(dataMap, keys, valueKey, Quantities.getQuantity(dispVal, unit));

        if (stringKey == null || !keys.contains(stringKey)) {
            return;
        }

        if (params != null && params.getDataMapping() != null) {
            for (DataMappingEntry entry : params.getDataMapping()
                    .getEntries()) {
                if (entry.getSample() == null) {
                    continue;
                }

                // Not a valid entry, probably something like NaN or RF
                if ((Double.isNaN(dispVal)
                        && entry.getPixelValue() == dataValue)
                        || (entry.getDisplayValue() != null
                                && entry.getDisplayValue() == dispVal)) {
                    dataMap.put(stringKey, entry.getSample());
                    return;
                }
            }
        }

        // Grab the sampleRange from the preferences
        ParamLevelMatchCriteria match = new ParamLevelMatchCriteria();
        match.setParameterName(
                Arrays.asList(Integer.toString(radarRecord.getProductCode())));
        StyleRule sr = null;
        try {
            sr = StyleManager.getInstance()
                    .getStyleRule(StyleManager.StyleType.IMAGERY, match);
        } catch (StyleException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
        SampleFormat sampleFormat = NumericFormat.DEFAULT;
        if (sr != null && sr.getPreferences() instanceof ImagePreferences) {
            ImagePreferences prefs = (ImagePreferences) sr.getPreferences();
            sampleFormat = prefs.getSampleFormat();

            if (sampleFormat instanceof NumericFormat) {
                // use the displayUnits in style rule
                if (prefs.getDisplayUnitLabel() != null
                        && !AbstractUnit.ONE.equals(prefs.getDisplayUnits())) {
                    unitString = prefs.getDisplayUnitLabel();
                }
            }
        }
        dataMap.put(stringKey, sampleFormat.format(dispVal, unitString));
    }

    @Override
    public int addParameters(RadarRecord radarRecord, Coordinate latLon,
            InterrogateMap dataMap, Set<InterrogationKey<?>> keys) {
        return 0;
    }

    protected UnitConverter getConverter(ColorMapParameters params,
            RadarRecord radarRecord) {
        UnitConverter converter = null;
        Unit<?> dataUnit = radarRecord.getDataUnit();
        if (params == null) {
            converter = AbstractConverter.IDENTITY;
        } else if (dataUnit != null && !dataUnit.equals(params.getDataUnit())) {
            Unit<?> displayUnit = params.getDisplayUnit();
            if (dataUnit.isCompatible(displayUnit)) {
                converter = UnitConv.getConverterToUnchecked(dataUnit, displayUnit);
            }
        } else {
            converter = params.getDataToDisplayConverter();
        }

        return converter;
    }

    protected <T> void addValueToMap(InterrogateMap dataMap,
            Set<InterrogationKey<?>> keys, InterrogationKey<T> key, T value) {
        if (key != null && keys.contains(key)) {
            dataMap.put(key, value);
        }
    }

    @Override
    public Set<InterrogationKey<?>> getInterrogationKeys() {
        Set<InterrogationKey<?>> keys = new HashSet<>();
        keys.add(Interrogator.VALUE);
        keys.add(Interrogator.GEOMETRY);
        keys.add(VALUE_STRING);
        keys.add(AGL);
        keys.add(PRIMAY_ELEVATION_ANGLE);
        keys.add(IRadarInterrogator.AZIMUTH);
        keys.add(CRS_LOCATION);
        keys.add(IRadarInterrogator.ICAO);
        keys.add(MNEMONIC);
        keys.add(MSL);
        keys.add(IRadarInterrogator.RANGE);
        keys.add(SHEAR);
        return keys;
    }

    /**
     * Get the set of keys who's values are already included within the Value
     * String, if any.
     *
     * @return The set of keys who's values are already included within the
     *         Value String, if any.
     */
    @Override
    public Set<InterrogationKey<?>> getValueStringKeys() {
        Set<InterrogationKey<?>> keys = new HashSet<>();
        keys.add(Interrogator.VALUE);
        return keys;
    }
}
