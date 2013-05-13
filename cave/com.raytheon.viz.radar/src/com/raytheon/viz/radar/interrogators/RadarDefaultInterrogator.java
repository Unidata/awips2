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
import java.util.HashMap;
import java.util.Map;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;

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
import com.raytheon.uf.common.units.PiecewisePixel;
import com.raytheon.uf.viz.core.style.ParamLevelMatchCriteria;
import com.raytheon.uf.viz.core.style.StyleManager;
import com.raytheon.uf.viz.core.style.StyleRule;
import com.raytheon.uf.viz.core.style.VizStyleException;
import com.raytheon.viz.core.style.image.ImagePreferences;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Default interrogator for radar resource.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 4, 2010            mnash     Initial creation
 * 05/02/2013   DR 14587   D. Friedman Refactor to store multiple types.
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class RadarDefaultInterrogator implements IRadarInterrogator {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RadarDefaultInterrogator.class);

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.radar.rsc.special.IRadarInterrogator#sample(com.raytheon
     * .edex.plugin.radar.RadarRecord, com.vividsolutions.jts.geom.Coordinate)
     */
    @Override
    public Map<String, String> sample(RadarRecord radarRecord,
            Coordinate latLon, ColorMapParameters params) {

        Map<String, String> dataMap = new HashMap<String, String>();
        if (latLon == null) {
            return null;
        }
        double[] input = { latLon.x, latLon.y }; // rr
        double[] output = new double[2]; // rr
        try {
            MathTransform mt = CRSCache.getInstance().getTransformFromLatLon(
                    radarRecord.getCRS());

            mt.transform(input, 0, output, 0, 1);
            dataMap.put("crsLocation", output == null ? "-1,-1" : output[0]
                    + "," + output[1]);
        } catch (FactoryException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to get transform from the radar record CRS.", e);
        } catch (TransformException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to transform lat/lon to grid.", e);
        }

        dataMap.put("ICAO", radarRecord.getIcao());
        dataMap.put("Mnemonic", radarRecord.getMnemonic());
        dataMap.put("Angle", radarRecord.getPrimaryElevationAngle().toString());

        int dataValue = addParameters(radarRecord, latLon, dataMap);
        addValueToMap(dataValue, "", radarRecord, params, dataMap);

        computeValues(radarRecord, dataMap, dataValue, params);
        return dataMap;
    }

    /**
     * @param dataValue
     * @param baseName
     * @param dataMap
     */
    protected void addValueToMap(int dataValue, String baseName,
            RadarRecord radarRecord, ColorMapParameters params,
            Map<String, String> dataMap) {
        String numericValueKey = baseName + "numericValue";
        dataMap.put(numericValueKey, String.valueOf(dataValue));
        String dataValueString = "";

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
                units = dispUnit.getStandardUnit().toString();
            } else {
                units = UnitFormat.getUCUMInstance().format(dispUnit);
            }
            if (dataValue == 0) {
                dataMap.put(numericValueKey, null);
                dataValueString = "NO DATA";
            } else if (th0 instanceof Float) {
                double f0 = (Float) th0;
                UnitConverter converter = getConverter(params, radarRecord);
                if (converter != null) {
                    f0 = converter.convert(dataValue);
                }
                dataMap.put(numericValueKey, String.valueOf(f0));

                if (dataValue < 15) {
                    Object th1 = radarRecord.getDecodedThreshold(dataValue + 1);
                    if (th1 instanceof Float) {
                        double f1 = (Float) th1;
                        if (converter != null) {
                            f1 = converter.convert(dataValue + 1);
                        }

                        dataValueString = String.format("%.1f to %.1f%s", f0,
                                f1, units);
                    } else {
                        dataValueString = String.format(">%.1f%s", f0, units);
                    }
                } else {
                    dataValueString = String.format(">%.1f%s", f0, units);
                }
                if (params.getDataMapping() != null) {
                    for (DataMappingEntry entry : params.getDataMapping()
                            .getEntries()) {
                        if (entry.getSample() != null
                                && entry.getDisplayValue() != null
                                && entry.getDisplayValue() == f0) {
                            dataValueString = entry.getSample();
                        }
                    }
                }
            } else {
                dataValueString = th0.toString();
                if (dataValueString.isEmpty()
                        && params.getDataMapping() != null) {
                    for (DataMappingEntry entry : params.getDataMapping()
                            .getEntries()) {
                        if (entry.getSample() != null
                                && entry.getDisplayValue() != null
                                && entry.getDisplayValue() == dataValue) {
                            dataValueString = entry.getSample();
                        }
                    }
                }
            }
        } else {
            // /////////////////////////////////////////////////////////
            // Handle cases where the the actual value is used
            // /////////////////////////////////////////////////////////
            if (dataValue == 0) {
                dataMap.put(numericValueKey, null);
                dataValueString = "NO DATA";
            } else {
                dataValueString = decodeValues(dataValue, baseName, dataMap, radarRecord,
                        params);
            }
        }
        dataMap.put(baseName + "Value", dataValueString);
    }

    public void computeValues(RadarRecord radarRecord,
            Map<String, String> dataMap, int dataValue,
            ColorMapParameters params) {
    }

    /**
     * Gives the ability to handle product specific sampling
     * 
     * @param dataValue
     * @param dataMap
     * @return
     */
    public String decodeValues(int dataValue, String baseName, Map<String, String> dataMap,
            RadarRecord radarRecord, ColorMapParameters params) {
        UnitConverter converter = getConverter(params, radarRecord);
        double dispVal = converter.convert(dataValue);
        dataMap.put(baseName + "numericValue", String.valueOf(dispVal));
        if (params.getDataMapping() != null) {
            for (DataMappingEntry entry : params.getDataMapping().getEntries()) {
                if (entry.getSample() == null) {
                    continue;
                }
                // Not a valid entry, probably something like NaN or RF
                if (Double.isNaN(dispVal) && entry.getPixelValue() == dataValue) {
                    return entry.getSample();
                }
                if (entry.getDisplayValue() == null) {
                    continue;
                }
                if (entry.getDisplayValue() == dispVal) {
                    return entry.getSample();
                }

            }
        }
        String unitString = "";
        if (params.getDisplayUnit() != Unit.ONE) {
            unitString = UnitFormat.getUCUMInstance().format(
                    params.getDisplayUnit());
        }
        // Grab the sampleRange from the preferences
        ParamLevelMatchCriteria match = new ParamLevelMatchCriteria();
        match.setParameterName(Arrays.asList(Integer.toString(radarRecord
                .getProductCode())));
        StyleRule sr = null;
        try {
            sr = StyleManager.getInstance().getStyleRule(
                    StyleManager.StyleType.IMAGERY, match);
        } catch (VizStyleException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
        if (sr != null && sr.getPreferences() instanceof ImagePreferences) {
            ImagePreferences prefs = (ImagePreferences) sr.getPreferences();
            if (prefs.getSamplePrefs() != null
                    && prefs.getSamplePrefs().getFormatString() != null
                    && !prefs.getSamplePrefs().getFormatString().isEmpty()) {
                return String.format("%."
                        + prefs.getSamplePrefs().getFormatString() + "f %s",
                        dispVal, unitString);
            }
        }
        return String.format("%.1f %s", dispVal, unitString);
    }

    @Override
    public int addParameters(RadarRecord radarRecord, Coordinate latLon,
            Map<String, String> dataMap) {
        return 0;
    }

    protected UnitConverter getConverter(ColorMapParameters params,
            RadarRecord radarRecord) {
        UnitConverter converter = null;
        Unit<?> dataUnit = radarRecord.getDataUnit();
        if (dataUnit != null && !dataUnit.equals(params.getDataUnit())) {
            Unit<?> displayUnit = params.getDisplayUnit();
            if (dataUnit.isCompatible(displayUnit)) {
                converter = dataUnit.getConverterTo(displayUnit);
            }
        } else {
            converter = params.getDataToDisplayConverter();
        }
        return converter;
    }
}
