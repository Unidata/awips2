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
package com.raytheon.viz.pointdata;

import java.text.ParsePosition;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import javax.measure.Unit;
import javax.measure.format.MeasurementParseException;

import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.PointDataDescription.Type;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.datacube.CubeUtil;
import com.raytheon.viz.pointdata.util.MetarPrecipDataContainer.PrecipData;

import tech.units.indriya.format.SimpleUnitFormat;

/**
 * Plot Data Object.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 19, 2011            njensen     Initial creation.
 * Jul 12, 2013    2096    mpduff      Add method to check if parameter is valid.
 * Aug 07, 2014 3478       bclement    removed PointDataDescription.Type.Double
 * Nov 01, 2019 71272      ksunil      tweaks to accommodate new plot
 *                                     customization changes
 * Mar 24, 2020 75529      ksunil      added new addData method for precipData. implemented isValidParameter
 * Apr 10, 2020 77336      ksunil      Minor logging enhancement
 * </pre>
 *
 * @author njensen
 */

public class PlotData implements IPlotData {

    private class PlotValue {
        private Object value;

        private Type type;

        private Unit<?> unit;

        private int dimension;
    }

    /** Map of data values */
    private final Map<String, PlotValue> valueMap = new HashMap<>();

    public void addData(PointDataView pdv) {
        for (String key : pdv.getContainer().getParameters()) {
            int dimensions = pdv.getDimensions(key);
            Type t = pdv.getType(key);
            PlotValue pv = new PlotValue();
            pv.type = t;
            pv.unit = pdv.getUnit(key);
            pv.dimension = dimensions;
            switch (t) {
            case FLOAT:
            case INT:
            case LONG:
                if (dimensions == 2) {
                    pv.value = pdv.getNumberAllLevels(key);
                    valueMap.put(key, pv);
                } else {
                    Number n = pdv.getNumber(key);
                    pv.value = n;
                    if (n.floatValue() > CubeUtil.MISSING
                            || !valueMap.containsKey(key)) {
                        valueMap.put(key, pv);
                    }
                }
                break;
            case STRING:
                if (dimensions == 2) {
                    pv.value = pdv.getStringAllLevels(key);
                    valueMap.put(key, pv);
                } else {
                    String s = pdv.getString(key);
                    pv.value = s;
                    if ((s != null && !s.isEmpty())
                            || !valueMap.containsKey(key)) {
                        valueMap.put(key, pv);
                    }
                }
                break;
            }
        }
    }

    public void addData(PlotData pdv) {
        for (String key : pdv.getParameters()) {
            int dimensions = pdv.getDimensions(key);
            Type t = pdv.getType(key);
            PlotValue pv = new PlotValue();
            pv.type = t;
            pv.unit = pdv.getUnit(key);
            pv.dimension = dimensions;
            switch (t) {
            case FLOAT:
            case INT:
            case LONG:
                if (dimensions == 2) {
                    pv.value = pdv.getNumberAllLevels(key);
                    valueMap.put(key, pv);
                } else {
                    Number n = pdv.getNumber(key);
                    pv.value = n;
                    if (n.floatValue() > CubeUtil.MISSING
                            || !valueMap.containsKey(key)) {
                        valueMap.put(key, pv);
                    }
                }
                break;
            case STRING:
                if (dimensions == 2) {
                    pv.value = pdv.getStringAllLevels(key);
                    valueMap.put(key, pv);
                } else {
                    String s = pdv.getString(key);
                    pv.value = s;
                    if ((s != null && !s.isEmpty())
                            || !valueMap.containsKey(key)) {
                        valueMap.put(key, pv);
                    }
                }
                break;
            }
        }

    }

    public void addData(PrecipData pdv) throws VizException {
        PlotValue pv = new PlotValue();
        pv.value = pdv.getPrecipAmt();
        String unitStr = pdv.getUnit();
        if (unitStr != null) {
            try {
                Unit<?> unit = SimpleUnitFormat
                        .getInstance(SimpleUnitFormat.Flavor.ASCII)
                        .parseProductUnit(unitStr, new ParsePosition(0));
                pv.unit = unit;
            } catch (MeasurementParseException e) {
                throw new VizException("Unable parse units:  " + unitStr, e);
            }
        }
        pv.type = PointDataDescription.Type.FLOAT;
        valueMap.put(pdv.getParamName(), pv);
    }

    @Override
    public int getInt(String parameter) {
        safetyCheck(parameter);
        return ((Number) valueMap.get(parameter).value).intValue();
    }

    @Override
    public float getFloat(String parameter) {
        safetyCheck(parameter);
        return ((Number) valueMap.get(parameter).value).floatValue();
    }

    @Override
    public long getLong(String parameter) {
        safetyCheck(parameter);
        return ((Number) valueMap.get(parameter).value).longValue();
    }

    @Override
    public String getString(String parameter) {
        safetyCheck(parameter);
        String s = null;
        PlotValue v = valueMap.get(parameter);
        if (v.dimension > 1) {
            s = ((String[]) v.value)[0];
        } else {
            s = (String) v.value;
        }
        return s;
    }

    @Override
    public String[] getStringAllLevels(String parameter) {
        safetyCheck(parameter);
        return (String[]) valueMap.get(parameter).value;
    }

    @Override
    public Type getType(String parameter) {
        safetyCheck(parameter);
        return valueMap.get(parameter).type;
    }

    @Override
    public Number getNumber(String parameter) {
        safetyCheck(parameter);
        PlotValue v = valueMap.get(parameter);
        Number n = null;
        if (v.dimension > 1) {
            n = ((Number[]) v.value)[0];
        } else {
            n = (Number) v.value;
        }
        return n;
    }

    @Override
    public Unit<?> getUnit(String parameter) {
        safetyCheck(parameter);
        return valueMap.get(parameter).unit;
    }

    @Override
    public int getDimensions(String parameter) {
        safetyCheck(parameter);
        return valueMap.get(parameter).dimension;
    }

    @Override
    public Number[] getNumberAllLevels(String parameter) {
        safetyCheck(parameter);
        return (Number[]) valueMap.get(parameter).value;
    }

    @Override
    public Set<String> getParameters() {
        return valueMap.keySet();
    }

    private void safetyCheck(String parameter) {

        if (!valueMap.containsKey(parameter)) {
            throw new IllegalArgumentException(
                    "Parameter " + parameter + " not present in PlotData");
        }
    }

    /**
     * Check if the provided parameter is valid (contained in the valueMap)
     *
     * @param param
     *            The parameter to check
     * @return true if the parameter exists in the valueMap, false if not
     */
    @Override
    public boolean isValidParameter(String param) {
        return valueMap.containsKey(param);
    }
}
