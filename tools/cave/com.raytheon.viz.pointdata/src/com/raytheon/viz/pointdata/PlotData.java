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

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import javax.measure.unit.Unit;

import com.raytheon.uf.common.pointdata.PointDataDescription.Type;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.viz.core.datastructure.CubeUtil;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 19, 2011            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class PlotData {

    private class PlotValue {
        private Object value;

        private Type type;

        private Unit<?> unit;

        private int dimension;
    }

    private Map<String, PlotValue> valueMap = new HashMap<String, PlotValue>();

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
            case DOUBLE:
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
            case DOUBLE:
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

    public int getInt(String parameter) {
        safetyCheck(parameter);
        return ((Number) valueMap.get(parameter).value).intValue();
    }

    public float getFloat(String parameter) {
        safetyCheck(parameter);
        return ((Number) valueMap.get(parameter).value).floatValue();
    }

    public long getLong(String parameter) {
        safetyCheck(parameter);
        return ((Number) valueMap.get(parameter).value).longValue();
    }

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

    public String[] getStringAllLevels(String parameter) {
        safetyCheck(parameter);
        return (String[]) valueMap.get(parameter).value;
    }

    public Type getType(String parameter) {
        safetyCheck(parameter);
        return valueMap.get(parameter).type;
    }

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

    public Unit<?> getUnit(String parameter) {
        safetyCheck(parameter);
        return valueMap.get(parameter).unit;
    }

    public int getDimensions(String parameter) {
        safetyCheck(parameter);
        return valueMap.get(parameter).dimension;
    }

    public Number[] getNumberAllLevels(String parameter) {
        safetyCheck(parameter);
        return (Number[]) valueMap.get(parameter).value;
    }

    public Set<String> getParameters() {
        return valueMap.keySet();
    }

    private void safetyCheck(String parameter) {
        if (!valueMap.containsKey(parameter)) {
            throw new IllegalArgumentException("Parameter " + parameter
                    + " not present in PlotData");
        }
    }

}
