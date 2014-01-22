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
package com.raytheon.uf.common.dataaccess.impl;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import javax.measure.converter.ConversionException;
import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataaccess.geom.IGeometryData;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.time.DataTime;
import com.vividsolutions.jts.geom.Geometry;

/**
 * A default geometry data object if factory developers do not wish to create
 * their own IGeometryData implementations.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 09, 2012            njensen     Initial creation
 * Jun 03, 2013  #2023     dgilling    Implement getAttributes().
 * Jan 21, 2014  2667      bclement    attribute method comments
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class DefaultGeometryData implements IGeometryData {

    /**
     * A simple object to hold a value, type, and unit of a parameter
     */
    private class GeomData {
        private Object value;

        private Type type;

        private Unit<?> unit;
    }

    protected Map<String, GeomData> dataMap = new HashMap<String, GeomData>();

    protected DataTime time;

    protected Level level;

    protected Geometry geometry;

    protected String locationName;

    protected Map<String, Object> attributes = new HashMap<String, Object>();

    @Override
    public Object getAttribute(String key) {
        Object result = null;
        if (attributes != null) {
            result = attributes.get(key);
        }
        return result;
    }

    @Override
    public Set<String> getAttributes() {
        return attributes.keySet();
    }

    @Override
    public DataTime getDataTime() {
        return time;
    }

    @Override
    public Level getLevel() {
        return level;
    }

    @Override
    public Geometry getGeometry() {
        return geometry;
    }

    @Override
    public Set<String> getParameters() {
        return dataMap.keySet();
    }

    @Override
    public String getString(String param) {
        String result = null;
        GeomData data = dataMap.get(param);
        if (data != null) {
            result = data.value.toString();
        }
        return result;
    }

    @Override
    public Number getNumber(String param) {
        Number result = null;
        GeomData data = dataMap.get(param);
        if (data != null) {
            switch (data.type) {
            case STRING:
                result = Double.valueOf((String) data.value);
                break;
            case INT:
                result = (Integer) data.value;
                break;
            case LONG:
                result = (Long) data.value;
                break;
            case FLOAT:
                result = (Float) data.value;
                break;
            case DOUBLE:
                result = (Double) data.value;
                break;
            default:
                throw new UnsupportedOperationException(
                        "Unable to handle data of type "
                                + data.value.getClass());
            }
        }
        return result;
    }

    @Override
    public Number getNumber(String param, Unit<?> unit) {
        Number result = null;
        if (unit == null) {
            throw new IllegalArgumentException(
                    "Unable to convert data to null unit");
        }
        GeomData data = dataMap.get(param);
        if (data != null) {
            if (data.unit != null) {
                if (data.unit.isCompatible(unit)) {
                    Number orig = getNumber(param);
                    result = data.unit.getConverterTo(unit).convert(
                            orig.doubleValue());
                } else {
                    throw new ConversionException("Requested unit " + unit
                            + " is incompatible with " + param
                            + " data's unit " + data.unit);
                }
            } else {
                throw new ConversionException(
                        "Unable to convert data due to no unit associated with "
                                + param);
            }
        }
        return result;
    }

    @Override
    public Unit<?> getUnit(String param) {
        Unit<?> result = null;
        GeomData data = dataMap.get(param);
        if (data != null) {
            result = data.unit;
        }
        return result;
    }

    @Override
    public Type getType(String param) {
        Type result = null;
        GeomData data = dataMap.get(param);
        if (data != null) {
            result = data.type;
        }
        return result;
    }

    @Override
    public String getLocationName() {
        return locationName;
    }

    /**
     * Adds data for this IGeometryData
     * 
     * @param parameter
     *            the parameter name
     * @param value
     *            the value of the parameter
     */
    public void addData(String parameter, Object value) {
        addData(parameter, value, null, null);
    }

    /**
     * Adds data for this IGeometryData
     * 
     * @param parameter
     *            the parameter name
     * @param value
     *            the value of the parameter
     * @param type
     *            the type of the value
     */
    public void addData(String parameter, Object value, Type type) {
        addData(parameter, value, type, null);
    }

    /**
     * Adds data for this IGeometryData
     * 
     * @param parameter
     *            the parameter name
     * @param value
     *            the value of the parameter
     * @param unit
     *            the unit of the value
     */
    public void addData(String parameter, Object value, Unit<?> unit) {
        addData(parameter, value, null, unit);
    }

    /**
     * Adds data for this IGeometryData
     * 
     * @param parameter
     *            the parameter name
     * @param value
     *            the value of the parameter
     * @param type
     *            the type of the value
     * @param unit
     *            the unit of the value
     */
    public void addData(String parameter, Object value, Type type, Unit<?> unit) {
        GeomData data = new GeomData();
        data.value = value;
        data.unit = unit;
        data.type = type;
        if (data.type == null) {
            if (data.value instanceof String) {
                data.type = Type.STRING;
            } else if (data.value instanceof Double) {
                // TODO do these ifs work or will any number fall into
                // the first one?
                data.type = Type.DOUBLE;
            } else if (data.value instanceof Integer) {
                data.type = Type.INT;
            } else if (data.value instanceof Long) {
                data.type = Type.LONG;
            } else if (data.value instanceof Float) {
                data.type = Type.FLOAT;
            }
        }
        this.dataMap.put(parameter, data);
    }

    /**
     * Add a key/value pair to the attributes map. Attributes are metadata
     * providing additional information on the dataset.
     * 
     * @param key
     * @param value
     */
    public void addAttribute(String key, Object value) {
        attributes.put(key, value);
    }

    public void setGeometry(Geometry geom) {
        this.geometry = geom;
    }

    public void setLevel(Level level) {
        this.level = level;
    }

    public void setDataTime(DataTime time) {
        this.time = time;
    }

    public void setLocationName(String locationName) {
        this.locationName = locationName;
    }

    /**
     * Replace the attribute map with attrs. Attributes are metadata providing
     * additional information on the dataset.
     * 
     * @param attrs
     */
    public void setAttributes(Map<String, Object> attrs) {
        this.attributes = attrs;
    }
}
