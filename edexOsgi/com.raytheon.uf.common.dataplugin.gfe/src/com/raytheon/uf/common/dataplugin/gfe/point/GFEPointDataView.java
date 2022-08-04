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
package com.raytheon.uf.common.dataplugin.gfe.point;

import java.text.ParsePosition;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import javax.measure.Unit;
import javax.measure.format.ParserException;
import javax.persistence.Transient;

import com.raytheon.uf.common.pointdata.IPointDataContainerReader;
import com.raytheon.uf.common.pointdata.IPointDataViewReader;
import com.raytheon.uf.common.pointdata.PointDataDescription.Type;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

import tec.uom.se.format.SimpleUnitFormat;

/**
 * GFE Point Data View to support AvnFPS
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 4, 2009            njensen     Initial creation
 * Apr 23, 2014  #3006    randerso    Added toString to aid in debugging
 *                                    Fixed conversion of Double to float
 * Nov 02, 2016   5979    njensen     Cleanup and cast to Number where applicable
 * 
 * </pre>
 * 
 * @author njensen
 */

@DynamicSerialize
public class GFEPointDataView implements IPointDataViewReader {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GFEPointDataView.class);

    @DynamicSerializeElement
    private Map<String, GFEPointDataValue> valueMap = new HashMap<>();

    @Transient
    private GFEPointDataContainer parent;

    public void setData(String parameter, Type type, Unit<?> unit, Object value) {
        GFEPointDataValue data = new GFEPointDataValue();
        data.setType(type);
        if (unit != null) {
            data.setUnit(unit.toString());
        }
        data.setValue(value);
        valueMap.put(parameter, data);
    }

    public Set<String> getParameters() {
        return valueMap.keySet();
    }

    @Override
    public IPointDataContainerReader getContainer() {
        return parent;
    }

    @Override
    public float getFloat(String parameter) {
        GFEPointDataValue pd = valueMap.get(parameter);
        if (pd != null) {
            Type type = pd.getType();
            Object obj = pd.getValue();
            if (type == Type.STRING) {
                return Float.valueOf(obj.toString());
            }

            return ((Number) obj).floatValue();
        }

        throw new IllegalArgumentException(
                "Parameter not present: " + parameter);
    }

    @Override
    public int getInt(String parameter) {
        GFEPointDataValue pd = valueMap.get(parameter);
        if (pd != null) {
            Type type = pd.getType();
            Object obj = pd.getValue();
            if (type == Type.STRING) {
                return Integer.valueOf(obj.toString());
            }

            return ((Number) obj).intValue();
        }

        throw new IllegalArgumentException(
                "Parameter not present: " + parameter);
    }

    @Override
    public long getLong(String parameter) {
        GFEPointDataValue pd = valueMap.get(parameter);
        if (pd != null) {
            Type type = pd.getType();
            Object obj = pd.getValue();
            if (type == Type.STRING) {
                return Long.valueOf(obj.toString());
            }

            return ((Number) obj).longValue();
        }

        throw new IllegalArgumentException(
                "Parameter not present: " + parameter);
    }

    @Override
    public Number getNumber(String parameter) {
        GFEPointDataValue pd = valueMap.get(parameter);
        if (pd != null) {
            Object obj = pd.getValue();
            if (pd.getType() == Type.STRING) {
                return Float.valueOf(obj.toString());
            }

            return (Number) obj;
        }

        throw new IllegalArgumentException(
                "Parameter not present: " + parameter);
    }

    @Override
    public String getString(String parameter) {
        GFEPointDataValue pd = valueMap.get(parameter);
        if (pd != null) {
            Object obj = pd.getValue();
            return obj.toString();
        }

        throw new IllegalArgumentException(
                "Parameter not present: " + parameter);
    }

    @Override
    public Type getType(String parameter) {
        if (valueMap.containsKey(parameter)) {
            return valueMap.get(parameter).getType();
        }

        throw new IllegalArgumentException(
                "Parameter not present: " + parameter);
    }

    @Override
    public Unit<?> getUnit(String parameter) {
        if (valueMap.containsKey(parameter)) {
            String unit = valueMap.get(parameter).getUnit();
            if (unit != null) {
                try {
                    return (Unit<?>) SimpleUnitFormat
                            .getInstance(SimpleUnitFormat.Flavor.ASCII)
                            .parseObject(unit, new ParsePosition(0));
                } catch (ParserException e) {
                    statusHandler.error("Error parsing unit " + unit, e);
                }
            }
        }
        return null;
    }

    public Map<String, GFEPointDataValue> getValueMap() {
        return valueMap;
    }

    public void setValueMap(Map<String, GFEPointDataValue> valueMap) {
        this.valueMap = valueMap;
    }

    public void setContainer(GFEPointDataContainer container) {
        parent = container;
    }

    @Override
    public String toString() {
        return valueMap.toString();
    }

}
