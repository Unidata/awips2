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

import java.text.ParseException;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;
import javax.persistence.Transient;

import com.raytheon.uf.common.pointdata.IPointDataContainerReader;
import com.raytheon.uf.common.pointdata.IPointDataViewReader;
import com.raytheon.uf.common.pointdata.PointDataDescription.Type;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * GFE Point Data View
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 4, 2009            njensen     Initial creation
 * Apr 23, 2014  #3006    randerso    Added toString to aid in debugging
 *                                    Fixed conversion of Double to float
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

@DynamicSerialize
public class GFEPointDataView implements IPointDataViewReader {

    @DynamicSerializeElement
    private Map<String, GFEPointDataValue> valueMap = new HashMap<String, GFEPointDataValue>();

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

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.pointdata.IPointDataViewReader#getContainer()
     */
    @Override
    public IPointDataContainerReader getContainer() {
        return parent;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.pointdata.IPointDataViewReader#getFloat(java.lang
     * .String)
     */
    @Override
    public float getFloat(String parameter) {
        GFEPointDataValue pd = valueMap.get(parameter);
        if (pd != null) {
            Type type = pd.getType();
            Object obj = pd.getValue();
            if (type == Type.FLOAT) {
                return ((Number) obj).floatValue();
            } else if (type == Type.STRING) {
                return Float.valueOf((String) obj);
            } else {
                return Float.valueOf((Long) obj);
            }
        } else {
            throw new IllegalArgumentException("Parameter not present: "
                    + parameter);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.pointdata.IPointDataViewReader#getInt(java.lang
     * .String)
     */
    @Override
    public int getInt(String parameter) {
        GFEPointDataValue pd = valueMap.get(parameter);
        if (pd != null) {
            Type type = pd.getType();
            Object obj = pd.getValue();
            if (type == Type.LONG) {
                return (Integer) obj;
            } else if (type == Type.STRING) {
                return Integer.valueOf((String) obj);
            } else {
                return Integer.valueOf((Integer) obj);
            }
        } else {
            throw new IllegalArgumentException("Parameter not present: "
                    + parameter);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.pointdata.IPointDataViewReader#getLong(java.lang
     * .String)
     */
    @Override
    public long getLong(String parameter) {
        GFEPointDataValue pd = valueMap.get(parameter);
        if (pd != null) {
            Type type = pd.getType();
            Object obj = pd.getValue();
            if (type == Type.LONG) {
                return (Long) obj;
            } else if (type == Type.STRING) {
                return Long.valueOf((String) obj);
            } else {
                return Long.valueOf((Long) obj);
            }
        } else {
            throw new IllegalArgumentException("Parameter not present: "
                    + parameter);
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.pointdata.IPointDataViewReader#getNumber(java.
     * lang.String)
     */
    @Override
    public Number getNumber(String parameter) {
        GFEPointDataValue pd = valueMap.get(parameter);
        if (pd != null) {
            Object obj = pd.getValue();
            if (pd.getType() != Type.STRING) {
                return (Number) obj;
            } else {
                return Float.valueOf((String) obj);
            }
        } else {
            throw new IllegalArgumentException("Parameter not present: "
                    + parameter);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.pointdata.IPointDataViewReader#getString(java.
     * lang.String)
     */
    @Override
    public String getString(String parameter) {
        GFEPointDataValue pd = valueMap.get(parameter);
        if (pd != null) {
            Type type = pd.getType();
            Object obj = pd.getValue();
            if (type == Type.STRING) {
                return (String) obj;
            } else {
                return obj.toString();
            }
        } else {
            throw new IllegalArgumentException("Parameter not present: "
                    + parameter);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.pointdata.IPointDataViewReader#getType(java.lang
     * .String)
     */
    @Override
    public Type getType(String parameter) {
        if (valueMap.containsKey(parameter)) {
            return valueMap.get(parameter).getType();
        } else {
            throw new IllegalArgumentException("Parameter not present: "
                    + parameter);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.pointdata.IPointDataViewReader#getUnit(java.lang
     * .String)
     */
    @Override
    public Unit<?> getUnit(String parameter) {
        if (valueMap.containsKey(parameter)) {
            String unit = valueMap.get(parameter).getUnit();
            if (unit != null) {
                try {
                    return (Unit<?>) UnitFormat.getUCUMInstance().parseObject(
                            unit);
                } catch (ParseException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
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

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return valueMap.toString();
    }

}
