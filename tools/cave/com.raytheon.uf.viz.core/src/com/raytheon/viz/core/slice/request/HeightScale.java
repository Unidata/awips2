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
package com.raytheon.viz.core.slice.request;

import java.util.ArrayList;
import java.util.List;

import javax.measure.unit.Unit;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.adapters.UnitAdapter;
import com.raytheon.uf.viz.core.style.level.Level.LevelType;

/**
 * Height scale used by volume browser
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 6, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class HeightScale implements ISerializableObject {

    @XmlAccessorType(XmlAccessType.NONE)
    public static enum ScaleType {
        LOG, LIN;
    }

    @XmlAttribute
    private String unit;

    @XmlAttribute
    private String name;

    @XmlAttribute
    private float minVal;

    @XmlAttribute
    private float maxVal;

    @XmlAttribute
    private String parameter;

    @XmlElement
    private String labels;

    @XmlAttribute
    @XmlJavaTypeAdapter(value = UnitAdapter.class)
    private Unit<?> parameterUnit = Unit.ONE;

    @XmlAttribute
    private ScaleType scale;

    @XmlAttribute
    private LevelType heightType;

    public HeightScale() {

    }

    public String getUnit() {
        return unit;
    }

    public void setUnit(String unit) {
        this.unit = unit;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public float getMinVal() {
        return minVal;
    }

    public void setMinVal(float minVal) {
        this.minVal = minVal;
    }

    public float getMaxVal() {
        return maxVal;
    }

    public void setMaxVal(float maxVal) {
        this.maxVal = maxVal;
    }

    public ScaleType getScale() {
        return scale;
    }

    public void setScale(ScaleType scale) {
        this.scale = scale;
    }

    public LevelType getHeightType() {
        return heightType;
    }

    public void setHeightType(LevelType heightType) {
        this.heightType = heightType;
    }

    /**
     * @return the parameter
     */
    public String getParameter() {
        return parameter;
    }

    /**
     * @param parameter
     *            the parameter to set
     */
    public void setParameter(String parameter) {
        this.parameter = parameter;
    }

    /**
     * @return the parameterUnit
     */
    public Unit<?> getParameterUnit() {
        return parameterUnit;
    }

    /**
     * @param parameterUnit
     *            the parameterUnit to set
     */
    public void setParameterUnit(Unit<?> parameterUnit) {
        this.parameterUnit = parameterUnit;
    }

    /**
     * @return the labels
     */
    public String getLabels() {
        return labels;
    }

    /**
     * @param labels
     *            the labels to set
     */
    public void setLabels(String labels) {
        this.labels = labels;
    }

    public List<Double> getLabelsList() {
        if (labels == null) {
            return null;
        }
        List<Double> list = new ArrayList<Double>();
        for (String label : labels.split(",")) {
            list.add(Double.parseDouble(label));
        }
        return list;
    }

    private int increment = -1;

    public int getIncrement() {
        // TODO: Add increment serialized field to HeightScale
        if (increment == -1) {
            float diff = getDifference();
            if (diff <= 1000) {
                increment = 25;
            } else if (diff <= 5000) {
                increment = 250;
            } else {
                increment = 1000;
            }
        }
        return increment;
    }

    public float getDifference() {
        return Math.abs(minVal - maxVal);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((heightType == null) ? 0 : heightType.hashCode());
        result = prime * result + increment;
        result = prime * result + ((labels == null) ? 0 : labels.hashCode());
        result = prime * result + Float.floatToIntBits(maxVal);
        result = prime * result + Float.floatToIntBits(minVal);
        result = prime * result + ((name == null) ? 0 : name.hashCode());
        result = prime * result
                + ((parameter == null) ? 0 : parameter.hashCode());
        result = prime * result
                + ((parameterUnit == null) ? 0 : parameterUnit.hashCode());
        result = prime * result + ((scale == null) ? 0 : scale.hashCode());
        result = prime * result + ((unit == null) ? 0 : unit.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        HeightScale other = (HeightScale) obj;
        if (heightType != other.heightType)
            return false;
        if (increment != other.increment)
            return false;
        if (labels == null) {
            if (other.labels != null)
                return false;
        } else if (!labels.equals(other.labels))
            return false;
        if (Float.floatToIntBits(maxVal) != Float.floatToIntBits(other.maxVal))
            return false;
        if (Float.floatToIntBits(minVal) != Float.floatToIntBits(other.minVal))
            return false;
        if (name == null) {
            if (other.name != null)
                return false;
        } else if (!name.equals(other.name))
            return false;
        if (parameter == null) {
            if (other.parameter != null)
                return false;
        } else if (!parameter.equals(other.parameter))
            return false;
        if (parameterUnit == null) {
            if (other.parameterUnit != null)
                return false;
        } else if (!parameterUnit.equals(other.parameterUnit))
            return false;
        if (scale != other.scale)
            return false;
        if (unit == null) {
            if (other.unit != null)
                return false;
        } else if (!unit.equals(other.unit))
            return false;
        return true;
    }

}
