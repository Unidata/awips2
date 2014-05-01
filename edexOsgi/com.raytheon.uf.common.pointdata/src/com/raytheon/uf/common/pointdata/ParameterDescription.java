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
package com.raytheon.uf.common.pointdata;

import java.text.ParseException;

import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.common.pointdata.PointDataDescription.Type;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * Describes a parameter of point data (units, fillValues, etc.)
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Apr 27, 2009           chammack    Initial creation
 * Jun 22, 2009  2538     jsanchez    Added missing  DynamicSerializeElement.
 * Dec 02, 2013  2537     bsteffen    Remove ISerializableObject
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class ParameterDescription {

    @XmlAttribute(name = "name", required = true)
    @DynamicSerializeElement
    private String parameterName;

    @XmlAttribute(name = "fillValue", required = false)
    @DynamicSerializeElement
    private Double fillValue;

    @XmlAttribute(name = "type", required = true)
    private Type type;

    @DynamicSerializeElement
    @XmlAttribute(name = "unit", required = false)
    private String unit;

    @DynamicSerializeElement
    @XmlAttribute(name = "numDims")
    private int numDims;

    @DynamicSerializeElement
    @XmlAttribute(name = "dimension")
    private String dimension;

    /** Maximum length for strings and sized structs */
    @DynamicSerializeElement
    @XmlAttribute(name = "size")
    private int maxLength;

    /**
     * Should not be stored in the parameter description xml, this is used as as
     * the resolved value, after it has been looked up in the dimension xml by
     * name.
     */
    @DynamicSerializeElement
    @XmlAttribute(name = "dimension")
    private int dimensionAsInt;

    private transient Unit<?> unitObj;

    public ParameterDescription() {
        this.fillValue = -9999.0;
        this.numDims = 1;
    }

    public ParameterDescription(String paramName, Type type) {
        this.parameterName = paramName;
        this.fillValue = -9999.0;
        this.type = type;
        this.numDims = 1;
    }

    public ParameterDescription(String paramName, Type type, Number fillValue) {
        this.parameterName = paramName;
        this.fillValue = fillValue.doubleValue();
        this.type = type;
        this.numDims = 1;
    }

    public ParameterDescription(String paramName, Type type, Number fillValue,
            String unit) {
        this.parameterName = paramName;
        this.fillValue = fillValue.doubleValue();
        this.type = type;
        this.unit = unit;
        this.numDims = 1;
    }

    /**
     * @return the unit
     */
    public String getUnit() {
        return unit;
    }

    /**
     * @param unit
     *            the unit to set
     */
    public void setUnit(String unit) {
        this.unit = unit;
    }

    /**
     * @return the numDims
     */
    public int getNumDims() {
        return numDims;
    }

    /**
     * @param numDims
     *            the numDims to set
     */
    public void setNumDims(int numDims) {
        this.numDims = numDims;
    }

    /**
     * @return the parameterName
     */
    public String getParameterName() {
        return parameterName;
    }

    /**
     * @param parameterName
     *            the parameterName to set
     */
    public void setParameterName(String parameterName) {
        this.parameterName = parameterName;
    }

    /**
     * @return the fillValue
     */
    public Double getFillValue() {
        return fillValue;
    }

    /**
     * @param fillValue
     *            the fillValue to set
     */
    public void setFillValue(Double fillValue) {
        this.fillValue = fillValue;
    }

    public Unit<?> getUnitObject() {
        if (this.unit == null)
            return Unit.ONE;

        if (this.unitObj == null) {
            try {
                this.unitObj = (Unit<?>) UnitFormat.getUCUMInstance()
                        .parseObject(this.unit);
            } catch (ParseException e) {
                // logger.warn("ParseException while parsing unit string: "
                // + this.unit + " defaulting to unit: " + Unit.ONE);
                this.unitObj = Unit.ONE;
            }
        }

        return this.unitObj;
    }

    /**
     * @return the type
     */
    public Type getType() {
        return type;
    }

    /**
     * @param type
     *            the type to set
     */
    public void setType(Type type) {
        this.type = type;
    }

    /**
     * @return the dimension
     */
    public String getDimension() {
        return dimension;
    }

    /**
     * @param dimension
     *            the dimension to set
     */
    public void setDimension(String dimension) {
        this.dimension = dimension;
    }

    /**
     * @return the dimensionAsInt
     */
    public int getDimensionAsInt() {
        return dimensionAsInt;
    }

    /**
     * @param dimensionAsInt
     *            the dimensionAsInt to set
     */
    public void setDimensionAsInt(int dimensionAsInt) {
        this.dimensionAsInt = dimensionAsInt;
    }

    /**
     * @return the maxLength
     */
    public int getMaxLength() {
        return maxLength;
    }

    /**
     * @param maxLength
     *            the maxLength to set
     */
    public void setMaxLength(int maxLength) {
        this.maxLength = maxLength;
    }

}