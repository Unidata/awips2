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

package com.raytheon.uf.common.parameter;

import java.text.ParseException;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * Defines a parameter as a unique abbreviation, a human readable name, and a
 * unit.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 12, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@DynamicSerialize
@Entity
@Table(name = "parameter")
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
public class Parameter extends PersistableDataObject implements
        ISerializableObject {

    private static final long serialVersionUID = 4866022583462757340L;

    @Id
    @DynamicSerializeElement
    @XmlElement
    @DataURI(position = 0)
    private String abbreviation;

    @Column(nullable = false)
    @DynamicSerializeElement
    @XmlElement
    private String name;

    @Column(name = "unit")
    @DynamicSerializeElement
    @XmlElement(name = "unit")
    private String unitString = "";

    private transient Unit<?> unit = null;

    public Parameter() {

    }

    public Parameter(String abbreviation) {
        this.abbreviation = abbreviation;
        this.name = abbreviation;
    }

    public Parameter(String abbreviation, Unit<?> unit) {
        this(abbreviation);
        this.setUnit(unit);
    }

    public Parameter(String abbrevation, String name) {
        this(abbrevation);
        this.name = name;
    }

    public Parameter(String abbrevation, String name, Unit<?> unit) {
        this(abbrevation, name);
        this.setUnit(unit);
    }

    public Parameter(String abbrevation, String name, String unitString) {
        this(abbrevation, name);
        this.setUnitString(unitString);
    }

    public String getAbbreviation() {
        return abbreviation;
    }

    public void setAbbreviation(String abbreviation) {
        this.abbreviation = abbreviation;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getUnitString() {
        return unitString;
    }

    public void setUnitString(String unitString) {
        this.unitString = unitString;
        this.unit = null;
    }

    public Unit<?> getUnit() {
        if (unit == null) {
            if (unitString != null && unitString.length() > 0) {
                try {
                    unit = (Unit<?>) UnitFormat.getUCUMInstance().parseObject(
                            unitString);
                } catch (ParseException e) {
                    // UFStatus.getHandler().handle(Priority.WARN,
                    // "Unable to parse Parameter unit string: " + unitString);
                    unit = Unit.ONE;
                }
            } else {
                unit = Unit.ONE;
            }
        }
        return unit;
    }

    public void setUnit(Unit<?> unit) {
        this.unit = unit;
        if (unit.equals(Unit.ONE) || unit == null) {
            this.unitString = "";
        } else {
            this.unitString = UnitFormat.getUCUMInstance().format(unit);
        }
    }

    /**
     * Compare the unit and unit string of two parameters and return a converter
     * or null if they are incompatible. In addition to checking the units for
     * convertability it will also return an IdentityConverter if the
     * unitStrings are equal, even if they are not parseable as a unit object.
     * 
     * @param sourceParameter
     * @param destParameter
     * @return a converter that can convert the units of sourceParameter to the
     *         units of destParameter
     */
    public static UnitConverter compareUnits(Parameter sourceParameter,
            Parameter destParameter) {
        String srcString = sourceParameter.getUnitString();
        String destString = destParameter.getUnitString();
        Unit<?> srcUnit = sourceParameter.getUnit();
        Unit<?> destUnit = destParameter.getUnit();

        if (srcString == null) {
            if (destString == null) {
                // Both strings are null, that is the same units
                return UnitConverter.IDENTITY;
            } else {
                // src string is null and dest is not, not compatible.
                return null;
            }
        } else if (srcString.equals(destString)) {
            // unit strings are equal, thay are the same
            return UnitConverter.IDENTITY;
        } else if (srcUnit == null || destUnit == null) {
            // One or both units are null and the strings are different, not
            // compatible
            return null;
        } else if (srcUnit.isCompatible(destUnit)) {
            // regular unit converter
            return srcUnit.getConverterTo(destUnit);
        } else {
            // strings are different and units are incompatible.
            return null;
        }
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((abbreviation == null) ? 0 : abbreviation.hashCode());
        result = prime * result + ((name == null) ? 0 : name.hashCode());
        result = prime * result
                + ((unitString == null) ? 0 : unitString.hashCode());
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
        Parameter other = (Parameter) obj;
        if (abbreviation == null) {
            if (other.abbreviation != null)
                return false;
        } else if (!abbreviation.equals(other.abbreviation))
            return false;
        if (name == null) {
            if (other.name != null)
                return false;
        } else if (!name.equals(other.name))
            return false;
        if (unitString == null) {
            if (other.unitString != null)
                return false;
        } else if (!unitString.equals(other.unitString))
            return false;
        return true;
    }

    @Override
    public String toString() {
        return "Parameter [abbreviation=" + abbreviation + ", name=" + name
                + ", unitString=" + unitString + "]";
    }

}
