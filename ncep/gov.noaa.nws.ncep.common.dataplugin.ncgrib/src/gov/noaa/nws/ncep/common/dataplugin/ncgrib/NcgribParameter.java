/*****************************************************************************************
 * COPYRIGHT (c), 2009, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/

package gov.noaa.nws.ncep.common.dataplugin.ncgrib;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Class encapsulating information about a grib parameter
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 4/7/09       1994        bphillip    Initial Creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Entity
@Table(name = "ncgrib_parameters")
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class NcgribParameter implements ISerializableObject {

    /** No subcenter id number */
    public static final int NO_SUBCENTER = -1;

    /** The id assigned from the hashcode of this object */
    @Id
    private int id;

    /** The center which defined this parameter */
    @Column
    @XmlElement
    @DynamicSerializeElement
    private int center;

    /** The subcenter which defined this parameter */
    @Column
    @XmlElement
    @DynamicSerializeElement
    private int subcenter;

    /** The grib discipline number (See Table 0.0) */
    @Column
    @XmlElement
    @DynamicSerializeElement
    private int discipline;

    /** The grib category number (See Table 4.1) */
    @Column
    @XmlElement
    @DynamicSerializeElement
    private int category;

    /** The grib parameter number (See tables 4.2.x) */
    @Column
    @XmlElement
    @DynamicSerializeElement
    private int number;

    /** The name of the parameter */
    @Column
    @XmlElement
    @DynamicSerializeElement
    private String name;

    /** The abbreviation of the parameter */
    @Column
    @XmlElement
    @DynamicSerializeElement
    private String abbreviation;
    
    /** The D2D abbreviation of the parameter */
    @Column
    @XmlElement
    @DynamicSerializeElement
    private String d2dAbbrev;

    /** The unit associated with the parameter, if any */
    @Column
    @XmlElement
    @DynamicSerializeElement
    private String unit;

    /**
     * Creates and empty GribParameter object
     */
    public NcgribParameter() {

    }

    /**
     * Creates a GribParameter object with the provided values
     * 
     * @param center
     *            The defining center
     * @param subcenter
     *            The defining subcenter
     * @param discipline
     *            The parameter discipline
     * @param category
     *            The parameter category
     * @param number
     *            The parameter number
     * @param name
     *            The parameter name
     * @param abbreviation
     *            The parameter abbreviation
     * @param unit
     *            The parameter unit
     */
    public NcgribParameter(int center, int subcenter, int discipline,
            int category, int number, String name, String abbreviation,
            String unit,String d2dAbbrev) {
        this.name = name;
        this.abbreviation = abbreviation;
        this.d2dAbbrev = d2dAbbrev;
        this.unit = unit;
        this.discipline = discipline;
        this.category = category;
        this.number = number;
        this.center = center;
        this.subcenter = subcenter;
        this.id = hashCode();
    }

    public int hashCode() {
        final int PRIME = 31;
        int result = 1;
        result = PRIME * result + center;
        result = PRIME * result + subcenter;
        result = PRIME * result + discipline;
        result = PRIME * result + category;
        result = PRIME * result + number;
        result = PRIME * result + ((name == null) ? 0 : name.hashCode());
        result = PRIME * result
                + ((abbreviation == null) ? 0 : abbreviation.hashCode());
        result = PRIME * result + ((unit == null) ? 0 : unit.hashCode());
        return result;
    }

    public String toString() {
        StringBuffer buf = new StringBuffer();
        buf.append("        Id: ").append(id).append("\n");
        buf.append("    Center: ").append(center).append("\n");
        buf.append(" Subcenter: ").append(subcenter).append("\n");
        buf.append("Discipline: ").append(discipline).append("\n");
        buf.append("  Category: ").append(category).append("\n");
        buf.append("    Number: ").append(number).append("\n");
        buf.append("      Name: ").append(name).append("\n");
        buf.append("    Abbrev: ").append(abbreviation).append("\n");
        buf.append("      unit: ").append(unit).append("\n");
        return buf.toString();
    }

    /**
     * Gets the name
     * 
     * @return The name
     */
    public String getName() {
        return name;
    }

    /**
     * Sets the name
     * 
     * @param name
     *            The name
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * Gets the abbreviation
     * 
     * @return The abbreviation
     */
    public String getAbbreviation() {
        return abbreviation;
    }

    /**
     * Sets the abbreviation
     * 
     * @param abbreviation
     *            The abbreviation
     */
    public void setAbbreviation(String abbreviation) {
        this.abbreviation = abbreviation;
    }

    /**
     * Gets the unit
     * 
     * @return The unit
     */
    public String getUnit() {
        return unit;
    }

    /**
     * Sets the unit
     * 
     * @param unit
     *            The unit
     */
    public void setUnit(String unit) {
        this.unit = unit;
    }

    /**
     * Gets the defining center
     * 
     * @return The defining center
     */
    public int getCenter() {
        return center;
    }

    /**
     * Sets the defining center
     * 
     * @param center
     *            The defining center
     */
    public void setCenter(int center) {
        this.center = center;
    }

    /**
     * Gets the defining subcenter
     * 
     * @return The definign subcenter
     */
    public int getSubcenter() {
        return subcenter;
    }

    /**
     * Sets the defining subcenter
     * 
     * @param subcenter
     *            The definign subcenter
     */
    public void setSubcenter(int subcenter) {
        this.subcenter = subcenter;
    }

    /**
     * Gets the parameter discipline
     * 
     * @return The parameter discipline
     */
    public int getDiscipline() {
        return discipline;
    }

    /**
     * Sets the parameter discipline
     * 
     * @param discipline
     *            The parameter discipline
     */
    public void setDiscipline(int discipline) {
        this.discipline = discipline;
    }

    /**
     * Gets the parameter category
     * 
     * @return The parameter category
     */
    public int getCategory() {
        return category;
    }

    /**
     * Sets the parameter category
     * 
     * @param category
     */
    public void setCategory(int category) {
        this.category = category;
    }

    /**
     * Gets the parameter number
     * 
     * @return The parameter number
     */
    public int getNumber() {
        return number;
    }

    /**
     * Sets the parameter number
     * 
     * @param number
     *            The parameter number
     */
    public void setNumber(int number) {
        this.number = number;
    }

    /**
     * Gets the id
     * 
     * @return The id
     */
    public int getId() {
        return id;
    }

    /**
     * Sets the id
     * 
     * @param id
     *            The id
     */
    public void setId(int id) {
        this.id = id;
    }

    public String getD2dAbbrev() {
        return d2dAbbrev;
    }

    public void setD2dAbbrev(String abbrev) {
        d2dAbbrev = abbrev;
    }
    
    
}
