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

import org.apache.commons.lang.builder.HashCodeBuilder;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Class encapsulating information about a grib surface.
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
@Table(name = "ncgrib_surfaces")
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class NcgribLevel implements ISerializableObject {

    /** The id generated from the hashcode of this object */
    @Id
    private int id;

    /** The defining center */
    @Column
    @XmlElement
    @DynamicSerializeElement
    private int center;

    @Column
    @XmlElement
    @DynamicSerializeElement
    private int subcenter;

    /** The surface number */
    @Column
    @XmlElement
    @DynamicSerializeElement
    private int number;

    /** The surface name */
    @Column
    @XmlElement
    @DynamicSerializeElement
    private String name;

    /** The surface abbreviation */
    @Column
    @XmlElement
    @DynamicSerializeElement
    private String abbreviation;

    /** The D2D surface abbreviation */
    @Column
    @XmlElement
    @DynamicSerializeElement
    private String d2dAbbrev;

    /** The surface unit */
    @Column
    @XmlElement
    @DynamicSerializeElement
    private String unit;

    /**
     * Creates an empty GribSurface object
     */
    public NcgribLevel() {

    }

    /**
     * Creates a GribSurface object with the provided information
     * 
     * @param center
     *            The defining center
     * @param number
     *            The surface number
     * @param name
     *            The surface name
     * @param unit
     *            The surface unit
     * @param abbreviation
     *            The surface abbreviation
     */
    public NcgribLevel(int center, int subcenter, int number, String name, String unit,
            String abbreviation, String d2dAbbrev) {
        this.center = center;
        this.subcenter = subcenter;
        this.number = number;
        this.name = name;
        this.unit = unit;
        this.abbreviation = abbreviation;
        this.d2dAbbrev = d2dAbbrev;
        this.id = hashCode();
    }

    public int hashCode() {
        HashCodeBuilder builder = new HashCodeBuilder();
        builder.append(center);
        builder.append(number);
        return builder.toHashCode();
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
     * Gets the surface number
     * 
     * @return The surface number
     */
    public int getNumber() {
        return number;
    }

    /**
     * Sets the surface number
     * 
     * @param number
     *            The surface number
     */
    public void setNumber(int number) {
        this.number = number;
    }

    /**
     * Gets the surface name
     * 
     * @return The surface name
     */
    public String getName() {
        return name;
    }

    /**
     * Sets the surface name
     * 
     * @param name
     *            The surface name
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * Gets the surface abbreviation
     * 
     * @return The surface abbreviation
     */
    public String getAbbreviation() {
        return abbreviation;
    }

    /**
     * Sets the surface abbreviation
     * 
     * @param abbreviation
     *            The surface abbreviation
     */
    public void setAbbreviation(String abbreviation) {
        this.abbreviation = abbreviation;
    }

    /**
     * Gets the surface unit
     * 
     * @return The surface unit
     */
    public String getUnit() {
        return unit;
    }

    /**
     * Sets the surface unit
     * 
     * @param unit
     *            The surface unit
     */
    public void setUnit(String unit) {
        this.unit = unit;
    }

    /**
     * Gets the d2d abbreviation for this level if one exists
     * 
     * @return The d2d abbreviation
     */
    public String getD2dAbbrev() {
        return d2dAbbrev;
    }

    /**
     * Sets the d2d abbreviation for this level
     * 
     * @param abbrev
     *            The d2d abbreviation
     */
    public void setD2dAbbrev(String abbrev) {
        d2dAbbrev = abbrev;
    }

    /**
     * Gets the subcenter id for this level
     * 
     * @return The subcenter id
     */
    public int getSubcenter() {
        return subcenter;
    }

    /**
     * Sets the subcenter id for this level
     * 
     * @param subcenter
     *            The subcenter id
     */
    public void setSubcenter(int subcenter) {
        this.subcenter = subcenter;
    }

}
