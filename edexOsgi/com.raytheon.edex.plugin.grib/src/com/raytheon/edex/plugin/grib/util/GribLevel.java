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

package com.raytheon.edex.plugin.grib.util;

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
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class GribLevel implements ISerializableObject {

    /** The id generated from the hashcode of this object */
    private int id;

    /** The defining center */
    @XmlElement
    @DynamicSerializeElement
    private int center;

    @XmlElement
    @DynamicSerializeElement
    private int subcenter;

    /** The surface number */
    @XmlElement
    @DynamicSerializeElement
    private int number;

    /** The surface name */
    @XmlElement
    @DynamicSerializeElement
    private String name;

    /** The surface abbreviation */
    @XmlElement
    @DynamicSerializeElement
    private String abbreviation;

    /** The D2D surface abbreviation */
    @XmlElement
    @DynamicSerializeElement
    private String d2dAbbrev;

    /** The surface unit */
    @XmlElement
    @DynamicSerializeElement
    private String unit;

    /**
     * Creates an empty GribSurface object
     */
    public GribLevel() {

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
    public GribLevel(int center, int subcenter, int number, String name,
            String unit, String abbreviation, String d2dAbbrev) {
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
