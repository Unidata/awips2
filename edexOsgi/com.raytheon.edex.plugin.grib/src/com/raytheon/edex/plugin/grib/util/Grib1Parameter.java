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

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * A class representing a grib 1 parameter
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Mar 09, 2010  4758     bphillip    Initial Creation
 * Oct 15, 2013  2473     bsteffen    Remove deprecated ISerializableObject.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@XmlRootElement
@DynamicSerialize
@XmlAccessorType(XmlAccessType.NONE)
public class Grib1Parameter {

    /** The id assigned from the hashcode of this object */
    private int id;

    /** The center defining this parameter */
    @XmlElement
    @DynamicSerializeElement
    private int center;

    /** The grib 1 parameter table version */
    @XmlElement
    @DynamicSerializeElement
    private int grib1TableVersion;

    /** The grib 1 parameter parameter value */
    @XmlElement
    @DynamicSerializeElement
    private int grib1Value;

    /** The grib 2 Discipline of this parameter */
    @XmlElement
    @DynamicSerializeElement
    private int grib2discipline;

    /** The grib 2 Category of this parameter */
    @XmlElement
    @DynamicSerializeElement
    private int grib2category;

    /** The grib 2 Parameter number of this parameter */
    @XmlElement
    @DynamicSerializeElement
    private int grib2Value;

    /**
     * Creates an empty Grib1Parameter
     */
    public Grib1Parameter() {

    }

    /**
     * Creates a new Grib1Parameter
     * 
     * @param center
     *            The center ID
     * @param grib1TableVersion
     *            The grib 1 table version
     * @param grib1Value
     *            The grib 1 table value
     * @param grib2Discipline
     *            The equivalent grib 2 discipline
     * @param grib2Category
     *            The equivalent grib 2 category
     * @param grib2Value
     *            The equivalent grib 2 table value
     */
    public Grib1Parameter(int center, int grib1TableVersion, int grib1Value,
            int grib2Discipline, int grib2Category, int grib2Value) {
        this.center = center;
        this.grib1TableVersion = grib1TableVersion;
        this.grib1Value = grib1Value;
        this.grib2discipline = grib2Discipline;
        this.grib2category = grib2Category;
        this.grib2Value = grib2Value;
        this.id = hashCode();

    }

    /**
     * Generates the unique id using the hashCode
     */
    public void generateId() {
        this.id = hashCode();
    }

    public int hashCode() {
        HashCodeBuilder builder = new HashCodeBuilder();
        builder.append(center);
        builder.append(grib1TableVersion);
        builder.append(grib1Value);
        builder.append(grib2discipline);
        builder.append(grib2category);
        builder.append(grib2Value);
        return builder.toHashCode();
    }

    /**
     * @return the id
     */
    public int getId() {
        return id;
    }

    /**
     * @return the center
     */
    public int getCenter() {
        return center;
    }

    /**
     * @param center
     *            the center to set
     */
    public void setCenter(int center) {
        this.center = center;
    }

    /**
     * @return the grib1TableVersion
     */
    public int getGrib1TableVersion() {
        return grib1TableVersion;
    }

    /**
     * @param grib1TableVersion
     *            the grib1TableVersion to set
     */
    public void setGrib1TableVersion(int grib1TableVersion) {
        this.grib1TableVersion = grib1TableVersion;
    }

    /**
     * @return the grib1Value
     */
    public int getGrib1Value() {
        return grib1Value;
    }

    /**
     * @param grib1Value
     *            the grib1Value to set
     */
    public void setGrib1Value(int grib1Value) {
        this.grib1Value = grib1Value;
    }

    /**
     * @return the grib2discipline
     */
    public int getGrib2discipline() {
        return grib2discipline;
    }

    /**
     * @param grib2discipline
     *            the grib2discipline to set
     */
    public void setGrib2discipline(int grib2discipline) {
        this.grib2discipline = grib2discipline;
    }

    /**
     * @return the grib2category
     */
    public int getGrib2category() {
        return grib2category;
    }

    /**
     * @param grib2category
     *            the grib2category to set
     */
    public void setGrib2category(int grib2category) {
        this.grib2category = grib2category;
    }

    /**
     * @return the grib2Value
     */
    public int getGrib2Value() {
        return grib2Value;
    }

    /**
     * @param grib2Value
     *            the grib2Value to set
     */
    public void setGrib2Value(int grib2Value) {
        this.grib2Value = grib2Value;
    }

}
