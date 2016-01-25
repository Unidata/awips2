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
package com.raytheon.uf.edex.netcdf.description;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlSeeAlso;

import ucar.nc2.NetcdfFile;

import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;

/**
 * Base class for field descriptions.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Aug 26, 2015  4699     nabowle   Initial creation
 * Sep 09, 2015  4696     nabowle   Add retrieval at different indices and
 *                                  getLength().
 * Dec 08, 2015  5059     nabowle   Add isNumeric() and isPresent().
 * Jan 25, 2016  5208     bsteffen  Add validation.
 * 
 * </pre>
 * 
 * @author nabowle
 * @version 1.0
 */
@XmlSeeAlso({ AttributeDescription.class, VariableDescription.class,
        ValueDescription.class })
@XmlAccessorType(XmlAccessType.NONE)
public abstract class AbstractFieldDescription {

    @XmlAttribute
    protected String name;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    /**
     * Gets a String from a scalar field.
     */
    public abstract String getString(NetcdfFile file)
            throws InvalidDescriptionException;

    /**
     * Gets the String at the supplied index from an n-dimensional field viewed
     * as a 1 dimensional array. Scalar fields will always return the same
     * String for any index.
     */
    public abstract String getString(NetcdfFile file, int index)
            throws InvalidDescriptionException;

    /**
     * Gets a Number value, if possible, from a scalar field.
     */
    public abstract Number getNumber(NetcdfFile file)
            throws InvalidDescriptionException;

    /**
     * Gets the Number at the supplied index from an n-dimensional field viewed
     * as a 1 dimensional array. Scalar fields will always return the same
     * Number for any index.
     */
    public abstract Number getNumber(NetcdfFile file, int n)
            throws InvalidDescriptionException;

    /**
     * Get the total number of elements for a field.
     */
    public abstract long getLength(NetcdfFile file);

    /**
     * Indicates if the described field is numeric.
     * 
     * @throws InvalidDescriptionException
     *             if this description is incorrectly configured, or if
     *             isPresent() would return false
     */
    public abstract boolean isNumeric(NetcdfFile file)
            throws InvalidDescriptionException;

    /**
     * Indicates if the described field is present in the file. This should
     * indicate if at least one of the getString or getNumber methods will
     * return a value.
     *
     * @throws InvalidDescriptionException
     *             if this description is incorrectly configured.
     */
    public abstract boolean isPresent(NetcdfFile file)
            throws InvalidDescriptionException;

    public void validate() throws InvalidDescriptionException {
        if (name == null) {
            throw new InvalidDescriptionException(
                    "The name attribute is not present.");
        }
    }

}
