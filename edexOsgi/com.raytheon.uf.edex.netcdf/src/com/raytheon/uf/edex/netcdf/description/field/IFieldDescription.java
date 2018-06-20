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
package com.raytheon.uf.edex.netcdf.description.field;

import ucar.nc2.NetcdfFile;

import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;

/**
 * Interface for Field Descriptions.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 19, 2016 5548       nabowle     Initial creation
 *
 * </pre>
 *
 * @author nabowle
 */

public interface IFieldDescription {

    /**
     * Gets a String from a scalar field.
     */
    String getString(NetcdfFile file)
            throws InvalidDescriptionException;

    /**
     * Gets the String at the supplied index from an n-dimensional field viewed
     * as a 1 dimensional array. Scalar fields will always return the same
     * String for any index.
     */
    String getString(NetcdfFile file, int index)
            throws InvalidDescriptionException;

    /**
     * Gets a Number value, if possible, from a scalar field.
     */
    Number getNumber(NetcdfFile file)
            throws InvalidDescriptionException;

    /**
     * Gets the Number at the supplied index from an n-dimensional field viewed
     * as a 1 dimensional array. Scalar fields will always return the same
     * Number for any index.
     */
    Number getNumber(NetcdfFile file, int index)
            throws InvalidDescriptionException;

    /**
     * Get the total number of elements for a field.
     * 
     * @throws InvalidDescriptionException
     */
    long getLength(NetcdfFile file) throws InvalidDescriptionException;

    /**
     * Indicates if the described field is numeric.
     *
     * @throws InvalidDescriptionException
     *             if this description is incorrectly configured, or if
     *             isPresent() would return false
     */
    boolean isNumeric(NetcdfFile file) throws InvalidDescriptionException;

    /**
     * Indicates if the described field is present in the file. This should
     * indicate if at least one of the getString or getNumber methods will
     * return a value.
     *
     * @throws InvalidDescriptionException
     *             if this description is incorrectly configured.
     */
    boolean isPresent(NetcdfFile file) throws InvalidDescriptionException;

    /**
     * Validate this field.
     *
     * @throws InvalidDescriptionException
     */
    void validate() throws InvalidDescriptionException;

    String getName();
}
