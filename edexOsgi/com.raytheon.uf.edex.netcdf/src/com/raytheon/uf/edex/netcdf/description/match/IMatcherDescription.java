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
package com.raytheon.uf.edex.netcdf.description.match;

import ucar.nc2.NetcdfFile;

import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;

/**
 * Interface for matchers.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 19, 2016 5584       nabowle     Initial creation
 *
 * </pre>
 *
 * @author nabowle
 */
public interface IMatcherDescription {
    /**
     * Indicates if the netcdf file matches this description.
     *
     * @param file
     *            The netcdf file to check.
     * @return True if the file matches this description, false otherwise.
     * @throws InvalidDescriptionException
     *             if this description is invalid.
     */
    boolean matches(NetcdfFile file) throws InvalidDescriptionException;

    /**
     * Validate this description.
     * 
     * @throws InvalidDescriptionException
     *             if this description is invalid.
     */
    void validate() throws InvalidDescriptionException;
}
