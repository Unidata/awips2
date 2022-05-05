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
package com.raytheon.uf.edex.plugin.grid.netcdf.description.coverage;

import ucar.nc2.NetcdfFile;

import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;

/**
 * Describes a coverage field and provides a means to update a given
 * GridCoverage from a NetcdfFile.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 14, 2015 4469       nabowle     Initial creation
 * Jun 09, 2016 5584       nabowle     Renamed
 *
 * </pre>
 *
 * @author nabowle
 * @version 1.0
 */

public interface ICoverageFieldDescription {
    void updateCoverage(GridCoverage coverage, NetcdfFile file)
            throws InvalidDescriptionException;

    void validate() throws InvalidDescriptionException;
}
