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
package com.raytheon.uf.edex.plugin.goesr.geospatial.crs;

import org.opengis.parameter.ParameterValueGroup;

import ucar.nc2.Variable;

import com.raytheon.uf.edex.plugin.goesr.exception.GoesrProjectionException;

/**
 * A class representation of the GOES-R LambertConformal projection information
 * contained in the GOES-R netCDF file.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jun 01, 2012  796      jkorman     Initial creation
 * Jul 05, 2013  2123     mschenke    Refactored to be CRS factory
 * Apr 17, 2015  4336     bsteffen    Converted to be only a CRS factory.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class LambertConformalCrsFactory extends GoesrCrsFactory {

    @Override
    protected String getProjectionName() {
        return "Lambert_Conformal_Conic_1SP";
    }

    @Override
    protected void addSpecificParameters(Variable projectionVariable,
            ParameterValueGroup parameters) throws GoesrProjectionException {
        setDoubleParameter(projectionVariable, "standard_parallel", parameters,
                "latitude_of_origin");
        setDoubleParameter(projectionVariable, "longitude_of_central_meridian",
                parameters, "longitude_of_origin");
    }

}
