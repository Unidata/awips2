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
package gov.noaa.nws.ncep.edex.plugin.ncgrib.handler;

import gov.noaa.nws.ncep.edex.plugin.ncgrib.spatial.NcgribSpatialCache;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.request.GetNcCoverageRequest;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.spatial.projections.NcgridCoverage;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 8, 2010            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class GetNcCoverageHandler implements IRequestHandler<GetNcCoverageRequest> {

    @Override
    public NcgridCoverage handleRequest(GetNcCoverageRequest request) {
        return NcgribSpatialCache.getInstance().getGrid(request.getModelName());
    }
}
