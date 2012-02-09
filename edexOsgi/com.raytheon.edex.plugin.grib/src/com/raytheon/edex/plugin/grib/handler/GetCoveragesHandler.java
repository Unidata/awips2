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
package com.raytheon.edex.plugin.grib.handler;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.edex.plugin.grib.spatial.GribSpatialCache;
import com.raytheon.uf.common.dataplugin.grib.request.GetCoveragesRequest;
import com.raytheon.uf.common.dataplugin.grib.spatial.projections.GridCoverage;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * 
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 20, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GetCoveragesHandler implements
        IRequestHandler<GetCoveragesRequest> {

    @Override
    public List<GridCoverage> handleRequest(GetCoveragesRequest request) {
        List<GridCoverage> result = new ArrayList<GridCoverage>();
        for (String modelName : request.getModelNames()) {
            result.add(GribSpatialCache.getInstance().getGrid(modelName));
        }
        return result;
    }
}
