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
package com.raytheon.uf.edex.dataaccess.handler;

import java.util.Arrays;

import com.raytheon.uf.common.dataaccess.DataAccessLayer;
import com.raytheon.uf.common.dataaccess.geom.IGeometryData;
import com.raytheon.uf.common.dataaccess.request.GetGeometryDataRequest;
import com.raytheon.uf.common.dataaccess.response.GetGeometryDataResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.util.CollectionUtil;

/**
 * Request handler for <code>GetGeometryDataRequest</code>. Data returned is in
 * form of <code>GetGeometryDataResponse</code> objects.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 30, 2013            dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public final class GetGeometryDataHandler implements
        IRequestHandler<GetGeometryDataRequest> {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.comm.IRequestHandler#handleRequest
     * (com.raytheon.uf.common.serialization.comm.IServerRequest)
     */
    @Override
    public GetGeometryDataResponse handleRequest(
            final GetGeometryDataRequest request) throws Exception {
        IGeometryData[] geoData = new IGeometryData[0];

        if (!CollectionUtil.isNullOrEmpty(request.getRequestedTimes())) {
            geoData = DataAccessLayer.getGeometryData(request
                    .getRequestParameters(), request.getRequestedTimes()
                    .toArray(new DataTime[request.getRequestedTimes().size()]));
        } else {
            geoData = DataAccessLayer.getGeometryData(
                    request.getRequestParameters(),
                    request.getRequestedPeriod());
        }

        GetGeometryDataResponse response = new GetGeometryDataResponse(
                Arrays.asList(geoData));
        return response;
    }
}
