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
import com.raytheon.uf.common.dataaccess.grid.IGridData;
import com.raytheon.uf.common.dataaccess.request.GetGridDataRequest;
import com.raytheon.uf.common.dataaccess.response.GetGridDataResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.util.CollectionUtil;

/**
 * Request handler for <code>GetGridDataRequest</code>. Data returned is in form
 * of <code>GetGridDataResponse</code> objects.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 4, 2013             dgilling    Initial creation
 * Oct 18, 2016 5916       bsteffen    Allow lazy loading of lat/lon data
 * 
 * </pre>
 * 
 * @author dgilling
 */
public final class GetGridDataHandler implements
        IRequestHandler<GetGridDataRequest> {

    @Override
    public GetGridDataResponse handleRequest(final GetGridDataRequest request)
            throws Exception {
        IGridData[] gridData = new IGridData[0];

        if (!CollectionUtil.isNullOrEmpty(request.getRequestedTimes())) {
            gridData = DataAccessLayer.getGridData(request
                    .getRequestParameters(), request.getRequestedTimes()
                    .toArray(new DataTime[request.getRequestedTimes().size()]));
        } else {
            gridData = DataAccessLayer.getGridData(
                    request.getRequestParameters(),
                    request.getRequestedPeriod());
        }

        GetGridDataResponse response = new GetGridDataResponse(
                Arrays.asList(gridData), request.isIncludeLatLonData());
        return response;
    }
}
