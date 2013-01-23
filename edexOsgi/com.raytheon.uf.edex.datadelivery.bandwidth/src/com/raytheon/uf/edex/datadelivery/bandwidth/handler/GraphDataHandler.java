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
package com.raytheon.uf.edex.datadelivery.bandwidth.handler;

import com.raytheon.uf.common.datadelivery.bandwidth.IBandwidthService;
import com.raytheon.uf.common.datadelivery.bandwidth.request.GraphDataRequest;
import com.raytheon.uf.common.datadelivery.bandwidth.response.GraphDataResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 25, 2012   2369     mpduff      Initial creation.
 * Dec 06, 2012   1397     djohnson    Delegate to the bandwidth service.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class GraphDataHandler implements IRequestHandler<GraphDataRequest> {

    private final IBandwidthService bandwidthService;

    public GraphDataHandler(IBandwidthService bandwidthService) {
        this.bandwidthService = bandwidthService;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Object handleRequest(GraphDataRequest request) throws Exception {
        return getResponse();
    }

    /**
     * Get the response object.
     *
     * @return GraphDataResponse
     */
    private GraphDataResponse getResponse() {
        GraphDataResponse response = new GraphDataResponse();
        response.setGraphData(bandwidthService.getBandwidthGraphData());

        return response;
    }
}
