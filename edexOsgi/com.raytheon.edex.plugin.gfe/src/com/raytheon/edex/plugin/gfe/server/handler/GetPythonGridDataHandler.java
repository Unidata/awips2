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
package com.raytheon.edex.plugin.gfe.server.handler;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.edex.plugin.gfe.server.GridParmManager;
import com.raytheon.uf.common.dataplugin.gfe.request.GetPythonGridDataRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.PythonWeatherGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.WeatherGridSlice;
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
 * Aug 4, 2011            dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

// TODO: REMOVE THIS CLASS AND ITS REQUEST TYPE if
// DiscreteDefinition/DiscreteKey and WxDefinition/WeatherKey class hierarchy is
// ever fully-implemented in Python.

public class GetPythonGridDataHandler implements
        IRequestHandler<GetPythonGridDataRequest> {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.comm.IRequestHandler#handleRequest
     * (com.raytheon.uf.common.serialization.comm.IServerRequest)
     */
    @Override
    public ServerResponse<List<IGridSlice>> handleRequest(
            GetPythonGridDataRequest request) throws Exception {
        ServerResponse<List<IGridSlice>> finalResp = new ServerResponse<List<IGridSlice>>();

        ServerResponse<List<IGridSlice>> sr = GridParmManager
                .getGridData(request.getRequests());
        if (!sr.isOkay()) {
            finalResp.addMessages(sr);
            finalResp.setPayload(new ArrayList<IGridSlice>(0));
            return finalResp;
        }

        // convert grid slices as needed
        List<IGridSlice> slices = new ArrayList<IGridSlice>(sr.getPayload()
                .size());
        for (IGridSlice slice : sr.getPayload()) {
            if (!(slice instanceof WeatherGridSlice)) {
                slices.add(slice);
            } else {
                slices.add(new PythonWeatherGridSlice((WeatherGridSlice) slice));
            }
        }
        finalResp.setPayload(slices);

        return finalResp;
    }
}
