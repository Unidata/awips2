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

import java.util.Arrays;
import java.util.List;

import com.raytheon.edex.plugin.gfe.server.GridParmManager;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.request.GetGridDataRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.server.request.GetGridRequest;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.util.SizeUtil;

/**
 * GFE task for getting grid data slices
 * 
 * May return less than the full amount of data requested if returned grids
 * exceed MAX_BYTES_PER_REQUEST in total size. The requestor is expected to
 * re-request remaining data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/18/08     #875       bphillip    Initial Creation
 * 09/22/09     3058       rjpeter     Converted to IRequestHandler
 * 06/13/13     2044       randerso    Refactored to use IFPServer
 * 07/01/2014  #3149       randerso    Changed to limit size of data returned
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
public class GetGridDataHandler extends BaseGfeRequestHandler implements
        IRequestHandler<GetGridDataRequest> {

    private int byteLimitInMB = 100;

    /**
     * @return the byteLimitInMB
     */
    public int getByteLimitInMB() {
        return this.byteLimitInMB;
    }

    /**
     * @param byteLimitInMB
     *            the byteLimitInMB to set
     */
    public void setByteLimitInMB(int byteLimitInMB) {
        this.byteLimitInMB = byteLimitInMB;
    }

    @Override
    public ServerResponse<List<IGridSlice>> handleRequest(
            GetGridDataRequest request) throws Exception {
        ServerResponse<List<IGridSlice>> sr = new ServerResponse<List<IGridSlice>>();

        GridParmManager gpm = getIfpServer(request).getGridParmMgr();
        int gridCount = 0;
        int remaining = (int) (this.byteLimitInMB * SizeUtil.BYTES_PER_MB * 0.9);
        List<IGridSlice> data = null;
        for (GetGridRequest req : request.getRequests()) {
            ParmID parmId = req.getParmId();
            List<TimeRange> times = req.getTimes();

            ServerResponse<GridParmInfo> ss1 = gpm.getGridParmInfo(parmId);
            if (!ss1.isOkay()) {
                sr.addMessages(ss1);
                return sr;
            }
            GridParmInfo gpi = ss1.getPayload();

            int gridSize = gpi.getGridLoc().getNx() * gpi.getGridLoc().getNy();
            switch (gpi.getGridType()) {
            case SCALAR:
                gridSize *= 4;
                break;

            case VECTOR:
                gridSize *= 8;
                break;

            case WEATHER:
            case DISCRETE:
                break;

            default:
                break;
            }

            int maxGrids = remaining / gridSize;
            // ensure we return at least 1 grid
            if ((maxGrids == 0) && (gridCount == 0)) {
                maxGrids = 1;
            }

            // no more grids will fit break out of loop
            if (maxGrids == 0) {
                break;
            }

            if (maxGrids < times.size()) {
                // truncate the request
                times = times.subList(0, maxGrids);
                req.setTimes(times);
            }
            gridCount += times.size();
            remaining -= times.size() * gridSize;

            ServerResponse<List<IGridSlice>> ssr = gpm.getGridData(Arrays
                    .asList(req));
            if (ssr.isOkay()) {
                if (data == null) {
                    data = ssr.getPayload();
                } else {
                    data.addAll(ssr.getPayload());
                }
            } else {
                sr.addMessages(ssr);
                break;
            }
        }

        if (sr.isOkay()) {
            sr.setPayload(data);
        }
        return sr;
    }
}
