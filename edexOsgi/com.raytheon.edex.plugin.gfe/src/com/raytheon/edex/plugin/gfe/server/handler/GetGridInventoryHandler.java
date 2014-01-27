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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.request.GetGridInventoryRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.time.TimeRange;

/**
 * GFE task to get the GFERecord inventory for the provided list of parmIDs
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/18/08     #875       bphillip    Initial Creation
 * 09/22/09     3058       rjpeter     Converted to IRequestHandler
 * 06/06/13     #2073      dgilling    Ensure payload is always populated.
 * 06/13/13     2044       randerso     Refactored to use IFPServer
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class GetGridInventoryHandler extends BaseGfeRequestHandler implements
        IRequestHandler<GetGridInventoryRequest> {
    @Override
    public ServerResponse<Map<ParmID, List<TimeRange>>> handleRequest(
            GetGridInventoryRequest request) throws Exception {
        ServerResponse<Map<ParmID, List<TimeRange>>> sr = new ServerResponse<Map<ParmID, List<TimeRange>>>();
        Map<ParmID, List<TimeRange>> inventory = new HashMap<ParmID, List<TimeRange>>();
        for (ParmID parmId : request.getParmIds()) {
            ServerResponse<List<TimeRange>> timeSr = getIfpServer(request)
                    .getGridParmMgr().getGridInventory(parmId);
            List<TimeRange> times = timeSr.getPayload();
            inventory.put(parmId, times);
            sr.addMessages(timeSr);
        }

        sr.setPayload(inventory);
        return sr;
    }
}
