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

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.request.GetGridParmInfoRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * GFE task to get the parm info for a specified list of parmIDs
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/08/08     #875       bphillip    Initial Creation
 * 09/22/09     3058       rjpeter     Converted to IRequestHandler
 * 06/13/13     2044       randerso     Refactored to use IFPServer
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class GetGridParmInfoHandler extends BaseGfeRequestHandler implements
        IRequestHandler<GetGridParmInfoRequest> {
    @Override
    public ServerResponse<List<GridParmInfo>> handleRequest(
            GetGridParmInfoRequest request) throws Exception {
        List<GridParmInfo> parmInfo = new ArrayList<GridParmInfo>();
        ServerResponse<List<GridParmInfo>> sr = new ServerResponse<List<GridParmInfo>>();
        for (ParmID parmId : request.getParmIds()) {
            ServerResponse<GridParmInfo> ssr = getIfpServer(request)
                    .getGridParmMgr().getGridParmInfo(parmId);
            if (ssr.isOkay()) {
                parmInfo.add(ssr.getPayload());
            }
            sr.addMessages(ssr);
        }
        if (sr.isOkay()) {
            sr.setPayload(parmInfo);
        }
        return sr;
    }
}
