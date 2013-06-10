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
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.request.GetParmListRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * GFE task to get a list of parms valid for the specified databaseIDs
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/08/08     #875       bphillip    Initial Creation
 * 09/22/09     3058       rjpeter     Converted to IRequestHandler
 * 05/02/13     #1969      randerso    Fixed null pointer if getParmList fails
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class GetParmListHandler implements IRequestHandler<GetParmListRequest> {
    @Override
    public ServerResponse<List<ParmID>> handleRequest(GetParmListRequest request)
            throws Exception {

        List<ParmID> retVal = new ArrayList<ParmID>();
        ServerResponse<List<ParmID>> sr = new ServerResponse<List<ParmID>>();
        for (DatabaseID id : request.getDbIds()) {
            ServerResponse<List<ParmID>> ssr = GridParmManager.getParmList(id);
            if (ssr.isOkay()) {
                retVal.addAll(ssr.getPayload());
            } else {
                sr.addMessages(ssr);
            }
        }
        if (sr.isOkay()) {
            sr.setPayload(retVal);
        }
        return sr;
    }
}
