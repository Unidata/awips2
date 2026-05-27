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

import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
import com.raytheon.uf.common.dataplugin.gfe.request.GetOfficialDbNameRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * GFE task to get official db name
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Apr 08, 0008  875      bphillip  Initial Creation
 * Sep 22, 0009  3058     rjpeter   Converted to IRequestHandler
 * Sep 12, 2016  5861     randerso  Remove references to IFPServerConfigManager
 *                                  which was largely redundant with IFPServer.
 *
 * </pre>
 *
 * @author bphillip
 */
public class GetOfficialDbNameHandler extends BaseGfeRequestHandler
        implements IRequestHandler<GetOfficialDbNameRequest> {

    @Override
    public ServerResponse<List<DatabaseID>> handleRequest(
            GetOfficialDbNameRequest request) throws Exception {
        ServerResponse<List<DatabaseID>> sr = new ServerResponse<List<DatabaseID>>();
        List<DatabaseID> databases = new ArrayList<DatabaseID>();
        try {
            databases = getIfpServer(request).getConfig()
                    .getOfficialDatabases();
        } catch (GfeException e) {
            sr.addMessage("No active IFPServer for " + request.getSiteID());
        }
        sr.setPayload(databases);
        return sr;
    }

}
