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

import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.uf.common.dataplugin.gfe.request.GetKnownOfficeTypesRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * GFE Task for getting office types known to the GFE server
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 07/15/09      1995       bphillip    Initial release
 * 09/22/09      3058       rjpeter     Converted to IRequestHandler
 * 11/18/15      5129       dgilling    Rewritten to better match A1.
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class GetKnownOfficeTypesHandler extends BaseGfeRequestHandler implements
        IRequestHandler<GetKnownOfficeTypesRequest> {

    @Override
    public ServerResponse<Map<String, String>> handleRequest(
            GetKnownOfficeTypesRequest request) throws Exception {
        IFPServerConfig config = getIfpServer(request).getConfig();
        List<String> ids = config.allSites();
        List<String> officeTypes = config.officeTypes();
        Map<String, String> sitesWithOfficeType = new HashMap<>();
        for (int i = 0; i < ids.size(); i++) {
            sitesWithOfficeType.put(ids.get(i), officeTypes.get(i));
        }

        ServerResponse<Map<String, String>> sr = new ServerResponse<>();
        sr.setPayload(sitesWithOfficeType);
        return sr;
    }
}
