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
import java.util.Map;
import java.util.Set;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfigManager;
import com.raytheon.edex.plugin.gfe.server.IFPServer;
import com.raytheon.uf.common.dataplugin.gfe.request.GetSiteTimeZoneInfoRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
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
 * Jan 19, 2011            dgilling     Initial creation
 * Feb 26, 2015  #4128     dgilling     Switch to IFPServer.getActiveSites().
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class GetSiteTimeZoneInfoRequestHandler implements
        IRequestHandler<GetSiteTimeZoneInfoRequest> {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.comm.IRequestHandler#handleRequest
     * (com.raytheon.uf.common.serialization.comm.IServerRequest)
     */
    @Override
    public ServerResponse<Map<String, String>> handleRequest(
            GetSiteTimeZoneInfoRequest request) throws Exception {
        ServerResponse<Map<String, String>> sr = new ServerResponse<Map<String, String>>();

        Set<String> sites = IFPServer.getActiveSites();
        Map<String, String> siteWithTimeZone = new HashMap<String, String>();
        for (String site : sites) {
            // getTimeZones() seems to only ever return a 1 sized List
            // containing the site's time zone
            siteWithTimeZone.put(site,
                    IFPServerConfigManager.getServerConfig(site).getTimeZones()
                            .get(0));
        }
        sr.setPayload(siteWithTimeZone);

        return sr;
    }

}
