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

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import com.raytheon.edex.plugin.gfe.server.IFPServer;
import com.raytheon.uf.common.dataplugin.gfe.request.GetSiteTimeZoneInfoRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.util.CollectionUtil;

/**
 * Returns the time zones associated with the given sites. The site must be
 * activated to get a result.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jan 19, 2011           dgilling  Initial creation
 * Feb 26, 2015  4128     dgilling  Switch to IFPServer.getActiveSites().
 * Nov 17, 2015  5129     dgilling  Support changes to
 *                                  GetSiteTimeZoneInfoRequest.
 * Jun 17, 2016  5703     dgilling  Allow null or empty requestedSiteIDs.
 * Sep 12, 2016  5861     randerso  Remove references to IFPServerConfigManager
 *                                  which was largely redundant with IFPServer.
 *
 * </pre>
 *
 * @author dgilling
 */

public class GetSiteTimeZoneInfoRequestHandler
        implements IRequestHandler<GetSiteTimeZoneInfoRequest> {

    @Override
    public ServerResponse<Map<String, String>> handleRequest(
            GetSiteTimeZoneInfoRequest request) throws Exception {
        ServerResponse<Map<String, String>> sr = new ServerResponse<Map<String, String>>();
        Map<String, String> siteWithTimeZone = new HashMap<String, String>();

        Set<String> activeSites = IFPServer.getActiveSites();
        Collection<String> requestedSites = request.getRequestedSiteIDs();
        if (CollectionUtil.isNullOrEmpty(requestedSites)) {
            requestedSites = activeSites;
        }

        for (String site : requestedSites) {
            IFPServer ifpServer = IFPServer.getActiveServer(site);
            if (ifpServer != null) {
                siteWithTimeZone.put(site,
                        ifpServer.getConfig().getTimeZones().get(0));
            } else {
                String message = String.format(
                        "Unknown site id: %s Known sites: %s", site,
                        activeSites);
                sr.addMessage(message);
                siteWithTimeZone = Collections.emptyMap();
                break;
            }
        }

        sr.setPayload(siteWithTimeZone);
        return sr;
    }
}
