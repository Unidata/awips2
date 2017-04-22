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
package com.raytheon.uf.edex.activetable;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.activetable.GetFourCharSitesRequest;
import com.raytheon.uf.common.activetable.GetFourCharSitesResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.site.SiteMap;

/**
 * Convert a list of 3-char site IDs to 4-char site IDs. A whole list of IDs is
 * converted to avoid the overhead of sending a separate request and response
 * for every site ID needing converted.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 3, 2010            wldougher     Initial creation
 * 
 * </pre>
 * 
 * @author wldougher
 * @version 1.0
 */

public class GetFourCharSitesHandler implements
        IRequestHandler<GetFourCharSitesRequest> {

    @Override
    public GetFourCharSitesResponse handleRequest(
            GetFourCharSitesRequest request) throws Exception {
        List<String> newSites = new ArrayList<String>();
        for (String oldSite : request.getSites()) {
            String newSite = null;
            // Make sure we only convert the 3-char site IDs
            // even if the list contains nulls or 4-char IDs
            if (oldSite != null && oldSite.length() == 3) {
                newSite = SiteMap.getInstance().getSite4LetterId(oldSite);
            } else {
                newSite = oldSite;
            }
            newSites.add(newSite);
        }

        GetFourCharSitesResponse response = new GetFourCharSitesResponse();
        response.setSites(newSites);
        return response;
    }
}
