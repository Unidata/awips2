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
package com.raytheon.uf.edex.site.handlers;

import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.site.requests.GetActiveSitesRequest;
import com.raytheon.uf.edex.site.SiteAwareRegistry;

/**
 * Activate Site Handler
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 05, 2011            randerso     Initial creation
 * Mar 19, 2015  #4300     randerso     Changed return type to what is actually returned
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class GetActiveSitesHandler implements
        IRequestHandler<GetActiveSitesRequest> {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.comm.IRequestHandler#handleRequest
     * (com.raytheon.uf.common.serialization.comm.IServerRequest)
     */
    @Override
    public String[] handleRequest(GetActiveSitesRequest request)
            throws Exception {
        return SiteAwareRegistry.getInstance().getActiveSites();
    }
}
