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
package com.raytheon.uf.edex.plugin.text.handler;

import com.raytheon.uf.common.dataplugin.text.request.RemoteRetrievalRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.edex.plugin.text.handler.RemoteRetrievalManager.RRJob;

/**
 * Request handler for RemoteRetrievalRequests. Forwards request to
 * RemoteRetrievalManager for delegation.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * ???                                 Initial creation
 * May 20, 2014 2536       bclement    moved from edex.textdb to edex.plugin.text
 * 
 * </pre>
 * 
 * @version 1.0
 */
public class RemoteRetrievalHandler implements
    IRequestHandler<RemoteRetrievalRequest> {

    private RemoteRetrievalManager rrManager;
    
    @Override
    public Object handleRequest(RemoteRetrievalRequest request)
            throws Exception {
        RRJob job = rrManager.submitRequest(request);
        return job.waitResponse();
    }

    /**
     * @return the rrManager
     */
    public RemoteRetrievalManager getRrManager() {
        return rrManager;
    }

    /**
     * @param rrManager the rrManager to set
     */
    public void setRrManager(RemoteRetrievalManager rrManager) {
        this.rrManager = rrManager;
    }

}
