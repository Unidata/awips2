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
package com.raytheon.uf.edex.activetable.handler;

import com.raytheon.uf.common.activetable.request.UnlockAndSetNextEtnRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.activetable.GetNextEtnUtil;

/**
 * Request handler that unlocks the active table for a specific phensig and
 * office ID combination and sets the next ETN to be used. Should only be
 * activated by another EDEX server.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 19, 2013  #1843     dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class UnlockActiveTablePhenSigHandler implements
        IRequestHandler<UnlockAndSetNextEtnRequest> {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(UnlockActiveTablePhenSigHandler.class);

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.comm.IRequestHandler#handleRequest
     * (com.raytheon.uf.common.serialization.comm.IServerRequest)
     */
    @Override
    public Boolean handleRequest(UnlockAndSetNextEtnRequest request)
            throws Exception {
        statusHandler.info("Received UnlockAndSetNextEtnRequest from ["
                + request.getRequestorSiteID() + "] for site ["
                + request.getSiteID() + "]: phensig= " + request.getPhensig()
                + ", nextETN= " + request.getNewEtn());
        GetNextEtnUtil.setNextEtnAndUnlock(request.getSiteID(),
                request.getMode(), request.getPhensig(), request.getYear(),
                request.getNewEtn());
        return Boolean.TRUE;
    }
}
