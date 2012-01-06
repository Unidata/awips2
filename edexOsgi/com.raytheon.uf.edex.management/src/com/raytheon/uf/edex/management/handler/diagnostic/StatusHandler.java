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
package com.raytheon.uf.edex.management.handler.diagnostic;

import com.raytheon.uf.common.management.request.diagnostic.StatusRequest;
import com.raytheon.uf.common.management.response.diagnostic.StatusResponse;
import com.raytheon.uf.common.management.stats.IStatistics;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.edex.management.MgmtUtil;

/**
 * Returns the hostname and jvm name along stats specific to the JVM
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 9, 2010            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class StatusHandler implements IRequestHandler<StatusRequest> {

    @Override
    public Object handleRequest(StatusRequest request) throws Exception {
        StatusResponse resp = new StatusResponse();
        resp.setHostname(MgmtUtil.getHostname());
        resp.setJvmName(MgmtUtil.getRunMode());
        resp.setStatistics(getStatistics());

        return resp;
    }

    private IStatistics getStatistics() {
        // TODO return something relevant to this particular JVM
        return null;
    }

}
