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

import com.raytheon.uf.common.activetable.RestoreLastEtnRequest;
import com.raytheon.uf.common.activetable.response.RestoreLastEtnResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * Request handler for <code>RestoreLastEtnRequest</code>. Returns the next ETN to
 * use for the given office id, phensig, and active table (PRACTICE or
 * OPERATIONAL).
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 02, 2018  20557     ryu         Initial creation
 * 
 * </pre>
 * 
 * @author ryu
 * @version 1.0
 */

public class RestoreLastEtnHandler implements IRequestHandler<RestoreLastEtnRequest> {

    @Override
    public RestoreLastEtnResponse handleRequest(RestoreLastEtnRequest request)
            throws Exception {
        RestoreLastEtnResponse response = GetNextEtnUtil.restoreLastEtn(
                request.getSiteID(), request.getMode(), request.getPhensig(),
                request.getCurrentTime(), 
                request.isPerformISC(), request.isReportConflictOnly(),
                request.getEtnOverride());
        return response;
    }
}

