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
package com.raytheon.viz.aviation.guidance;

import com.raytheon.uf.common.dataplugin.gfe.point.GFEPointDataContainers;
import com.raytheon.uf.common.dataplugin.gfe.request.GetPointDataRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;

/**
 * Static utility methods for use with python code.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 29, 2009            njensen     Initial creation
 * Mar 11, 2013 1735       rferrel     Get a list of GFE Point Data Containers
 * Sep 11, 2013 2277       mschenke    Got rid of ScriptCreator references
 * Apr 23, 2014 3006       randerso    Added error logging
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class GuidanceUtil {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GuidanceUtil.class);

    /**
     * Get a list of GFE Point Data information for the task request.
     * 
     * @param task
     * @return gfePointDataContainers
     * @throws VizException
     */
    @SuppressWarnings("unchecked")
    public static GFEPointDataContainers getGFEPointsData(
            GetPointDataRequest task) throws VizException {
        task.setWorkstationID(VizApp.getWsId());
        ServerResponse<GFEPointDataContainers> sr = (ServerResponse<GFEPointDataContainers>) ThriftClient
                .sendRequest(task);
        if (!sr.isOkay()) {
            // some kind of error occurred on the server side
            statusHandler.error(sr.message());
        }
        return sr.getPayload();
    }

}
