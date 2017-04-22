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

import java.util.List;
import java.util.Set;

import com.raytheon.edex.plugin.gfe.server.IFPServer;
import com.raytheon.edex.plugin.gfe.smartinit.IFPDB;
import com.raytheon.edex.plugin.gfe.smartinit.IFPWE;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
import com.raytheon.uf.common.dataplugin.gfe.request.PurgeGfeGridsRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.TimeRange;

/**
 * Request handler for PurgeGfeGrids. Will delete all grids from all parms for
 * the {@link DatabaseID} specified in the request.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 23, 2010            dgilling     Initial creation
 * Mar 07, 2013  1759      dgilling     Refactored to remove dependency
 *                                      on GfeScriptExecutor.
 * Jun 13, 2013     #2044  randerso     Refactored to use IFPServer
 * Sep 05, 2013  #2307     dgilling     Use better PythonScript constructor.
 * Jul 14, 2016  #5747     dgilling     Rewrite without python.
 * 
 * </pre>
 * 
 * @author dgilling
 */

public class PurgeGfeGridsRequestHandler implements
        IRequestHandler<PurgeGfeGridsRequest> {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PurgeGfeGridsRequestHandler.class);

    @Override
    public ServerResponse<Boolean> handleRequest(PurgeGfeGridsRequest request)
            throws Exception {
        statusHandler.info("PurgeAllGrids starting");

        ServerResponse<Boolean> sr = new ServerResponse<>();
        sr.setPayload(Boolean.FALSE);

        Set<String> siteList = IFPServer.getActiveSites();
        if (!siteList.contains(request.getSiteID())) {
            sr.addMessage("DatabaseID " + request.getDatabaseID()
                    + " is unknown.");
            return sr;
        }

        String dbId = request.getDatabaseID().toString();
        statusHandler.info("Purging all grids from: " + dbId);

        try {
            IFPDB db = new IFPDB(dbId);
            List<String> parms = db.getKeys();

            for (String p : parms) {
                IFPWE we = db.getItem(p);
                List<TimeRange> inv = we.getKeys();
                for (TimeRange i : inv) {
                    we.removeItem(i);
                }
            }
        } catch (GfeException e) {
            String msg = "PurgeAllGrids caught exception: "
                    + e.getLocalizedMessage();
            statusHandler.error(msg, e);
            sr.addMessage(msg);
            return sr;
        }

        statusHandler.info("PurgeAllGrids finished");

        sr.setPayload(Boolean.TRUE);
        return sr;
    }
}
