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
import java.util.concurrent.Future;

import com.raytheon.edex.plugin.gfe.isc.IscScript;
import com.raytheon.edex.plugin.gfe.isc.IscScriptExecutor;
import com.raytheon.edex.plugin.gfe.server.IFPServer;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.request.ExecuteIfpNetCDFGridRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.python.concurrent.PythonJobCoordinator;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * {@link IRequestHandler} class for {@link ExecuteIfpNetCDFGridRequest} that
 * executes the python script ifpnetCDF to export GFE grids to the NetCDF file
 * format.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 24, 2010            dgilling     Initial creation
 * Mar 11, 2013  #1759     dgilling     Re-write using new IscScript classes.
 * Nov 20, 2014  #98       lshi			ifpnetCDF no longer gives an error code in some cases
 * May 13, 2015  #4427     dgilling     Add siteIdOverride field.
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class ExecuteIfpNetCDFGridRequestHandler implements
        IRequestHandler<ExecuteIfpNetCDFGridRequest> {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ExecuteIfpNetCDFGridRequestHandler.class);

    private static final String METHOD_NAME = "main";

    private final PythonJobCoordinator<IscScript> threadPool;

    public ExecuteIfpNetCDFGridRequestHandler(
            PythonJobCoordinator<IscScript> threadPool) {
        this.threadPool = threadPool;
    }

    @Override
    public ServerResponse<Boolean> handleRequest(
            ExecuteIfpNetCDFGridRequest request) throws Exception {
        ServerResponse<Boolean> sr = new ServerResponse<Boolean>();
        sr.setPayload(Boolean.FALSE);

        // #98 verify requested database exists
        IFPServer ifpServer = IFPServer.getActiveServer(request.getDatabaseID()
                .getSiteId());
        ServerResponse<java.util.List<DatabaseID>> ssr = ifpServer
                .getGridParmMgr().getDbInventory();
        if (!ssr.isOkay()) {
            sr.addMessages(ssr);
            return sr;
        } else if (!ssr.getPayload().contains(request.getDatabaseID())) {
            sr.addMessage(request.getDatabaseID() + " does not exist");
            return sr;
        }

        statusHandler.debug("Received ifpnetCDF request: " + request);
        Map<String, Object> args = new HashMap<String, Object>();
        args.put("outputFilename", request.getOutputFilename());
        args.put("parmList", request.getParmList());
        args.put("databaseID", request.getDatabaseID().toString());
        args.put("startTime", request.getStartTime());
        args.put("endTime", request.getEndTime());
        args.put("mask", request.getMask());
        args.put("geoInfo", request.isGeoInfo());
        args.put("compressFileFlag", request.isCompressFile());
        args.put("configFileName", request.getConfigFileName());
        args.put("compressFileFactor", request.getCompressFileFactor());
        args.put("trim", request.isTrim());
        args.put("krunch", request.isKrunch());
        args.put("userID", request.getUserID());
        args.put("logFileName", request.getLogFileName());
        args.put("siteIdOverride", request.getSiteIdOverride());
        IscScriptExecutor executor = new IscScriptExecutor(METHOD_NAME,
                request.getSiteID(), args);
        Future<String> task = threadPool.submitJob(executor);

        String retVal = task.get();
        if (retVal != null) {
            sr.addMessage(retVal);
            return sr;
        }

        sr.setPayload(Boolean.TRUE);
        return sr;
    }
}
