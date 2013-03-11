package com.raytheon.edex.plugin.gfe.server.handler;

import java.util.HashMap;
import java.util.Map;

import com.raytheon.edex.plugin.gfe.isc.IscScript;
import com.raytheon.edex.plugin.gfe.isc.IscScriptExecutor;
import com.raytheon.edex.site.SiteUtil;
import com.raytheon.uf.common.dataplugin.gfe.request.ExecuteIscMosaicRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.python.concurrent.PythonJobCoordinator;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * {@link IRequestHandler} class for {@link ExecuteIscMosaicRequest} that
 * executes the python script iscMosaic, which imports GFE grids from a NetCDF
 * file.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 15, 2010            dgilling     Initial creation
 * Mar 12, 2013  #1759     dgilling     Re-write using new IscScript classes.
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */
public class ExecuteIscMosaicRequestHandler implements
        IRequestHandler<ExecuteIscMosaicRequest> {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ExecuteIscMosaicRequest.class);

    private static final String METHOD_NAME = "main";

    private final PythonJobCoordinator<IscScript> threadPool;

    public ExecuteIscMosaicRequestHandler(
            PythonJobCoordinator<IscScript> threadPool) {
        this.threadPool = threadPool;
    }

    @Override
    public ServerResponse<Boolean> handleRequest(ExecuteIscMosaicRequest request)
            throws Exception {
        ServerResponse<Boolean> sr = new ServerResponse<Boolean>();
        sr.setPayload(Boolean.FALSE);

        statusHandler.debug("Received iscMosaic request: " + request);
        Map<String, Object> args = new HashMap<String, Object>();
        args.put("siteID", determineSiteId(request.getSiteID()));
        args.put("userID", request.getUserID());
        args.put("databaseID", request.getDatabaseID().toString());
        args.put("parmsToProcess", request.getParmsToProcess());
        args.put("blankOtherPeriods", request.isBlankOtherPeriods());
        args.put("startTime", request.getStartTime());
        args.put("endTime", request.getEndTime());
        args.put("altMask", request.getAltMask());
        args.put("replaceOnly", request.isReplaceOnly());
        args.put("eraseFirst", request.isEraseFirst());
        args.put("announce", request.getAnnounce());
        args.put("renameWE", request.isRenameWE());
        args.put("iscSends", request.isIscSends());
        args.put("inFiles", request.getInFiles());
        args.put("ignoreMask", request.isIgnoreMask());
        args.put("adjustTranslate", request.isAdjustTranslate());
        args.put("deleteInput", request.isDeleteInput());
        args.put("parmsToIgnore", request.getParmsToIgnore());
        args.put("gridDelay", request.getGridDelay());
        args.put("logFileName", request.getLogFileName());

        IscScriptExecutor executor = new IscScriptExecutor(METHOD_NAME,
                request.getSiteID(), args);
        String retVal = threadPool.submitSyncJob(executor);

        if (retVal != null) {
            sr.addMessage(retVal);
            return sr;
        }

        sr.setPayload(Boolean.TRUE);
        return sr;
    }

    private String determineSiteId(String inputSite) {
        if ((inputSite == null) || inputSite.isEmpty()) {
            return SiteUtil.getSite();
        } else {
            return inputSite;
        }
    }
}
