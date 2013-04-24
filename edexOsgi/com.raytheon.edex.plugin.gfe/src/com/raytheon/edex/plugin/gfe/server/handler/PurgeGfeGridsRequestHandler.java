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

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import jep.JepException;

import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.dataplugin.gfe.request.PurgeGfeGridsRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.edex.site.SiteAwareRegistry;

/**
 * Request handler for PurgeGfeGrids. Will execute the purgeAllGrids.py script
 * with the supplied argument string from the request.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 23, 2010            dgilling     Initial creation
 * Mar 07, 2013  1759      dgilling     Refactored to remove dependency
 *                                      on GfeScriptExecutor.
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class PurgeGfeGridsRequestHandler implements
        IRequestHandler<PurgeGfeGridsRequest> {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PurgeGfeGridsRequestHandler.class);

    private static final String SCRIPT_PATH = FileUtil.join(
            GfePyIncludeUtil.ISC, "purgeAllGrids.py");

    private static final String METHOD_NAME = "executeFromJava";

    @Override
    public ServerResponse<Boolean> handleRequest(PurgeGfeGridsRequest request)
            throws Exception {
        ServerResponse<Boolean> sr = new ServerResponse<Boolean>();
        sr.setPayload(Boolean.FALSE);

        List<String> siteList = Arrays.asList(SiteAwareRegistry.getInstance()
                .getActiveSites());
        if (!siteList.contains(request.getSiteID())) {
            sr.addMessage("DatabaseID " + request.getDatabaseID()
                    + " is unknown.");
            return sr;
        }

        PythonScript script = null;
        try {
            IPathManager pathMgr = PathManagerFactory.getPathManager();
            LocalizationContext cx = pathMgr.getContext(
                    LocalizationType.EDEX_STATIC, LocalizationLevel.BASE);
            String scriptPath = pathMgr.getFile(cx, SCRIPT_PATH).getPath();
            String includePath = PyUtil.buildJepIncludePath(
                    GfePyIncludeUtil.getCommonPythonIncludePath(),
                    GfePyIncludeUtil.getIscScriptsIncludePath());
            script = new PythonScript(scriptPath, includePath);
            try {
                Map<String, Object> args = new HashMap<String, Object>();
                args.put("databaseID", request.getDatabaseID().toString());
                script.execute(METHOD_NAME, args);
            } catch (JepException e) {
                String msg = "Error purging data from DatabaseID ["
                        + request.getDatabaseID() + "]";
                statusHandler.handle(Priority.PROBLEM, msg, e);
                sr.addMessage(msg + ": " + e.getLocalizedMessage());
                return sr;
            }
        } catch (JepException e) {
            String msg = "Error initializing purgeAllGrids python script";
            statusHandler.handle(Priority.PROBLEM, msg, e);
            sr.addMessage(msg + ": " + e.getLocalizedMessage());
            return sr;
        } finally {
            if (script != null) {
                script.dispose();
            }
        }

        sr.setPayload(Boolean.TRUE);
        return sr;
    }
}
