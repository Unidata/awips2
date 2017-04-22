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

import java.util.HashMap;
import java.util.Map;

import jep.JepException;

import com.raytheon.uf.common.activetable.request.SendActiveTableRequest;
import com.raytheon.uf.common.activetable.response.ActiveTableSharingResponse;
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
import com.raytheon.uf.edex.activetable.ActiveTablePyIncludeUtil;

/**
 * Handler for <code>SendActiveTableRequest</code>. Runs the sendAT application
 * using the parameters from the request as arguments to the PythonScript.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 6, 2013            dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class SendActiveTableHandler implements
        IRequestHandler<SendActiveTableRequest> {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SendActiveTableHandler.class);

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.comm.IRequestHandler#handleRequest
     * (com.raytheon.uf.common.serialization.comm.IServerRequest)
     */
    @Override
    public ActiveTableSharingResponse handleRequest(
            SendActiveTableRequest request) throws Exception {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext commonBaseCx = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        String scriptPath = pathMgr.getFile(commonBaseCx,
                FileUtil.join(ActiveTablePyIncludeUtil.VTEC, "sendAT.py"))
                .getPath();
        final String siteId = request.getServerSite();
        String pythonIncludePath = PyUtil.buildJepIncludePath(
                ActiveTablePyIncludeUtil.getCommonPythonIncludePath(),
                ActiveTablePyIncludeUtil.getVtecIncludePath(siteId),
                ActiveTablePyIncludeUtil.getGfeConfigIncludePath(siteId),
                ActiveTablePyIncludeUtil.getIscScriptsIncludePath());

        PythonScript script = null;
        try {
            script = new PythonScript(scriptPath, pythonIncludePath,
                    RetrieveRemoteActiveTableHandler.class.getClassLoader());

            try {
                Map<String, Object> argMap = new HashMap<String, Object>();
                argMap.put("myServerHost", request.getServerHost());
                argMap.put("myServerPort",
                        Integer.toString(request.getServerPort()));
                argMap.put("myServerProtocol", request.getServerProtocol());
                argMap.put("myServerMHSID", request.getMhsId());
                argMap.put("myServerSite", siteId);
                argMap.put("sites", request.getSites());
                argMap.put("filterSites", request.getFilterSites());
                argMap.put("mhsSites", request.getMhsSites());
                argMap.put("issueTime", request.getIssueTime());
                argMap.put("countDict", request.getCountDict());
                argMap.put("fname", request.getFileName());
                argMap.put("xmlIncoming", request.getXmlIncoming());
                argMap.put("xmtScript", request.getTransmitScript());
                script.execute("runFromJava", argMap);
            } catch (JepException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error executing sendAT.", e);
                return new ActiveTableSharingResponse(false,
                        "Error executing sendAT: " + e.getLocalizedMessage());
            }
        } catch (JepException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to instantiate sendAT python script object.", e);
            return new ActiveTableSharingResponse(false,
                    "Unable to instantiate sendAT python script object: "
                            + e.getLocalizedMessage());
        } finally {
            if (script != null) {
                script.dispose();
            }
        }

        return new ActiveTableSharingResponse(true, null);
    }
}
