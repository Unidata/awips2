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

import jep.JepException;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.dataplugin.gfe.request.SendWFOMessageRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.util.FileUtil;

/**
 * Send WFO Message Handler
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 22, 2016  #5374     randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
public class SendWFOMessageHandler extends BaseGfeRequestHandler implements
        IRequestHandler<SendWFOMessageRequest> {

    private static final String SCRIPT_PATH = FileUtil.join(
            GfePyIncludeUtil.ISC, "sendWFOMessage.py");

    @Override
    public ServerResponse<?> handleRequest(SendWFOMessageRequest request) {
        ServerResponse<?> response = new ServerResponse<>();

        String siteID = request.getSiteID();
        IFPServerConfig config;
        try {
            config = getIfpServer(request).getConfig();

            Map<String, Object> args = new HashMap<>();
            args.put("siteID", siteID);
            args.put("config", config);
            args.put("destSites", request.getWfos());
            args.put("message", request.getMessage());

            IPathManager pathMgr = PathManagerFactory.getPathManager();
            LocalizationContext cx = pathMgr.getContext(
                    LocalizationType.EDEX_STATIC, LocalizationLevel.BASE);
            final String scriptPath = pathMgr.getFile(cx, SCRIPT_PATH)
                    .getPath();
            final String includePath = PyUtil.buildJepIncludePath(
                    GfePyIncludeUtil.getCommonPythonIncludePath(),
                    GfePyIncludeUtil.getIscScriptsIncludePath(),
                    GfePyIncludeUtil.getGfeConfigIncludePath(siteID));

            try (PythonScript script = new PythonScript(scriptPath,
                    includePath, SendWFOMessageHandler.class.getClassLoader())) {
                try {
                    script.execute("runFromJava", args);
                } catch (JepException e) {
                    String msg = "Error servicing SendWFOMessageRequest from site ["
                            + siteID + "]: " + e.getLocalizedMessage();
                    response.addMessage(msg);
                }
            } catch (JepException e) {
                String msg = "Error creating PythonScript object for: ["
                        + scriptPath + "]: " + e.getLocalizedMessage();
                response.addMessage(msg);
            }
        } catch (GfeException e) {
            String msg = "Error retrieving site config for site [" + siteID
                    + "]: " + e.getLocalizedMessage();
            response.addMessage(msg);
        }

        return response;
    }
}
