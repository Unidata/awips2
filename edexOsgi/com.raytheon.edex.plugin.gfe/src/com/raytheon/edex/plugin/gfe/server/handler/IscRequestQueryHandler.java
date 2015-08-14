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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import jep.JepException;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.edex.plugin.gfe.config.IFPServerConfigManager;
import com.raytheon.edex.plugin.gfe.exception.GfeConfigurationException;
import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.dataplugin.gfe.request.IscRequestQueryRequest;
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

/**
 * Port of the requestQuery from AWIPS I
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 07/06/09      1995       bphillip    Initial port
 * 09/22/09      3058       rjpeter     Converted to IRequestHandler
 * 09/05/13      2307       dgilling    Use better PythonScript constructor.
 * 08/14/15      4750       dgilling    Send domainDicts back as Maps, not
 *                                      python pickled strings.
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class IscRequestQueryHandler implements
        IRequestHandler<IscRequestQueryRequest> {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    private static final String ISC_UTIL = "gfe" + IPathManager.SEPARATOR
            + "isc" + IPathManager.SEPARATOR + "iscUtil.py";

    private static final String IRT_SERVER = "gfe" + IPathManager.SEPARATOR
            + "isc" + IPathManager.SEPARATOR + "IrtServer.py";

    @Override
    public ServerResponse<List<Object>> handleRequest(
            IscRequestQueryRequest request) throws Exception {
        ServerResponse<List<Object>> response = new ServerResponse<List<Object>>();
        IFPServerConfig config = null;
        String siteID = request.getSiteID();
        try {
            config = IFPServerConfigManager.getServerConfig(siteID);
        } catch (GfeConfigurationException e) {
            response.addMessage("Unable to get server config for site ["
                    + siteID + "]");
            return response;
        }

        if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
            statusHandler.debug("Request:  requestor="
                    + request.getWorkstationID().toString());
        }

        // ensure there is a routing table
        String ancf, bncf = null;
        ancf = config.iscRoutingTableAddress().get("ANCF");
        bncf = config.iscRoutingTableAddress().get("BNCF");

        boolean irtUnavailable = false;
        if (ancf == null) {
            irtUnavailable = true;
        } else if (ancf.isEmpty()) {
            irtUnavailable = true;
        }

        if (irtUnavailable) {
            if (bncf == null) {
                irtUnavailable = true;
            } else if (bncf.isEmpty()) {
                irtUnavailable = true;
            } else {
                irtUnavailable = false;
            }
        }

        if (!config.requestISC()) {
            irtUnavailable = true;
        }

        if (irtUnavailable) {
            response.addMessage("IRT Address/requestISC not enabled. No ISC data available");
            return response;
        }

        // calculate XML
        List<String> domains = new ArrayList<String>(config.requestedISCsites());

        String xml = null;
        Object domainDict = null;

        try (PythonScript script = new PythonScript(getScriptPath(IRT_SERVER),
                getIncludePath(siteID), getClass().getClassLoader())) {
            Map<String, Object> args = new HashMap<String, Object>();
            args.put("ancfURL", ancf);
            args.put("bncfURL", bncf);
            args.put("iscWfosWanted", domains);
            xml = (String) script.execute("irtGetServers", args);

        } catch (JepException e) {
            statusHandler.error("Error occurred running IscRequestQuery", e);
        }

        try (PythonScript script = new PythonScript(getScriptPath(ISC_UTIL),
                getIncludePath(siteID), getClass().getClassLoader())) {
            Map<String, Object> args = new HashMap<String, Object>();
            args.put("xml", xml);
            domainDict = script.execute("createDomainDict", args);
        } catch (JepException e) {
            statusHandler.error("Error occurred creating Isc Domain Dict", e);
        }

        List<Object> responseList = new ArrayList<Object>();
        responseList.add(xml);
        responseList.add(domainDict);
        responseList.add(config.requestedISCparms());
        response.setPayload(responseList);
        return response;
    }

    private static String getScriptPath(String locPath) {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext cx = pathMgr.getContext(
                LocalizationType.EDEX_STATIC, LocalizationLevel.BASE);
        String scriptFile = pathMgr.getLocalizationFile(cx, locPath).getFile()
                .getPath();
        return scriptFile;
    }

    private static String getIncludePath(String siteID) {
        String includePath = PyUtil.buildJepIncludePath(
                GfePyIncludeUtil.getCommonPythonIncludePath(),
                GfePyIncludeUtil.getIscScriptsIncludePath(),
                GfePyIncludeUtil.getGfeConfigIncludePath(siteID));
        return includePath;
    }
}
