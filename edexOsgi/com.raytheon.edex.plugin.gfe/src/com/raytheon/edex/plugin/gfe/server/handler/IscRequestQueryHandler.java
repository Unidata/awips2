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

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import jep.JepException;

import org.apache.commons.lang3.StringUtils;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.dataplugin.gfe.request.IscRequestQueryRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.IscRequestQueryRequest.IscQueryResponse;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

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
 * 11/30/15      5129       dgilling    Code cleanup.
 * 07/14/16      5747       dgilling    Move edex_static to common_static.
 * 08/04/16      5788       dgilling    Throw error when empty response received from 
 *                                      IRT.
 * </pre>
 * 
 * @author bphillip
 */
public class IscRequestQueryHandler extends BaseGfeRequestHandler implements
        IRequestHandler<IscRequestQueryRequest> {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    private static final String IRT_SERVER = LocalizationUtil.join("gfe",
            "python", "isc", "IrtServer.py");

    private static final String ISC_UTIL = LocalizationUtil.join("gfe",
            "python", "isc", "iscUtil.py");

    @Override
    public ServerResponse<IscQueryResponse> handleRequest(
            IscRequestQueryRequest request) throws Exception {
        ServerResponse<IscQueryResponse> sr = new ServerResponse<>();

        // init return values
        String xmlResponse = StringUtils.EMPTY;
        Collection<String> parmsWanted = Collections.emptyList();
        Map<String, Map<String, List<Map<String, String>>>> domainDict = Collections
                .emptyMap();
        Map<String, Map<String, String>> serverDictT2S = Collections.emptyMap();

        // ensure there is a routing table
        IFPServerConfig config = getIfpServer(request).getConfig();
        String ancf = config.iscRoutingTableAddress().get("ANCF");
        String bncf = config.iscRoutingTableAddress().get("BNCF");
        if ((StringUtils.isBlank(ancf)) || (StringUtils.isBlank(bncf))
                || (!config.requestISC())) {
            sr.addMessage("IRT Address/requestISC not enabled. No ISC data available.");
        } else {
            // calculate XML
            try (PythonScript script = new PythonScript(
                    getScriptPath(IRT_SERVER),
                    getIncludePath(request.getSiteID()), getClass()
                            .getClassLoader())) {
                Map<String, Object> args = new HashMap<>();
                args.put("ancfURL", ancf);
                args.put("bncfURL", bncf);
                args.put("iscWfosWanted", config.requestedISCsites());
                String xml = (String) script.execute("irtGetServers", args);
                xmlResponse = xml;
                parmsWanted = config.requestedISCparms();
            } catch (JepException e) {
                statusHandler.error("Can't create IrtAccess instance: ", e);
                sr.addMessage("No ISC data available due to inability to access Irt");
            }

            if (xmlResponse.isEmpty()) {
                statusHandler
                        .warn("Empty response returned from IRT for irtGetServers.");
                sr.addMessage("No ISC data available due to inability to process Irt server response.");
                return sr;
            }

            try (PythonScript script = new PythonScript(
                    getScriptPath(ISC_UTIL),
                    getIncludePath(request.getSiteID()), getClass()
                            .getClassLoader())) {
                Map<String, Object> args = new HashMap<>();
                args.put("xml", xmlResponse);
                Map<String, Object> objectDict = (Map<String, Object>) script
                        .execute("createDomainDict", args);
                domainDict = (Map<String, Map<String, List<Map<String, String>>>>) objectDict
                        .get("domains");
                serverDictT2S = (Map<String, Map<String, String>>) objectDict
                        .get("serverDictT2S");
            } catch (JepException e) {
                statusHandler.error("Error occurred creating Isc Domain Dict",
                        e);
                sr.addMessage("No ISC data available due to inability to process Irt server response.");
                return sr;
            }
        }

        sr.setPayload(new IscQueryResponse(domainDict, serverDictT2S,
                parmsWanted));
        return sr;
    }

    private static String getScriptPath(String locPath) {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext cx = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        String scriptFile = pathMgr.getFile(cx, locPath).getPath();
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
