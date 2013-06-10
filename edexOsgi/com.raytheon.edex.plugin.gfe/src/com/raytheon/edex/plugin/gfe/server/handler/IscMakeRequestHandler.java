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
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.RejectedExecutionException;

import jep.JepException;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.edex.plugin.gfe.config.IFPServerConfigManager;
import com.raytheon.edex.plugin.gfe.exception.GfeConfigurationException;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.dataplugin.gfe.request.IscMakeRequest;
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
 * Processes an ISC grid request from the client
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 08/21/09      1995       bphillip    Initial port
 * 09/22/09      3058       rjpeter     Converted to IRequestHandler
 * 03/07/13      1759       dgilling    Refactor to not use GfeScript.
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class IscMakeRequestHandler implements IRequestHandler<IscMakeRequest> {

    private static final String SCRIPT_PATH = FileUtil.join(
            GfePyIncludeUtil.ISC, "IrtServer.py");

    private static final String METHOD_NAME = "makeISCrequest";

    private static final ExecutorService scriptRunner = Executors
            .newCachedThreadPool();

    @Override
    public ServerResponse<Boolean> handleRequest(IscMakeRequest request)
            throws Exception {
        ServerResponse<Boolean> response = new ServerResponse<Boolean>();
        response.setPayload(Boolean.FALSE);

        final String siteID = request.getSiteID();
        IFPServerConfig config = null;
        try {
            config = IFPServerConfigManager.getServerConfig(siteID);
        } catch (GfeConfigurationException e) {
            response.addMessage("Unable to get server config for site ["
                    + siteID + "]");
            return response;
        }

        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext cx = pathMgr.getContext(
                LocalizationType.EDEX_STATIC, LocalizationLevel.BASE);
        final String scriptPath = pathMgr.getFile(cx, SCRIPT_PATH).getPath();
        final String includePath = PyUtil.buildJepIncludePath(
                GfePyIncludeUtil.getCommonPythonIncludePath(),
                GfePyIncludeUtil.getIscScriptsIncludePath(),
                GfePyIncludeUtil.getGfeConfigIncludePath(siteID));

        GridLocation domain = config.dbDomain();
        List<Integer> gridDims = new ArrayList<Integer>();
        gridDims.add(domain.getNy());
        gridDims.add(domain.getNx());

        List<Double> gridBoundBox = new ArrayList<Double>();
        gridBoundBox.add(domain.getOrigin().x);
        gridBoundBox.add(domain.getOrigin().y);
        gridBoundBox.add(domain.getExtent().x);
        gridBoundBox.add(domain.getExtent().y);

        final Map<String, Object> args = new HashMap<String, Object>();
        args.put("xmlRequest", request.getXml());
        args.put("gridDims", gridDims);
        args.put("gridProj", domain.getProjection().getProjectionID()
                .toString());
        args.put("gridBoundBox", gridBoundBox);
        args.put("mhs", config.getMhsid());
        args.put("host", config.getServerHost());
        args.put("port", config.getRpcPort());
        args.put("protocol", String.valueOf(config.getProtocolVersion()));
        args.put("site", siteID);
        args.put("xmtScript", config.transmitScript());

        Callable<String> scriptJob = new Callable<String>() {

            @Override
            public String call() throws Exception {
                try {
                    PythonScript script = null;
                    try {
                        script = new PythonScript(scriptPath, includePath);
                        try {
                            script.execute(METHOD_NAME, args);
                        } catch (JepException e) {
                            String msg = "Error servicing IscMakeRequest from site ["
                                    + siteID + "]: " + e.getLocalizedMessage();
                            return msg;
                        }
                    } catch (JepException e) {
                        String msg = "Error initializing IrtServer python script: "
                                + e.getLocalizedMessage();
                        return msg;
                    } finally {
                        if (script != null) {
                            script.dispose();
                        }
                    }
                } catch (Throwable t) {
                    String msg = "Error servicing IscMakeRequest from site ["
                            + siteID + "]: " + t.getLocalizedMessage();
                    return msg;
                }

                return null;
            }
        };

        try {
            Future<String> result = scriptRunner.submit(scriptJob);
            String errorMessage = result.get();
            if (errorMessage != null) {
                response.addMessage(errorMessage);
                return response;
            }
        } catch (RejectedExecutionException e) {
            String msg = "IscMakeRequest job was rejected: "
                    + e.getLocalizedMessage();
            response.addMessage(msg);
            return response;
        }

        response.setPayload(Boolean.TRUE);
        return response;
    }
}
