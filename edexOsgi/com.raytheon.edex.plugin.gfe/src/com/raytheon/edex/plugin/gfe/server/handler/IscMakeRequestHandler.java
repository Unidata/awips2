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

import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.dataplugin.gfe.request.IscMakeRequest;
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

import jep.JepConfig;
import jep.JepException;

/**
 * Processes an ISC grid request from the client
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Aug 21, 0009  1995     bphillip  Initial port
 * Sep 22, 0009  3058     rjpeter   Converted to IRequestHandler
 * Mar 07, 0013  1759     dgilling  Refactor to not use GfeScript.
 * Sep 05, 0013  2307     dgilling  Use better PythonScript constructor.
 * Jul 14, 0016  5747     dgilling  Move edex_static to common_static.
 * Sep 12, 2016  5861     randerso  Remove references to IFPServerConfigManager
 *                                  which was largely redundant with IFPServer.
 *
 * </pre>
 *
 * @author bphillip
 */
public class IscMakeRequestHandler extends BaseGfeRequestHandler
        implements IRequestHandler<IscMakeRequest> {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    private static final String SCRIPT_PATH = LocalizationUtil.join("gfe",
            "python", "isc", "IrtServer.py");

    private static final String METHOD_NAME = "makeISCrequest";

    private static final ExecutorService scriptRunner = Executors
            .newCachedThreadPool();

    @Override
    public ServerResponse<Boolean> handleRequest(IscMakeRequest request)
            throws Exception {
        ServerResponse<Boolean> response = new ServerResponse<>();
        response.setPayload(Boolean.FALSE);

        final String siteID = request.getSiteID();
        IFPServerConfig config = null;
        try {
            config = getIfpServer(request).getConfig();
        } catch (GfeException e) {
            response.addMessage(e.getLocalizedMessage());
            return response;
        }

        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext cx = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        final String scriptPath = pathMgr.getFile(cx, SCRIPT_PATH).getPath();
        final String includePath = PyUtil.buildJepIncludePath(
                GfePyIncludeUtil.getCommonPythonIncludePath(),
                GfePyIncludeUtil.getIscScriptsIncludePath(),
                GfePyIncludeUtil.getGfeConfigIncludePath(siteID));

        GridLocation domain = config.dbDomain();
        List<Integer> gridDims = new ArrayList<>();
        gridDims.add(domain.getNy());
        gridDims.add(domain.getNx());

        List<Double> gridBoundBox = new ArrayList<>();
        gridBoundBox.add(domain.getOrigin().x);
        gridBoundBox.add(domain.getOrigin().y);
        gridBoundBox.add(domain.getExtent().x);
        gridBoundBox.add(domain.getExtent().y);

        final Map<String, Object> args = new HashMap<>();
        args.put("xmlRequest", request.getXml());
        args.put("gridDims", gridDims);
        args.put("gridProj",
                domain.getProjection().getProjectionID().toString());
        args.put("gridBoundBox", gridBoundBox);
        args.put("mhs", config.getMhsid());
        args.put("host", config.getServerHost());
        args.put("port", config.getRpcPort());
        args.put("protocol", String.valueOf(config.getProtocolVersion()));
        args.put("site", siteID);
        args.put("xmtScript", config.transmitScript());

        Callable<String> scriptJob = () -> {
            try {
                try (PythonScript script = new PythonScript(
                        new JepConfig().setIncludePath(includePath)
                                .setClassLoader(getClass().getClassLoader()),
                        scriptPath)) {
                    try {
                        script.execute(METHOD_NAME, args);
                    } catch (JepException e) {
                        String msg = "Error servicing IscMakeRequest from site ["
                                + siteID + "]: " + e.getLocalizedMessage();
                        statusHandler.error(msg, e);
                        return msg;
                    }
                } catch (JepException e) {
                    String msg = "Error initializing IrtServer python script: "
                            + e.getLocalizedMessage();
                    statusHandler.error(msg, e);
                    return msg;
                }
            } catch (Throwable t) {
                String msg = "Error servicing IscMakeRequest from site ["
                        + siteID + "]: " + t.getLocalizedMessage();
                statusHandler.error(msg, t);
                return msg;
            }

            return null;
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
            statusHandler.warn(msg, e);
            response.addMessage(msg);
            return response;
        }

        response.setPayload(Boolean.TRUE);
        return response;
    }
}
