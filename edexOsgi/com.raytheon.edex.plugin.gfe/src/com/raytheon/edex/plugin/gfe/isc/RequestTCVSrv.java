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
package com.raytheon.edex.plugin.gfe.isc;

import java.util.HashMap;
import java.util.Map;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.edex.activetable.ActiveTablePyIncludeUtil;
import com.raytheon.uf.edex.core.EDEXUtil;

import jep.JepConfig;
import jep.JepException;

/**
 * Bean to run requestTCV at GFE site activation time.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 11, 2015  #4128     dgilling     Initial creation
 * Jun 03, 2019  7852      dgilling     Update code for jep 3.8.
 *
 * </pre>
 *
 * @author dgilling
 */

public final class RequestTCVSrv implements IISCServiceBean {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RequestTCVSrv.class);

    @Override
    public void activateSite(String siteID, IFPServerConfig gfeConfig) {
        EDEXUtil.waitForRunning();
        requestTCVFiles(siteID, gfeConfig);
    }

    private void requestTCVFiles(String siteId, IFPServerConfig config) {
        statusHandler.info("Running requestTCV for site " + siteId);

        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext commonBaseCx = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        String scriptPath = pathMgr.getFile(commonBaseCx,
                FileUtil.join(ActiveTablePyIncludeUtil.VTEC, "requestTCV.py"))
                .getPath();

        String pythonIncludePath = PyUtil.buildJepIncludePath(
                ActiveTablePyIncludeUtil.getCommonPythonIncludePath(),
                ActiveTablePyIncludeUtil.getCommonGfeIncludePath(),
                ActiveTablePyIncludeUtil.getVtecIncludePath(siteId),
                ActiveTablePyIncludeUtil.getGfeConfigIncludePath(siteId),
                ActiveTablePyIncludeUtil.getIscScriptsIncludePath());

        try (PythonScript script = new PythonScript(
                new JepConfig().setIncludePath(pythonIncludePath)
                        .setClassLoader(getClass().getClassLoader()),
                scriptPath)) {
            try {
                Map<String, Object> argMap = new HashMap<>();
                argMap.put("siteID", siteId);
                argMap.put("config", config);
                script.execute("runFromJava", argMap);
            } catch (JepException e) {
                statusHandler.error("Error executing requestTCV.", e);
            }
        } catch (JepException e) {
            statusHandler
                    .error("Unable to instantiate requestTCV python script object.",
                            e);
        }
    }

    @Override
    public void deactivateSite(String siteID) {
        // no-op
    }

    @Override
    public void startup() {
        // no-op
    }

    @Override
    public void preShutdown() {
        // no-op
    }

    @Override
    public void postShutdown() {
        // no-op
    }
}
