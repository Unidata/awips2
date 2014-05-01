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

import java.io.File;
import java.io.FilenameFilter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import jep.JepException;

import com.raytheon.edex.plugin.gfe.config.GridDbConfig;
import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.edex.plugin.gfe.config.IFPServerConfigManager;
import com.raytheon.edex.plugin.gfe.server.IFPServer;
import com.raytheon.edex.plugin.gfe.exception.GfeConfigurationException;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.FileUtil;

/**
 * Class for interfacing with the IRT server
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 07/14/09     1995       bphillip    Initial creation
 * Mar 14, 2013 1794       djohnson    FileUtil.listFiles now returns List.
 * 06/13/13     2044       randerso    Refactored to use IFPServer
 * Sep 05, 2013 2307       dgilling    Use better PythonScript constructor.
 * Oct 16, 2013 2475       dgilling    Move logic previously in IrtServer.py
 *                                     into this class to avoid Jep memory leak.
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class GfeIRT extends Thread {

    /** The logger */
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GfeIRT.class);

    private static final String PYTHON_INSTANCE = "irt";

    /** The site ID associated with this IRT thread */
    private final String siteID;

    /** The MHS ID associated with this IRT thread */
    private final String mhsID;

    private final String serverHost;

    private final long serverPort;

    private final long serverProtocol;

    private List<String> parmsWanted;

    private final List<Integer> gridDims;

    private final String gridProj;

    private final List<Double> gridBoundBox;

    private List<String> iscWfosWanted;

    /** The Python script object */
    private PythonScript script;

    /**
     * Map of threads used to unregister sites from the IRT server upon shutdown
     */
    private static Map<String, Thread> shutdownHooks = new ConcurrentHashMap<String, Thread>();

    /**
     * Creates a new GfeIRT object for the provided site ID
     * 
     * @param siteID
     *            The site ID to create the GfeIRT object for
     * @throws GfeConfigurationException
     *             If the GFE configuration for the specified site could not be
     *             loaded.
     */
    public GfeIRT(String siteid, IFPServerConfig config)
            throws GfeConfigurationException {
        this.setDaemon(true);
        this.siteID = siteid;
        this.mhsID = config.getMhsid();

        this.serverHost = config.getServerHost();
        this.serverPort = config.getRpcPort();
        this.serverProtocol = config.getProtocolVersion();

        GridLocation domain = config.dbDomain();

        this.gridProj = domain.getProjection().getProjectionID().toString();

        this.gridDims = new ArrayList<Integer>(2);
        this.gridDims.add(domain.getNy());
        this.gridDims.add(domain.getNx());

        this.gridBoundBox = new ArrayList<Double>(4);
        this.gridBoundBox.add(domain.getOrigin().x);
        this.gridBoundBox.add(domain.getOrigin().y);
        this.gridBoundBox.add(domain.getExtent().x);
        this.gridBoundBox.add(domain.getExtent().y);

        this.parmsWanted = config.requestedISCparms();
        if (this.parmsWanted.isEmpty()) {
            List<DatabaseID> dbs = IFPServer.getActiveServer(this.siteID)
                    .getGridParmMgr().getDbInventory().getPayload();
            for (DatabaseID dbId : dbs) {
                if ((dbId.getModelName().equals("ISC"))
                        && (dbId.getDbType().equals(""))
                        && (dbId.getSiteId().equals(this.siteID))) {
                    GridDbConfig gdc = config.gridDbConfig(dbId);
                    this.parmsWanted = gdc.parmAndLevelList();
                }
            }
            config.setRequestedISCparms(this.parmsWanted);
        }
        statusHandler.info("ParmsWanted: " + this.parmsWanted);

        this.iscWfosWanted = config.requestedISCsites();
        if (this.iscWfosWanted.isEmpty()) {
            List<String> knownSites = config.allSites();

            IPathManager pathMgr = PathManagerFactory.getPathManager();
            LocalizationContext commonStaticConfig = pathMgr.getContext(
                    LocalizationType.COMMON_STATIC,
                    LocalizationLevel.CONFIGURED);
            commonStaticConfig.setContextName(this.siteID);
            File editAreaDir = pathMgr.getFile(commonStaticConfig,
                    "gfe/editAreas");

            FilenameFilter filter = new FilenameFilter() {
                @Override
                public boolean accept(File dir, String name) {
                    return name.trim().matches("ISC_\\p{Alnum}{3}\\.xml");
                }
            };
            List<File> editAreas = FileUtil.listFiles(editAreaDir, filter,
                    false);

            this.iscWfosWanted = new ArrayList<String>();
            for (File f : editAreas) {
                String name = f.getName().replace("ISC_", "")
                        .replace(".xml", "");
                if (knownSites.contains(name)) {
                    iscWfosWanted.add(name);
                }
            }
            config.setRequestedISCsites(this.iscWfosWanted);
        }

        Thread hook = new Thread() {
            @Override
            public void run() {
                statusHandler.info("Unregistering site [" + siteID
                        + "] from IRT server...");
                IRTManager.getInstance().disableISC(mhsID, siteID);
                statusHandler.info("Site [" + siteID + "] unregistered!");
            }
        };
        java.lang.Runtime.getRuntime().addShutdownHook(hook);
        shutdownHooks.put(mhsID + siteID, hook);
    }

    @Override
    public void run() {

        try {
            IPathManager pathMgr = PathManagerFactory.getPathManager();
            LocalizationContext cx = pathMgr.getContext(
                    LocalizationType.EDEX_STATIC, LocalizationLevel.BASE);
            String scriptPath = pathMgr
                    .getLocalizationFile(cx, "gfe/isc/IrtAccess.py").getFile()
                    .getPath();
            String includePath = PyUtil.buildJepIncludePath(
                    GfePyIncludeUtil.getCommonPythonIncludePath(),
                    GfePyIncludeUtil.getIscScriptsIncludePath(),
                    GfePyIncludeUtil.getGfeConfigIncludePath(this.siteID));
            this.script = new PythonScript(scriptPath, includePath, getClass()
                    .getClassLoader());

            IFPServerConfig config = IFPServerConfigManager
                    .getServerConfig(siteID);
            Map<String, Object> initArgs = new HashMap<String, Object>(2, 1f);
            initArgs.put("ancfURL", config.iscRoutingTableAddress().get("ANCF"));
            initArgs.put("bncfURL", config.iscRoutingTableAddress().get("BNCF"));
            this.script.instantiatePythonClass(PYTHON_INSTANCE, "IrtAccess",
                    initArgs);
        } catch (GfeConfigurationException e) {
            throw new RuntimeException("Could not load GFE configuration", e);
        } catch (JepException e) {
            throw new RuntimeException(
                    "Could not instantiate IRT python script instance", e);
        }

        try {
            // upon any overall failure, start thread over
            while (IRTManager.getInstance().isRegistered(mhsID, siteID)) {
                try {
                    // do initial registration, keep trying until successful
                    while (IRTManager.getInstance().isRegistered(mhsID, siteID)) {
                        statusHandler
                                .info("performing initial IRT registration.");

                        Map<String, Object> args = new HashMap<String, Object>(
                                10, 1f);
                        args.put("mhsid", mhsID);
                        args.put("serverHost", serverHost);
                        args.put("serverPort", serverPort);
                        args.put("serverProtocol", serverProtocol);
                        args.put("site", siteID);
                        args.put("parmsWanted", parmsWanted);
                        args.put("gridDims", gridDims);
                        args.put("gridProj", gridProj);
                        args.put("gridBoundBox", gridBoundBox);
                        args.put("iscWfosWanted", iscWfosWanted);
                        Boolean okay = (Boolean) script.execute("register",
                                PYTHON_INSTANCE, args);

                        if (okay) {
                            break;
                        } else if (!IRTManager.getInstance().isRegistered(
                                mhsID, siteID)) {
                            break; // exit processing loop
                        } else {
                            sleep(3 * TimeUtil.MILLIS_PER_SECOND);
                        }
                    }

                    // if we are here, we had a successful registration, check
                    // for re-register every few seconds, check the StopIRT flag
                    // every few seconds
                    statusHandler.info("initial IRT registration complete.");
                    while (IRTManager.getInstance().isRegistered(mhsID, siteID)) {
                        sleep(3 * TimeUtil.MILLIS_PER_SECOND); // wait 3 seconds

                        Boolean status1 = (Boolean) script.execute(
                                "checkForReregister", PYTHON_INSTANCE, null);
                        if (!status1) {
                            statusHandler.error("FAIL on checkForRegister().");
                            break; // break out of rereg loop, to cause another
                                   // reg
                        }
                    }
                } catch (Throwable t) {
                    statusHandler.error("Exception in IRT register thread.", t);
                }
            }

            // if we get here, we have been told to stop IRT, so we unregister.
            // We try only once.
            statusHandler.info("FINAL IRT unregister.");
            try {
                script.execute("unregister", PYTHON_INSTANCE, null);
            } catch (JepException e) {
                statusHandler.error("Exception unregister IRT.", e);
            }
            statusHandler.info("FINAL -- exiting IRT registration thread.");
        } finally {
            if (script != null) {
                script.dispose();
            }
        }
    }

    /**
     * Removes the site's entry from the shutdown hook map
     * 
     * @param mhsid
     *            The MHS ID of the site
     * @param siteid
     *            The Site ID of the site
     */
    public void removeShutdownHook(String mhsid, String siteid) {
        if (shutdownHooks.containsKey(mhsid + siteid)) {
            Thread hook = shutdownHooks.get(mhsid + siteid);
            try {
                Runtime.getRuntime().removeShutdownHook(hook);
            } catch (IllegalStateException e) {
                // Ignore. Shutdown in progress
            }
        }
    }
}
