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
import com.raytheon.edex.plugin.gfe.server.GridParmManager;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
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
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class GfeIRT extends Thread {

    /** The logger */
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GfeIRT.class);

    /** The site ID associated with this IRT thread */
    private final String siteID;

    /** The MHS ID associated with this IRT thread */
    private final String mhsID;

    /** The script file name */
    private final String scriptFile;

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
     * @throws GfeException
     */
    public GfeIRT(String mhsid, String siteid) throws GfeException {
        this.setDaemon(true);
        this.siteID = siteid;
        this.mhsID = mhsid;
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext cx = pathMgr.getContext(
                LocalizationType.EDEX_STATIC, LocalizationLevel.BASE);

        scriptFile = pathMgr
                .getLocalizationFile(cx,
                        "gfe/isc" + File.separator + "IrtServer.py").getFile()
                .getPath();
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
        shutdownHooks.put(mhsid + siteid, hook);
    }

    @Override
    public void run() {

        try {
            String includePath = PyUtil.buildJepIncludePath(
                    GfePyIncludeUtil.getCommonPythonIncludePath(),
                    GfePyIncludeUtil.getIscScriptsIncludePath(),
                    GfePyIncludeUtil.getGfeConfigIncludePath(siteID));
            script = new PythonScript(scriptFile, includePath);
            Map<String, Object> args = new HashMap<String, Object>();

            IFPServerConfig config = IFPServerConfigManager
                    .getServerConfig(siteID);
            GridLocation domain = config.dbDomain();

            String site = config.getSiteID().get(0);
            List<Integer> gridDims = new ArrayList<Integer>();
            gridDims.add(domain.getNy());
            gridDims.add(domain.getNx());

            List<Double> gridBoundBox = new ArrayList<Double>();
            gridBoundBox.add(domain.getOrigin().x);
            gridBoundBox.add(domain.getOrigin().y);
            gridBoundBox.add(domain.getExtent().x);
            gridBoundBox.add(domain.getExtent().y);

            // determine which parms are wanted
            List<String> parmsWanted = config.requestedISCparms();
            if (parmsWanted.isEmpty()) {
                List<DatabaseID> dbs = GridParmManager.getDbInventory(site)
                        .getPayload();

                for (int i = 0; i < dbs.size(); i++) {
                    if (dbs.get(i).getModelName().equals("ISC")
                            && dbs.get(i).getDbType().equals("")
                            && dbs.get(i).getSiteId().equals(site)) {
                        GridDbConfig gdc = config.gridDbConfig(dbs.get(i));
                        parmsWanted = gdc.parmAndLevelList();
                    }
                }
            }
            statusHandler.info("ParmsWanted: " + parmsWanted);

            // reset them to actual values
            config.setRequestedISCparms(parmsWanted);

            // determine isc areas that are wanted
            List<String> iscWfosWanted = config.requestedISCsites();

            if (iscWfosWanted.isEmpty()) {
                List<String> knownSites = config.allSites();

                IPathManager pathMgr = PathManagerFactory.getPathManager();
                LocalizationContext commonStaticConfig = pathMgr.getContext(
                        LocalizationType.COMMON_STATIC,
                        LocalizationLevel.CONFIGURED);
                commonStaticConfig.setContextName(site);
                File editAreaDir = pathMgr.getFile(commonStaticConfig,
                        "gfe/editAreas");

                FilenameFilter filter = new FilenameFilter() {
                    @Override
                    public boolean accept(File dir, String name) {
                        return name.trim().matches("ISC_\\p{Alnum}{3}\\.xml");
                    }
                };
                List<File> editAreas = FileUtil.listFiles(editAreaDir,
                        filter, false);

                String name = "";
                for (File f : editAreas) {
                    name = f.getName().replace("ISC_", "").replace(".xml", "");
                    if (knownSites.contains(name)) {
                        iscWfosWanted.add(name);
                    }
                }
                config.setRequestedISCsites(iscWfosWanted);
            }

            args.put("ancfURL", config.iscRoutingTableAddress().get("ANCF"));
            args.put("bncfURL", config.iscRoutingTableAddress().get("BNCF"));
            args.put("mhsid", config.getMhsid());
            args.put("serverHost", config.getServerHost());
            args.put("serverPort", config.getRpcPort());
            args.put("serverProtocol", config.getProtocolVersion());
            args.put("site", site);
            args.put("parmsWanted", config.requestedISCparms());
            args.put("gridDims", gridDims);
            args.put("gridProj", domain.getProjection().getProjectionID()
                    .toString());
            args.put("gridBoundBox", gridBoundBox);
            args.put("iscWfosWanted", iscWfosWanted);

            boolean regSuccess = (Boolean) script.execute("irtReg", args);
            if (!regSuccess) {
                statusHandler
                        .error("Error registering site with IRT server. ISC functionality will be unavailable. Check config and IRT connectivity.");
                removeShutdownHook(this.mhsID, this.siteID);
            }
        } catch (JepException e) {
            statusHandler
                    .fatal("Error starting GFE ISC. ISC functionality will be unavailable!!",
                            e);
        } catch (GfeException e) {
            statusHandler
                    .fatal("Unable to get Mhs ID. ISC functionality will be unavailable!!",
                            e);
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
