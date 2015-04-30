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
package com.raytheon.edex.plugin.gfe.textproducts;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import jep.JepException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.utility.ProtectedFiles;
import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterTask;
import com.raytheon.uf.edex.database.tasks.SqlQueryTask;

/**
 * Generate and configure text products when needed.
 * <p>
 * This class takes care of checking if text products need to be generated for
 * the current site. If text products need generating, the class calls
 * configureTextProducts.py to create the text products from templates and place
 * them into the appropriate files.
 * </p>
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Jul 7,  2008 1222        jelkins     Initial creation
 * Jul 24, 2012 #944        dgilling    Fix text product template generation
 *                                      to create textProducts and textUtilities.
 * Sep 07, 2012 #1150       dgilling    Fix isConfigured to check for textProducts
 *                                      and textUtilities dirs.
 * Oct 20, 2014 #3685       randerso    Added code to generate SiteCFG.py from GIS database
 *                                      Cleaned up how protected file updates are returned
 * Jan 23, 2015 #4027       randerso    Fixed python include path
 * Apr 27, 2015  4259       njensen     Updated for new JEP API
 * 
 * </pre>
 * 
 * @author jelkins
 * @version 1.0
 */

public class Configurator {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(Configurator.class);

    private static final String CWA_QUERY = "select wfo, region, fullstaid, citystate, city, state from mapdata.cwa order by wfo;";

    private static final String CONFIG_TEXT_PRODUCTS_TASK = "GfeConfigureTextProducts";

    private String siteID;

    private String destinationDirectory;

    private String pythonDirectory;

    private transient Log log = LogFactory.getLog(getClass());

    /**
     * The Protected Files list.
     */
    private List<String> protectedFilesList;

    /**
     * Default Constructor
     * <p>
     * Get the siteID and determine the appropriate paths.
     * </p>
     */
    public Configurator(String siteID) {

        // ---- Determine the Site ID ---------------------------------------
        this.siteID = siteID;

        // ---- Determine the Paths ------------------------------------------

        // destinationDirectory
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext caveStaticConfig = pathMgr.getContext(
                LocalizationContext.LocalizationType.CAVE_STATIC,
                LocalizationContext.LocalizationLevel.CONFIGURED);
        caveStaticConfig.setContextName(siteID);

        try {
            destinationDirectory = pathMgr.getFile(caveStaticConfig,
                    FileUtil.join("gfe", "userPython")).getCanonicalPath();
        } catch (IOException e) {
            log.error("Unable to determine the destination directory", e);
            log.warn("Textproducts will not be configured.");
            return;
        }

        // configureTextProductsFile
        LocalizationContext edexStaticBase = pathMgr.getContext(
                LocalizationContext.LocalizationType.EDEX_STATIC,
                LocalizationContext.LocalizationLevel.BASE);

        try {
            pythonDirectory = pathMgr.getFile(edexStaticBase, ".")
                    .getCanonicalPath();
        } catch (IOException e) {
            log.error("Unable to determine the python directory", e);
            log.warn("Textproducts will not be configured.");
            return;
        }

        // ---- Log the results -----------------------------------------------
        if (log.isDebugEnabled()) {
            log.debug(String.format("using site ID: %s", siteID));
            log.debug(String.format("destination Directory: %s",
                    destinationDirectory));
            log.debug(String.format("python Directory: %s", pythonDirectory));
        }
    }

    /**
     * Check if text products have been configured already
     * 
     * @return true if text products have been configured
     */
    public boolean isConfigured() {
        boolean filesHaveChanges = false;

        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext ctx = pathMgr.getContext(
                LocalizationType.EDEX_STATIC, LocalizationLevel.BASE);
        File templatesDir = pathMgr.getFile(ctx, "textproducts/templates");
        ClusterTask task = ClusterLockUtils.lookupLock(
                CONFIG_TEXT_PRODUCTS_TASK, this.siteID);
        filesHaveChanges = FileUtil.hasBeenModifiedSince(templatesDir,
                task.getLastExecution(), true);

        // if the destination dir does not exist, configurator needs run

        if ((!new File(FileUtil.join(destinationDirectory, "textProducts"))
                .isDirectory())
                || (!new File(FileUtil.join(destinationDirectory,
                        "textUtilities")).isDirectory())) {
            filesHaveChanges = true;
        }

        return !filesHaveChanges;
    }

    /**
     * Create and configure text products
     * <p>
     * Call the configureTextProduct.py script. After successfully creating and
     * configuring the text products, a file marker will be created to indicate
     * that the site's text products have been configured.
     * </p>
     * 
     * @see Configurator#isConfigured()
     */
    @SuppressWarnings("unchecked")
    public void execute() {
        if (isConfigured()) {
            statusHandler.info("All text products are up to date");
            return;
        }
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext context = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.CONFIGURED);
        context.setContextName(siteID);

        // regenerate siteCFG.py
        LocalizationFile lf = null;
        try {
            lf = pathMgr.getLocalizationFile(context,
                    FileUtil.join("python", "gfe", "SiteCFG.py"));

            SqlQueryTask task = new SqlQueryTask(CWA_QUERY, "maps");
            QueryResult results = task.execute();
            try (PrintWriter out = new PrintWriter(lf.openOutputStream())) {
                out.println("##");
                out.println("# Contains information about products, regions, etc. for each site");
                out.println("# in the country.");
                out.println("# region= two-letter regional identifier, mainly used for installation of");
                out.println("#         text product templates");
                out.println("SiteInfo= {");
                for (QueryResultRow row : results.getRows()) {
                    String wfo = (String) row.getColumn(0);
                    String region = (String) row.getColumn(1);
                    String fullStationID = (String) row.getColumn(2);
                    String wfoCityState = (String) row.getColumn(3);
                    String wfoCity = (String) row.getColumn(4);
                    String state = (String) row.getColumn(5);

                    out.println(formatEntry(wfo, region, fullStationID,
                            wfoCityState, wfoCity, state));

                    // Add in AFC's dual domain sites
                    if (wfo.equals("AFC")) {
                        out.println(formatEntry("AER", region, fullStationID,
                                wfoCityState, wfoCity, state));
                        out.println(formatEntry("ALU", region, fullStationID,
                                wfoCityState, wfoCity, state));
                    }
                }

                // Add in the national centers since they
                // aren't in the shape file
                out.println(formatEntry("NH1", "NC", "KNHC",
                        "National Hurricane Center Miami FL", "Miami", ""));
                out.println(formatEntry("NH2", "NC", "KNHC",
                        "National Hurricane Center Miami FL", "Miami", ""));
                out.println(formatEntry("ONA", "NC", "KWBC",
                        "Ocean Prediction Center Washington DC",
                        "Washington DC", ""));
                out.println(formatEntry("ONP", "NC", "KWBC",
                        "Ocean Prediction Center Washington DC",
                        "Washington DC", ""));

                out.println("}");
            } // out is closed here

            lf.save();
        } catch (Exception e) {
            statusHandler.error(e.getLocalizedMessage(), e);
        }

        PythonScript python = null;

        LocalizationContext edexCx = pathMgr.getContext(
                LocalizationType.EDEX_STATIC, LocalizationLevel.BASE);

        File file = pathMgr.getFile(edexCx, "textproducts"
                + IPathManager.SEPARATOR + "Generator.py");
        String filePath = file.getPath();
        String commonPython = GfePyIncludeUtil.getCommonGfeIncludePath();

        Map<String, Object> argList = new HashMap<String, Object>();
        argList.put("siteId", siteID);
        argList.put("destinationDir", destinationDirectory);

        List<String> preEval = new ArrayList<String>(1);
        String scriptDir = file.getParent();
        preEval.add("SCRIPT_DIR = '" + scriptDir + "'");

        try {
            python = new PythonScript(filePath, PyUtil.buildJepIncludePath(
                    pythonDirectory, commonPython), this.getClass()
                    .getClassLoader(), preEval);

            // Open the Python interpreter using the designated script.
            protectedFilesList = (List<String>) python.execute("runFromJava",
                    argList);

            updateProtectedFile();
            updateLastRuntime();
        } catch (JepException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error Configuring Text Products", e);
        } finally {
            if (python != null) {
                python.dispose();
            }
        }
    }

    private String formatEntry(String wfo, String region, String fullStationID,
            String wfoCityState, String wfoCity, String state) {
        StringBuilder sb = new StringBuilder();
        sb.append("  '").append(wfo).append("': {\n");
        sb.append("         'region': '").append(region).append("',\n");
        sb.append("         'fullStationID': '").append(fullStationID)
                .append("',\n");
        sb.append("         'wfoCityState': '").append(wfoCityState)
                .append("',\n");
        sb.append("         'wfoCity': '").append(wfoCity).append("',\n");
        sb.append("         'state': '").append(state).append("',\n");
        sb.append("         },");

        return sb.toString();
    }

    /**
     * Update the protected files.
     */
    private void updateProtectedFile() {
        if ((protectedFilesList != null) && (protectedFilesList.size() > 0)) {
            ProtectedFiles.protect(siteID, protectedFilesList);
        }
    }

    private void updateLastRuntime() {
        ClusterLockUtils.lock(CONFIG_TEXT_PRODUCTS_TASK, this.siteID, 0, false);
        ClusterLockUtils.unlock(CONFIG_TEXT_PRODUCTS_TASK, this.siteID);
    }
}
