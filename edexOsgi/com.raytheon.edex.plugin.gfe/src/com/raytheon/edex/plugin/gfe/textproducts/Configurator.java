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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.protectedfiles.ProtectedFiles;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

import jep.JepConfig;
import jep.JepException;

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
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jul 07, 2008  1222     jelkins   Initial creation
 * Jul 24, 2012  944      dgilling  Fix text product template generation to
 *                                  create textProducts and textUtilities.
 * Sep 07, 2012  1150     dgilling  Fix isConfigured to check for textProducts
 *                                  and textUtilities dirs.
 * Oct 20, 2014  3685     randerso  Added code to generate SiteCFG.py from GIS
 *                                  database Cleaned up how protected file
 *                                  updates are returned
 * Jan 23, 2015  4027     randerso  Fixed python include path
 * Apr 27, 2015  4259     njensen   Updated for new JEP API
 * Jul 13, 2015  4500     rjpeter   Removed SqlQueryTask.
 * Dec 15, 2015  5166     kbisanz   Update logging to use SLF4J
 * Jan 08, 2016  5237     tgurney   Replace calls to deprecated LocalizationFile
 *                                  methods
 * Jul 18, 2016  5747     dgilling  Move edex_static to common_static.
 * Sep 19, 2016  19293    randerso  Add NHA to generated SiteCFG.py file
 * Apr 20, 2017  5898     randerso  Make configureTextProducts run
 *                                  unconditionally
 * Aug 07, 2017  6379     njensen   Updated import of ProtectedFiles
 * Nov 27, 2017  6538     randerso  Move National center configurations to
 *                                  external Python file
 * Jun 03, 2019  7852     dgilling  Update code for jep 3.8.
 * Apr 28, 2020  8151     randerso  Changed to write SiteCFG.py to the
 *                                  configured AW_SITE_IDENTIFIER directory so
 *                                  only one copy is needed per site and so it
 *                                  will be found even if the AW_SITE_IDENTIFER
 *                                  site is not normally an activated GFE site.
 *
 * </pre>
 *
 * @author jelkins
 */

public class Configurator {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(Configurator.class);

    private static final String CWA_QUERY = "select wfo, region, fullstaid, citystate, city, state from mapdata.cwa order by wfo;";

    private String siteID;

    private String destinationDirectory;

    private String pythonDirectory;

    private transient Logger log = LoggerFactory.getLogger(getClass());

    /**
     * Default Constructor
     * <p>
     * Get the siteID and determine the appropriate paths.
     * </p>
     *
     * @param siteID
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
            destinationDirectory = pathMgr
                    .getFile(caveStaticConfig,
                            LocalizationUtil.join("gfe", "userPython"))
                    .getCanonicalPath();
        } catch (IOException e) {
            log.error("Unable to determine the destination directory", e);
            log.warn("Textproducts will not be configured.");
            return;
        }

        // configureTextProductsFile
        LocalizationContext commonStaticBase = pathMgr.getContext(
                LocalizationContext.LocalizationType.COMMON_STATIC,
                LocalizationContext.LocalizationLevel.BASE);

        try {
            pythonDirectory = pathMgr.getFile(commonStaticBase, ".")
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
     * Create and configure text products
     * <p>
     * Call the configureTextProduct.py script. After successfully creating and
     * configuring the text products, a cluster lock will be created to indicate
     * the last time the site's text products were configured.
     * </p>
     */
    public void execute() {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext context = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.CONFIGURED);

        // regenerate siteCFG.py
        ILocalizationFile lf = null;
        try {
            lf = pathMgr.getLocalizationFile(context, LocalizationUtil
                    .join(GfePyIncludeUtil.COMMON_GFE, "SiteCFG.py"));

            CoreDao dao = new CoreDao(DaoConfig.forDatabase("maps"));
            Object[] results = dao.executeSQLQuery(CWA_QUERY);
            try (SaveableOutputStream lfStream = lf.openOutputStream();
                    PrintWriter out = new PrintWriter(lfStream)) {
                out.println("##");
                out.println(
                        "# Contains information about products, regions, etc. for each site");
                out.println("# in the country.");
                out.println(
                        "# region= two-letter regional identifier, mainly used for installation of");
                out.println("#         text product templates");
                out.println("SiteInfo= {");

                for (Object obj : results) {
                    Object[] row = (Object[]) obj;
                    String wfo = (String) row[0];
                    String region = (String) row[1];
                    String fullStationID = (String) row[2];
                    String wfoCityState = (String) row[3];
                    String wfoCity = (String) row[4];
                    String state = (String) row[5];

                    out.println(formatEntry(wfo, region, fullStationID,
                            wfoCityState, wfoCity, state));

                    // Add in AFC's dual domain sites
                    if ("AFC".equals(wfo)) {
                        out.println(formatEntry("AER", region, fullStationID,
                                wfoCityState, wfoCity, state));
                        out.println(formatEntry("ALU", region, fullStationID,
                                wfoCityState, wfoCity, state));
                    }
                }
                out.println("}");

                // Import national centers info
                out.println("try:");
                out.println("    import OtherCFG");
                out.println("    SiteInfo.update(OtherCFG.SiteInfo)");
                out.println("except:");
                out.println(
                        "    LogStream.logProblem(\"Error importing OtherCFG:\\n\"+LogStream.exc())");

                out.close();
                lfStream.save();
            }

        } catch (Exception e) {
            statusHandler.error(e.getLocalizedMessage(), e);
        }

        LocalizationContext baseCx = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);

        File file = pathMgr.getFile(baseCx,
                LocalizationUtil.join("gfe", "textproducts", "Generator.py"));
        String filePath = file.getPath();
        String commonPython = GfePyIncludeUtil.getCommonGfeIncludePath();

        Map<String, Object> argList = new HashMap<>();
        argList.put("siteId", siteID);
        argList.put("destinationDir", destinationDirectory);

        try (PythonScript python = new PythonScript(
                new JepConfig()
                        .setIncludePath(PyUtil.buildJepIncludePath(
                                pythonDirectory, commonPython, file.getParent()))
                        .setClassLoader(getClass().getClassLoader()), filePath)) {

            // Open the Python interpreter using the designated script.
            @SuppressWarnings("unchecked")
            List<String> protectedFilesList = (List<String>) python
                    .execute("runFromJava", argList);

            updateProtectedFile(protectedFilesList);
        } catch (JepException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error Configuring Text Products", e);
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
     *
     * @param protectedFilesList
     */
    private void updateProtectedFile(List<String> protectedFilesList) {
        if ((protectedFilesList != null) && (!protectedFilesList.isEmpty())) {
            ProtectedFiles.protect(siteID, protectedFilesList);
        }
    }
}
