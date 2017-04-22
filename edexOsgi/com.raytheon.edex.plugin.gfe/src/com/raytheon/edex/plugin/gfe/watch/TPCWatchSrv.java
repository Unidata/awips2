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
package com.raytheon.edex.plugin.gfe.watch;

import java.io.File;
import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.edex.plugin.gfe.server.IFPServer;
import com.raytheon.uf.common.activetable.VTECPartners;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.dataplugin.gfe.textproduct.CombinationsFileUtil;
import com.raytheon.uf.common.dataplugin.warning.AbstractWarningRecord;
import com.raytheon.uf.common.dataplugin.warning.PracticeWarningRecord;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonIncludePathUtil;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.common.util.RunProcess;
import com.raytheon.uf.edex.activetable.ActiveTablePyIncludeUtil;

import jep.JepException;

/**
 * Watches ingested warnings for WOU products from the SPC (Storm Prediction
 * Center). If the warning is a WOU, then it looks to see if the site is in the
 * ATTN...WFO... line, and if so, sends a user message to GFE to alert users.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Oct 03, 2008           njensen   Initial creation
 * Jul 10, 2009  2590     njensen   Added multiple site support
 * May 12, 2014  3157     dgilling  Re-factor based on AbstractWatchNotifierSrv.
 * Jun 10, 2014  3268     dgilling  Re-factor based on AbstractWatchNotifierSrv.
 * Oct 08, 2014  4953     randerso  Refactored AbstractWatchNotifierSrv to allow
 *                                  subclasses to handle all watches if desired.
 *                                  Added hooks for TCVAdvisory creation Changed
 *                                  to use Python to store TCVAdvisory files
 *                                  Added code to keep practice and operational
 *                                  advisory files separated Added call to
 *                                  nwrwavestcv.csh Added support for sending
 *                                  TCVAdvisory files to VTEC partners
 * Nov 12, 2015   4834     njensen     Changed LocalizationOpFailedException to LocalizationException
 * Jan 27, 2016   5237     tgurney     Replace LocalizationFile with ILocalizationFile
 * Apr 13, 2016  5577     randerso  Add support for pre-TCV
 * Nov 12, 2015  4834     njensen   Changed LocalizationOpFailedException to
 *                                  LocalizationException
 * Jan 27, 2016  5237     tgurney   Replace LocalizationFile with
 *                                  ILocalizationFile
 * Sep 01, 2016  5873     randerso  Changed to use StormNumber instead of
 *                                  StormName and add leading zeroes to advisory
 *                                  number in json file names to ensure they
 *                                  sort correctly.
 * Oct 03, 2016  19293    randerso  Changes for 2017 Tropical Season
 * Nov 11, 2016  19293    randerso  Use new CombinationsFileUtil methods that
 *                                  allow specifying the site ID
 *


 *                                  correctly
 * Sep 01, 2016  5873     randerso  Add leading zeroes to advisory number in
 *                                  json file names to ensure they sort
 * 
 * </pre>
 *
 * @author njensen
 * @version 1.0
 */

public final class TPCWatchSrv extends AbstractWatchNotifierSrv {
    private static final String NWRWAVES_SCRIPT = "/awips/adapt/NWRWAVES/nwrwavestcv.csh ";

    private static final String TCV_ADVISORY_PATH = FileUtil.join("gfe",
            "tcvAdvisories");

    private static final String PRACTICE_PATH = FileUtil.join(TCV_ADVISORY_PATH,
            "practice");

    private static final String TPC_WATCH_TYPE = "TPC";

    private static final String DEFAULT_TPC_SITE = "KNHC";

    private static final String ALERT_TXT = "Alert: TCV has arrived %s. "
            + "Check for 'red' locks (owned by others) on your Hazard grid and resolve them. "
            + "If hazards are separated into temporary grids, please run Mergehazards. "
            + "Next...save Hazards grid. Finally, select PlotTPCEvents from Hazards menu.";

    private static final Pattern ADVISORY_NUMBER_PATTERN = Pattern
            .compile("([0-9]+)([A-Z]*)");

    private static final Pattern ATLANTIC_XXX = Pattern.compile("AT\\d");

    private static final Pattern PACIFIC_XXX = Pattern.compile("[CE]P\\d");

    private static final ThreadLocal<PythonScript> pythonScript = new ThreadLocal<PythonScript>() {

        @Override
        protected PythonScript initialValue() {
            IPathManager pathMgr = PathManagerFactory.getPathManager();
            LocalizationContext context = pathMgr.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
            LocalizationFile lf = pathMgr.getLocalizationFile(context, FileUtil
                    .join(GfePyIncludeUtil.COMMON_GFE, "JsonSupport.py"));

            String filePath = lf.getFile().getAbsolutePath();

            String includePath = PyUtil.buildJepIncludePath(true,
                    GfePyIncludeUtil.getCommonGfeIncludePath());

            try {
                return new PythonScript(filePath, includePath,
                        this.getClass().getClassLoader());
            } catch (JepException e) {
                statusHandler.error(e.getLocalizedMessage(), e);
            }
            return null;
        }

    };

    /*
     * regex to parse HLS MND header
     */
    private static final Pattern HLS_MND_PATTERN = Pattern.compile(
            "^.*\\s(?<stormName>\\w+)\\sLOCAL STATEMENT "
                    + "(?<advisoryType>SPECIAL |INTERMEDIATE )?"
                    + "(ADVISORY NUMBER\\s(?<advisoryNumber>[0-9]+[A-Z]*))\\s*\\n"
                    + "NATIONAL WEATHER SERVICE .*\\s(?<stormNumber>[A-Z]+[0-9]+)\\s*$",
            Pattern.CASE_INSENSITIVE | Pattern.MULTILINE);

    /**
     * Constructor
     */
    public TPCWatchSrv() {
        super(TPC_WATCH_TYPE);
    }

    @Override
    public void handleWatch(List<AbstractWarningRecord> warningRecs) {
        /*
         * Since all records originate from a single TCV product the issuing
         * office and record type will be the same we only need to look at the
         * first record.
         */
        AbstractWarningRecord record = warningRecs.get(0);
        String pil = record.getPil();
        String xxxId = record.getXxxid();
        boolean practiceMode = (record instanceof PracticeWarningRecord);
        String issuingOffice = record.getOfficeid();

        // if it's a TCV or pre-TCV
        if ("TCV".equals(pil) || "PTC".equals(pil)) {
            super.handleWatch(warningRecs);
        }

        // if we are not in practice mode
        if (!practiceMode && "TCV".equals(pil)) {

            // if xxxId does NOT end with a digit (i.e. its NOT a national TCV)
            if (!Character.isDigit(xxxId.charAt(xxxId.length() - 1))) {

                // build the full 9-letter PIL
                String fullPil = SiteMap.getInstance()
                        .mapICAOToCCC(issuingOffice) + pil + xxxId;

                // build the command line for the NWRWAVES script
                final String command = NWRWAVES_SCRIPT + fullPil;

                // Create a separate thread to run the script
                Thread thread = new Thread(new Runnable() {

                    @Override
                    public void run() {
                        RunProcess proc;
                        try {
                            proc = RunProcess.getRunProcess().exec(command);
                        } catch (IOException e) {
                            statusHandler.error("Error executing " + command,
                                    e);
                            return;
                        }

                        int exitCode = proc.waitFor();
                        if (exitCode != 0) {
                            statusHandler.error(
                                    command + " terminated abnormally with exit code: "
                                            + exitCode);
                        }
                    }
                });

                thread.start();
            }
        }

        // update TCV Advisories
        Matcher xxxMatcher = ATLANTIC_XXX.matcher(xxxId);
        for (String siteId : getActiveSites()) {
            String site4 = SiteMap.getInstance().getSite4LetterId(siteId);
            if (issuingOffice.equals(site4)) {
                if ("TCV".equals(pil)) {
                    if (xxxMatcher.matches()) {
                        this.handleNationalTCV(siteId, xxxId, practiceMode);
                    } else {
                        // if WFO TCV save the pending.json files
                        this.saveTCVAdvisories(siteId, pil, practiceMode);
                    }

                }
                // if HLS then delete the advisory files if the watch is
                // canceled
                else if ("HLS".equals(pil)) {
                    this.deleteTCVAdvisoriesIfCanceled(siteId, pil,
                            practiceMode, record.getRawmessage());
                }
                break; // found matching officeId so we're done
            }
        }
    }

    private void handleNationalTCV(String siteId, String xxxid,
            boolean practiceMode) {
        this.synchronizeTCVAdvisories(siteId, practiceMode);
        String filename = xxxid + ".json";
        LocalizationFile lf = this.getLocalizationFile(siteId, filename,
                practiceMode);

        if (lf != null && lf.exists()) {
            Map<String, Object> advDict = loadJSONDictionary(lf);

            Object obj = advDict.get("AllCAN");
            boolean allCAN = (obj == null ? false : (Boolean) obj);

            String comboName = String.format("Combinations_%s_%s", xxxid,
                    siteId);
            if (allCAN) {
                // create a flag file to indicate AllCAN has occurred
                LocalizationFile allCANFile = this.getLocalizationFile(siteId,
                        xxxid + ".allCAN", practiceMode);
                try (SaveableOutputStream stream = allCANFile
                        .openOutputStream()) {
                    // Nothing to write, just need empty file
                    stream.save();
                } catch (IOException | LocalizationException e) {
                    statusHandler.error("Error creating .allCAN file", e);
                }

                // delete existing json file
                try {
                    lf.delete();
                } catch (LocalizationException e) {
                    statusHandler.error("Error removing " + lf.toString(), e);
                }

                // empty the combinations files
                List<List<String>> combinations = Collections.emptyList();
                try {
                    CombinationsFileUtil.saveCombinationsFile(siteId,
                            combinations, comboName);
                    CombinationsFileUtil.saveCombinationsFile(siteId,
                            combinations, "prev" + comboName);
                } catch (LocalizationException e) {
                    statusHandler.error("Error emptying combinations files.",
                            e);
                }

            } else {
                // copy combinations file to prev
                try {
                    List<List<String>> combinations = CombinationsFileUtil
                            .loadCombinationsFile(siteId, comboName);
                    CombinationsFileUtil.saveCombinationsFile(siteId,
                            combinations, "prev" + comboName);
                } catch (GfeException | LocalizationException e) {
                    statusHandler.error(
                            "Error copying current combinations file to prev.",
                            e);
                }
            }
            sendTCVFiles(siteId);
        }
    }

    private void saveTCVAdvisories(String siteId, String pil,
            boolean practiceMode) {
        this.synchronizeTCVAdvisories(siteId, practiceMode);

        String pendingFilename = "pending.json";
        LocalizationFile pendingFile = this.getLocalizationFile(siteId,
                pendingFilename, practiceMode);

        Map<String, Object> pendingDict = this.loadJSONDictionary(pendingFile);
        if (pendingDict == null) {
            return;
        }

        pendingDict.put("Transmitted", true);

        String stormNumber = (String) pendingDict.get("StormNumber");
        String advisoryNumber = (String) pendingDict.get("AdvisoryNumber");

        Matcher matcher = ADVISORY_NUMBER_PATTERN.matcher(advisoryNumber);
        if (matcher.matches()) {
            advisoryNumber = String.format("%03d%s",
                    Integer.valueOf(matcher.group(1)), matcher.group(2));
        }

        String transmittedFilename = stormNumber + "_" + advisoryNumber
                + ".json";

        LocalizationFile transmittedFile = this.getLocalizationFile(siteId,
                transmittedFilename, practiceMode);

        boolean transmittedFileSaved = this.saveJSONDictionary(transmittedFile,
                pendingDict);

        if (transmittedFileSaved) {
            try {
                pendingFile.delete();
            } catch (LocalizationException e) {
                statusHandler.error("Unable to delete " + pendingFile, e);
            }

            // if not practice mode
            if (!practiceMode) {
                // send TCV files to VTEC partner sites
                sendTCVFiles(siteId);
            }
        }
    }

    private void deleteTCVAdvisoriesIfCanceled(String siteId, String pil,
            boolean practiceMode, String productText) {
        File advisoriesDirectory = this.synchronizeTCVAdvisories(siteId,
                practiceMode);

        Matcher matcher = HLS_MND_PATTERN.matcher(productText);
        if (matcher.find()) {

            String stormNumber = matcher.group("stormNumber");

            // get the list of json files for this storm
            List<String> jsonFiles = new ArrayList<>();
            try (DirectoryStream<Path> stream = Files.newDirectoryStream(
                    advisoriesDirectory.toPath(), stormNumber + "_*.json")) {
                for (Path path : stream) {
                    jsonFiles.add(path.getFileName().toString());
                }
            } catch (IOException e) {
                statusHandler
                        .error("Unable to get list of json files for storm: "
                                + stormNumber, e);
            }

            if (!jsonFiles.isEmpty()) {
                // load the highest numbered file for this storm
                Collections.sort(jsonFiles, Collections.reverseOrder());
                LocalizationFile advFile = this.getLocalizationFile(siteId,
                        jsonFiles.get(0), practiceMode);

                Map<String, Object> advDict = loadJSONDictionary(advFile);

                boolean allCAN = (Boolean) advDict.get("AllCAN");
                if (allCAN) {
                    // create a flag file to indicate AllCAN has occurred
                    LocalizationFile allCANFile = this.getLocalizationFile(
                            siteId, stormNumber + ".allCAN", practiceMode);
                    try (SaveableOutputStream stream = allCANFile
                            .openOutputStream()) {
                        // Nothing to write, just need empty file
                        stream.save();
                    } catch (IOException | LocalizationException e) {
                        statusHandler.error("Error creating .allCAN file", e);
                    }

                    for (String advisoryName : jsonFiles) {
                        LocalizationFile advisoryFile = this
                                .getLocalizationFile(siteId, advisoryName,
                                        practiceMode);
                        try {
                            advisoryFile.delete();
                        } catch (LocalizationException e) {
                            statusHandler.error(
                                    "Unable to delete " + advisoryFile, e);
                        }
                    }

                    // if not practice mode
                    if (!practiceMode) {
                        // send TCV files to VTEC partner sites
                        sendTCVFiles(siteId);
                    }
                }
            }
        }
    }

    private void sendTCVFiles(String siteId) {
        IFPServer ifpServer = IFPServer.getActiveServer(siteId);
        if (ifpServer == null) {
            return;
        }
        IFPServerConfig config = ifpServer.getConfig();

        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext commonBaseCx = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        String scriptPath = pathMgr
                .getFile(commonBaseCx, FileUtil
                        .join(ActiveTablePyIncludeUtil.VTEC, "sendTCV.py"))
                .getPath();

        String pythonIncludePath = PyUtil.buildJepIncludePath(
                PythonIncludePathUtil.getCommonPythonIncludePath(),
                ActiveTablePyIncludeUtil.getCommonGfeIncludePath(),
                ActiveTablePyIncludeUtil.getVtecIncludePath(siteId),
                ActiveTablePyIncludeUtil.getGfeConfigIncludePath(siteId),
                ActiveTablePyIncludeUtil.getIscScriptsIncludePath());

        PythonScript script = null;
        try {
            script = new PythonScript(scriptPath, pythonIncludePath,
                    this.getClass().getClassLoader());

            try {
                Map<String, Object> argMap = new HashMap<>();
                argMap.put("siteID", siteId);
                argMap.put("config", config);
                script.execute("runFromJava", argMap);
            } catch (JepException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error executing sendTCV.", e);
            }

            script.dispose();

        } catch (JepException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to instantiate sendTCV python script object.", e);
        }
    }

    private File synchronizeTCVAdvisories(String siteId, boolean practiceMode) {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext context = pathMgr
                .getContextForSite(LocalizationType.CAVE_STATIC, siteId);

        // Retrieving a directory causes synching to occur
        File file = pathMgr
                .getLocalizationFile(context, getTCVAdvisoryPath(practiceMode))
                .getFile();

        return file;
    }

    private LocalizationFile getLocalizationFile(String siteId, String filename,
            boolean practiceMode) {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext context = pathMgr
                .getContextForSite(LocalizationType.CAVE_STATIC, siteId);

        LocalizationFile localizationFile = pathMgr.getLocalizationFile(context,
                FileUtil.join(getTCVAdvisoryPath(practiceMode), filename));

        return localizationFile;
    }

    private String getTCVAdvisoryPath(boolean practiceMode) {
        return practiceMode ? PRACTICE_PATH : TCV_ADVISORY_PATH;
    }

    private Map<String, Object> loadJSONDictionary(ILocalizationFile lf) {
        if (lf != null) {
            PythonScript script = pythonScript.get();
            if (script != null) {
                Map<String, Object> args = new HashMap<>();
                args.put("localizationType",
                        lf.getContext().getLocalizationType());
                args.put("siteID", lf.getContext().getContextName());
                args.put("fileName", lf.getPath());
                try {
                    @SuppressWarnings("unchecked")
                    Map<String, Object> retVal = (Map<String, Object>) script
                            .execute("loadJsonFromJava", args);
                    return retVal;
                } catch (JepException e) {
                    statusHandler.error("Error loading TCV advisory from " + lf,
                            e);
                }
            }
        }

        return null;
    }

    private boolean saveJSONDictionary(ILocalizationFile lf,
            Map<String, Object> dict) {
        if (lf != null) {
            PythonScript script = pythonScript.get();
            if (script != null) {
                Map<String, Object> args = new HashMap<>();
                args.put("localizationType",
                        lf.getContext().getLocalizationType());
                args.put("siteID", lf.getContext().getContextName());
                args.put("fileName", lf.getPath());
                args.put("javaObject", dict);
                try {
                    script.execute("saveJsonFromJava", args);
                    return true;
                } catch (JepException e) {
                    statusHandler.error("Error saving TCV advisory to " + lf,
                            e);
                    return false;
                }
            }
        }
        return false;
    }

    @Override
    protected String buildNotification(List<AbstractWarningRecord> decodedVTEC,
            VTECPartners partnersConfig) {
        Collection<String> tpcSites = partnersConfig
                .getTpcSites(DEFAULT_TPC_SITE);

        // get all VTEC records, assemble unique list of phen/sig and storm#
        Map<String, Set<String>> phensigStormAct = new HashMap<>();
        for (AbstractWarningRecord e : decodedVTEC) {
            if (tpcSites.contains(e.getOfficeid())) {
                String phensig = e.getPhensig();
                String storm = e.getEtn();
                String act = e.getAct();
                Set<String> psActs = phensigStormAct.get(phensig + ":" + storm);
                if (psActs == null) {
                    psActs = new TreeSet<>();
                    phensigStormAct.put(phensig + ":" + storm, psActs);
                }
                psActs.add(act);
            }
        }

        if (phensigStormAct.isEmpty()) {
            statusHandler.debug(
                    "TPC Notification: no HU/TR vtec lines, or not NEW action code");
            return null;
        }

        String pil = decodedVTEC.get(0).getPil();
        String xxxId = decodedVTEC.get(0).getXxxid();
        Matcher xxxMatcher = PACIFIC_XXX.matcher(xxxId);
        if ("PTC".equals(pil)) {
            return String.format(ALERT_TXT, "from NHC");
        } else if ("TCV".equals(pil) && xxxMatcher.matches()) {
            return String.format(ALERT_TXT, "for your office");
        } else {
            return null;
        }

    }
}
