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
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import jep.JepException;

import com.raytheon.uf.common.activetable.VTECPartners;
import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.dataplugin.warning.AbstractWarningRecord;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.util.FileUtil;

/**
 * Watches ingested warnings for WOU products from the SPC (Storm Prediction
 * Center). If the warning is a WOU, then it looks to see if the site is in the
 * ATTN...WFO... line, and if so, sends a user message to GFE to alert users.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 03, 2008            njensen     Initial creation
 * Jul 10, 2009  #2590     njensen     Added multiple site support
 * Jun 10, 2014  #3268     dgilling    Re-factor based on AbstractWatchNotifierSrv.
 * Oct 08, 2014  #4953     randerso    Refactored AbstractWatchNotifierSrv to allow 
 *                                     subclasses to handle all watches if desired.
 *                                     Added hooks for TCVAdvisory creation
 *                                     Changed to use Python to store TCVAdvisory files
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public final class TPCWatchSrv extends AbstractWatchNotifierSrv {

    private static final String TPC_WATCH_TYPE = "TPC";

    private static final String TPC_SITE_ATTRIBUTE = "VTEC_TPC_SITE";

    private static final String DEFAULT_TPC_SITE = "KNHC";

    private static final String ALERT_TXT = "Alert: TCV has arrived from TPC. "
            + "Check for 'red' locks (owned by others) on your Hazard grid and resolve them. "
            + "If hazards are separated into temporary grids, please run Mergehazards. "
            + "Next...save Hazards grid. Finally, select PlotTPCEvents from Hazards menu.";

    private static final Map<String, String> phensigMap;

    private static final Map<String, String> actMap;

    static {
        Map<String, String> phensigMapTemp = new HashMap<String, String>(5, 1f);
        phensigMapTemp.put("HU.A", "Hurricane Watch");
        phensigMapTemp.put("HU.S", "Hurricane Local Statement");
        phensigMapTemp.put("HU.W", "Hurricane Warning");
        phensigMapTemp.put("TR.A", "Tropical Storm Watch");
        phensigMapTemp.put("TR.W", "Tropical Storm Warning");
        phensigMap = Collections.unmodifiableMap(phensigMapTemp);

        Map<String, String> actMapTemp = new HashMap<String, String>(3, 1f);
        actMapTemp.put("CON", "Continued");
        actMapTemp.put("CAN", "Cancelled");
        actMapTemp.put("NEW", "New");
        actMap = Collections.unmodifiableMap(actMapTemp);
    }

    private ThreadLocal<PythonScript> pythonScript = new ThreadLocal<PythonScript>() {

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.ThreadLocal#initialValue()
         */
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
                return new PythonScript(filePath, includePath, this.getClass()
                        .getClassLoader());
            } catch (JepException e) {
                statusHandler.error(e.getLocalizedMessage(), e);
            }
            return null;
        }

    };

    /**
     * Constructor
     */
    public TPCWatchSrv() {
        super(TPC_WATCH_TYPE);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.plugin.gfe.watch.AbstractWatchNotifierSrv#handleWatch
     * (java.util.List)
     */
    @Override
    public void handleWatch(List<AbstractWarningRecord> warningRecs) {
        super.handleWatch(warningRecs);

        // FIXME: This marks the pending.json file as Transmitted when ANY TCV
        // is decoded not the one just sent by this office. You need to look and
        // the warningRecs for a match to the issuing site and ETN or something
        // like that.

        // FIXME: The python created dicts are storing CreationTime and EndTime
        // (but not StartTime for some reason)
        // as python floats. These values are converted to Java floats which are
        // single precision 32-bit floats causing a possible loss of precision.
        // These should be stored a integer values or datetime objects to ensure
        // no precision loss. I have fixed a couple of places where endTime may
        // have been set to a float but we need to ensure we've got them all.
        for (String siteId : getActiveSites()) {
            this.saveTCVAdvisories(siteId);
        }
    }

    private void saveTCVAdvisories(String siteId) {
        File advisoriesDirectory = this.synchronizeTCVAdvisories(siteId);

        String pendingFilename = "pending.json";
        LocalizationFile pendingFile = this.getLocalizationFile(siteId,
                pendingFilename);

        Map<String, Object> pendingDict = this.loadJSONDictionary(pendingFile);
        if (pendingDict == null) {
            return;
        }

        pendingDict.put("Transmitted", true);

        String stormName = (String) pendingDict.get("StormName");
        String advisoryNumber = (String) pendingDict.get("AdvisoryNumber");
        String transmittedFilename = stormName + advisoryNumber + ".json";

        LocalizationFile transmittedFile = this.getLocalizationFile(siteId,
                transmittedFilename);
        this.saveJSONDictionary(transmittedFile, pendingDict);

        boolean transmittedFileSaved = false;
        try {
            transmittedFile.save();
            transmittedFileSaved = true;
        } catch (LocalizationOpFailedException e) {
            statusHandler.error("Failed to save advisory "
                    + transmittedFilename);
        }

        if (transmittedFileSaved) {
            boolean allCAN = (Boolean) pendingDict.get("AllCAN");
            if (allCAN) {
                for (File advisory : advisoriesDirectory.listFiles()) {
                    String advisoryName = advisory.getName();
                    if (advisoryName.startsWith(stormName)) {
                        LocalizationFile advisoryFile = this
                                .getLocalizationFile(siteId, advisoryName);
                        try {
                            advisoryFile.delete();
                        } catch (LocalizationOpFailedException e) {
                            statusHandler.error("Unable to delete "
                                    + advisoryFile, e);
                        }
                    }
                }
            }

            try {
                pendingFile.delete();
            } catch (LocalizationOpFailedException e) {
            }
        }

        this.synchronizeTCVAdvisories(siteId);
    }

    private File synchronizeTCVAdvisories(String siteId) {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext context = pathMgr.getContextForSite(
                LocalizationType.CAVE_STATIC, siteId);

        // Retrieving a directory causes synching to occur
        File file = pathMgr.getLocalizationFile(context,
                this.getTCVAdvisoryPath()).getFile();

        return file;
    }

    private LocalizationFile getLocalizationFile(String siteId, String filename) {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext context = pathMgr.getContextForSite(
                LocalizationType.CAVE_STATIC, siteId);
        LocalizationFile localizationFile = pathMgr.getLocalizationFile(
                context, this.getTCVAdvisoryPath() + filename);

        return localizationFile;
    }

    private String getTCVAdvisoryPath() {
        return "gfe/tcvAdvisories/";
    }

    private Map<String, Object> loadJSONDictionary(LocalizationFile lf) {
        if (lf != null) {
            PythonScript script = this.pythonScript.get();
            if (script != null) {
                Map<String, Object> args = new HashMap<String, Object>();
                args.put("localizationType", lf.getContext()
                        .getLocalizationType());
                args.put("siteID", lf.getContext().getContextName());
                args.put("fileName", lf.getName());
                try {
                    @SuppressWarnings("unchecked")
                    Map<String, Object> retVal = (Map<String, Object>) script
                            .execute("loadJsonFromJava", args);
                    return retVal;
                } catch (JepException e) {
                    statusHandler.error(
                            "Error loading TCV advisory from " + lf, e);
                }
            }
        }

        return null;
    }

    private void saveJSONDictionary(LocalizationFile lf,
            Map<String, Object> dict) {
        if (lf != null) {
            PythonScript script = this.pythonScript.get();
            if (script != null) {
                Map<String, Object> args = new HashMap<String, Object>();
                args.put("localizationType", lf.getContext()
                        .getLocalizationType());
                args.put("siteID", lf.getContext().getContextName());
                args.put("fileName", lf.getName());
                args.put("javaObject", dict);
                try {
                    script.execute("saveJsonFromJava", args);
                } catch (JepException e) {
                    statusHandler
                            .error("Error saving TCV advisory to " + lf, e);
                }
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.plugin.gfe.warning.AbstractWarningNotifierSrv#
     * buildNotification(java.util.List,
     * com.raytheon.uf.common.activetable.VTECPartners)
     */
    @Override
    protected String buildNotification(List<AbstractWarningRecord> decodedVTEC,
            VTECPartners partnersConfig) {
        String tpcSite = partnersConfig.getattr(TPC_SITE_ATTRIBUTE,
                DEFAULT_TPC_SITE).toString();

        // get all VTEC records, assemble unique list of phen/sig and storm#
        Map<String, Set<String>> phensigStormAct = new HashMap<String, Set<String>>();
        for (AbstractWarningRecord e : decodedVTEC) {
            if (tpcSite.equals(e.getOfficeid())) {
                String phensig = e.getPhensig();
                String storm = e.getEtn();
                String act = e.getAct();
                Set<String> psActs = phensigStormAct.get(phensig + ":" + storm);
                if (psActs == null) {
                    psActs = new TreeSet<String>();
                    phensigStormAct.put(phensig + ":" + storm, psActs);
                }
                psActs.add(act);
            }
        }

        if (phensigStormAct.isEmpty()) {
            statusHandler
                    .debug("TPC Notification: no HU/TR vtec lines, or not NEW action code");
            return null;
        }

        // create the message
        StringBuilder msg = new StringBuilder(ALERT_TXT);
        for (String phensigStorm : phensigStormAct.keySet()) {
            Collection<String> acts = phensigStormAct.get(phensigStorm);
            String[] splitKey = phensigStorm.split(":");
            String phensig = splitKey[0];
            String storm = splitKey[1];

            String t1 = phensigMap.get(phensig);
            if (t1 == null) {
                t1 = phensig;
            }
            msg.append(t1 + ": #" + storm + "(");
            String sep = "";
            for (String a : acts) {
                String a1 = actMap.get(a);
                if (a1 == null) {
                    a1 = a;
                }
                msg.append(sep).append(a1);
                sep = ",";
            }
            msg.append("). ");
        }

        return msg.toString();
    }
}