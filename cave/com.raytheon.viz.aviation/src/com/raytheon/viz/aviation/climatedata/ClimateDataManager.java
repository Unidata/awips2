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
package com.raytheon.viz.aviation.climatedata;

import java.io.File;
import java.io.FileFilter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.aviation.climatology.ClimatePythonTask;
import com.raytheon.viz.aviation.climatology.ClimatePythonTask.ClimatePythonListener;

/**
 * This manages sending a request to an AWIPS 1 python script and returning the
 * results. Used by the various climate data dialogs.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 17, 2009            avarani     Initial creation
 * Jan 20, 2011 4864       rferrel     Use win.executeDone
 * Feb 16, 2011 7878       rferrel     Modifications for create ident/site.
 * May 10, 2011 9059       rferrel     Extended times outs on executes
 * Jun 01, 2011 7878       rferrel     Adjusted time out when creating nc files
 * Jul 10, 2015 16907      zhao        Changed time limit from 600 to 6000 for processData() & assessData()
 * Jan 21, 2016 18395      zhao        Modified processData()
 * Jan 29, 2016 18396      zhao        Modified sendObj()
 * Jun 16, 2016 5693       rferrel     Added fixClimateFiles().
 * Aug  6, 2019 7878       tgurney     Python 3 rewrite. Replace String stnPickle
 *                                     with byte[]. Move some utility methods
 *                                     here from ClimateDataPython.
 *
 * </pre>
 *
 * @author avarani
 */

public class ClimateDataManager {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ClimateDataManager.class);

    private static final String HD5_SUFFIX = ".hd5";

    private static final ClimateDataManager INSTANCE = new ClimateDataManager();

    /* Allows only one Python script in this class to be running at a time */
    private final Object pythonLock = new Object();

    private volatile IClimateDataMenuDlg cdmDlg;

    private volatile Map<String, Object> stationsMap;

    private volatile byte[] stnPickle;

    private ClimatePythonTask pythonTask;

    private int numSites = 1;

    private ClimateDataManager() {
        // singleton
    }

    public static ClimateDataManager getInstance() {
        return INSTANCE;
    }

    public void reset() {
        stnPickle = null;
    }

    public void cancelPythonTask() {
        if (pythonTask != null) {
            pythonTask.cancel();
            pythonTask = null;
        }
    }

    private synchronized void startPythonTask(String methodName,
            Map<String, Object> methodKwargs, ClimatePythonListener listener,
            int timeoutSec) {
        cancelPythonTask();
        pythonTask = ClimatePythonTask.execute(methodName, methodKwargs,
                listener, timeoutSec, pythonLock);
    }

    public void getIdnum(final List<String> idents, final int timeout,
            final IClimateDataMenuDlg cdmDlg) {
        this.cdmDlg = cdmDlg;
        ClimatePythonListener listener = new ClimatePythonListener() {

            @Override
            public void sendObj(Object obj) {
                ClimateDataManager.getInstance().sendObj(obj);
            }

            @Override
            public void finished() {
                cdmDlg.executeDone();
                cdmDlg.checkSite();
            }
        };
        try {
            String ishDirPath = ClimateDataManager.getIshFilePath();
            List<String> climateFilePaths = new ArrayList<>();
            for (String ident : idents) {
                climateFilePaths
                        .add(ClimateDataManager.getClimateFilePath(ident));
            }
            Map<String, Object> args = new HashMap<>();
            args.put("idents", idents);
            args.put("ishDirPath", ishDirPath);
            args.put("climateFilePaths", climateFilePaths);
            startPythonTask("get_id_nums", args, listener, timeout);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error retrieving climate data", e);
        }
    }

    private void killStnPickle() throws Exception {
        Map<String, Object> args = new HashMap<>();
        args.put("stnPickle", stnPickle);
        ClimatePythonListener listener = ClimateDataManager.this::sendObj;
        startPythonTask("kill", args, listener, 20);
        pythonTask.waitFor();
    }

    public void assessStationsMap(final ClimateDataMenuDlg cdmDlg) {
        this.cdmDlg = cdmDlg;
        Map<String, Object> args = new HashMap<>();
        args.put("stnPickle", stnPickle);
        ClimatePythonListener listener = ClimateDataManager.this::sendObj;
        try {
            startPythonTask("get_stations_map", args, listener, 30);
            pythonTask.waitFor();
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error retrieving stations climate map", e);
        }
    }

    public void assessData(final boolean append, final List<String> items,
            final ClimateDataMenuDlg cdmDlg) {
        this.cdmDlg = cdmDlg;

        ClimatePythonListener listener = new ClimatePythonListener() {
            @Override
            public void sendObj(Object obj) {
                ClimateDataManager.this.sendObj(obj);
            }

            @Override
            public void finished() {
                cdmDlg.executeDone();
            }
        };
        try {
            if (stnPickle != null) {
                killStnPickle();
            }
            String ishDir = getIshFilePath();
            Map<String, Object> args = new HashMap<>();
            args.put("append", append);
            args.put("sites", items);
            args.put("climateDir", ishDir);
            numSites = items.size();
            startPythonTask("start", args, listener,
                    2 * TimeUtil.SECONDS_PER_HOUR * numSites);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error retrieving climate data", e);
        }
    }

    /**
     * Run the AWIPS 1 fix climate files for the desired sites. Assumes hd5
     * files exist in climate data directory.
     *
     * @param sites
     * @param cdmDlg
     */
    public void fixClimateFiles(final String[] sites,
            final IClimateDataMenuDlg cdmDlg) {
        this.cdmDlg = cdmDlg;

        try {
            final long t0 = System.currentTimeMillis();
            if (stnPickle != null) {
                killStnPickle();
            }
            String ishDir = getIshFilePath();
            Map<String, Object> args = new HashMap<>();
            args.put("sites", Arrays.asList(sites));
            args.put("climateDir", ishDir);
            numSites = sites.length;

            ClimatePythonListener listener = new ClimatePythonListener() {
                @Override
                public void sendObj(Object obj) {
                    ClimateDataManager.this.sendObj(obj);
                }

                @Override
                public void started() {
                    StringBuilder sb = new StringBuilder(
                            "Start Fix Climate Files: ");
                    sb.append(String.join(", ", sites));
                    cdmDlg.updateMonitor(sb.toString());
                    cdmDlg.executeStart();
                }

                @Override
                public void finished() {
                    long t1 = System.currentTimeMillis();
                    cdmDlg.updateMonitor("Finished Fix Climate Files in "
                            + TimeUtil.prettyDuration(t1 - t0));
                    cdmDlg.executeDone();
                }
            };
            startPythonTask("fixClimateFiles", args, listener, 0);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error retrieving climate data", e);
        }
    }

    public void processData(final boolean append, final List<String> items,
            final ClimateDataMenuDlg cdmDlg) {
        this.cdmDlg = cdmDlg;

        try {
            String ishDir = getIshFilePath();
            Map<String, Object> args = new HashMap<>();
            args.put("stnPickle", stnPickle);
            args.put("append", append);
            args.put("sites", items);
            args.put("climateDir", ishDir);
            numSites = items.size();

            ClimatePythonListener listener = new ClimatePythonListener() {
                @Override
                public void sendObj(Object obj) {
                    ClimateDataManager.this.sendObj(obj);
                }

                @Override
                public void finished() {
                    cdmDlg.executeDone();
                }
            };
            startPythonTask("process", args, listener,
                    2 * TimeUtil.SECONDS_PER_HOUR * numSites);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error retrieving climate data", e);
        }
    }

    public void validateData(final String ident,
            final ClimateDataMenuDlg cdmDlg) {
        this.cdmDlg = cdmDlg;

        ClimatePythonListener listener = new ClimatePythonListener() {
            @Override
            public void sendObj(Object obj) {
                ClimateDataManager.this.sendObj(obj);
            }

            @Override
            public void finished() {
                cdmDlg.executeDone();
            }
        };
        Map<String, Object> args = new HashMap<>();
        args.put("stnPickle", stnPickle);
        try {
            startPythonTask("validate", args, listener,
                    10 * TimeUtil.SECONDS_PER_MINUTE * numSites);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error retrieving climate data", e);
        }
    }

    public void commitData(final ClimateDataMenuDlg cdmDlg) {
        this.cdmDlg = cdmDlg;

        ClimatePythonListener listener = new ClimatePythonListener() {
            @Override
            public void sendObj(Object obj) {
                ClimateDataManager.this.sendObj(obj);
            }

            @Override
            public void finished() {
                cdmDlg.executeDone();
            }
        };
        try {
            Map<String, Object> args = new HashMap<>();
            args.put("stnPickle", stnPickle);
            startPythonTask("commit", args, listener,
                    10 * TimeUtil.SECONDS_PER_MINUTE * numSites);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error retrieving climate data", e);
        }
    }

    private void genQCStatFiles() {
        try {
            String ishDir = getIshFilePath();
            Map<String, Object> args = new HashMap<>();
            Set<String> sites = getStationsMap().keySet();
            args.put("siteList", String.join(",", sites));
            args.put("monthList", "01,02,03,04,05,06,07,08,09,10,11,12");
            args.put("climateDir", ishDir);
            ClimatePythonListener listener = obj -> ClimateDataManager.this
                    .sendObj(obj);
            // Time out 90 seconds for each month being generated.
            startPythonTask("genQCFiles", args, listener, 90 * 12 * numSites);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error generating climate QC files", e);
        }
    }

    public void rejectData(final ClimateDataMenuDlg cdmDlg) {
        this.cdmDlg = cdmDlg;
        Map<String, Object> args = new HashMap<>();
        args.put("stnPickle", stnPickle);

        ClimatePythonListener listener = new ClimatePythonListener() {
            @Override
            public void sendObj(Object obj) {
                ClimateDataManager.this.sendObj(obj);
            }

            @Override
            public void finished() {
                cdmDlg.executeDone();
            }
        };
        try {
            startPythonTask("reject", args, listener,
                    TimeUtil.SECONDS_PER_MINUTE * numSites);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error retrieving climate data", e);
        }
    }

    public List<String> getSites() {
        return cdmDlg.getSites();
    }

    public void setStationsMap(Map<String, Object> stationsMap) {
        this.stationsMap = stationsMap;
    }

    public Map<String, Object> getStationsMap() {
        return stationsMap;
    }

    @SuppressWarnings("unchecked")
    public void sendObj(Object obj) {
        if (!(obj instanceof Map)) {
            statusHandler.debug(
                    "ClimateDataManager.sendObj " + "got unrecognized object "
                            + obj + ". " + "Doing nothing with it");
            return;
        }
        Map<String, Object> returnMap = (Map<String, Object>) obj;
        String method = (String) returnMap.get("method");

        if ("get_id_nums".equals(method)) {
            String ident = (String) returnMap.get("ident");
            List<List<String>> list = (List<List<String>>) returnMap
                    .get("results");
            cdmDlg.populateSiteInfoList(ident, list);
            cdmDlg.assessBtn(true);
            cdmDlg.validateBtn(false);
        } else if ("updateMonitor".equals(method)) {
            String msg = (String) returnMap.get("msg");
            cdmDlg.updateMonitor(msg);
        } else if ("overwriteMonitor".equals(method)) {
            String msg = (String) returnMap.get("msg");
            cdmDlg.overwriteMonitor(msg);
        } else if ("make_qc".equals(method)) {
            genQCStatFiles();
        } else if ("stnPickle".equals(method)) {
            stnPickle = (byte[]) returnMap.get("stnPickle");
        } else if ("scriptsSetEnabled".equals(method)) {
            boolean enabled = "True".equals(returnMap.get("value").toString());
            cdmDlg.scriptsBtn(enabled);
            cdmDlg.processBtn(enabled);
        } else if ("validateSetEnabled".equals(method)) {
            boolean enabled = "True".equals(returnMap.get("value").toString());
            cdmDlg.validateBtn(enabled);
        } else if ("commitRejectSetEnabled".equals(method)) {
            boolean enabled = "True".equals(returnMap.get("value").toString());
            cdmDlg.commitBtn(enabled);
            cdmDlg.rejectBtn(enabled);
        } else if ("get_stations_map".equals(method)) {
            setStationsMap((Map<String, Object>) returnMap.get("map"));
        } else {
            statusHandler.debug("ClimateDataManager.sendObj: "
                    + "unimplemented method: " + method);
        }
    }

    public static String getClimateFilePath(String site) throws VizException {
        File dataDir = getClimateDir();
        File file = new File(dataDir, site + HD5_SUFFIX);
        String climateFile = file.getPath();
        return climateFile;
    }

    /**
     * Obtain path name of the directory with the ISD files and verify the files
     * exist.
     *
     * @return dataDir
     * @throws VizException
     *             when unable to find ISD files
     */
    public static String getIshFilePath() throws VizException {
        File dataDir = getClimateDir();

        File histFile = new File(dataDir, "isd-history.txt");
        if (!histFile.isFile()) {
            throw new VizException(
                    String.format("ISD history file: \"%s\" does not exist.",
                            histFile.getPath()));
        }

        File invFile = new File(dataDir, "isd-inventory.txt");
        if (!invFile.isFile()) {
            throw new VizException(
                    String.format("ISD inventory file: \"%s\" does not exist.",
                            invFile.getPath()));
        }

        return dataDir.getPath();
    }

    /**
     * Get a list of sites with hd5 files in the climate directory.
     *
     * @return sites
     * @throws VizException
     */
    public static String[] getSiteList() throws VizException {
        File dataDir = getClimateDir();
        File[] files = dataDir.listFiles(
                (FileFilter) pathname -> pathname.getName().endsWith(HD5_SUFFIX)
                        && pathname.isFile());
        String[] sites = new String[files.length];
        int sufixLength = HD5_SUFFIX.length();
        for (int index = 0; index < files.length; ++index) {
            String filename = files[index].getName();
            sites[index] = filename.substring(0,
                    filename.length() - sufixLength);
        }
        return sites;
    }

    /**
     *
     * @return climate data directory
     * @throws VizException
     */
    private static File getClimateDir() throws VizException {
        String dataDirStr = VizApp.getDataDir();
        File dataDir = new File(dataDirStr + "/aviation");
        if (!dataDir.isDirectory()) {
            throw new VizException(String.format(
                    "Directory: \"%s\" does not exist.", dataDir.getPath()));
        }
        return dataDir;
    }

}
