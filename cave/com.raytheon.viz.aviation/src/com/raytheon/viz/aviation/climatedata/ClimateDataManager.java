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

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import jep.JepException;

import com.raytheon.uf.common.python.multiprocessing.PyProcessListener;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.exception.VizException;

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
 * Jan 29, 2016 18396      zhao        Modified objReceived()
 * Jun 16, 2016 5693       rferrel     Added fixClimateFiles().
 * 
 * </pre>
 * 
 * @author avarani
 * @version 1.0
 */

public class ClimateDataManager implements PyProcessListener {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ClimateDataManager.class);

    private static ClimateDataManager dataMgr;

    private IClimateDataMenuDlg cdmDlg;

    private Map<String, Object> stationsMap;

    private String stnPickle;

    private int numSites;

    private ClimateDataManager() {
        numSites = 1;
    }

    @Override
    public Object clone() throws CloneNotSupportedException {
        throw new CloneNotSupportedException();
    }

    public static synchronized ClimateDataManager getInstance() {
        if (dataMgr == null) {
            dataMgr = new ClimateDataManager();
        }

        return dataMgr;
    }

    public void reset() {
        stnPickle = null;
    }

    public void getIdnum(final String ident, final int timeout,
            final IClimateDataMenuDlg cdmDlg) {
        this.cdmDlg = cdmDlg;
        Runnable run = new Runnable() {

            @Override
            public void run() {
                PythonClimateDataProcess pythonScript = null;
                try {
                    long t0 = System.currentTimeMillis();
                    String ishDirPath = ClimateDataPython.getIshFilePath();
                    String climateFilePath = ClimateDataPython
                            .getClimateFilePath(ident);
                    pythonScript = ClimateDataPython.getClimateInterpreter();
                    Map<String, Object> args = new HashMap<>();
                    args.put("ident", ident);
                    args.put("ishDirPath", ishDirPath);
                    args.put("climateFilePath", climateFilePath);
                    pythonScript.execute("get_id_nums", args,
                            ClimateDataManager.this, timeout);
                    long t1 = System.currentTimeMillis();
                    System.out.println("getIdnum() running time: " + (t1 - t0));
                } catch (JepException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error retrieving climate data", e);
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM, e.getMessage(), e);
                } finally {
                    if (pythonScript != null) {
                        pythonScript.dispose();
                    }
                    cdmDlg.executeDone();
                    cdmDlg.checkSite();
                }
            }
        };
        Thread t = new Thread(run);
        t.start();
    }

    private void killStnPickle() throws JepException {
        PythonClimateDataProcess pythonScript = ClimateDataPython
                .getClimateInterpreter();
        Map<String, Object> args = new HashMap<>();
        args.put("stnPickle", stnPickle);
        pythonScript.execute("kill", args, this, 20);
    }

    public void assessStationsMap(final ClimateDataMenuDlg cdmDlg) {
        this.cdmDlg = cdmDlg;
        PythonClimateDataProcess pythonScript = ClimateDataPython
                .getClimateInterpreter();
        Map<String, Object> args = new HashMap<>();
        args.put("stnPickle", stnPickle);
        try {
            pythonScript.execute("get_stations_map", args, this, 30);
        } catch (JepException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error retrieving stations climate map", e);
        } finally {
            if (pythonScript != null) {
                pythonScript.dispose();
            }
        }
    }

    public void assessData(final boolean append, final List<String> items,
            final ClimateDataMenuDlg cdmDlg) {
        this.cdmDlg = cdmDlg;

        Runnable run = new Runnable() {

            @Override
            public void run() {
                PythonClimateDataProcess pythonScript = null;
                try {
                    long t0 = System.currentTimeMillis();
                    if (stnPickle != null) {
                        killStnPickle();
                    }
                    String ishDir = ClimateDataPython.getIshFilePath();
                    pythonScript = ClimateDataPython.getClimateInterpreter();
                    Map<String, Object> args = new HashMap<>();
                    args.put("append", append);
                    args.put("sites", items);
                    args.put("climateDir", ishDir);
                    numSites = items.size();
                    pythonScript.execute("start", args,
                            ClimateDataManager.this, 6000 * numSites);
                    long t1 = System.currentTimeMillis();
                    System.out.println("assessData() running time: "
                            + (t1 - t0));
                } catch (JepException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error retrieving climate data", e);
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error retrieving climate data", e);
                } finally {
                    if (pythonScript != null) {
                        pythonScript.dispose();
                    }
                    cdmDlg.executeDone();
                }
            }
        };
        Thread t = new Thread(run);
        t.start();
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
        StringBuilder sb = new StringBuilder("Start Fix Climate Files:");
        String prefix = " ";
        for (String site : sites) {
            sb.append(prefix).append(site);
            prefix = ", ";
        }
        updateMonitor(sb.toString());
        cdmDlg.executeStart();

        Runnable run = new Runnable() {

            @Override
            public void run() {
                PythonClimateDataProcess pythonScript = null;
                long t0 = -1;
                try {
                    t0 = System.currentTimeMillis();
                    if (stnPickle != null) {
                        killStnPickle();
                    }
                    String ishDir = ClimateDataPython.getIshFilePath();
                    pythonScript = ClimateDataPython.getClimateInterpreter();
                    Map<String, Object> args = new HashMap<>();
                    args.put("sites", Arrays.asList(sites));
                    args.put("climateDir", ishDir);
                    numSites = sites.length;
                    pythonScript.execute("fixClimateFiles", args,
                            ClimateDataManager.this, 6000 * numSites);
                } catch (JepException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error retrieving climate data", e);
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error retrieving climate data", e);
                } finally {
                    long t1 = System.currentTimeMillis();
                    if (pythonScript != null) {
                        pythonScript.dispose();
                    }
                    updateMonitor("Finished Fix Climate Files in "
                            + TimeUtil.prettyDuration(t1 - t0));
                    cdmDlg.executeDone();
                }
            }
        };
        Thread t = new Thread(run);
        t.start();
    }

    public void processData(final boolean append, final List<String> items,
            final ClimateDataMenuDlg cdmDlg) {
        this.cdmDlg = cdmDlg;

        Runnable run = new Runnable() {

            @Override
            public void run() {
                PythonClimateDataProcess pythonScript = null;
                try {
                    long t0 = System.currentTimeMillis();
                    String ishDir = ClimateDataPython.getIshFilePath();
                    pythonScript = ClimateDataPython.getClimateInterpreter();
                    Map<String, Object> args = new HashMap<>();
                    args.put("stnPickle", stnPickle);
                    args.put("append", append);
                    args.put("sites", items);
                    args.put("climateDir", ishDir);
                    numSites = items.size();
                    pythonScript.execute("process", args,
                            ClimateDataManager.this, 6000 * numSites);
                    long t1 = System.currentTimeMillis();
                    System.out.println("processData() running time: "
                            + (t1 - t0));
                } catch (JepException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error retrieving climate data", e);
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error retrieving climate data", e);
                } finally {
                    if (pythonScript != null) {
                        pythonScript.dispose();
                    }
                    cdmDlg.executeDone();
                }
            }
        };
        Thread t = new Thread(run);
        t.start();
    }

    public void validateData(final String ident, final ClimateDataMenuDlg cdmDlg) {
        this.cdmDlg = cdmDlg;

        Runnable run = new Runnable() {

            @Override
            public void run() {
                PythonClimateDataProcess pythonScript = null;
                try {
                    long t0 = System.currentTimeMillis();
                    pythonScript = ClimateDataPython.getClimateInterpreter();
                    Map<String, Object> args = new HashMap<>();
                    args.put("stnPickle", stnPickle);
                    pythonScript.execute("validate", args,
                            ClimateDataManager.this, 600 * numSites);
                    long t1 = System.currentTimeMillis();
                    System.out.println("validateData() running time: "
                            + (t1 - t0));
                } catch (JepException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error retrieving climate data", e);
                } finally {
                    if (pythonScript != null) {
                        pythonScript.dispose();
                    }
                    cdmDlg.executeDone();
                }
            }
        };
        Thread t = new Thread(run);
        t.start();
    }

    public void commitData(final ClimateDataMenuDlg cdmDlg) {
        this.cdmDlg = cdmDlg;

        Runnable run = new Runnable() {

            @Override
            public void run() {
                PythonClimateDataProcess pythonScript = null;
                try {
                    long t0 = System.currentTimeMillis();
                    pythonScript = ClimateDataPython.getClimateInterpreter();
                    Map<String, Object> args = new HashMap<>();
                    args.put("stnPickle", stnPickle);
                    pythonScript.execute("commit", args,
                            ClimateDataManager.this, 600 * numSites);
                    long t1 = System.currentTimeMillis();
                    System.out.println("commitData() running time: "
                            + (t1 - t0));
                } catch (JepException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error retrieving climate data", e);
                } finally {
                    if (pythonScript != null) {
                        pythonScript.dispose();
                    }
                    cdmDlg.executeDone();
                }
            }
        };
        Thread t = new Thread(run);
        t.start();
    }

    private void genQCStatFiles() {
        Runnable run = new Runnable() {

            @Override
            public void run() {
                String monthList = "01,02,03,04,05,06,07,08,09,10,11,12";
                PythonClimateDataProcess pythonScript = null;
                try {
                    long t0 = System.currentTimeMillis();
                    String ishDir = ClimateDataPython.getIshFilePath();
                    pythonScript = ClimateDataPython.getClimateInterpreter();
                    Map<String, Object> args = new HashMap<>();
                    StringBuilder siteList = new StringBuilder();
                    String sep = "";
                    for (String site : getStationsMap().keySet()) {
                        siteList.append(sep).append(site);
                        sep = ",";
                    }
                    args.put("siteList", siteList.toString());
                    args.put("monthList", monthList);
                    args.put("climateDir", ishDir);
                    // Time out 90 seconds for each month being generated.
                    pythonScript.execute("genQCFiles", args,
                            ClimateDataManager.this, (90 * 12) * numSites);
                    long t1 = System.currentTimeMillis();
                    System.out.println("getQCFiles() running time: "
                            + (t1 - t0));
                } catch (JepException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error generating climate QC files", e);
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error generating climate QC Files", e);
                } finally {
                    if (pythonScript != null) {
                        pythonScript.dispose();
                    }
                }
            }
        };
        Thread t = new Thread(run);
        t.start();
    }

    public void rejectData(final ClimateDataMenuDlg cdmDlg) {
        this.cdmDlg = cdmDlg;

        Runnable run = new Runnable() {

            @Override
            public void run() {
                PythonClimateDataProcess pythonScript = null;
                try {
                    long t0 = System.currentTimeMillis();
                    pythonScript = ClimateDataPython.getClimateInterpreter();
                    Map<String, Object> args = new HashMap<>();
                    args.put("stnPickle", stnPickle);
                    pythonScript.execute("reject", args,
                            ClimateDataManager.this, 60 * numSites);
                    long t1 = System.currentTimeMillis();
                    System.out.println("rejectData() running time: "
                            + (t1 - t0));
                } catch (JepException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error retrieving climate data", e);
                } finally {
                    if (pythonScript != null) {
                        pythonScript.dispose();
                    }
                    cdmDlg.executeDone();
                }
            }
        };
        Thread t = new Thread(run);
        t.start();
    }

    public List<String> getSites() {
        return cdmDlg.getSites();
    }

    public void updateMonitor(String msg) {
        cdmDlg.updateMonitor(msg);
    }

    public void overwriteMonitor(String msg) {
        cdmDlg.overwriteMonitor(msg);
    }

    public void scriptsBtn(boolean enabled) {
        cdmDlg.scriptsBtn(enabled);
    }

    public void processBtn(boolean enabled) {
        cdmDlg.processBtn(enabled);
    }

    public void validateBtn(boolean enabled) {
        cdmDlg.validateBtn(enabled);
    }

    public void commitBtn(boolean enabled) {
        cdmDlg.commitBtn(enabled);
    }

    public void rejectBtn(boolean enabled) {
        cdmDlg.rejectBtn(enabled);
    }

    public void saveLogBtn(boolean enabled) {
        cdmDlg.saveLogBtn(enabled);
    }

    public void setStationsMap(Map<String, Object> stationsMap) {
        this.stationsMap = stationsMap;
    }

    public Map<String, Object> getStationsMap() {
        return stationsMap;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.python.multiprocessing.PyProcessListener#objReceived
     * (java.lang.Object)
     */
    @SuppressWarnings("unchecked")
    @Override
    public void objReceived(Object obj) {
        if (obj instanceof Map) {
            Map<String, Object> returnMap = (Map<String, Object>) obj;
            String method = (String) returnMap.get("method");

            if (method.equals("get_id_nums")) {
                String ident = (String) returnMap.get("ident");
                List<List<String>> list = (List<List<String>>) returnMap
                        .get("results");
                cdmDlg.populateSiteInfoList(ident, list);
                cdmDlg.assessBtn(true);
                cdmDlg.validateBtn(false);
            } else if (method.equals("updateMonitor")) {
                String msg = (String) returnMap.get("msg");
                cdmDlg.updateMonitor(msg);
            } else if (method.equals("overwriteMonitor")) {
                String msg = (String) returnMap.get("msg");
                cdmDlg.overwriteMonitor(msg);
            } else if (method.equals("make_qc")) {
                genQCStatFiles();
            } else if (method.equals("stnPickle")) {
                stnPickle = (String) returnMap.get("stnPickle");
            } else if (method.equals("scriptsSetEnabled")) {
                boolean enabled = "True".equals(returnMap.get("value")
                        .toString());
                cdmDlg.scriptsBtn(enabled);
                cdmDlg.processBtn(enabled);
            } else if (method.equals("validateSetEnabled")) {
                boolean enabled = "True".equals(returnMap.get("value")
                        .toString());
                cdmDlg.validateBtn(enabled);
            } else if (method.equals("commitRejectSetEnabled")) {
                boolean enabled = "True".equals(returnMap.get("value")
                        .toString());
                cdmDlg.commitBtn(enabled);
                cdmDlg.rejectBtn(enabled);
            } else if (method.equals("get_stations_map")) {
                setStationsMap((Map<String, Object>) returnMap.get("map"));
            } else {
                System.out.println("Unimplemented method: " + method);
            }
        }
    }
}
