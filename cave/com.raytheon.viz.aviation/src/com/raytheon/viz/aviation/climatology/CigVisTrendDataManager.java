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
package com.raytheon.viz.aviation.climatology;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;

import com.raytheon.uf.common.status.IPerformanceStatusHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.PerformanceStatus;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.aviation.climatedata.ClimateDataManager;
import com.raytheon.viz.aviation.climatology.ClimatePythonTask.ClimatePythonListener;

/**
 * Data manager for Ceiling and Visibility Trend displays
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 05, 2009            avarani     Initial creation
 * Mar 31, 2011  8774      rferrel     killProcess when doing a disposed
 * Apr 04, 2011  8896      rferrel     Made timeout configurable
 * 19Mar2014    #2925      lvenable    Added dispose checks for runAsync.
 * Nov 02, 2016  5979      njensen     Cast to Number where applicable
 * Jan 13, 2017  5959      njensen     Cleaned up warnings
 * Feb 06, 2016  5979      njensen     Cast to Number where applicable
 * Mar 22, 2017  6183      tgurney     Move python files to common_static
 * Aug  6, 2019  7878      tgurney     Rewrite data retrieval logic for Python 3
 *
 * </pre>
 *
 * @author avarani
 */

public class CigVisTrendDataManager implements ClimatePythonListener {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(CigVisTrendDataManager.class);

    private final IPerformanceStatusHandler perfLog = PerformanceStatus
            .getHandler("CigVisTrend:");

    private static final CigVisTrendDataManager INSTANCE = new CigVisTrendDataManager();

    private String metarText;

    private Map<String, Object> metarDcd;

    private static final int NUM_CAT = 4;

    private float[] totalData;

    private float[][] cigData;

    private float[][] visData;

    private float[][] jointData;

    private int hours;

    private AtomicBoolean firstReceived = new AtomicBoolean();

    private String dataSite;

    private CigVisTrendDlg parent;

    private volatile ClimatePythonTask pythonScript = null;

    private long t0;

    private long t1;

    private CigVisTrendDataManager() {
    }

    public static CigVisTrendDataManager getInstance() {
        return INSTANCE;
    }

    public String getDataSite() {
        return dataSite;
    }

    @SuppressWarnings("unchecked")
    public void getData(final String site, Map<String, Object> selection,
            final int hours, CigVisTrendDlg parent) {
        final int timeout = ClimateTimeoutManager.getInstance()
                .getCigVisTrendTimeout();
        this.parent = parent;
        this.hours = hours + 1;
        final Object cigRange0 = ((List<Object>) selection.get("cig")).get(0);
        final Object cigRange1 = ((List<Object>) selection.get("cig")).get(1);

        final Object vsbyRange0 = ((List<Object>) selection.get("vsby")).get(0);
        final Object vsbyRange1 = ((List<Object>) selection.get("vsby")).get(1);

        final Object wndSpdRange0 = ((List<Object>) selection.get("wind_speed"))
                .get(0);
        final Object wndSpdRange1 = ((List<Object>) selection.get("wind_speed"))
                .get(1);

        final Object wndDirRange0 = ((List<Object>) selection.get("wind_dir"))
                .get(0);
        final Object wndDirRange1 = ((List<Object>) selection.get("wind_dir"))
                .get(1);

        final Object hourRange0 = ((List<Object>) selection.get("hour")).get(0);
        final Object hourRange1 = ((List<Object>) selection.get("hour")).get(1);

        final Object dayRange0 = ((List<Object>) selection.get("yday")).get(0);
        final Object dayRange1 = ((List<Object>) selection.get("yday")).get(1);

        final String pcp = (String) selection.get("pcp");

        final int cur_hour = ((Number) selection.get("cur_hour")).intValue();

        firstReceived.set(false);

        t0 = System.currentTimeMillis();

        try {
            String climateFile = ClimateDataManager.getClimateFilePath(site);
            Map<String, Object> args = new HashMap<>();
            args.put("cigRange0", cigRange0);
            args.put("cigRange1", cigRange1);
            args.put("vsbyRange0", vsbyRange0);
            args.put("vsbyRange1", vsbyRange1);
            args.put("wndSpdRange0", wndSpdRange0);
            args.put("wndSpdRange1", wndSpdRange1);
            args.put("wndDirRange0", wndDirRange0);
            args.put("wndDirRange1", wndDirRange1);
            args.put("hourRange0", hourRange0);
            args.put("hourRange1", hourRange1);
            args.put("dayRange0", dayRange0);
            args.put("dayRange1", dayRange1);
            args.put("pcp", pcp);
            args.put("cur_hour", cur_hour);
            args.put("id_", site);
            args.put("hours", hours + 1);
            args.put("fname", climateFile);
            cancelPythonTask();
            pythonScript = ClimatePythonTask.execute("get_cigvistrend_data",
                    args, this, timeout);
            dataSite = site;
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error retrieving cig/vis trend data", e);
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public void sendObj(Object obj) {
        if (firstReceived.compareAndSet(false, true)) {
            t1 = System.currentTimeMillis();
            cigData = new float[hours][NUM_CAT];
            visData = new float[hours][NUM_CAT];
            jointData = new float[hours][NUM_CAT];
            totalData = new float[hours];

            Map<String, Object> resultMap = (HashMap<String, Object>) obj;
            List<Object> cigList = (List<Object>) resultMap.get("cig");
            List<Object> visList = (List<Object>) resultMap.get("vis");
            List<Object> jntList = (List<Object>) resultMap.get("joint");
            List<Number> totalList = (List<Number>) resultMap.get("total");

            for (int h = 0; h < hours; h++) {
                List<Number> tmpCig = (List<Number>) cigList.get(h);
                List<Number> tmpVis = (List<Number>) visList.get(h);
                List<Number> tmpJnt = (List<Number>) jntList.get(h);
                totalData[h] = totalList.get(h).floatValue();

                for (int c = 0; c < NUM_CAT; c++) {
                    cigData[h][c] = tmpCig.get(c).floatValue();
                    visData[h][c] = tmpVis.get(c).floatValue();
                    jointData[h][c] = tmpJnt.get(c).floatValue();
                }
            }
        } else {
            long t2 = System.currentTimeMillis();
            perfLog.logDuration("Total process time", t2 - t0);
            perfLog.logDuration("Separate process time", t1 - t0);
            perfLog.logDuration("Transferring back time", t2 - t1);
            VizApp.runAsync(() -> {
                if (!parent.isDisposed()) {
                    parent.dataReceived();
                }
            });
        }
    }

    @Override
    public void finished() {
        pythonScript = null;
        VizApp.runAsync(() -> {
            if (!CigVisTrendDataManager.this.parent.isDisposed()) {
                CigVisTrendDataManager.this.parent.resetCursor();
            }
        });
    }

    @SuppressWarnings("unchecked")
    public void getMetar(String site) {
        metarText = "";
        Map<String, Object> args = new HashMap<>();
        args.put("siteID", site);
        try {
            Object[] resultObj = new Object[1];
            ClimatePythonTask.execute("get_cigvistrend_metar", args,
                    (obj) -> resultObj[0] = obj, 0).waitFor();
            Map<String, Object> results = (Map<String, Object>) resultObj[0];
            String rawText = (String) results.get("text");
            String[] textBits = rawText.split("\\s");
            StringBuilder sb = new StringBuilder();

            for (String bit : textBits) {
                if (bit.isEmpty()) {
                    continue;
                }

                sb.append(bit + " ");
            }

            metarText = sb.toString();
            metarDcd = (Map<String, Object>) results.get("dcd");
        } catch (Exception e) {
            metarText = "No data available for site " + site;
            statusHandler.error("Error getting metar", e);
        }
    }

    public String getMetarText() {
        return String.valueOf(metarText);
    }

    public Map<String, Object> getMetarDcd() {
        return metarDcd;
    }

    public boolean hasRetrievedMetar() {
        return metarDcd != null;
    }

    public void cancelPythonTask() {
        if (pythonScript != null) {
            pythonScript.cancel();
            pythonScript = null;
        }
    }

    public void setTotalData(float[] data) {
        totalData = new float[hours];

        for (int i = 0; i < hours; i++) {
            totalData[i] = data[i];
        }
    }

    public void setVisData(float[] data) {
        visData = new float[hours][NUM_CAT];

        for (int i = 0; i < hours; i++) {
            for (int j = 0; j < NUM_CAT; j++) {
                visData[i][j] = data[i * NUM_CAT + j];
            }
        }
    }

    public void setCigData(float[] data) {
        cigData = new float[hours][NUM_CAT];

        for (int i = 0; i < hours; i++) {
            for (int j = 0; j < NUM_CAT; j++) {
                cigData[i][j] = data[i * NUM_CAT + j];
            }
        }
    }

    public void setJointData(float[] data) {
        jointData = new float[hours][NUM_CAT];

        for (int i = 0; i < hours; i++) {
            for (int j = 0; j < NUM_CAT; j++) {
                jointData[i][j] = data[i * NUM_CAT + j];
            }
        }
    }

    public float[] getTotalData() {
        return totalData;
    }

    public float[][] getVisData() {
        return visData;
    }

    public float[][] getCigData() {
        return cigData;
    }

    public float[][] getJointData() {
        return jointData;
    }
}
