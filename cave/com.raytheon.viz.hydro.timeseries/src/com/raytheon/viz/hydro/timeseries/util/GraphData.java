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
package com.raytheon.viz.hydro.timeseries.util;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.common.dataplugin.shef.tables.Fcstheight;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydro.timeseries.TimeSeriesDataManager;
import com.raytheon.viz.hydro.timeseries.util.GraphData.RateTrace.StageOrDischarge;
import com.raytheon.viz.hydro.timeseries.util.GraphInfo.DERIVE_PP;
import com.raytheon.viz.hydro.timeseries.util.TimeSeriesPoint.MODE;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.util.HydroUtils;

/**
 * Object to hold the data and metadata for the time series.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jun 24, 2008           mpduff    Initial creation.
 * May 06, 2013  1976     mpduff    Use List interface.
 * 26 Oct, 2015  14217    jwu       Removed DAYS_MAX & MAX_TRACES limitations
 * May 16, 2018  6749     randerso  Removed unused fields. Code cleanup.
 * Jun 27, 2018  6748     randerso  Reworked to use GraphInfo for metadata.
 * Jul 10, 2019  6748     randerso  Check for empty floodStageList. Fix min/max
 *                                  scaling.
 * 
 * </pre>
 *
 * @author mpduff
 */

public class GraphData {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(GraphData.class);

    /**
     * height info
     */
    public static class HGData {
        private boolean valid;

        private String currentPe;

        private double floodFlow;

        private double actionFlow;

        private double floodStage;

        private double actionStage;

        private double major;

        private double moderate;

        private double minor;

        private double ymax;

        private double ymin;

        /**
         * Constructor
         */
        public HGData() {
            valid = false;
            floodFlow = HydroConstants.MISSING_VALUE;
            floodStage = HydroConstants.MISSING_VALUE;
            actionFlow = HydroConstants.MISSING_VALUE;
            actionStage = HydroConstants.MISSING_VALUE;
            major = HydroConstants.MISSING_VALUE;
            moderate = HydroConstants.MISSING_VALUE;
            minor = HydroConstants.MISSING_VALUE;
        }

        /**
         * @return the valid
         */
        public boolean isValid() {
            return valid;
        }

        /**
         * @return the currentPe
         */
        public String getCurrentPe() {
            return currentPe;
        }

        /**
         * @return the floodDischarge
         */
        public double getFloodDischarge() {
            return floodFlow;
        }

        /**
         * @return the actionFlow
         */
        public double getActionFlow() {
            return actionFlow;
        }

        /**
         * @return the floodStage
         */
        public double getFloodStage() {
            return floodStage;
        }

        /**
         * @return the actionStage
         */
        public double getActionStage() {
            return actionStage;
        }

        /**
         * @return the major
         */
        public double getMajor() {
            return major;
        }

        /**
         * @return the moderate
         */
        public double getModerate() {
            return moderate;
        }

        /**
         * @return the minor
         */
        public double getMinor() {
            return minor;
        }

        /**
         * @return the ymax
         */
        public double getYmax() {
            return ymax;
        }

        /**
         * @return the ymin
         */
        public double getYmin() {
            return ymin;
        }
    }

    /**
     * rate trace info
     */
    public static class RateTrace {
        public static enum StageOrDischarge {
            STAGE, DISCHARGE
        }

        private boolean valid;

        private StageOrDischarge stageOrDischarge;

        /**
         * Constructor
         */
        public RateTrace() {
            valid = false;
            stageOrDischarge = StageOrDischarge.STAGE;
        }

        /**
         * @return the valid
         */
        public boolean isValid() {
            return valid;
        }

        /**
         * @return the stageOrDischarge
         */
        public StageOrDischarge getStageOrDischarge() {
            return stageOrDischarge;
        }

    }

    private boolean displayFlowUnit;

    private Date xmin;

    private Date xmax;

    private double ymin;

    private double ymax;

    private double dataInc;

    private Date oldXmin;

    private Date oldXmax;

    private double oldYmin;

    private double oldYmax;

    private double oldDataInc;

    private Date origXmin;

    private Date origXmax;

    private double origYmin;

    private double origYmax;

    private double origDataInc;

    private double ymin2;

    private double ymax2;

    private double dataInc2;

    private boolean zoomed = false;

    /**
     * List of traces for graphing
     */
    private List<TraceData> traces = new ArrayList<>();

    private String title = null;

    private HGData hgData;

    private RateTrace rateTrace;

    private GraphInfo graphInfo;

    private Rectangle graphArea;

    /**
     * Constructor
     *
     * @param graphInfo
     */
    public GraphData(GraphInfo graphInfo) {
        this.graphInfo = graphInfo;
        this.hgData = new HGData();
        this.rateTrace = new RateTrace();
    }

    /**
     * @return the numTraces
     */
    public int getNumTraces() {
        return traces.size();
    }

    /**
     * @return the display_flow_unit
     */
    public boolean isDisplayFlowUnit() {
        return displayFlowUnit;
    }

    /**
     * @param displayFlowUnit
     *            the display_flow_unit to set
     */
    public void setDisplayFlowUnit(boolean displayFlowUnit) {
        this.displayFlowUnit = displayFlowUnit;
    }

    /**
     * @return the xmin
     */
    public Date getXmin() {
        return xmin;
    }

    /**
     * @param xmin
     *            the xmin to set
     */
    public void setXmin(Date xmin) {
        this.xmin = xmin;
    }

    /**
     * @return the xmax
     */
    public Date getXmax() {
        return xmax;
    }

    /**
     * @param xmax
     *            the xmax to set
     */
    public void setXmax(Date xmax) {
        this.xmax = xmax;
    }

    /**
     * @return the ymin
     */
    public double getYmin() {
        return ymin;
    }

    /**
     * @param ymin
     *            the ymin to set
     */
    public void setYmin(double ymin) {
        this.ymin = ymin;
    }

    /**
     * @return the ymax
     */
    public double getYmax() {
        return ymax;
    }

    /**
     * @param ymax
     *            the ymax to set
     */
    public void setYmax(double ymax) {
        this.ymax = ymax;
    }

    /**
     * @return the dataInc
     */
    public double getDataInc() {
        return dataInc;
    }

    /**
     * @param dataInc
     *            the dataInc to set
     */
    public void setDataInc(double dataInc) {
        this.dataInc = dataInc;
    }

    /**
     * @return the oldXmin
     */
    public Date getOldXmin() {
        return oldXmin;
    }

    /**
     * @param oldXmin
     *            the oldXmin to set
     */
    public void setOldXmin(Date oldXmin) {
        this.oldXmin = oldXmin;
    }

    /**
     * @return the oldXmax
     */
    public Date getOldXmax() {
        return oldXmax;
    }

    /**
     * @param oldXmax
     *            the oldXmax to set
     */
    public void setOldXmax(Date oldXmax) {
        this.oldXmax = oldXmax;
    }

    /**
     * @return the oldYmin
     */
    public double getOldYmin() {
        return oldYmin;
    }

    /**
     * @param oldYmin
     *            the oldYmin to set
     */
    public void setOldYmin(double oldYmin) {
        this.oldYmin = oldYmin;
    }

    /**
     * @return the oldYmax
     */
    public double getOldYmax() {
        return oldYmax;
    }

    /**
     * @param oldYmax
     *            the oldYmax to set
     */
    public void setOldYmax(double oldYmax) {
        this.oldYmax = oldYmax;
    }

    /**
     * @return the oldDataInc
     */
    public double getOldDataInc() {
        return oldDataInc;
    }

    /**
     * @param oldDataInc
     *            the oldDataInc to set
     */
    public void setOldDataInc(double oldDataInc) {
        this.oldDataInc = oldDataInc;
    }

    /**
     * @return the origXmin
     */
    public Date getOrigXmin() {
        return origXmin;
    }

    /**
     * @param origXmin
     *            the origXmin to set
     */
    public void setOrigXmin(Date origXmin) {
        this.origXmin = origXmin;
    }

    /**
     * @return the origXmax
     */
    public Date getOrigXmax() {
        return origXmax;
    }

    /**
     * @param origXmax
     *            the origXmax to set
     */
    public void setOrigXmax(Date origXmax) {
        this.origXmax = origXmax;
    }

    /**
     * @return the origYmin
     */
    public double getOrigYmin() {
        return origYmin;
    }

    /**
     * @param origYmin
     *            the origYmin to set
     */
    public void setOrigYmin(double origYmin) {
        this.origYmin = origYmin;
    }

    /**
     * @return the origYmax
     */
    public double getOrigYmax() {
        return origYmax;
    }

    /**
     * @param origYmax
     *            the origYmax to set
     */
    public void setOrigYmax(double origYmax) {
        this.origYmax = origYmax;
    }

    /**
     * @return the origDataInc
     */
    public double getOrigDataInc() {
        return origDataInc;
    }

    /**
     * @param origDataInc
     *            the origDataInc to set
     */
    public void setOrigDataInc(double origDataInc) {
        this.origDataInc = origDataInc;
    }

    /**
     * @return the traces
     */
    public List<TraceData> getTraces() {
        return traces;
    }

    /**
     * @param index
     * @return the indexed TraceData
     */
    public TraceData getTraceData(int index) {
        return traces.get(index);
    }

    /**
     * @param traces
     *            the traces to set
     */
    public void setTraces(List<TraceData> traces) {
        this.traces = traces;
    }

    /**
     * @param trace
     */
    public void addTraceData(TraceData trace) {
        if (!traces.contains(trace)) {
            traces.add(trace);
        }
    }

    /**
     * @return the title
     */
    public String getTitle() {
        return title;
    }

    /**
     * @param title
     *            the title to set
     */
    public void setTitle(String title) {
        this.title = title;
    }

    /**
     * @return the graphPos
     */
    public int getGraphPos() {
        return this.graphInfo.getGraphPos();
    }

    /**
     * @return the xsize
     */
    public int getXsize() {
        return this.graphInfo.getXsize();
    }

    /**
     * @return the ysize
     */
    public int getYsize() {
        return this.graphInfo.getYsize();
    }

    /**
     * @return true if Y scale is Data, false if Category
     */
    public boolean isYscaleToData() {
        return this.graphInfo.isYscaleToData();
    }

    /**
     * @return the ylinear
     */
    public String getYlinear() {
        return this.graphInfo.getYlinear();
    }

    /**
     * @return the showcat
     */
    public boolean isShowcat() {
        return this.graphInfo.isShowcat();
    }

    /**
     * @return the derivepp
     */
    public DERIVE_PP getDerivepp() {
        return this.graphInfo.getDerivepp();
    }

    /**
     * @return the latestfcstonly
     */
    public boolean isLatestfcstonly() {
        return this.graphInfo.isLatestfcstonly();
    }

    /**
     * @return the ymin2
     */
    public double getYmin2() {
        return ymin2;
    }

    /**
     * @param ymin2
     *            the ymin2 to set
     */
    public void setYmin2(double ymin2) {
        this.ymin2 = ymin2;
    }

    /**
     * @return the ymax2
     */
    public double getYmax2() {
        return ymax2;
    }

    /**
     * @param ymax2
     *            the ymax2 to set
     */
    public void setYmax2(double ymax2) {
        this.ymax2 = ymax2;
    }

    /**
     * @return the dataInc2
     */
    public double getDataInc2() {
        return dataInc2;
    }

    /**
     * @param dataInc2
     *            the dataInc2 to set
     */
    public void setDataInc2(double dataInc2) {
        this.dataInc2 = dataInc2;
    }

    /**
     * @return true if graph is zoomed
     */
    public boolean isZoomed() {
        return zoomed;
    }

    /**
     * @param zoomed
     *            the zoomed to set
     */
    public void setZoomed(boolean zoomed) {
        this.zoomed = zoomed;
    }

    /**
     * @return the hgData
     */
    public HGData getHgData() {
        return hgData;
    }

    /**
     * @param hgData
     *            the hgData to set
     */
    public void setHgData(HGData hgData) {
        this.hgData = hgData;
    }

    /**
     * @return the rateTrace
     */
    public RateTrace getRateTrace() {
        return rateTrace;
    }

    /**
     * @param rateTrace
     *            the rateTrace to set
     */
    public void setRateTrace(RateTrace rateTrace) {
        this.rateTrace = rateTrace;
    }

    /**
     * @return the graphInfo
     */
    public GraphInfo getGraphInfo() {
        return graphInfo;
    }

    /**
     * @return the graphArea
     */
    public Rectangle getGraphArea() {
        return graphArea;
    }

    /**
     * @param graphArea
     *            the graphArea to set
     */
    public void setGraphArea(Rectangle graphArea) {
        this.graphArea = graphArea;
    }

    /**
     * @return true if this GraphData is valid
     */
    public boolean isValid() {
        for (TraceData trace : traces) {
            if (trace.isValid()) {
                return true;
            }
        }
        return false;
    }

    /**
     * Get data for graph
     *
     * @param beginDate
     * @param endDate
     */
    public void getGraphData(Date beginDate, Date endDate) {

        Set<String> uniqueList = new HashSet<>();

        traces.clear();
        for (TraceInfo traceInfo : graphInfo.getTraceInfoList()) {
            TraceData traceData = new TraceData(traceInfo);
            traces.add(traceData);
            if (!traceInfo.isForecast()) {
                boolean traceValid = false;
                boolean traceOn = false;
                boolean status = traceData.getObsTrace(beginDate, endDate);
                if (status) {
                    traceValid = true;
                    traceOn = true;
                }
                traceData.setValid(traceValid);
                traceData.setTraceOn(traceOn);
            } else {
                String traceKey = traceData.getLid() + traceData.getPe()
                        + traceData.getTs() + traceData.getDur()
                        + traceData.getExtremum();
                if (uniqueList.contains(traceKey)) {
                    continue;
                }
                uniqueList.add(traceKey);
                getFcstData(traceData, beginDate, endDate);
            }
        }

        /* **************************************** */
        /* Loading rating table if pe is HG/HT/QR */
        /* if pe = QR then stage = f(discharge) */
        /* if pe = HG then discharge = f(stage) */
        /* if pe = HT then discharge = f(stage) */
        /* **************************************** */

        hgData.valid = false;
        rateTrace.stageOrDischarge = StageOrDischarge.STAGE;

        for (TraceData traceData : getTraces()) {
            if (traceData.getPe().toUpperCase().startsWith("Q")) {
                rateTrace.stageOrDischarge = StageOrDischarge.DISCHARGE;
            }

            if (traceData.getPe().toUpperCase().startsWith("H")
                    || traceData.getPe().toUpperCase().startsWith("Q")) {
                rateTrace.valid = StageDischargeUtils
                        .checkRatingTable(traceData.getLid());
                loadHGvars(traceData.getLid(), traceData.getPe());
                break;
            }

        }

    }

    private void loadHGvars(String lid, String pe) {
        hgData.floodFlow = HydroConstants.MISSING_VALUE;
        hgData.floodStage = HydroConstants.MISSING_VALUE;
        hgData.actionFlow = HydroConstants.MISSING_VALUE;
        hgData.actionStage = HydroConstants.MISSING_VALUE;
        hgData.major = HydroConstants.MISSING_VALUE;
        hgData.moderate = HydroConstants.MISSING_VALUE;
        hgData.minor = HydroConstants.MISSING_VALUE;
        hgData.currentPe = pe;

        TimeSeriesDataManager dataManager = TimeSeriesDataManager.getInstance();
        try {
            List<Object[]> floodStageList = dataManager.getFloodStage(lid);
            if (floodStageList != null && !floodStageList.isEmpty()) {
                /* Only one row should be returned */
                if (floodStageList.size() != 1) {
                    statusHandler.warn(
                            "Multiple flood stage rows returned for " + lid);
                }

                Object[] row = floodStageList.get(0);
                if (row[1] != null) {
                    hgData.floodStage = ((Number) row[1]).doubleValue();
                }
                if (row[2] != null) {
                    hgData.floodFlow = ((Number) row[2]).doubleValue();
                }
                if (row[3] != null) {
                    hgData.actionStage = ((Number) row[3]).doubleValue();
                }
                if (row[4] != null) {
                    hgData.actionFlow = ((Number) row[4]).doubleValue();
                }
            }
        } catch (Exception e) {
            statusHandler.error("Error getting flood stage info", e);
        }

        try {
            List<Object[]> floodCatList = dataManager.getFloodCategories(lid);
            if (floodCatList != null && !floodCatList.isEmpty()) {
                /* Only one row should be returned */
                if (floodCatList.size() != 1) {
                    statusHandler
                            .warn("Multiple flood categories rows returned for "
                                    + lid);
                }
                Object[] row = floodCatList.get(0);
                if (pe.toUpperCase().startsWith("Q")) {
                    if (row[4] != null) {
                        hgData.minor = ((Number) row[4]).doubleValue();
                    }
                    if (row[5] != null) {
                        hgData.moderate = ((Number) row[5]).doubleValue();
                    }
                    if (row[6] != null) {
                        hgData.major = ((Number) row[6]).doubleValue();
                    }
                } else {
                    if (row[1] != null) {
                        hgData.minor = ((Number) row[1]).doubleValue();
                    }
                    if (row[2] != null) {
                        hgData.moderate = ((Number) row[2]).doubleValue();
                    }
                    if (row[3] != null) {
                        hgData.major = ((Number) row[3]).doubleValue();
                    }
                }
            }
        } catch (Exception e) {
            statusHandler.error("Error getting flood categories", e);
        }

        /* Check if data is valid data */
        hgData.valid = true;

        if (hgData.getMajor() == HydroConstants.MISSING_VALUE
                && hgData.getModerate() == HydroConstants.MISSING_VALUE
                && hgData.getMinor() == HydroConstants.MISSING_VALUE
                && hgData.getActionStage() == HydroConstants.MISSING_VALUE
                && hgData.getFloodStage() == HydroConstants.MISSING_VALUE) {
            hgData.valid = false;
            return;
        }

        /* find Max and Min value of the category */
        double tmpmax = 0.0;
        double tmpmin = Double.MAX_VALUE;

        if (hgData.getMajor() != HydroConstants.MISSING_VALUE) {
            if (hgData.getMajor() > tmpmax) {
                tmpmax = hgData.getMajor();
            }
            if (hgData.getMajor() < tmpmin) {
                tmpmin = hgData.getMajor();
            }
        }
        if (hgData.getModerate() != HydroConstants.MISSING_VALUE) {
            if (hgData.getModerate() > tmpmax) {
                tmpmax = hgData.getModerate();
            }
            if (hgData.getModerate() < tmpmin) {
                tmpmin = hgData.getModerate();
            }
        }
        if (hgData.getMinor() != HydroConstants.MISSING_VALUE) {
            if (hgData.getMinor() > tmpmax) {
                tmpmax = hgData.getMinor();
            }
            if (hgData.getMinor() < tmpmin) {
                tmpmin = hgData.getMinor();
            }
        }

        if (pe.toUpperCase().startsWith("Q")) {
            if (hgData.getActionFlow() != HydroConstants.MISSING_VALUE) {
                if (hgData.getActionFlow() > tmpmax) {
                    tmpmax = hgData.getActionFlow();
                }
                if (hgData.getActionFlow() < tmpmin) {
                    tmpmin = hgData.getActionFlow();
                }
            }
            if (hgData.getFloodDischarge() != HydroConstants.MISSING_VALUE) {
                if (hgData.getFloodDischarge() > tmpmax) {
                    tmpmax = hgData.getFloodDischarge();
                }
                if (hgData.getFloodDischarge() < tmpmin) {
                    tmpmin = hgData.getFloodDischarge();
                }
            }
        } else {
            if (hgData.getFloodStage() != HydroConstants.MISSING_VALUE) {
                if (hgData.getFloodStage() > tmpmax) {
                    tmpmax = hgData.getFloodStage();
                }
                if (hgData.getFloodStage() < tmpmin) {
                    tmpmin = hgData.getFloodStage();
                }
            }
            if (hgData.getActionStage() != HydroConstants.MISSING_VALUE) {
                if (hgData.getActionStage() > tmpmax) {
                    tmpmax = hgData.getActionStage();
                }
                if (hgData.getActionStage() < tmpmin) {
                    tmpmin = hgData.getActionStage();
                }
            }
        }

        hgData.ymax = tmpmax;
        hgData.ymin = tmpmin;
    }

    /**
     * Get the forecast data.
     *
     * @param traceData
     *            The TraceData object to fill with forecast data
     * @return The number of traces
     */
    private int getFcstData(TraceData traceData, Date beginDate, Date endDate) {
        TimeSeriesDataManager dataManager = TimeSeriesDataManager.getInstance();

        List<TraceData> traceDataList = new ArrayList<>();
        List<TimeSeriesPoint> points = new ArrayList<>();
        List<Fcstheight> results;

        String lid = traceData.getLid();
        String ts = traceData.getTs();
        String pe = traceData.getPe();
        String name = traceData.getName();
        int dur = traceData.getDur();
        String extremum = traceData.getExtremum();

        int ntraces = 0;

        // Get data from database based on table name and trace info
        String tablename = HydroUtils.getTableName(traceData.getPe(),
                traceData.getTs());

        /* Get the data from IHFS and store in TimeSeriesPoint object */
        try {
            Date basisTime = null;
            Date prevBasisTime = null;
            double ymin = Double.MAX_VALUE;
            double ymax = -Double.MAX_VALUE;
            Date xmin = new Date(Long.MAX_VALUE);
            Date xmax = new Date(Long.MIN_VALUE);

            /*
             * convert times to 19-character String time and build where clause
             */
            String begin = HydroConstants.DATE_FORMAT.format(beginDate);
            String end = HydroConstants.DATE_FORMAT.format(endDate);

            String where = String.format(" WHERE lid = '%s' "
                    + " AND pe = '%s'  " + " AND ts = '%s'  "
                    + " AND dur = %d   " + " AND extremum = '%s' "
                    + " AND validtime >= '%s' " + " AND validtime <= '%s' "
                    + " ORDER BY ts, basistime DESC, validtime",
                    traceData.getLid().toUpperCase(),
                    traceData.getPe().toUpperCase(),
                    traceData.getTs().toUpperCase(), traceData.getDur(),
                    traceData.getExtremum().toUpperCase(), begin, end);

            results = dataManager.getForecast(where, tablename);

            if (results != null && !results.isEmpty()) {
                int n = 0;
                for (Fcstheight row : results) {
                    TimeSeriesPoint p = new TimeSeriesPoint();
                    Date validTime = row.getId().getValidtime();
                    p.setX(validTime);
                    Date productTime = row.getProducttime();
                    traceData.setProductTime(productTime);

                    if (validTime.getTime() >= beginDate.getTime()
                            && validTime.getTime() <= endDate.getTime()
                            && row.getValue() != HydroConstants.MISSING_VALUE) {

                        p.setY(row.getValue());
                        basisTime = row.getId().getBasistime();
                        if (n == 0) {
                            prevBasisTime = basisTime;
                            ymin = row.getValue();
                            ymax = row.getValue();
                            xmin = validTime;
                            xmax = validTime;
                        }

                        if (basisTime.getTime() != prevBasisTime.getTime()) {
                            traceData.setXmin(beginDate);
                            traceData.setXmax(endDate);
                            n = 0;
                            traceData.setBasistime(prevBasisTime);
                            ntraces++;
                            traceData.setTsData(points);
                            points = new ArrayList<>();

                            if (ntraces >= 1) {
                                traceDataList.add(traceData);
                            }

                            TraceInfo traceInfo = new TraceInfo();
                            traceInfo.setForecast(true);
                            traceInfo.setDur(dur);
                            traceInfo.setExtremum(extremum);
                            traceInfo.setLid(lid);
                            traceInfo.setPe(pe);
                            traceInfo.setTs(ts);
                            traceInfo.setName(name);

                            traceData = new TraceData(traceInfo);
                            traceData.setBasistime(basisTime);
                            traceData.setProductTime(productTime);
                            traceData.setTraceOn(!this.isLatestfcstonly());
                        }

                        p.setY(row.getValue());
                        p.setX(validTime);
                        p.setMode(MODE.NONE);
                        p.setRevision(row.getRevision().shortValue());
                        p.setQuality_code(row.getQualityCode());
                        p.setProbability(row.getId().getProbability());

                        if (row.getValue() != HydroConstants.MISSING_VALUE) {
                            points.add(p);
                        }

                        ymin = Math.min(ymin, row.getValue());
                        ymax = Math.max(ymax, row.getValue());

                        if (validTime.before(xmin)) {
                            xmin = validTime;
                        }
                        if (validTime.after(xmax)) {
                            xmax = validTime;
                        }

                        n++;
                        prevBasisTime = basisTime;
                        traceData.setValue_ymin(ymin);
                        traceData.setValue_ymax(ymax);
                        traceData.setYmin(ymin);
                        traceData.setYmax(ymax);
                        traceData.setXmin(xmin);
                        traceData.setXmax(xmax);
                    }
                }

                /*
                 * Copy last trace into forecast trace
                 */
                traceData.setBasistime(prevBasisTime);
                traceDataList.add(traceData);
                ntraces++;
            } else {
                traceData.setTraceOn(false);
                traceData.setValid(false);
                traceDataList.add(traceData);
            }
        } catch (VizException e) {
            statusHandler.error("Error retrieving graph data", e);
        }

        traceData.setTsData(points);

        for (TraceData td : traceDataList) {
            addTraceData(td);
        }

        return traceDataList.size();
    }

    /**
     * find max and mix of the graph based on the data in trace/traces of the
     * graph
     *
     * @param chkFcst
     * @param beginDate
     * @param endDate
     */
    public void findMinMax(boolean chkFcst, Date beginDate, Date endDate) {

        /* ******************************** */
        /* Check for valid traces */
        /* ******************************** */
        Map<String, TraceData> latestFcstMap = new HashMap<>();

        for (TraceData tptr : getTraces()) {
            if (!tptr.isValid()) {
                tptr.setTraceOn(false);
            } else if (chkFcst && tptr.isForecast()) {

                if (!graphInfo.isLatestfcstonly()) {
                    tptr.setTraceOn(true);
                } else {
                    String traceKey = tptr.getLid() + tptr.getPe()
                            + tptr.getTs() + tptr.getDur() + tptr.getExtremum();

                    TraceData latest = latestFcstMap.get(traceKey);
                    if (latest == null) {
                        latestFcstMap.put(traceKey, tptr);
                        tptr.setTraceOn(true);
                    } else if (tptr.getBasistime()
                            .after(latest.getBasistime())) {
                        tptr.setTraceOn(true);
                        latest.setTraceOn(false);
                        latestFcstMap.put(traceKey, tptr);
                    } else {
                        tptr.setTraceOn(false);
                    }
                }

            }
        }

        boolean hasPP = false;
        boolean hasQR = false;
        double ymin = Double.MAX_VALUE;
        double ymax = -Double.MAX_VALUE;

        for (TraceData tptr : getTraces()) {

            if (tptr.isTraceOn()) {
                if (tptr.getNpts() > 0) {
                    if (HydroConstants.PP.equalsIgnoreCase(tptr.getPe())) {
                        hasPP = true;
                    }

                    if ("QR".equalsIgnoreCase(tptr.getPe())) {
                        hasQR = true;
                    }

                    ymin = Math.min(tptr.getYmin(), ymin);
                    ymax = Math.max(tptr.getYmax(), ymax);
                }
            }
        }

        if (hasQR && Math.abs(ymax - ymin) <= 10) {
            setYmin(ymin - 5.0);
            setYmax(ymax + 5.0);
            setDataInc(2.0);

        } else {
            adjustYmaxYmin(ymin, ymax);
        }

        xmin = beginDate;
        xmax = endDate;

        /* ********************************************************** */
        /* Store original graph's max and min for Zoom_Reset purpose */
        /* ********************************************************** */

        origXmin = xmin;
        origXmax = xmax;
        origYmin = getYmin();
        origYmax = getYmax();
        origDataInc = getDataInc();

        oldXmin = xmin;
        oldXmax = xmax;
        oldYmin = getYmin();
        oldYmax = getYmax();
        oldDataInc = getDataInc();

        /* Check if pe = PP then force ymin = 0 */
        if (hasPP) {
            setYmin(0);
        }
    }

    /**
     * Adjust and set the yMin and yMax
     *
     * @param yMin
     *            new yMin
     * @param yMax
     *            new yMax
     */
    public void adjustYmaxYmin(double yMin, double yMax) {

        int n;
        double dMinMax = yMax - yMin;
        if (dMinMax <= 0.1) {
            n = 0;
        } else {
            n = (int) Math.log10(Math.abs(dMinMax));
        }

        int j = (int) Math.pow(10, n);

        yMin = Math.floor(yMin / j) * j;

        yMax = Math.ceil(yMax / j) * j;

        dMinMax = yMax - yMin;

        double dinc = dMinMax / 5.0;

        if (dMinMax >= j * 1.0) {
            dinc = 0.2 * j;
        }
        if (dMinMax >= j * 2.0) {
            dinc = 0.5 * j;
        }
        if (dMinMax >= j * 4.0) {
            dinc = 1.0 * j;
        }
        if (dMinMax >= j * 8.0) {
            dinc = 2.0 * j;
        }

        setYmin(yMin);
        setYmax(yMax);
        setDataInc(dinc);
    }

    public void adjustPcYmax(double minval, double maxval) {
        double dminmax;
        dminmax = maxval - minval;

        double dinc;
        if (dminmax <= 0.5) {

            minval = 0.0;
            maxval = (int) maxval + 0.5;
            dinc = 0.1;
        } else if (dminmax <= 2.0) {

            minval = (int) minval;
            maxval = (int) maxval + 1.0;
            dinc = 0.5;

        } else {

            minval = (int) minval;
            maxval = (int) maxval + 1.0;
            dinc = 1.0;
        }

        setYmin2(minval);
        setYmax2(maxval);
        setDataInc2(dinc);
    }
}
