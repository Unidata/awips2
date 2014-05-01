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
import java.util.List;

import com.raytheon.viz.hydrocommon.HydroConstants;

/**
 * Object to hold the data and metadata for the time series.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jun 24, 2008				mpduff	    Initial creation.
 * May 06, 2013   1976      mpduff      Use List interface.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class GraphData {
    private double x;

    /**
     * yMax - yMin
     */
    private double y;

    private double y2;

    /**
     * Graph Area Width
     */
    private int w;

    /**
     * Graph Area Height
     */
    private int h;

    private boolean displayFlowUnit;

    private Date xMin;

    private Date xMax;

    private double ymin = Integer.MAX_VALUE;

    private double ymax = Integer.MIN_VALUE;

    private double ymin2;

    private double ymax2;

    /**
     * List of traces for graphing
     */
    private List<TraceData> traces = new ArrayList<TraceData>();

    /**
     * List of traces for this graphing object
     */
    private final List<TraceData> originalTraces = new ArrayList<TraceData>();

    private List<Boolean> isStage = new ArrayList<Boolean>();

    private Date beginDate = null;

    private Date endDate = null;

    private String title = null;

    private int num_traces;

    private int graph_pos;

    private int xsize;

    private int ysize;

    private String yscale = null;

    private String ylinear = null;

    private boolean showcat = false;

    private String derivepp = null;

    private boolean showpp = false;

    private boolean latestfcstonly = false;

    private String ylinearc = null;

    private String yscalec = null;

    private String showcatc = null;

    private String deriveppc = null;

    private String showppc = null;

    private String latestfcstonlyc = null;

    private double actionStage = -9999;

    private double floodStage = -9999;

    private double minorStage = -9999;

    private double moderateStage = -9999;

    private double majorStage = -9999;

    private double actionFlow = -9999;

    private double floodFlow = -9999;

    private double minorFlow = -9999;

    private double moderateFlow = -9999;

    private double majorFlow = -9999;

    /**
     * @return the x
     */
    public double getX() {
        return x;
    }

    /**
     * @param x
     *            the x to set
     */
    public void setX(double x) {
        this.x = x;
    }

    /**
     * @return the y
     */
    public double getY() {
        return y;
    }

    /**
     * y = yMax - yMin
     * 
     * @param y
     *            the y to set
     */
    public void setY(double y) {
        this.y = y;
    }

    /**
     * @return the w
     */
    public int getW() {
        return w;
    }

    /**
     * @param w
     *            the w to set
     */
    public void setW(int w) {
        this.w = w;
    }

    /**
     * @return the h
     */
    public int getH() {
        return h;
    }

    /**
     * @param h
     *            the h to set
     */
    public void setH(int h) {
        this.h = h;
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
     * @param display_flow_unit
     *            the display_flow_unit to set
     */
    public void setDisplayFlowUnit(boolean displayFlowUnit) {
        this.displayFlowUnit = displayFlowUnit;
    }

    /**
     * @return the xMin
     */
    public Date getXMin() {
        return xMin;
    }

    /**
     * @param min
     *            the xMin to set
     */
    public void setXMin(Date min) {
        xMin = min;
    }

    /**
     * @return the xMax
     */
    public Date getXMax() {
        return xMax;
    }

    /**
     * @param max
     *            the xMax to set
     */
    public void setXMax(Date max) {
        xMax = max;
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
     * @return the traces
     */
    public List<TraceData> getTraces() {
        return traces;
    }

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

    public void addTrace(TraceData trace) {
        if (traces.size() < HydroConstants.MAX_TRACES) {
            if (!traces.contains(trace)) {
                traces.add(trace);
            }
        }
    }

    /**
     * @return the isStage
     */
    public List<Boolean> getIsStage() {
        return isStage;
    }

    /**
     * @param isStage
     *            the isStage to set
     */
    public void setIsStage(ArrayList<Boolean> isStage) {
        this.isStage = isStage;
    }

    /**
     * Add the isStage boolean value to the list
     * 
     * @param isStage
     *            true if value is stage, false otherwise
     */
    public void addIsStage(boolean isStage) {
        if (traces.size() < HydroConstants.MAX_TRACES) {
            this.isStage.add(isStage);
        }
    }

    /**
     * @return the beginCal
     */
    public Date getBeginCal() {
        return beginDate;
    }

    /**
     * @param beginCal
     *            the beginCal to set
     */
    public void setBeginDate(Date beginDate) {
        this.beginDate = beginDate;
    }

    /**
     * @return the endCal
     */
    public Date getEndDate() {
        return endDate;
    }

    /**
     * @param endCal
     *            the endCal to set
     */
    public void setEndDate(Date endDate) {
        this.endDate = endDate;
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
     * @return the num_traces
     */
    public int getNum_traces() {
        return num_traces;
    }

    /**
     * @param num_traces
     *            the num_traces to set
     */
    public void setNum_traces(int num_traces) {
        this.num_traces = num_traces;
    }

    /**
     * @return the graph_pos
     */
    public int getGraph_pos() {
        return graph_pos;
    }

    /**
     * @param graph_pos
     *            the graph_pos to set
     */
    public void setGraph_pos(int graph_pos) {
        this.graph_pos = graph_pos;
    }

    /**
     * @return the xsize
     */
    public int getXsize() {
        return xsize;
    }

    /**
     * @param xsize
     *            the xsize to set
     */
    public void setXsize(int xsize) {
        this.xsize = xsize;
    }

    /**
     * @return the ysize
     */
    public int getYsize() {
        return ysize;
    }

    /**
     * @param ysize
     *            the ysize to set
     */
    public void setYsize(int ysize) {
        this.ysize = ysize;
    }

    /**
     * @return the yscale
     */
    public String getYscale() {
        return yscale;
    }

    /**
     * @param yscale
     *            the yscale to set
     */
    public void setYscale(String yscale) {
        this.yscale = yscale;
    }

    /**
     * @return the ylinear
     */
    public String getYlinear() {
        return ylinear;
    }

    /**
     * @param ylinear
     *            the ylinear to set
     */
    public void setYlinear(String ylinear) {
        this.ylinear = ylinear;
    }

    /**
     * @return the showcat
     */
    public boolean getShowcat() {
        return showcat;
    }

    /**
     * @param showcat
     *            the showcat to set
     */
    public void setShowcat(boolean showcat) {
        this.showcat = showcat;
    }

    /**
     * @return the derivepp
     */
    public String getDerivepp() {
        return derivepp;
    }

    /**
     * @param derivepp
     *            the derivepp to set
     */
    public void setDerivepp(String derivepp) {
        this.derivepp = derivepp;
    }

    /**
     * @return the showpp
     */
    public boolean getShowpp() {
        return showpp;
    }

    /**
     * @param showpp
     *            the showpp to set
     */
    public void setShowpp(boolean showpp) {
        this.showpp = showpp;
    }

    /**
     * @return the latestfcstonly
     */
    public boolean getLatestfcstonly() {
        return latestfcstonly;
    }

    /**
     * @param latestfcstonly
     *            the latestfcstonly to set
     */
    public void setLatestfcstonly(boolean latestfcstonly) {
        this.latestfcstonly = latestfcstonly;
    }

    /**
     * @return the ylinearc
     */
    public String getYlinearc() {
        return ylinearc;
    }

    /**
     * @param ylinearc
     *            the ylinearc to set
     */
    public void setYlinearc(String ylinearc) {
        this.ylinearc = ylinearc;
    }

    /**
     * @return the yscalec
     */
    public String getYscalec() {
        return yscalec;
    }

    /**
     * @param yscalec
     *            the yscalec to set
     */
    public void setYscalec(String yscalec) {
        this.yscalec = yscalec;
    }

    /**
     * @return the showcatc
     */
    public String getShowcatc() {
        return showcatc;
    }

    /**
     * @param showcatc
     *            the showcatc to set
     */
    public void setShowcatc(String showcatc) {
        this.showcatc = showcatc;
    }

    /**
     * @return the deriveppc
     */
    public String getDeriveppc() {
        return deriveppc;
    }

    /**
     * @param deriveppc
     *            the deriveppc to set
     */
    public void setDeriveppc(String deriveppc) {
        this.deriveppc = deriveppc;
    }

    /**
     * @return the showppc
     */
    public String getShowppc() {
        return showppc;
    }

    /**
     * @param showppc
     *            the showppc to set
     */
    public void setShowppc(String showppc) {
        this.showppc = showppc;
    }

    /**
     * @return the latestfcstonlyc
     */
    public String getLatestfcstonlyc() {
        return latestfcstonlyc;
    }

    /**
     * @param latestfcstonlyc
     *            the latestfcstonlyc to set
     */
    public void setLatestfcstonlyc(String latestfcstonlyc) {
        this.latestfcstonlyc = latestfcstonlyc;
    }

    @Override
    public String toString() {
        return "\nTitle:  " + title;
    }

    /**
     * @return the actionStage
     */
    public double getActionStage() {
        return actionStage;
    }

    /**
     * @param actionStage
     *            the actionStage to set
     */
    public void setActionStage(double actionStage) {
        this.actionStage = actionStage;
    }

    /**
     * @return the minorStage
     */
    public double getMinorStage() {
        return minorStage;
    }

    /**
     * @param minorStage
     *            the minorStage to set
     */
    public void setMinorStage(double minorStage) {
        this.minorStage = minorStage;
    }

    /**
     * @return the moderateStage
     */
    public double getModerateStage() {
        return moderateStage;
    }

    /**
     * @param moderateStage
     *            the moderateStage to set
     */
    public void setModerateStage(double moderateStage) {
        this.moderateStage = moderateStage;
    }

    /**
     * @return the majorStage
     */
    public double getMajorStage() {
        return majorStage;
    }

    /**
     * @param majorStage
     *            the majorStage to set
     */
    public void setMajorStage(double majorStage) {
        this.majorStage = majorStage;
    }

    /**
     * @return the actionFlow
     */
    public double getActionFlow() {
        return actionFlow;
    }

    /**
     * @param actionFlow
     *            the actionFlow to set
     */
    public void setActionFlow(double actionFlow) {
        this.actionFlow = actionFlow;
    }

    /**
     * @return the minorFlow
     */
    public double getMinorFlow() {
        return minorFlow;
    }

    /**
     * @param minorFlow
     *            the minorFlow to set
     */
    public void setMinorFlow(double minorFlow) {
        this.minorFlow = minorFlow;
    }

    /**
     * @return the moderateFlow
     */
    public double getModerateFlow() {
        return moderateFlow;
    }

    /**
     * @param moderateFlow
     *            the moderateFlow to set
     */
    public void setModerateFlow(double moderateFlow) {
        this.moderateFlow = moderateFlow;
    }

    /**
     * @return the majorFlow
     */
    public double getMajorFlow() {
        return majorFlow;
    }

    /**
     * @param majorFlow
     *            the majorFlow to set
     */
    public void setMajorFlow(double majorFlow) {
        this.majorFlow = majorFlow;
    }

    /**
     * @return the floodStage
     */
    public double getFloodStage() {
        return floodStage;
    }

    /**
     * @param floodStage
     *            the floodStage to set
     */
    public void setFloodStage(double floodStage) {
        this.floodStage = floodStage;
    }

    /**
     * @return the floodFlow
     */
    public double getFloodFlow() {
        return floodFlow;
    }

    /**
     * @param floodFlow
     *            the floodFlow to set
     */
    public void setFloodFlow(double floodFlow) {
        this.floodFlow = floodFlow;
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
     * @return the y2
     */
    public double getY2() {
        return y2;
    }

    /**
     * @param y2
     *            the y2 to set
     */
    public void setY2(double y2) {
        this.y2 = y2;
    }

    /**
     * @return the originalTraces
     */
    public List<TraceData> getOriginalTraces() {
        List<TraceData> rtnList = new ArrayList<TraceData>(
                originalTraces.size());

        for (TraceData td : originalTraces) {
            TraceData data = new TraceData(td);
            rtnList.add(data);
        }
        return rtnList;
    }

    /**
     * Generate a copy of the original trace data.
     */
    public void saveTraceInfo() {
        for (TraceData td : traces) {
            TraceData data = new TraceData(td);
            originalTraces.add(data);
        }
    }
}
