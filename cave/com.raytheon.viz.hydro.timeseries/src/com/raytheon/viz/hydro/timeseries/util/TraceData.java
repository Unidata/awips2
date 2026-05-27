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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydro.timeseries.TimeSeriesDataManager;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.util.HydroUtils;

/**
 * Object holds the metadata for the individual traces.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#     Engineer     Description
 * ------------- ----------- ------------ --------------------------
 * Jun 25, 2008  1194        mpduff       Initial creation
 * Mar 04, 2011  7758        Jingtao      added isSelectionCheckOn flag for pop
 *                                        up menu
 * Apr 05, 2011  8732        jpiatt       Added product_id.
 * Jun 01, 2011  9499        djingtao     change setDur()
 * Oct 22, 2015  13736       xwei         Added getZoomIndexOffset() method
 * Jan 13, 2016  5243        tgurney      Added duration codes E (5 min.) and G
 *                                        (10 min.)
 * Apr 12, 2018  6619        randerso     Changed dur to int
 * Jun 27, 2018  6748        randerso     Separated metadata into TraceInfo
 *
 * </pre>
 *
 * @author mpduff
 */

public class TraceData implements Serializable {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(TraceData.class);

    private static final long serialVersionUID = -4345330013536912094L;

    private RGB traceColor;

    private double ymin;

    private double ymax;

    private double value_ymin;

    private double value_ymax;

    private Date xmin;

    private Date xmax;

    private Date basistime;

    private Date productTime;

    private List<TimeSeriesPoint> tsData;

    private String product_id = null;

    private String label = null;

    private boolean isSelected = false;

    private boolean traceOn = true;

    private boolean valid = true;

    private boolean selectionCheckOn = true;

    private TraceInfo traceInfo;

    /**
     * Constructor
     *
     * @param traceInfo
     *            the metadata for this trace
     */
    public TraceData(TraceInfo traceInfo) {
        this.traceInfo = traceInfo;
    }

    /**
     * Copy constructor.
     *
     * @param data
     */
    public TraceData(TraceData data) {
        basistime = data.basistime;
        product_id = data.product_id;
        isSelected = data.isSelected;
        label = data.label;
        productTime = data.productTime;
        traceColor = data.traceColor;
        traceOn = data.traceOn;
        tsData = data.tsData;
        value_ymax = data.value_ymax;
        value_ymin = data.value_ymin;
        xmax = data.xmax;
        xmin = data.xmin;
        ymax = data.ymax;
        ymin = data.ymin;
        selectionCheckOn = data.selectionCheckOn;
        traceInfo = new TraceInfo(data.traceInfo);
    }

    /**
     *
     * @return pop up check box selection
     */
    public boolean isSelectionCheckOn() {
        return selectionCheckOn;
    }

    /**
     *
     * @param selectionCheckOn
     */
    public void setSelectionCheckOn(boolean selectionCheckOn) {
        this.selectionCheckOn = selectionCheckOn;
    }

    /**
     * @return the npts
     */
    public int getNpts() {
        if (tsData == null) {
            return 0;
        }
        return tsData.size();
    }

    /**
     * @return the traceColor
     */
    public RGB getTraceColor() {
        return traceColor;
    }

    /**
     * @param traceColor
     *            the traceColor to set
     */
    public void setTraceColor(RGB traceColor) {
        this.traceColor = traceColor;
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
     * @return the value_ymin
     */
    public double getValue_ymin() {
        return value_ymin;
    }

    /**
     * @param value_ymin
     *            the value_ymin to set
     */
    public void setValue_ymin(double value_ymin) {
        this.value_ymin = value_ymin;
    }

    /**
     * @return the value_ymax
     */
    public double getValue_ymax() {
        return value_ymax;
    }

    /**
     * @param value_ymax
     *            the value_ymax to set
     */
    public void setValue_ymax(double value_ymax) {
        this.value_ymax = value_ymax;
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
     * @return the basistime
     */
    public Date getBasistime() {
        return basistime;
    }

    /**
     * @param basistime
     *            the basistime to set
     */
    public void setBasistime(Date basistime) {
        this.basistime = basistime;
    }

    /**
     * @return the tsData
     */
    public List<TimeSeriesPoint> getTsData() {
        return tsData;
    }

    /**
     * @param tsData
     *            the tsData to set
     */
    public void setTsData(List<TimeSeriesPoint> tsData) {
        this.tsData = tsData;
    }

    /**
     * @return the name
     */
    public String getName() {
        return this.traceInfo.getName();
    }

    /**
     * @return the lid
     */
    public String getLid() {
        return this.traceInfo.getLid();
    }

    /**
     * @return the pe
     */
    public String getPe() {
        return this.traceInfo.getPe();
    }

    /**
     * @return the dur
     */
    public int getDur() {
        return this.traceInfo.getDur();
    }

    /**
     * @return the ts
     */
    public String getTs() {
        return this.traceInfo.getTs();
    }

    /**
     * @return the pe dur ts andextremum formatted as a string
     */
    public String getPEDTSE() {
        return traceInfo.getPEDTSE();
    }

    /**
     * @return the product_id
     */
    public String getProductId() {
        return product_id;
    }

    /**
     * @param product_id
     *            the product_id to set
     */
    public void setProductId(String product_id) {
        this.product_id = product_id;
    }

    /**
     * @return the extremum
     */
    public String getExtremum() {
        return this.traceInfo.getExtremum();
    }

    /**
     * @return the cdur
     */
    public String getCdur() {
        return this.traceInfo.getCdur();
    }

    /**
     * @return the colorName
     */
    public String getColorName() {
        return this.traceInfo.getColorName();
    }

    /**
     * @return the isForecast
     */
    public boolean isForecast() {
        return this.traceInfo.isForecast();
    }

    /**
     * @return the label
     */
    public String getLabel() {
        return label;
    }

    /**
     * @param label
     *            the label to set
     */
    public void setLabel(String label) {
        this.label = label;
    }

    /**
     * @return the isSelected
     */
    public boolean isSelected() {
        return isSelected;
    }

    /**
     * @param isSelected
     *            the isSelected to set
     */
    public void setSelected(boolean isSelected) {
        this.isSelected = isSelected;
    }

    /**
     * @return the traceOn
     */
    public boolean isTraceOn() {
        return traceOn;
    }

    /**
     * @param traceOn
     *            the traceOn to set
     */
    public void setTraceOn(boolean traceOn) {
        this.traceOn = traceOn;
    }

    /**
     * @return true if this trace is valid
     */
    public boolean isValid() {
        return valid;
    }

    /**
     * @param valid
     *            true if this trace is valid
     */
    public void setValid(boolean valid) {
        this.valid = valid;
    }

    /**
     * @return the productTime
     */
    public Date getProductTime() {
        return productTime;
    }

    /**
     * @param productTime
     *            the productTime to set
     */
    public void setProductTime(Date productTime) {
        this.productTime = productTime;
    }

    /**
     * @return the traceInfo
     */
    public TraceInfo getTraceInfo() {
        return traceInfo;
    }

    /**
     * Get the trace data for this trace over the desired time interval
     *
     * @param beginDate
     * @param endDate
     * @return true if data is found
     */
    public boolean getObsTrace(Date beginDate, Date endDate) {
        boolean status = false;

        TimeSeriesDataManager dataManager = TimeSeriesDataManager.getInstance();
        List<TimeSeriesPoint> points = new ArrayList<>();
        String lid = getLid();
        String ts = getTs().toUpperCase();
        String pe = getPe().toUpperCase();
        int dur = getDur();
        String ext = getExtremum().toUpperCase();

        String tablename = HydroUtils.getTableName(pe, ts);

        /* Get the data from IHFS and store in TimeSeriesPoint object */
        try {
            double ymin = Double.MAX_VALUE;
            double ymax = -Double.MAX_VALUE;
            Date xmin = new Date(Long.MAX_VALUE);
            Date xmax = new Date(Long.MIN_VALUE);
            List<Object[]> data = dataManager.getGraphData(tablename, lid, pe,
                    ts, dur, ext, beginDate, endDate);

            if (data != null && !data.isEmpty()) {
                for (int i = 0; i < data.size(); i++) {
                    TimeSeriesPoint p = new TimeSeriesPoint();
                    Object[] oa = data.get(i);
                    Date x = (Date) oa[1];
                    p.setX(x);
                    if (!((Double) oa[2] == HydroConstants.MISSING_VALUE)) {
                        double y = ((Number) oa[2]).doubleValue();
                        p.setY(y);
                        ymin = Math.min(ymin, y);
                        ymax = Math.max(ymax, y);

                        if (x.before(xmin)) {
                            xmin = x;
                        }
                        if (x.after(xmax)) {
                            xmax = x;
                        }

                        points.add(p);
                    }
                }

                setYmin(ymin);
                setYmax(ymax);
                setXmin(xmin);
                setXmax(xmax);
                status = true;
            }
        } catch (VizException e) {
            statusHandler.error("Error retrieving graph data", e);
        }

        setTsData(points);

        return status;
    }

}
