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
package com.raytheon.viz.hydrocommon.data;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.Map;
import java.util.TimeZone;

import com.raytheon.uf.common.dataquery.db.QueryResultRow;

/**
 * this class contains the low water data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Nov 11, 2008				askripsky	Initial creation
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */
public class LowWaterData {

    public static final int MISSING_VALUE = -9999;

    public static final double MISSING_VALUE_D = -9999;

    /**
     * Lid that the data refers to
     */
    private String lid;

    /**
     * Date.
     */
    private Date lwDate;

    /**
     * flow or q (in DB).
     */
    private int flow;

    /**
     * Remark.
     */
    private String remark;

    /**
     * Stage.
     */
    private double stage;

    /**
     * Date format.
     */
    private SimpleDateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy");

    /**
     * Constructor
     */
    public LowWaterData() {
        lid = "";
        stage = MISSING_VALUE_D;
        flow = MISSING_VALUE;
        remark = "";
        lwDate = Calendar.getInstance(TimeZone.getTimeZone("GMT")).getTime();
        dateFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
    }

    /**
     * Constructor that accepts the object array from a db call
     * 
     * @param data
     *            The raw data from the database.
     */
    public LowWaterData(QueryResultRow data, Map<String, Integer> dataMap) {
        setStage(data.getColumn(dataMap.get("stage")));
        setFlow(data.getColumn(dataMap.get("q")));
        setRemark(data.getColumn(dataMap.get("lwrem")));

        // not NULL in DB
        setLid((String) data.getColumn(dataMap.get("lid")));
        setDate((Date) data.getColumn(dataMap.get("lwdat")));

        dateFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
    }

    public String getLid() {
        return lid;
    }

    public void setLid(String lid) {
        this.lid = lid;
    }

    public double getStage() {
        return stage;
    }

    public void setStage(Object stage) {
        this.stage = (stage != null) ? (Double) stage : MISSING_VALUE_D;
    }

    public Date getDate() {
        return lwDate;
    }

    public void setDate(Date lwDate) {
        this.lwDate = lwDate;
    }

    public int getFlow() {
        return flow;
    }

    public void setFlow(Object q) {
        flow = (q != null) ? (Integer) q : MISSING_VALUE;
    }

    public String getRemark() {
        return remark;
    }

    public void setRemark(Object remark) {
        this.remark = (remark != null) ? (String) remark : "";
    }

    @Override
    public String toString() {
        return String
                .format("%-24s        %-27s                 %-12s",
                        (stage != MISSING_VALUE_D) ? String.format("%8.2f",
                                stage) : "", (flow != MISSING_VALUE) ? String
                                .format("%8d", flow) : "", dateFormat
                                .format(lwDate));
    }
}
