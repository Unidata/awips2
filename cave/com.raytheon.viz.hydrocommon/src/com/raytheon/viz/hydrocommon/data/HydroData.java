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

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.util.QualityCodeUtil;

/**
 * this is the base data class for HydroView and HydroBase.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Oct 28, 2008 1636        askripsky	Initial creation
 * 11/5/2008    1662        grichard    Added another constructor.
 * Jul 21, 2015 4500        rjpeter     Use Number in blind cast.
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */
public class HydroData extends HydroDBData {
    /**
     * Station.
     */
    protected String lid = "";

    /**
     * Name.
     */
    protected String name = "";

    /**
     * PE
     */
    protected String pe = "";

    /**
     * DUR.
     */
    protected int dur;

    /**
     * TS.
     */
    protected String ts = "";

    /**
     * EXTREMUM.
     */
    protected String extremum = "";

    /**
     * Value.
     */
    protected double value;

    /**
     * Previous Value.
     */
    protected double previousValue;

    /**
     * 
     * SHEF Quality Code.
     */
    protected String shefQualCode = "";

    /**
     * Quality code symbol.
     */
    protected int qualityCode;

    /**
     * Revision.
     */
    protected int revision;

    /**
     * Product ID.
     */
    protected String productID = "";

    /**
     * Product Time.
     */
    protected Date productTime;

    /**
     * Observation Time.
     */
    protected Date obsTime;

    /**
     * Posting Time.
     */
    protected Date postingTime;

    /**
     * Name of the table the data are from.
     */
    protected String table = null;

    private static SimpleDateFormat postingFormat;

    protected static SimpleDateFormat obsFormat;

    static {
        postingFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

        obsFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        obsFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
    }

    /**
     * Constructor
     */
    public HydroData() {
    }

    /**
     * Handles the first baker's dozen columns from a DB call
     * 
     * @param data
     */
    public HydroData(Object[] data) {
        setLid((String) data[0]);
        setName((String) data[1]);
        setPe((String) data[2]);
        setDur(((Number) data[3]).intValue());
        setTs((String) data[4]);
        setExtremum((String) data[5]);
        setValue(data[6]);
        setShefQualCode(data[7]);
        setQualityCode((data[8]));
        setRevision(data[9]);
        setProductID(data[10]);
        setProductTime(data[11]);
        setPostingTime(data[12]);
    }

    /**
     * Handles the first dozen columns from a DB call plus the name.
     * 
     * @param data
     * @param name
     */
    public HydroData(Object[] data, Object name) {
        setLid((String) data[0]);
        setPe((String) data[1]);
        setDur(((Number) data[2]).intValue());
        setTs((String) data[3]);
        setExtremum((String) data[4]);
        setValue(data[5]);
        setShefQualCode(data[6]);
        setQualityCode(data[7]);
        setRevision(data[8]);
        setProductID(data[9]);
        setProductTime(data[10]);
        setPostingTime(data[11]);
        setName((String) name);
    }

    public HydroData(Object[] data, String table) {
        setLid((String) data[0]);
        setPe((String) data[1]);
        setDur(((Number) data[2]).intValue());
        setTs((String) data[3]);
        setExtremum((String) data[4]);
        setObsTime((Date) data[5]);
        setValue(data[6]);
        setShefQualCode(data[7]);
        setQualityCode(data[8]);
        setRevision(data[9]);
        setProductID(data[10]);
        setProductTime(data[11]);
        setPostingTime(data[12]);
        setTable(table);
    }

    public String getLid() {
        return lid;
    }

    public void setLid(String lid) {
        this.lid = (lid != null) ? lid : "";
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = (name != null) ? name : "";
    }

    public String getPe() {
        return pe;
    }

    public void setPe(String pe) {
        this.pe = (pe != null) ? pe : "";
    }

    public int getDur() {
        return dur;
    }

    public void setDur(int dur) {
        this.dur = dur;
    }

    public String getTs() {
        return ts;
    }

    public void setTs(String ts) {
        this.ts = (ts != null) ? ts : "";
    }

    public String getExtremum() {
        return extremum;
    }

    public void setExtremum(String extremum) {
        this.extremum = (extremum != null) ? extremum : "";
    }

    public double getValue() {
        return value;
    }

    public void setValue(Object value) {
        this.value = (value != null) ? (Double) value : Double.NaN;
    }

    public String getShefQualCode() {
        return shefQualCode;
    }

    public void setShefQualCode(Object shefQualCode) {
        this.shefQualCode = (shefQualCode != null) ? (String) shefQualCode : "";
    }

    /**
     * Sets the default behavior to return the human readable quality code
     */
    public int getQualityCode() {
        return qualityCode;
    }

    /**
     * Returns quality code symbol
     * 
     * @return The symbol for the quality code
     */
    public String getQualityCodeSymbol() {
        return QualityCodeUtil.buildQcSymbol(qualityCode);
    }

    public void setQualityCode(Object qualityCode) {
        this.qualityCode = (qualityCode != null) ? ((Number) qualityCode)
                .intValue() : HydroConstants.MISSING_VALUE;
    }

    public int getRevision() {
        return revision;
    }

    public void setRevision(Object revision) {
        this.revision = (revision != null) ? ((Number) revision).intValue() : 0;
    }

    public String getProductID() {
        return productID;
    }

    public void setProductID(Object productID) {
        this.productID = (productID != null) ? (String) productID : "";
    }

    public Date getProductTime() {
        return productTime;
    }

    public String getProductTimeString() {
        return obsFormat.format(productTime);
    }

    public void setProductTime(Object productTime) {
        this.productTime = (productTime != null) ? (Date) productTime : null;
    }

    public Date getPostingTime() {
        return postingTime;
    }

    public String getPostingTimeString() {
        return obsFormat.format(postingTime);
    }

    public void setPostingTime(Object postingTime) {
        if (postingTime instanceof String) {
            try {
                this.postingTime = postingFormat.parse((String) postingTime);
            } catch (ParseException e) {
                e.printStackTrace();
            }
        } else {
            this.postingTime = (postingTime != null) ? (Date) postingTime
                    : null;
        }
    }

    /**
     * @return the obsTime
     */
    public Date getObsTime() {
        return obsTime;
    }

    /**
     * @param obsTime
     *            the obsTime to set
     */
    public void setObsTime(Date obsTime) {
        this.obsTime = obsTime;
    }

    /**
     * @return the table
     */
    public String getTable() {
        return table;
    }

    /**
     * @param table
     *            the table to set
     */
    public void setTable(String table) {
        this.table = table;
    }

    /**
     * @return the previousValue
     */
    public double getPreviousValue() {
        return previousValue;
    }

    /**
     * @param previousValue
     *            the previousValue to set
     */
    public void setPreviousValue(double previousValue) {
        this.previousValue = previousValue;
    }
}
