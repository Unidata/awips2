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
package com.raytheon.viz.hydro.stationreporting;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

/**
 * This class contains station reporting data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * OCT 8, 2007  1580       askripsky   Initial creation.
 * 21 Feb 2010  2915       mpduff      Fixed Time Zone problem.
 * Sep 03, 2015 4845       rjpeter     Fix NPE.
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 * 
 */
public class StationReportingData implements Comparable<StationReportingData> {
    /**
     * Location (Station) ID.
     */
    private String lid;

    private String name;

    private String pe;

    private short dur;

    private String ts;

    private String extremum;

    private Date obstime;

    private Double value;

    private short revision;

    private String shefQualCode;

    private int qualityCode;

    private String productId;

    private Date producttime;

    private Date postingtime;

    // private SimpleDateFormat dbDatabaseFormat;
    //
    // private SimpleDateFormat moreInfoFormat;
    //
    // private SimpleDateFormat insertFormat;

    private Date defaultDate;
//
//    static {
//        insertFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
//        // insertFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SS");
//
//        dbDatabaseFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SS");
//        dbDatabaseFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
//
//        moreInfoFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm");
//        moreInfoFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
//
//        try {
//            defaultDate = insertFormat.parse("1-1-1 00:00:00.00");
//        } catch (ParseException e) {
//        }
//    }

    /**
     * Constructor.
     */
    public StationReportingData(String lid, String name, String pe, short dur,
            String ts, String extremum, Date obstime, double value,
            short revision, String shefQualCode, int qualityCode,
            String productId, Date producttime, Date postingtime) {
        try {
            SimpleDateFormat insertFormat = new SimpleDateFormat(
            "yyyy-MM-dd HH:mm:ss");
            insertFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
            defaultDate = insertFormat.parse("1-1-1 00:00:00.00");
        } catch (ParseException e) {
        }

        if (obstime == null) {
            obstime = defaultDate;
        }
        if (producttime == null) {
            producttime = defaultDate;
        }
        if (postingtime == null) {
            postingtime = defaultDate;
        }
        
        this.lid = lid;
        this.name = name;
        this.pe = pe;
        this.dur = dur;
        this.ts = ts;
        this.extremum = extremum;
        this.obstime = obstime;
        this.value = value;
        this.revision = revision;
        this.shefQualCode = shefQualCode;
        this.qualityCode = qualityCode;
        this.productId = productId;
        this.producttime = producttime;
        this.postingtime = postingtime;

        SimpleDateFormat insertFormat = new SimpleDateFormat(
                "yyyy-MM-dd HH:mm:ss");
        insertFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
        
        SimpleDateFormat dbDatabaseFormat = new SimpleDateFormat(
                "yyyy-MM-dd HH:mm:ss.SS");
        dbDatabaseFormat.setTimeZone(TimeZone.getTimeZone("UTC"));

        SimpleDateFormat moreInfoFormat = new SimpleDateFormat(
                "yyyy-MM-dd HH:mm");
        moreInfoFormat.setTimeZone(TimeZone.getTimeZone("UTC"));

    }

    public StationReportingData(String lid, String name) {
        this(lid, name, "", (short) 0, "", "", null, 0.0, (short) 0, "",
                0, "", null, null);
    }

    public StationReportingData() {
    }

    public String getLid() {
        return lid;
    }

    public void setLid(String lid) {
        this.lid = lid;
    }

    public String getName() {
        return (name != null) ? name : "";
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getPe() {
        return pe;
    }

    public void setPe(String pe) {
        this.pe = pe;
    }

    public short getDur() {
        return dur;
    }

    public void setDur(short dur) {
        this.dur = dur;
    }

    public String getTs() {
        return ts;
    }

    public void setTs(String ts) {
        this.ts = ts;
    }

    public String getExtremum() {
        return extremum;
    }

    public void setExtremum(String extremum) {
        this.extremum = extremum;
    }

    public Date getObstime() {
        return obstime;
    }

    public void setObstime(String obstime) throws ParseException {
        SimpleDateFormat insertFormat = new SimpleDateFormat(
                "yyyy-MM-dd HH:mm:ss");
        insertFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
        this.obstime = insertFormat.parse(obstime);
    }

    public Double getValue() {
        return value;
    }

    public void setValue(Double value) {
        this.value = value;
    }

    public short getRevision() {
        return revision;
    }

    public void setRevision(short revision) {
        this.revision = revision;
    }

    public String getShefQualCode() {
        return shefQualCode;
    }

    public void setShefQualCode(String shefQualCode) {
        this.shefQualCode = shefQualCode;
    }

    public Integer getQualityCode() {
        return qualityCode;
    }

    public void setQualityCode(Number qualityCode) {
        this.qualityCode = (qualityCode != null) ? qualityCode.intValue() : 0;
    }

    public String getProductId() {
        return productId;
    }

    public void setProductId(String productId) {
        this.productId = productId;
    }

    public Date getProducttime() {
        return producttime;
    }

    public void setProducttime(String producttime) throws ParseException {
        SimpleDateFormat insertFormat = new SimpleDateFormat(
                "yyyy-MM-dd HH:mm:ss");
        insertFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
        this.producttime = insertFormat.parse(producttime);
    }

    public Date getPostingtime() {
        return postingtime;
    }

    public void setPostingtime(String postingtime) throws ParseException {
        SimpleDateFormat insertFormat = new SimpleDateFormat(
                "yyyy-MM-dd HH:mm:ss");
        insertFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
        this.postingtime = insertFormat.parse(postingtime);
    }

    public static String formatDBTimestamp(Date currDate) {
        SimpleDateFormat format = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SS");
        format.setTimeZone(TimeZone.getTimeZone("UTC"));
        return format.format(currDate);
    }

    public static String formatInfoTimestamp(Date currDate) {
        SimpleDateFormat format = new SimpleDateFormat("yyyy-MM-dd HH:mm");
        format.setTimeZone(TimeZone.getTimeZone("UTC"));
        return format.format(currDate);
    }

    @Override
    public int compareTo(StationReportingData o) {
        return lid.compareTo(o.lid);
    }
}