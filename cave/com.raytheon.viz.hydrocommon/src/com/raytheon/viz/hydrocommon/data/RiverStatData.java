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
import java.util.Date;
import java.util.Map;
import java.util.TimeZone;

import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.viz.hydrocommon.HydroConstants;

/**
 * This class contains the data for the RiverStat table.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jan 6, 2008	1802    	askripsky	Initial creation
 * Jan 3, 2013  15520       lbousaidi   added a dollar-quoted string to getUpdate and
 *                                      getInsert statements to always write literally
 *                                      the string content.

 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */
public class RiverStatData extends HydroDBData implements IHydroDBData {
    /**
     * Location
     */
    private String lid;

    /**
     * primary_pe
     */
    private String primaryPE;

    /**
     * bf - bankfull
     */
    private double bankFull;

    /**
     * cb - Check Bar
     */
    private double checkBar;

    /**
     * da - Drainage Area
     */
    private double drainageArea;

    /**
     * response_time
     */
    private double responseTime;

    /**
     * threshold_runoff
     */
    private double thresholdRunoff;

    /**
     * fq - Flood Flow
     */
    private double floodFlow;

    /**
     * fs - Flood Stage
     */
    private double floodStage;

    /**
     * gsno
     */
    private String gageNumber;

    /**
     * level
     */
    private String level;

    /**
     * mile
     */
    private double riverMile;

    /**
     * pool
     */
    private double pool;

    /**
     * por - Period of Record
     */
    private String periodOfRecord;

    /**
     * rated
     */
    private String rated;

    /**
     * lat
     */
    private double latitude;

    /**
     * lon
     */
    private double longitude;

    /**
     * remark
     */
    private String remark;

    /**
     * rrevise
     */
    private Date reviseDate;

    /**
     * rsource - lat/lon source
     */
    private String latLonSource;

    /**
     * stream
     */
    private String stream;

    /**
     * tide
     */
    private String tidalEffect;

    /**
     * backwater
     */
    private String backWater;

    /**
     * vdatum
     */
    private String verticalDatum;

    /**
     * action_flow
     */
    private double actionFlow;

    /**
     * wstg
     */
    private double actionStage;

    /**
     * zd
     */
    private double zeroDatum;

    /**
     * ratedat - Date of Rating
     */
    private Date dateOfRating;

    /**
     * usgs_ratenum
     */
    private String usgsRateNumber;

    /**
     * uhgdur
     */
    private int unitHydrographDuration;

    /**
     * use_latest_fcst
     */
    private String useLatestForecast;

    /**
     * Date format
     */
    private SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");

    /**
     * Constructor
     */
    public RiverStatData() {
        initDateFormat();
    }

    /**
     * Constructor that accepts the object array from a db call
     * 
     * @param data
     *            The raw data from the database.
     */
    public RiverStatData(QueryResultRow data, Map<String, Integer> dataMap) {
        initDateFormat();

        setLid(getDBValue("lid", data, dataMap, ""));
        setPrimaryPE(getDBValue("primary_pe", data, dataMap, ""));
        setBankFull(getDBValue("bf", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setCheckBar(getDBValue("cb", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setDrainageArea(getDBValue("da", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setResponseTime(getDBValue("response_time", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setThresholdRunoff(getDBValue("threshold_runoff", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setFloodFlow(getDBValue("fq", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setFloodStage(getDBValue("fs", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setGageNumber(getDBValue("gsno", data, dataMap, ""));
        setLevel(getDBValue("level", data, dataMap, ""));
        setRiverMile(getDBValue("mile", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setPool(getDBValue("pool", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setPeriodOfRecord(getDBValue("por", data, dataMap, ""));
        setRated(getDBValue("rated", data, dataMap, ""));
        setLatitude(getDBValue("lat", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setLongitude(getDBValue("lon", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setRemark(getDBValue("remark", data, dataMap, ""));
        setReviseDate(getDBValue("rrevise", data, dataMap, (Date) null));
        setLatLonSource(getDBValue("rsource", data, dataMap, ""));
        setStream(getDBValue("stream", data, dataMap, ""));
        setTidalEffect(getDBValue("tide", data, dataMap, ""));
        setBackWater(getDBValue("backwater", data, dataMap, ""));
        setVerticalDatum(getDBValue("vdatum", data, dataMap, ""));
        setActionFlow(getDBValue("action_flow", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setActionStage(getDBValue("wstg", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setZeroDatum(getDBValue("zd", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setDateOfRating(getDBValue("ratedat", data, dataMap, (Date) null));
        setUsgsRateNumber(getDBValue("usgs_ratenum", data, dataMap, ""));
        setUnitHydrographDuration(getDBValue("uhgdur", data, dataMap,
                HydroConstants.MISSING_VALUE));
        setUseLatestForecast(getDBValue("use_latest_fcst", data, dataMap, ""));
    }

    private void initDateFormat() {
        dateFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
    }

    /**
     * @return the lid
     */
    public String getLid() {
        return lid;
    }

    /**
     * @param lid
     *            the lid to set
     */
    public void setLid(String lid) {
        this.lid = lid;
    }

    /**
     * @return the primaryPE
     */
    public String getPrimaryPE() {
        return primaryPE;
    }

    /**
     * @param primaryPE
     *            the primaryPE to set
     */
    public void setPrimaryPE(String primaryPE) {
        this.primaryPE = primaryPE;
    }

    /**
     * @return the bankFull
     */
    public double getBankFull() {
        return bankFull;
    }

    /**
     * @param bankFull
     *            the bankFull to set
     */
    public void setBankFull(double bankFull) {
        this.bankFull = bankFull;
    }

    /**
     * @return the checkBar
     */
    public double getCheckBar() {
        return checkBar;
    }

    /**
     * @param checkBar
     *            the checkBar to set
     */
    public void setCheckBar(double checkBar) {
        this.checkBar = checkBar;
    }

    /**
     * @return the drainageArea
     */
    public double getDrainageArea() {
        return drainageArea;
    }

    /**
     * @param drainageArea
     *            the drainageArea to set
     */
    public void setDrainageArea(double drainageArea) {
        this.drainageArea = drainageArea;
    }

    /**
     * @return the responseTime
     */
    public double getResponseTime() {
        return responseTime;
    }

    /**
     * @param responseTime
     *            the responseTime to set
     */
    public void setResponseTime(double responseTime) {
        this.responseTime = responseTime;
    }

    /**
     * @return the thresholdRunoff
     */
    public double getThresholdRunoff() {
        return thresholdRunoff;
    }

    /**
     * @param thresholdRunoff
     *            the thresholdRunoff to set
     */
    public void setThresholdRunoff(double thresholdRunoff) {
        this.thresholdRunoff = thresholdRunoff;
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
     * @return the gageNumber
     */
    public String getGageNumber() {
        return gageNumber;
    }

    /**
     * @param gageNumber
     *            the gageNumber to set
     */
    public void setGageNumber(String gageNumber) {
        this.gageNumber = gageNumber;
    }

    /**
     * @return the level
     */
    public String getLevel() {
        return level;
    }

    /**
     * @param level
     *            the level to set
     */
    public void setLevel(String level) {
        this.level = level;
    }

    /**
     * @return the riverMile
     */
    public double getRiverMile() {
        return riverMile;
    }

    /**
     * @param riverMile
     *            the riverMile to set
     */
    public void setRiverMile(double riverMile) {
        this.riverMile = riverMile;
    }

    /**
     * @return the pool
     */
    public double getPool() {
        return pool;
    }

    /**
     * @param pool
     *            the pool to set
     */
    public void setPool(double pool) {
        this.pool = pool;
    }

    /**
     * @return the periodOfRecord
     */
    public String getPeriodOfRecord() {
        return periodOfRecord;
    }

    /**
     * @param periodOfRecord
     *            the periodOfRecord to set
     */
    public void setPeriodOfRecord(String periodOfRecord) {
        this.periodOfRecord = periodOfRecord;
    }

    /**
     * @return the rated
     */
    public String getRated() {
        return rated;
    }

    /**
     * @param rated
     *            the rated to set
     */
    public void setRated(String rated) {
        this.rated = rated;
    }

    /**
     * @return the latitude
     */
    public double getLatitude() {
        return latitude;
    }

    /**
     * @param latitude
     *            the latitude to set
     */
    public void setLatitude(double latitude) {
        this.latitude = latitude;
    }

    /**
     * @return the longitude
     */
    public double getLongitude() {
        return longitude;
    }

    /**
     * @param longitude
     *            the longitude to set
     */
    public void setLongitude(double longitude) {
        this.longitude = longitude;
    }

    /**
     * @return the remark
     */
    public String getRemark() {
        return remark;
    }

    /**
     * @param remark
     *            the remark to set
     */
    public void setRemark(String remark) {
        this.remark = remark;
    }

    /**
     * @return the reviseDate
     */
    public Date getReviseDate() {
        return reviseDate;
    }

    /**
     * @param reviseDate
     *            the reviseDate to set
     */
    public void setReviseDate(Date reviseDate) {
        this.reviseDate = reviseDate;
    }

    /**
     * @return the latLonSource
     */
    public String getLatLonSource() {
        return latLonSource;
    }

    /**
     * @param latLonSource
     *            the latLonSource to set
     */
    public void setLatLonSource(String latLonSource) {
        this.latLonSource = latLonSource;
    }

    /**
     * @return the stream
     */
    public String getStream() {
        return stream;
    }

    /**
     * @param stream
     *            the stream to set
     */
    public void setStream(String stream) {
        this.stream = stream;
    }

    /**
     * @return the tidalEffect
     */
    public String getTidalEffect() {
        return tidalEffect;
    }

    /**
     * @param tidalEffect
     *            the tidalEffect to set
     */
    public void setTidalEffect(String tidalEffect) {
        this.tidalEffect = tidalEffect;
    }

    /**
     * @return the backWater
     */
    public String getBackWater() {
        return backWater;
    }

    /**
     * @param backWater
     *            the backWater to set
     */
    public void setBackWater(String backWater) {
        this.backWater = backWater;
    }

    /**
     * @return the verticalDatum
     */
    public String getVerticalDatum() {
        return verticalDatum;
    }

    /**
     * @param verticalDatum
     *            the verticalDatum to set
     */
    public void setVerticalDatum(String verticalDatum) {
        this.verticalDatum = verticalDatum;
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
     * @return the zeroDatum
     */
    public double getZeroDatum() {
        return zeroDatum;
    }

    /**
     * @param zeroDatum
     *            the zeroDatum to set
     */
    public void setZeroDatum(double zeroDatum) {
        this.zeroDatum = zeroDatum;
    }

    /**
     * @return the dateOfRating
     */
    public Date getDateOfRating() {
        return dateOfRating;
    }

    /**
     * @param dateOfRating
     *            the dateOfRating to set
     */
    public void setDateOfRating(Date dateOfRating) {
        this.dateOfRating = dateOfRating;
    }

    /**
     * @return the usgsRateNumber
     */
    public String getUsgsRateNumber() {
        return usgsRateNumber;
    }

    /**
     * @param usgsRateNumber
     *            the usgsRateNumber to set
     */
    public void setUsgsRateNumber(String usgsRateNumber) {
        this.usgsRateNumber = usgsRateNumber;
    }

    /**
     * @return the unitHydrographDuration
     */
    public int getUnitHydrographDuration() {
        return unitHydrographDuration;
    }

    /**
     * @param unitHydrographDuration
     *            the unitHydrographDuration to set
     */
    public void setUnitHydrographDuration(int unitHydrographDuration) {
        this.unitHydrographDuration = unitHydrographDuration;
    }

    /**
     * @return the useLatestForecast
     */
    public String getUseLatestForecast() {
        return useLatestForecast;
    }

    /**
     * @param useLatestForecast
     *            the useLatestForecast to set
     */
    public void setUseLatestForecast(String useLatestForecast) {
        this.useLatestForecast = useLatestForecast;
    }

    @Override
    public String getConstrainedSelectStatement() {
        String columns = "lid, primary_pe, bf, cb, da, response_time, threshold_runoff,"
                + " fq, fs, gsno, level, mile, pool, por, rated, lat, lon, remark, rrevise,"
                + " rsource, stream, tide, backwater, vdatum, action_flow, wstg, zd, ratedat,"
                + " usgs_ratenum, uhgdur, use_latest_fcst";
        String query = "SELECT " + columns + " FROM riverstat WHERE lid="
                + getDBString(lid);
        return query;
    }

    @Override
    public String getDeleteStatement() {
        return "DELETE FROM riverstat WHERE " + getPKStatement();
    }

    @Override
    public String getExistsStatement() {
        return "SELECT lid FROM riverstat WHERE " + getPKStatement();
    }

    @Override
    public String getInsertStatement() {
        String columns = "lid, primary_pe, bf, cb, da, response_time, threshold_runoff,"
                + " fq, fs, gsno, level, mile, pool, por, rated, lat, lon, remark, rrevise,"
                + " rsource, stream, tide, backwater, vdatum, action_flow, wstg, zd, ratedat,"
                + " usgs_ratenum, uhgdur, use_latest_fcst";

        String rval = "INSERT INTO riverstat ( " + columns
                + " ) VALUES ( %s, %s, %s, %s, %s, %s, %s,"
                + " %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, $$%s$$,%s,"
                + " %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s )";

        rval = String.format(rval, getDBString(lid), getDBString(primaryPE),
                getDBString(bankFull), getDBString(checkBar),
                getDBString(drainageArea), getDBString(responseTime),
                getDBString(thresholdRunoff), getDBString(floodFlow),
                getDBString(floodStage), getDBString(gageNumber),
                getDBString(level), getDBString(riverMile), getDBString(pool),
                getDBString(periodOfRecord), getDBString(rated),
                getDBString(latitude), getDBString(longitude),
                getDBStringNoQuote(remark), getDBString(reviseDate, dateFormat),
                getDBString(latLonSource), getDBString(stream),
                getDBString(tidalEffect), getDBString(backWater),
                getDBString(verticalDatum), getDBString(actionFlow),
                getDBString(actionStage), getDBString(zeroDatum), getDBString(
                        dateOfRating, dateFormat), getDBString(usgsRateNumber),
                getDBString(unitHydrographDuration),
                getDBString(useLatestForecast));

        return rval;
    }

    @Override
    public String getPKStatement() {
        String pk = "lid=%s";
        return String.format(pk, getDBString(lid));
    }

    @Override
    public String getSelectStatement() {
        String columns = "lid, primary_pe, bf, cb, da, response_time, threshold_runoff,"
                + " fq, fs, gsno, level, mile, pool, por, rated, lat, lon, remark, rrevise,"
                + " rsource, stream, tide, backwater, vdatum, action_flow, wstg, zd, ratedat,"
                + " usgs_ratenum, uhgdur, use_latest_fcst";

        String query = "SELECT " + columns + " FROM riverstat";
        return query;
    }

    @Override
    public String getUpdateStatement() {
        // Set the basic update statement
        String rval = "UPDATE riverstat SET lid=%s, primary_pe=%s, bf=%s, cb=%s, da=%s, response_time=%s, threshold_runoff=%s,"
                + " fq=%s, fs=%s, gsno=%s, level=%s, mile=%s, pool=%s, por=%s, rated=%s, lat=%s, lon=%s, remark=$$%s$$, rrevise=%s,"
                + " rsource=%s, stream=%s, tide=%s, backwater=%s, vdatum=%s, action_flow=%s, wstg=%s, zd=%s, ratedat=%s,"
                + " usgs_ratenum=%s, uhgdur=%s, use_latest_fcst=%s WHERE %s";

        // Populate the values
        rval = String.format(rval, getDBString(lid), getDBString(primaryPE),
                getDBString(bankFull), getDBString(checkBar),
                getDBString(drainageArea), getDBString(responseTime),
                getDBString(thresholdRunoff), getDBString(floodFlow),
                getDBString(floodStage), getDBString(gageNumber),
                getDBString(level), getDBString(riverMile), getDBString(pool),
                getDBString(periodOfRecord), getDBString(rated),
                getDBString(latitude), getDBString(longitude),
                getDBStringNoQuote(remark), getDBString(reviseDate, dateFormat),
                getDBString(latLonSource), getDBString(stream),
                getDBString(tidalEffect), getDBString(backWater),
                getDBString(verticalDatum), getDBString(actionFlow),
                getDBString(actionStage), getDBString(zeroDatum), getDBString(
                        dateOfRating, dateFormat), getDBString(usgsRateNumber),
                getDBString(unitHydrographDuration),
                getDBString(useLatestForecast), getPKStatement());

        return rval;
    }
}
