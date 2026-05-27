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

import java.util.Map;

import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.viz.hydrocommon.HydroConstants;

/**
 * this class contains the NWR Transmitter data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jan 9, 2008	1802    	askripsky	Initial creation
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */
public class NWRTransmitterData extends HydroDBData implements IHydroDBData {

    /**
     * call_sign character varying(6) NOT NULL,
     */
    private String callSign;

    /**
     * wfo character varying(3),
     */
    private String wfo;

    /**
     * city character varying(20),
     */
    private String city;

    /**
     * county character varying(20),
     */
    private String county;

    /**
     * state character varying(2),
     */
    private String state;

    /**
     * coverage_area character varying(25),
     */
    private String coverageArea;

    /*
     * lat double precision,
     */
    private double latitude;

    /**
     * lon double precision,
     */
    private double longitude;

    /**
     * transmit_freq double precision,
     */
    private double transmitFrequency;

    /**
     * transmit_power integer,
     */
    private int transmitPower;

    /**
     * transmit_prod_code character varying(3),
     */
    private String transmitProductCode;

    /**
     * transmit_countynum character varying(4),
     */
    private String transmitCountyNumber;

    /**
     * use_transmitter character varying(1),
     */
    private String useTransmitter;

    /**
     * Constructor
     */
    public NWRTransmitterData() {
    }

    /**
     * Constructor that accepts the object array from a db call
     * 
     * @param data
     *            The raw data from the database.
     */
    public NWRTransmitterData(QueryResultRow data, Map<String, Integer> dataMap) {
        setCallSign(getDBValue("call_sign", data, dataMap, ""));
        setWfo(getDBValue("wfo", data, dataMap, ""));
        setCity(getDBValue("city", data, dataMap, ""));
        setCounty(getDBValue("county", data, dataMap, ""));
        setState(getDBValue("state", data, dataMap, ""));
        setCoverageArea(getDBValue("coverage_area", data, dataMap, ""));
        setLatitude(getDBValue("lat", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setLongitude(getDBValue("lon", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setTransmitFrequency(getDBValue("transmit_freq", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setTransmitPower(getDBValue("transmit_power", data, dataMap,
                HydroConstants.MISSING_VALUE));
        setTransmitProductCode(getDBValue("transmit_prod_code", data, dataMap,
                ""));
        setTransmitCountyNumber(getDBValue("transmit_countynum", data, dataMap,
                ""));
        setUseTransmitter(getDBValue("use_transmitter", data, dataMap, ""));
    }

    /**
     * @return the callSign
     */
    public String getCallSign() {
        return callSign;
    }

    /**
     * @param callSign
     *            the callSign to set
     */
    public void setCallSign(String callSign) {
        this.callSign = callSign;
    }

    /**
     * @return the wfo
     */
    public String getWfo() {
        return wfo;
    }

    /**
     * @param wfo
     *            the wfo to set
     */
    public void setWfo(String wfo) {
        this.wfo = wfo;
    }

    /**
     * @return the city
     */
    public String getCity() {
        return city;
    }

    /**
     * @param city
     *            the city to set
     */
    public void setCity(String city) {
        this.city = city;
    }

    /**
     * @return the county
     */
    public String getCounty() {
        return county;
    }

    /**
     * @param county
     *            the county to set
     */
    public void setCounty(String county) {
        this.county = county;
    }

    /**
     * @return the state
     */
    public String getState() {
        return state;
    }

    /**
     * @param state
     *            the state to set
     */
    public void setState(String state) {
        this.state = state;
    }

    /**
     * @return the coverageArea
     */
    public String getCoverageArea() {
        return coverageArea;
    }

    /**
     * @param coverageArea
     *            the coverageArea to set
     */
    public void setCoverageArea(String coverageArea) {
        this.coverageArea = coverageArea;
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
     * @return the transmitFrequency
     */
    public double getTransmitFrequency() {
        return transmitFrequency;
    }

    /**
     * @param transmitFrequency
     *            the transmitFrequency to set
     */
    public void setTransmitFrequency(double transmitFrequency) {
        this.transmitFrequency = transmitFrequency;
    }

    /**
     * @return the transmitPower
     */
    public int getTransmitPower() {
        return transmitPower;
    }

    /**
     * @param transmitPower
     *            the transmitPower to set
     */
    public void setTransmitPower(int transmitPower) {
        this.transmitPower = transmitPower;
    }

    /**
     * @return the transmitProductCode
     */
    public String getTransmitProductCode() {
        return transmitProductCode;
    }

    /**
     * @param transmitProductCode
     *            the transmitProductCode to set
     */
    public void setTransmitProductCode(String transmitProductCode) {
        this.transmitProductCode = transmitProductCode;
    }

    /**
     * @return the transmitCountyNumber
     */
    public String getTransmitCountyNumber() {
        return transmitCountyNumber;
    }

    /**
     * @param transmitCountyNumber
     *            the transmitCountyNumber to set
     */
    public void setTransmitCountyNumber(String transmitCountyNumber) {
        this.transmitCountyNumber = transmitCountyNumber;
    }

    /**
     * @return the useTransmitter
     */
    public String getUseTransmitter() {
        return useTransmitter;
    }

    /**
     * @param useTransmitter
     *            the useTransmitter to set
     */
    public void setUseTransmitter(String useTransmitter) {
        this.useTransmitter = useTransmitter;
    }

    @Override
    public String getConstrainedSelectStatement() {
        return getSelectStatement();
    }

    @Override
    public String getDeleteStatement() {
        return "DELETE FROM nwrtransmitter WHERE " + getPKStatement();
    }

    @Override
    public String getExistsStatement() {
        return "SELECT call_sign FROM nwrtransmitter WHERE " + getPKStatement();
    }

    @Override
    public String getInsertStatement() {
        String columns = "call_sign, wfo, city, county, state, coverage_area, lat, lon, "
                + "transmit_freq, transmit_power, transmit_prod_code, transmit_countynum, use_transmitter";

        String rval = "INSERT INTO nwrtransmitter ( "
                + columns
                + " ) VALUES ( %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s )";

        rval = String.format(rval, getDBString(callSign), getDBString(wfo),
                getDBString(city), getDBString(county), getDBString(state),
                getDBString(coverageArea), getDBString(latitude),
                getDBString(longitude), getDBString(transmitFrequency),
                getDBString(transmitPower), getDBString(transmitProductCode),
                getDBString(transmitCountyNumber), getDBString(useTransmitter));

        return rval;
    }

    @Override
    public String getPKStatement() {
        return "call_sign=" + getDBString(callSign);
    }

    @Override
    public String getSelectStatement() {
        StringBuffer rval = new StringBuffer();

        String columns = "call_sign, wfo, city, county, state, coverage_area, lat, lon, "
                + "transmit_freq, transmit_power, transmit_prod_code, transmit_countynum, use_transmitter";

        rval.append("SELECT ");
        rval.append(columns);
        rval.append(" FROM nwrtransmitter");
        rval.append(" ORDER BY call_sign");

        return rval.toString();
    }

    @Override
    public String getUpdateStatement() {
        // Set the basic update statement
        String rval = "UPDATE nwrtransmitter SET call_sign=%s, wfo=%s, city=%s, county=%s, state=%s, coverage_area=%s, lat=%s, lon=%s, "
                + "transmit_freq=%s, transmit_power=%s, transmit_prod_code=%s, transmit_countynum=%s, use_transmitter=%s WHERE %s";

        // Populate the values
        rval = String.format(rval, getDBString(callSign), getDBString(wfo),
                getDBString(city), getDBString(county), getDBString(state),
                getDBString(coverageArea), getDBString(latitude),
                getDBString(longitude), getDBString(transmitFrequency),
                getDBString(transmitPower), getDBString(transmitProductCode),
                getDBString(transmitCountyNumber), getDBString(useTransmitter),
                getPKStatement());

        return rval;
    }
}
