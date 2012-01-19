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
 * this class contains the Administration data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Nov 11, 2008	1697		askripsky	Initial creation
 * Nov 21, 2008             askripsky   Changed to implement IHydroDBData to work with new DB access.
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */
public class AdministrationData extends HydroDBData implements IHydroDBData {

    /**
     * Name or focalpoint
     */
    private String name;

    /**
     * Station or ofc.
     */
    private String stationName;

    /**
     * phone number.
     */
    private String phoneNumber;

    /**
     * Region.
     */
    private String region;

    /**
     * Region Number.
     */
    private String regionNumber;

    /**
     * CD-404.
     */
    private String cd404;

    /**
     * Ten year date or tenyr.
     */
    private Date tenYearDate;

    /**
     * One Year Date or oneyr.
     */
    private Date oneYearDate;

    /**
     * Station ID or HSA.
     */
    private String hsa;

    /**
     * HSA Number.
     */
    private int hsaNumber;

    /**
     * HydroBase password.
     */
    private String hbPassword;

    /**
     * Formats the date for the DB
     */
    private SimpleDateFormat dateFormat;

    /**
     * Constructor
     */
    public AdministrationData() {
        initDateFormat();
    }

    /**
     * Constructor that accepts the object array from a db call
     * 
     * @param data
     *            The raw data from the database.
     */
    public AdministrationData(QueryResultRow data, Map<String, Integer> dataMap) {
        initDateFormat();

        setName(getDBValue("focalpoint", data, dataMap, ""));
        setStationName(getDBValue("ofc", data, dataMap, ""));
        setPhoneNumber(getDBValue("phone", data, dataMap, ""));
        setRegion(getDBValue("region", data, dataMap, ""));
        setRegionNumber(getDBValue("regno", data, dataMap, ""));
        setCd404(getDBValue("cd404", data, dataMap, ""));
        setTenYearDate(getDBValue("tenyr", data, dataMap, (Date) null));
        setOneYearDate(getDBValue("oneyr", data, dataMap, (Date) null));
        setHsa(getDBValue("hsa", data, dataMap, ""));
        setHsaNumber(getDBValue("hsa_num", data, dataMap,
                HydroConstants.MISSING_VALUE));
        setHbPassword(getDBValue("hb_password", data, dataMap, ""));
    }

    private void initDateFormat() {
        dateFormat = new SimpleDateFormat("MM/dd/yyyy");
        dateFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getStationName() {
        return stationName;
    }

    public void setStationName(String stationName) {
        this.stationName = stationName;
    }

    public String getPhoneNumber() {
        return phoneNumber;
    }

    public void setPhoneNumber(String phoneNumber) {
        this.phoneNumber = phoneNumber;
    }

    public String getRegion() {
        return region;
    }

    public void setRegion(String region) {
        this.region = region;
    }

    public String getRegionNumber() {
        return regionNumber;
    }

    public void setRegionNumber(String regionNumber) {
        this.regionNumber = regionNumber;
    }

    public String getCd404() {
        return cd404;
    }

    public void setCd404(String cd404) {
        this.cd404 = cd404;
    }

    public Date getTenYearDate() {
        return tenYearDate;
    }

    public String getTenYearDateDBString() {
        return (tenYearDate != null) ? "'" + dateFormat.format(tenYearDate)
                + "'" : "null";
    }

    public void setTenYearDate(Date tenYearDate) {
        this.tenYearDate = tenYearDate;
    }

    public Date getOneYearDate() {
        return oneYearDate;
    }

    public String getOneYearDateDBString() {
        return (oneYearDate != null) ? "'" + dateFormat.format(oneYearDate)
                + "'" : "null";
    }

    public void setOneYearDate(Date oneYearDate) {
        this.oneYearDate = oneYearDate;
    }

    public String getHsa() {
        return hsa;
    }

    public void setHsa(String hsa) {
        this.hsa = hsa;
    }

    public int getHsaNumber() {
        return hsaNumber;
    }

    public String getHsaNumberDBString() {
        return (hsaNumber != HydroConstants.MISSING_VALUE) ? Integer
                .toString(hsaNumber) : "null";
    }

    public void setHsaNumber(int hsaNumber) {
        this.hsaNumber = hsaNumber;
    }

    public String getHbPassword() {
        return hbPassword;
    }

    public void setHbPassword(String hbPassword) {
        this.hbPassword = hbPassword;
    }

    @Override
    public String getInsertStatement() {
        String rval = "INSERT INTO admin ( focalpoint, ofc, phone, region, regno, "
                + "cd404, tenyr, oneyr, hsa, hsa_num, hb_password ) VALUES ('%s', '%s', "
                + "'%s', '%s', '%s', '%s', %s, %s, '%s', %s, '%s')";

        rval = String.format(rval, getName(), getStationName(),
                getPhoneNumber(), getRegion(), getRegionNumber(), getCd404(),
                getTenYearDateDBString(), getOneYearDateDBString(), getHsa(),
                getHsaNumberDBString(), getHbPassword());

        return rval;
    }

    @Override
    public String getUpdateStatement() {
        // Set the basic update statement
        String rval = "UPDATE admin SET focalpoint='%s', ofc='%s', phone='%s', region='%s', regno='%s', cd404='%s', tenyr=%s, oneyr=%s,  hsa='%s', hsa_num=%s, hb_password='%s' WHERE %s";

        // Populate the values
        rval = String.format(rval, getName(), getStationName(),
                getPhoneNumber(), getRegion(), getRegionNumber(), getCd404(),
                getTenYearDateDBString(), getOneYearDateDBString(), getHsa(),
                getHsaNumberDBString(), getHbPassword(), getPKStatement());

        return rval;
    }

    @Override
    public String getSelectStatement() {
        return "SELECT focalpoint, ofc, phone, region, regno, cd404, tenyr, oneyr, hsa, hsa_num, hb_password FROM admin";
    }

    @Override
    public String getDeleteStatement() {
        return String.format("DELETE FROM admin WHERE %s", getPKStatement());
    }

    @Override
    public String getPKStatement() {
        // There is only one record in Admin table
        return "hsa like '%'";
    }

    @Override
    public String getExistsStatement() {
        String selectQuery = getSelectStatement() + " WHERE "
                + getPKStatement();

        return selectQuery;
    }

    @Override
    public String getConstrainedSelectStatement() {
        return getSelectStatement();
    }
}
