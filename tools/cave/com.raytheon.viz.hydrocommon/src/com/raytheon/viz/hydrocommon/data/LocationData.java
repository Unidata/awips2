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
 * this class contains the Location data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Nov 19, 2008	1697    	askripsky	Initial creation
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */
public class LocationData extends HydroDBData implements IHydroDBData {

    /**
     * lid - Location/Station ID
     */
    private String lid;

    /**
     * County
     */
    private String county;

    /**
     * coe
     */
    private String coe;

    /**
     * coe
     */
    private String cpm;

    /**
     * Detail contains the mileage and direction
     */
    private String detail;

    /**
     * elev - Elevation
     */
    private double elevation;

    /**
     * hdatum - Horizontal Datum
     */
    private String horizontalDatum;

    /**
     * HSA
     */
    private String hsa;

    /**
     * hu - Hydrologic Unit
     */
    private String hu;

    /**
     * lat - Latitude
     */
    private double latitude;

    /**
     * lon - Longitude
     */
    private double longitude;

    /**
     * lremark - Remark
     */
    private String remark;

    /**
     * lrevise - Revision Date
     */
    private Date reviseDate;

    /**
     * name - Name of the location/station
     */
    private String name;

    /**
     * network
     */
    private String network;

    /**
     * rb - River Basin
     */
    private String riverBasin;

    /**
     * RFC
     */
    private String rfc;

    /**
     * sbd - Station Begin Date
     */
    private Date beginDate;

    /**
     * sn - Station number
     */
    private String stationNumber;

    /**
     * State
     */
    private String state;

    /**
     * waro
     */
    private String waro;

    /**
     * WFO
     */
    private String wfo;

    /**
     * wsfo -
     */
    private String wsfo;

    /**
     * type - If "I", then inactive
     */
    private String type;

    /**
     * des - Description
     */
    private String description;

    /**
     * det - Information
     */
    private String information;

    /**
     * post - Post Observed Values, either 0 or 1
     */
    private int post;

    /**
     * stntype - Station Type
     */
    private String stationType;

    /**
     * tzone - Time Zone
     */
    private String timeZone;

    /**
     * Formats the date for the DB
     */
    private SimpleDateFormat dateFormat;

    /**
     * Constructor
     */
    public LocationData() {
        initDateFormat();

        setBeginDate((Date) null);
        setCoe("");
        setCounty("");
        setCpm("");
        setDescription("");
        setDetail("");
        setElevation(Double.valueOf(HydroConstants.MISSING_VALUE));
        setHorizontalDatum("");
        setHsa("");
        setHu("");
        setInformation("");

        setLatitude(Double.valueOf(HydroConstants.MISSING_VALUE));
        setLid("");
        setLongitude(Double.valueOf(HydroConstants.MISSING_VALUE));
        setName("");
        setNetwork("");
        setPost(HydroConstants.MISSING_VALUE);
        setRemark("");
        setReviseDate((Date) null);
        setRfc("");
        setRiverBasin("");
        setState("");
        setStationNumber("");
        setStationType("");
        setTimeZone("");
        setType("");
        setWaro("");
        setWfo("");
        setWsfo("");
    }

    /**
     * Constructor that accepts the object array from a db call
     * 
     * @param data
     *            The raw data from the database.
     */
    public LocationData(QueryResultRow data, Map<String, Integer> dataMap) {
        initDateFormat();

        setBeginDate(getDBValue("sbd", data, dataMap, (Date) null));
        setCoe(getDBValue("coe", data, dataMap, ""));
        setCounty(getDBValue("county", data, dataMap, ""));
        setCpm(getDBValue("cpm", data, dataMap, ""));
        setDescription(getDBValue("des", data, dataMap, ""));
        setDetail(getDBValue("detail", data, dataMap, ""));
        setElevation(getDBValue("elev", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setHorizontalDatum(getDBValue("hdatum", data, dataMap, ""));
        setHsa(getDBValue("hsa", data, dataMap, ""));
        setHu(getDBValue("hu", data, dataMap, ""));
        setInformation(getDBValue("det", data, dataMap, ""));

        setLatitude(getDBValue("lat", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setLid(getDBValue("lid", data, dataMap, ""));
        setLongitude(getDBValue("lon", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setName(getDBValue("name", data, dataMap, ""));
        setNetwork(getDBValue("network", data, dataMap, ""));
        setPost(getDBValue("post", data, dataMap, HydroConstants.MISSING_VALUE));
        setRemark(getDBValue("lremark", data, dataMap, ""));
        setReviseDate(getDBValue("lrevise", data, dataMap, (Date) null));
        setRfc(getDBValue("rfc", data, dataMap, ""));
        setRiverBasin(getDBValue("rb", data, dataMap, ""));
        setState(getDBValue("state", data, dataMap, ""));
        setStationNumber(getDBValue("sn", data, dataMap, ""));
        setStationType(getDBValue("stntype", data, dataMap, ""));
        setTimeZone(getDBValue("tzone", data, dataMap, ""));
        setType(getDBValue("type", data, dataMap, ""));
        setWaro(getDBValue("waro", data, dataMap, ""));
        setWfo(getDBValue("wfo", data, dataMap, ""));
        setWsfo(getDBValue("wsfo", data, dataMap, ""));
    }

    private void initDateFormat() {
        dateFormat = new SimpleDateFormat("MM/dd/yyyy");
        dateFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
    }

    public String getLid() {
        return lid;
    }

    public void setLid(String lid) {
        this.lid = lid;
    }

    public String getCounty() {
        return county;
    }

    public void setCounty(String county) {
        this.county = county;
    }

    public String getCoe() {
        return coe;
    }

    public void setCoe(String coe) {
        this.coe = coe;
    }

    public String getCpm() {
        return cpm;
    }

    public void setCpm(String cpm) {
        this.cpm = cpm;
    }

    public String getDetail() {
        return detail;
    }

    public void setDetail(String detail) {
        this.detail = detail;
    }

    public double getElevation() {
        return elevation;
    }

    public void setElevation(double elevation) {
        this.elevation = elevation;
    }

    public String getHorizontalDatum() {
        return horizontalDatum;
    }

    public void setHorizontalDatum(String horizontalDatum) {
        this.horizontalDatum = horizontalDatum;
    }

    public String getHsa() {
        return hsa;
    }

    public void setHsa(String hsa) {
        this.hsa = hsa;
    }

    public String getHu() {
        return hu;
    }

    public void setHu(String hu) {
        this.hu = hu;
    }

    /**
     * Returns "null" if the value is set to the missing_value constant, else
     * returns the value
     * 
     * @return
     */
    public double getLatitude() {
        return latitude;
    }

    public void setLatitude(double latitude) {
        this.latitude = latitude;
    }

    public double getLongitude() {
        return longitude;
    }

    public void setLongitude(double longitude) {
        this.longitude = longitude;
    }

    public String getRemark() {
        return remark;
    }

    public void setRemark(String remark) {
        this.remark = remark;
    }

    public Date getReviseDate() {
        return reviseDate;
    }

    public String getReviseDateDBString() {
        return (reviseDate != null) ? "'" + dateFormat.format(reviseDate) + "'"
                : "null";
    }

    public void setReviseDate(Date reviseDate) {
        this.reviseDate = reviseDate;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getNetwork() {
        return network;
    }

    public void setNetwork(String network) {
        this.network = network;
    }

    public String getRiverBasin() {
        return riverBasin;
    }

    public void setRiverBasin(String riverBasin) {
        this.riverBasin = riverBasin;
    }

    public String getRfc() {
        return rfc;
    }

    public void setRfc(String rfc) {
        this.rfc = rfc;
    }

    public Date getBeginDate() {
        return beginDate;
    }

    public String getBeginDateDBString() {
        return (beginDate != null) ? "'" + dateFormat.format(beginDate) + "'"
                : "null";
    }

    public void setBeginDate(Date beginDate) {
        this.beginDate = beginDate;
    }

    public String getStationNumber() {
        return stationNumber;
    }

    public void setStationNumber(String stationNumber) {
        this.stationNumber = stationNumber;
    }

    public String getState() {
        return state;
    }

    public void setState(String state) {
        this.state = state;
    }

    public String getWaro() {
        return waro;
    }

    public void setWaro(String waro) {
        this.waro = waro;
    }

    public String getWfo() {
        return wfo;
    }

    public void setWfo(String wfo) {
        this.wfo = wfo;
    }

    public String getWsfo() {
        return wsfo;
    }

    public void setWsfo(String wsfo) {
        this.wsfo = wsfo;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getInformation() {
        return information;
    }

    public void setInformation(String information) {
        this.information = information;
    }

    public int getPost() {
        return post;
    }

    public void setPost(int post) {
        this.post = post;
    }

    public String getStationType() {
        return stationType;
    }

    public void setStationType(String stationType) {
        this.stationType = stationType;
    }

    public String getTimeZone() {
        return timeZone;
    }

    public void setTimeZone(String timeZone) {
        this.timeZone = timeZone;
    }

    public SimpleDateFormat getDateFormat() {
        return dateFormat;
    }

    public void setDateFormat(SimpleDateFormat dateFormat) {
        this.dateFormat = dateFormat;
    }

    @Override
    public String getConstrainedSelectStatement() {
        return getSelectStatement() + " WHERE lid='" + lid + "'";
    }

    @Override
    public String getDeleteStatement() {
        return "";
    }

    @Override
    public String getExistsStatement() {
        return getSelectStatement() + " WHERE " + getPKStatement();
    }

    @Override
    public String getInsertStatement() {
        String rval = "INSERT INTO location ( lid, county, coe, cpm, detail, elev, hdatum, hsa, hu, lat, "
                + "lon, lremark, lrevise, name, network, rb, rfc, sbd, sn, state, waro, wfo, wsfo, type, des, "
                + "det, post, stntype, tzone )"
                + " VALUES ( '%s', '%s', '%s', '%s', '%s', %s, '%s', '%s', '%s', %s, "
                + "%s, '%s', %s, '%s', '%s', '%s', '%s', %s, '%s', '%s', '%s', '%s', '%s', '%s', '%s', "
                + "'%s', %s, '%s', '%s')";

        rval = String.format(rval, lid, county, coe, cpm, detail,
                getDBString(elevation), horizontalDatum, hsa, hu,
                getDBString(latitude), getDBString(longitude), remark,
                getReviseDateDBString(), name, network, riverBasin, rfc,
                getBeginDateDBString(), stationNumber, state, waro, wfo, wsfo,
                type, description, information, getDBString(post), stationType,
                timeZone);

        return rval;
    }

    @Override
    public String getPKStatement() {
        return "lid='" + getLid() + "'";
    }

    @Override
    public String getSelectStatement() {
        StringBuffer rval = new StringBuffer();

        rval
                .append("SELECT lid, county, coe, cpm, detail, elev, hdatum, hsa, hu, lat, ");
        rval.append("lon, lremark, lrevise, name, ");
        rval
                .append("network, rb, rfc, sbd, sn, state, waro, wfo, wsfo, type, des, ");
        rval.append("det, post, stntype, tzone FROM location");

        return rval.toString();
    }

    @Override
    public String getUpdateStatement() {
        // Set the basic update statement
        String rval = "UPDATE location SET county='%s', coe='%s', cpm='%s', detail='%s', "
                + "elev=%s, hdatum='%s', hsa='%s', hu='%s', lat=%s, lon=%s, lremark='%s', lrevise=%s, "
                + "name='%s', network='%s', rb='%s', rfc='%s', sbd=%s, sn='%s', state='%s', waro='%s', "
                + "wfo='%s', wsfo='%s', type='%s', des='%s', det='%s', post=%s, stntype='%s', tzone='%s' WHERE %s";

        // Populate the values
        rval = String.format(rval, county, coe, cpm, detail,
                getDBString(elevation), horizontalDatum, hsa, hu,
                getDBString(latitude), getDBString(longitude), remark,
                getReviseDateDBString(), name, network, riverBasin, rfc,
                getBeginDateDBString(), stationNumber, state, waro, wfo, wsfo,
                type, description, information, getDBString(post), stationType,
                timeZone, getPKStatement());

        return rval;
    }
}
