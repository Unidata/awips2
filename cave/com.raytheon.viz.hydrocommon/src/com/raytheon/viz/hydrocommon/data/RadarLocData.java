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
 * this class contains the Radar Location data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jan 9, 2008	1802    	askripsky	Initial creation
 * Aug 07, 2015 4500        rjpeter     Fix type case.
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */
public class RadarLocData extends HydroDBData implements IHydroDBData {

    /**
     * radid character varying(3) NOT NULL,
     */
    private String radarID;

    /**
     * "name" character varying(20),
     */
    private String name;

    /**
     * radid_prefix character varying(1),
     */
    private String radarPrefix;

    /**
     * radar_num smallint,
     */
    private int radarNumber;

    /**
     * state character varying(2),
     */
    private String state;

    /**
     * lat double precision,
     */
    private double latitude;

    /**
     * lon double precision,
     */
    private double longitude;

    /**
     * elev double precision,
     */
    private double elevation;

    /**
     * tower_ht double precision,
     */
    private double towerHeight;

    /**
     * use_radar character varying(1),
     */
    private String useRadar;

    /**
     * office_id character varying(5),
     */
    private String officeID;

    /**
     * Database table name
     */
    private String dbTable = "radarloc";

    /**
     * Constructor
     */
    public RadarLocData() {
    }

    /**
     * Constructor that accepts the object array from a db call
     * 
     * @param data
     *            The raw data from the database.
     */
    public RadarLocData(QueryResultRow data, Map<String, Integer> dataMap) {
        setRadarID(getDBValue("radid", data, dataMap, ""));
        setName(getDBValue("name", data, dataMap, ""));
        setRadarPrefix(getDBValue("radid_prefix", data, dataMap, ""));
        setRadarNumber(getDBValue("radar_num", data, dataMap,
                (short) HydroConstants.MISSING_VALUE).intValue());
        setState(getDBValue("state", data, dataMap, ""));
        setLatitude(getDBValue("lat", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setLongitude(getDBValue("lon", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setElevation(getDBValue("elev", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setTowerHeight(getDBValue("tower_ht", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setUseRadar(getDBValue("use_radar", data, dataMap, ""));
        setOfficeID(getDBValue("office_id", data, dataMap, ""));
    }

    /**
     * @return the radarID
     */
    public String getRadarID() {
        return radarID;
    }

    /**
     * @param radarID
     *            the radarID to set
     */
    public void setRadarID(String radarID) {
        this.radarID = radarID;
    }

    /**
     * @return the name
     */
    public String getName() {
        return name;
    }

    /**
     * @param name
     *            the name to set
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * @return the radarPrefix
     */
    public String getRadarPrefix() {
        return radarPrefix;
    }

    /**
     * @param radarPrefix
     *            the radarPrefix to set
     */
    public void setRadarPrefix(String radarPrefix) {
        this.radarPrefix = radarPrefix;
    }

    /**
     * @return the radarNumber
     */
    public int getRadarNumber() {
        return radarNumber;
    }

    /**
     * @param radarNumber
     *            the radarNumber to set
     */
    public void setRadarNumber(int radarNumber) {
        this.radarNumber = radarNumber;
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
     * @return the elevation
     */
    public double getElevation() {
        return elevation;
    }

    /**
     * @param elevation
     *            the elevation to set
     */
    public void setElevation(double elevation) {
        this.elevation = elevation;
    }

    /**
     * @return the towerHeight
     */
    public double getTowerHeight() {
        return towerHeight;
    }

    /**
     * @param towerHeight
     *            the towerHeight to set
     */
    public void setTowerHeight(double towerHeight) {
        this.towerHeight = towerHeight;
    }

    /**
     * @return the useRadar
     */
    public String getUseRadar() {
        return useRadar;
    }

    /**
     * @param useRadar
     *            the useRadar to set
     */
    public void setUseRadar(String useRadar) {
        this.useRadar = useRadar;
    }

    /**
     * @return the officeID
     */
    public String getOfficeID() {
        return officeID;
    }

    /**
     * @param officeID
     *            the officeID to set
     */
    public void setOfficeID(String officeID) {
        this.officeID = officeID;
    }

    @Override
    public String getConstrainedSelectStatement() {
        return getSelectStatement();
    }

    @Override
    public String getDeleteStatement() {
        return "DELETE FROM " + dbTable + " WHERE " + getPKStatement();
    }

    @Override
    public String getExistsStatement() {
        return "SELECT radid FROM " + dbTable + " WHERE " + getPKStatement();
    }

    @Override
    public String getInsertStatement() {
        String columns = "radid, name, radid_prefix, radar_num, state, lat, lon, elev, tower_ht, use_radar, office_id";

        String rval = "INSERT INTO " + dbTable + " ( " + columns
                + " ) VALUES ( %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s )";

        rval = String.format(rval, getDBString(radarID), getDBString(name),
                getDBString(radarPrefix), getDBString(radarNumber),
                getDBString(state), getDBString(latitude),
                getDBString(longitude), getDBString(elevation),
                getDBString(towerHeight), getDBString(useRadar),
                getDBString(officeID));

        return rval;
    }

    @Override
    public String getPKStatement() {
        return "radid=" + getDBString(radarID);
    }

    @Override
    public String getSelectStatement() {
        StringBuffer rval = new StringBuffer();

        String columns = "radid, name, radid_prefix, radar_num, state, lat, lon, elev, tower_ht, use_radar, office_id";

        rval.append("SELECT ");
        rval.append(columns);
        rval.append(" FROM ");
        rval.append(dbTable);
        rval.append(" WHERE radid != 'ZZZ' ORDER BY radid");

        return rval.toString();
    }

    @Override
    public String getUpdateStatement() {
        // Set the basic update statement
        String rval = "UPDATE "
                + dbTable
                + " SET radid=%s, name=%s, radid_prefix=%s, radar_num=%s, state=%s, lat=%s, lon=%s, elev=%s, tower_ht=%s, use_radar=%s, office_id=%s WHERE %s";

        // Populate the values
        rval = String.format(rval, getDBString(radarID), getDBString(name),
                getDBString(radarPrefix), getDBString(radarNumber),
                getDBString(state), getDBString(latitude),
                getDBString(longitude), getDBString(elevation),
                getDBString(towerHeight), getDBString(useRadar),
                getDBString(officeID), getPKStatement());

        return rval;
    }
}
