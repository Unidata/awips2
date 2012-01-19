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
package com.raytheon.viz.hydrocommon.datamanager;

import java.util.ArrayList;
import java.util.Date;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.data.AgencyOfficeData;
import com.raytheon.viz.hydrocommon.data.LocationAgencyOfficeData;
import com.raytheon.viz.hydrocommon.data.LocationData;

/**
 * Class for managing database query calls. AddModifyLocationDataManager.java
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 19, 2008 1697       askripsky   Initial Creation
 * Sep 09, 2009 2769       mpduff      Added copyTableData method and the calls
 *                                     to it for copying data from one table to another.
 * Oct 20, 2011 11266	   lbousaidi   added getHSAsForFilter() method to query from 
 * 									   location table instead of hsa table. 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */

public class AddModifyLocationDataManager extends HydroDataManager {
    protected static AddModifyLocationDataManager manager = null;
    
    private static final String[] rval = { "AT", "N", "NNE", "NE", "ENE", "E", "ESE", "SE",
        "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW" };

    /**
     * Private constructor.
     */
    private AddModifyLocationDataManager() {
    }

    /**
     * Singleton pattern of data manager.
     * 
     * @return manager
     */
    public static synchronized AddModifyLocationDataManager getInstance() {
        if (manager == null) {
            manager = new AddModifyLocationDataManager();
        }

        return manager;
    }

    /**
     * Deletes each record passed in.
     * 
     * @param dataToDelete
     * @throws VizException
     */
    public void deleteRecord(LocationData dataToDelete) throws VizException {
        String deleteFunction = String.format("delete_location('%s')",
                dataToDelete.getLid());

        HydroDBDataManager.getInstance().execFunction(deleteFunction);
    }

    public LocationData getLocationData(String lid) throws VizException {
        LocationData locData = new LocationData();
        locData.setLid(lid);

        ArrayList<LocationData> data = HydroDBDataManager.getInstance()
                .getData(locData);

        if ((data != null) && (data.size() > 0)) {
            locData = data.get(0);
        } else {
            locData = null;
        }

        return locData;
    }

    /**
     * Returns the directions used in the Details of a Location
     * 
     * @return the possible directions for a location
     */
    public String[] getDirections() {
        return rval;
    }

    /**
     * Retrieves the networks from the DB
     * 
     * @return The networks from the DB
     * @throws VizException
     */
    public ArrayList<String> getNetworks() throws VizException {
        ArrayList<String> rval = new ArrayList<String>();

        String query = "Select network from network order by network";

        QueryResult data = HydroDBDataManager.getInstance().runMappedQuery(
                query);

        if (data != null) {
            for (QueryResultRow currNet : data.getRows()) {
                rval.add((String) currNet.getColumn(data.getColumnNames().get(
                        "network")));
            }
        }

        return rval;
    }

    /**
     * Retrieves the rfcs from the DB
     * 
     * @return The rfcs from the DB
     * @throws VizException
     */
    public ArrayList<String> getRFCs() throws VizException {
        ArrayList<String> rval = new ArrayList<String>();

        String query = "Select rfc from rfc order by rfc";

        QueryResult data = HydroDBDataManager.getInstance().runMappedQuery(
                query);

        if (data != null) {
            for (QueryResultRow currNet : data.getRows()) {
                rval.add((String) currNet.getColumn(data.getColumnNames().get(
                        "rfc")));
            }
        }

        return rval;
    }

    /**
     * Retrieves the HSAs from the DB
     * 
     * @return The HSAs from the DB
     * @throws VizException
     */
    public ArrayList<String> getHSAs() throws VizException {
        ArrayList<String> rval = new ArrayList<String>();

        String query = "Select hsa from hsa order by hsa";

        QueryResult data = HydroDBDataManager.getInstance().runMappedQuery(
                query);

        if (data != null) {
            for (QueryResultRow currNet : data.getRows()) {
                rval.add((String) currNet.getColumn(data.getColumnNames().get(
                        "hsa")));
            }
        }

        return rval;
    }

    /**
     * Retrieves the HSAs from location table
     * 
     * @return The HSAs from the DB
     * @throws VizException
     */
    public ArrayList<String> getHSAsForFilter() throws VizException {
        ArrayList<String> rval = new ArrayList<String>();

        String query = "Select distinct(hsa) from location order by hsa";

        QueryResult data = HydroDBDataManager.getInstance().runMappedQuery(
                query);

        if (data != null) {
            for (QueryResultRow currNet : data.getRows()) {
                rval.add((String) currNet.getColumn(data.getColumnNames().get(
                        "hsa")));
            }
        }

        return rval;
    }

    /**
     * Retrieves the WFOs from the DB
     * 
     * @return The WFOs from the DB
     * @throws VizException
     */
    public ArrayList<String> getWFOs() throws VizException {
        ArrayList<String> rval = new ArrayList<String>();

        String query = "Select wfo from wfo order by wfo";

        QueryResult data = HydroDBDataManager.getInstance().runMappedQuery(
                query);

        if (data != null) {
            for (QueryResultRow currNet : data.getRows()) {
                rval.add((String) currNet.getColumn(data.getColumnNames().get(
                        "wfo")));
            }
        }

        return rval;
    }

    /**
     * Retrieves the Time Zones from the DB
     * 
     * @return The Time Zones from the DB
     * @throws VizException
     */
    public ArrayList<String> getTimeZones() throws VizException {
        ArrayList<String> rval = new ArrayList<String>();

        String query = "Select tzone, name from timezone order by tzone";

        QueryResult data = HydroDBDataManager.getInstance().runMappedQuery(
                query);

        String timeZone;
        String name;
        if (data != null) {
            for (QueryResultRow currNet : data.getRows()) {

                timeZone = (String) currNet.getColumn(data.getColumnNames()
                        .get("tzone"));

                name = (String) currNet.getColumn(data.getColumnNames().get(
                        "name"));

                rval.add(String.format("%-8s (%s)", timeZone, name));
            }
        }

        return rval;
    }

    /**
     * Gets the available Agencies and Offices from the DB.
     * 
     * @return
     * @throws VizException
     */
    public ArrayList<String> getAvailableAgenciesAndOffices()
            throws VizException {
        ArrayList<String> rval = new ArrayList<String>();

        String agencyCode;
        String office;
        for (AgencyOfficeData currRow : getAvailableAgenciesAndOfficesData()) {

            agencyCode = currRow.getAgencyCode();

            office = currRow.getOffice();

            rval.add(String.format("%-11S %S", agencyCode, office));
        }

        return rval;
    }

    /**
     * Gets the available Agencies and Offices from the DB.
     * 
     * @return
     * @throws VizException
     */
    public ArrayList<AgencyOfficeData> getAvailableAgenciesAndOfficesData()
            throws VizException {
        return HydroDBDataManager.getInstance().getData(AgencyOfficeData.class);
    }

    /**
     * Gets the Agencies and Offices for a location from the DB.
     * 
     * @return
     * @throws VizException
     */
    public ArrayList<LocationAgencyOfficeData> getSelectedAgenciesAndOfficesData(
            String lid) throws VizException {
        LocationAgencyOfficeData dataToGet = new LocationAgencyOfficeData();
        dataToGet.setLid(lid);

        return HydroDBDataManager.getInstance().getData(dataToGet);
    }

    /**
     * Gets the Agencies and Offices for a location from the DB.
     * 
     * @return
     * @throws VizException
     */
    public ArrayList<String> getSelectedAgenciesAndOffices(String lid)
            throws VizException {
        ArrayList<String> rval = new ArrayList<String>();

        String agencyCode;
        String office;
        for (LocationAgencyOfficeData currRow : getSelectedAgenciesAndOfficesData(lid)) {

            agencyCode = currRow.getAgencyCode();

            office = currRow.getOffice();

            rval.add(String.format("%-11S %S", agencyCode, office));
        }

        return rval;
    }

    /**
     * Parses out the mileage and direction components from the Detail of a
     * location.
     * 
     * It can handle the following cases for the Detail string: 4 4M 4 WSW 4M
     * WSW WNW M M
     * 
     * @param details
     *            The details string for a location from the database.
     * @return String array where the 0 element contains the mileage and the 1
     *         element has the direction. If either component is not there, the
     *         respective element will be an empty string.
     */
    public String[] parseDetails(String details) {
        String[] rval = { "", "" };

        // Build regex for Mileage part of detail
        String mileRegex = "(\\d*)";
        Pattern milePattern = Pattern.compile(mileRegex);

        Matcher mileMatcher = milePattern.matcher(details);

        // Build regex for Direction part of detail
        String dirRegex = "(\\D+)";
        Pattern dirPattern = Pattern.compile(dirRegex);

        Matcher dirMatcher = dirPattern.matcher(details);

        // Find the mileage component.
        if (mileMatcher.find()) {
            rval[0] = mileMatcher.group(1);
        }

        // Find the direction component.
        if (dirMatcher.find()) {
            String dir = dirMatcher.group(1);

            // "4M W" case
            if (dir.contains(" ")) {
                dir = dir.split(" ")[1];
            }

            rval[1] = dir.trim();
        }

        return rval;
    }

    /**
     * Parses out the county and state from the display string of the dialog.
     * 
     * @param countyState
     *            The county/state string for a location from the database.
     * @return String array where the 0 element contains the county and the 1
     *         element has the State. If either component is not there, the
     *         respective element will be an empty string.
     */
    public String[] parseCountyState(String countyState) {
        String[] rval = { "", "" };

        String[] parts = countyState.split(",");

        if (parts.length > 1) {
            rval[0] = parts[0].trim();
            rval[1] = parts[1].trim();
        }

        return rval;
    }

    public void CopyAllDataForLid(LocationData sourceLocation,
            String destinationLid) throws VizException {
        String sourceLid = sourceLocation.getLid();

        // Verify Lid exists
        if (locationExists(sourceLocation)) {
            
            /*
             * Copy all necessary information.
             */
            copyTableData(sourceLid, destinationLid, "location");
            copyTableData(sourceLid, destinationLid, "riverstat");
            copyTableData(sourceLid, destinationLid, "rivermonlocation");
            copyTableData(sourceLid, destinationLid, "fcstptservice");
            copyTableData(sourceLid, destinationLid, "fcstptwatsup");
            copyTableData(sourceLid, destinationLid, "fcstptdeterm");
            copyTableData(sourceLid, destinationLid, "fcstptesp");
            copyTableData(sourceLid, destinationLid, "adjustfactor");
            copyTableData(sourceLid, destinationLid, "agricultural");
            copyTableData(sourceLid, destinationLid, "alertalarmval");
            copyTableData(sourceLid, destinationLid, "arealfcst");
            copyTableData(sourceLid, destinationLid, "arealobs");
            copyTableData(sourceLid, destinationLid, "benchmark");
            copyTableData(sourceLid, destinationLid, "commentvalue");
            copyTableData(sourceLid, destinationLid, "contacts");
            copyTableData(sourceLid, destinationLid, "contingencyvalue");
            copyTableData(sourceLid, destinationLid, "countynum");
            copyTableData(sourceLid, destinationLid, "crest");
            copyTableData(sourceLid, destinationLid, "dailypp");
            copyTableData(sourceLid, destinationLid, "datum");
            copyTableData(sourceLid, destinationLid, "dcp");
            copyTableData(sourceLid, destinationLid, "descrip");
            copyTableData(sourceLid, destinationLid, "discharge");
            copyTableData(sourceLid, destinationLid, "evaporation");
            copyTableData(sourceLid, destinationLid, "fcstdischarge");
            copyTableData(sourceLid, destinationLid, "fcstheight");
            copyTableData(sourceLid, destinationLid, "fcstprecip");
            copyTableData(sourceLid, destinationLid, "fcsttemperature");
            copyTableData(sourceLid, destinationLid, "fcstother");
            copyTableData(sourceLid, destinationLid, "fishcount");
            copyTableData(sourceLid, destinationLid, "flood");
            copyTableData(sourceLid, destinationLid, "floodcat");
            copyTableData(sourceLid, destinationLid, "floodstmt");
            copyTableData(sourceLid, destinationLid, "floodts");
            copyTableData(sourceLid, destinationLid, "fpprevprod");
            copyTableData(sourceLid, destinationLid, "gage");
            copyTableData(sourceLid, destinationLid, "gatedam");
            copyTableData(sourceLid, destinationLid, "ground");
            copyTableData(sourceLid, destinationLid, "height");
            copyTableData(sourceLid, destinationLid, "hgstation");
            copyTableData(sourceLid, destinationLid, "hourlypc");
            copyTableData(sourceLid, destinationLid, "hourlypp");
            copyTableData(sourceLid, destinationLid, "ice");
            copyTableData(sourceLid, destinationLid, "ingestfilter");
            copyTableData(sourceLid, destinationLid, "lake");
            copyTableData(sourceLid, destinationLid, "latestobsvalue");
            copyTableData(sourceLid, destinationLid, "locarea");
            copyTableData(sourceLid, destinationLid, "locextagency");
            copyTableData(sourceLid, destinationLid, "locimage");
            copyTableData(sourceLid, destinationLid, "lowwater");
            copyTableData(sourceLid, destinationLid, "moisture");
            copyTableData(sourceLid, destinationLid, "monthlyvalues");
            copyTableData(sourceLid, destinationLid, "ofsstntrans");
            copyTableData(sourceLid, destinationLid, "observer");
            copyTableData(sourceLid, destinationLid, "pairedvalue");
            copyTableData(sourceLid, destinationLid, "power");
            copyTableData(sourceLid, destinationLid, "pressure");
            copyTableData(sourceLid, destinationLid, "procvalue");
            copyTableData(sourceLid, destinationLid, "productlink");
            copyTableData(sourceLid, destinationLid, "pub");
            copyTableData(sourceLid, destinationLid, "radiation");
            copyTableData(sourceLid, destinationLid, "rating");
            copyTableData(sourceLid, destinationLid, "ratingshift");
            copyTableData(sourceLid, destinationLid, "rawpc");
            copyTableData(sourceLid, destinationLid, "rawpp");
            copyTableData(sourceLid, destinationLid, "rawpother");
            copyTableData(sourceLid, destinationLid, "refer");
            copyTableData(sourceLid, destinationLid, "rejecteddata");
            copyTableData(sourceLid, destinationLid, "reservoir");
            copyTableData(sourceLid, destinationLid, "rescap");
            copyTableData(sourceLid, destinationLid, "rpffcstpoint");
            copyTableData(sourceLid, destinationLid, "snow");
            copyTableData(sourceLid, destinationLid, "sshpconfig");
            copyTableData(sourceLid, destinationLid, "stnclass");
            copyTableData(sourceLid, destinationLid, "telem");
            copyTableData(sourceLid, destinationLid, "temperature");
            copyTableData(sourceLid, destinationLid, "unitgraph");
            copyTableData(sourceLid, destinationLid, "unkstn");
            copyTableData(sourceLid, destinationLid, "unkstnvalue");
            copyTableData(sourceLid, destinationLid, "waterquality");
            copyTableData(sourceLid, destinationLid, "weather");
            copyTableData(sourceLid, destinationLid, "wind");
            copyTableData(sourceLid, destinationLid, "yunique");
            copyTableData(sourceLid, destinationLid, "zonenum");
        }
    }

    public void CopyReferenceDataForLid(LocationData sourceLocation,
            String destinationLid) throws VizException {
        String sourceLid = sourceLocation.getLid();

        // Verify Lid exists
        if (locationExists(sourceLocation)) {

            /*
             * Copy all necessary information.
             */
            copyTableData(sourceLid, destinationLid, "location");
            copyTableData(sourceLid, destinationLid, "riverstat");
            copyTableData(sourceLid, destinationLid, "rivermonlocation");
            copyTableData(sourceLid, destinationLid, "fcstptservice");
            copyTableData(sourceLid, destinationLid, "fcstptwatsup");
            copyTableData(sourceLid, destinationLid, "fcstptdeterm");
            copyTableData(sourceLid, destinationLid, "fcstptesp");
            copyTableData(sourceLid, destinationLid, "adjustfactor");
            copyTableData(sourceLid, destinationLid, "benchmark");
            copyTableData(sourceLid, destinationLid, "contacts");
            copyTableData(sourceLid, destinationLid, "countynum");
            copyTableData(sourceLid, destinationLid, "crest");
            copyTableData(sourceLid, destinationLid, "datum");
            copyTableData(sourceLid, destinationLid, "dcp");
            copyTableData(sourceLid, destinationLid, "descrip");
            copyTableData(sourceLid, destinationLid, "flood");
            copyTableData(sourceLid, destinationLid, "floodcat");
            copyTableData(sourceLid, destinationLid, "floodstmt");
            copyTableData(sourceLid, destinationLid, "gage");
            copyTableData(sourceLid, destinationLid, "ingestfilter");
            copyTableData(sourceLid, destinationLid, "locarea");
            copyTableData(sourceLid, destinationLid, "locdatalimits");
            copyTableData(sourceLid, destinationLid, "locextagency");
            copyTableData(sourceLid, destinationLid, "locimage");
            copyTableData(sourceLid, destinationLid, "lowwater");
            copyTableData(sourceLid, destinationLid, "observer");
            copyTableData(sourceLid, destinationLid, "ofsstntrans");
            copyTableData(sourceLid, destinationLid, "pub");
            copyTableData(sourceLid, destinationLid, "rating");
            copyTableData(sourceLid, destinationLid, "ratingshift");
            copyTableData(sourceLid, destinationLid, "refer");
            copyTableData(sourceLid, destinationLid, "reservoir");
            copyTableData(sourceLid, destinationLid, "rescap");
            copyTableData(sourceLid, destinationLid, "rpffcstpoint");
            copyTableData(sourceLid, destinationLid, "sshpconfig");
            copyTableData(sourceLid, destinationLid, "stnclass");
            copyTableData(sourceLid, destinationLid, "telem");
            copyTableData(sourceLid, destinationLid, "zonenum");
        }
    }

    /**
     * Checks if the location exists in the location table.
     * 
     * @param sourceLocation
     * @return True if the location exist, False otherwise
     * @throws VizException
     */
    private boolean locationExists(LocationData sourceLocation)
            throws VizException {
        int rowCount = HydroDBDataManager.getInstance().checkData(
                sourceLocation);

        return (rowCount > 0);
    }

    private void copyTableData(String sourceLid, String destinationLid, String table) {
        // Get the columns for the table
        final String columnQuery = "SELECT column_name FROM information_schema.columns WHERE table_name = '" + table + "' ORDER BY ordinal_position";

        ArrayList<Object[]> columns = runQuery(columnQuery);
        
        // Dynamically create the select statement to
        //  get the data out
        StringBuffer query = new StringBuffer("select ");
        
        for (int i = 0; i < columns.size(); i++) {
            Object[] oa = columns.get(i);
            if (i == 0) {
                query.append(oa[0]);
            } else {
                query.append(", " + oa[0]);
            }
        }
        
        query.append(" from " + table + " where lid = '" + sourceLid + "'");
        
        // execute the dynamically created query
        ArrayList<Object[]> rs = runQuery(query.toString());
        
        // Get the data from the query and dynamically build
        //  an insert statement
        StringBuilder insert = new StringBuilder("insert into " + table + " (");
        if ((rs != null) && (rs.size() > 0)) {
            for (int i = 0; i < columns.size(); i++) {
                Object[] oa = columns.get(i);
                if (i == 0) {
                    insert.append(oa[0]);
                } else {
                    insert.append(", " + oa[0]);
                }
            }
            
            insert.append(") values (");
            String insertBegin = insert.toString();
            for (Object[] oa: rs) {
                insert.setLength(0);
                insert.append(insertBegin);
                for (int i = 0; i < oa.length; i++) {                
                    if (i == 0) {
                        insert.append("'" + destinationLid + "'");
                    } else {
                        if (oa[i] instanceof String) { 
                            String s = (String) oa[i];
                            if (s.contains("'")) {
                                s = s.replace("'", "''");
                            }
                            if (s.indexOf("\"") > 0) {
                                s = s.replace("\"", "\\\"");
                            }
                            		
                            insert.append(", '" + s + "'");
                        } else if (oa[i] instanceof Date) {
                            insert.append(", '" + HydroConstants.DATE_FORMAT.format(oa[i]) + "'");
                        } else {
                            insert.append(", " + oa[i]);
                        }
                    }               
                }
                insert.append(")");
                
                try {
                    runStatement(insert.toString());
                } catch (VizException e) {
                    e.printStackTrace();
                }
            }
        }
    }
}
