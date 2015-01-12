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
import java.util.List;

import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.data.DataIngestFilterData;

/**
 * Class for managing database query calls for Data Ingest Filter dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 11, 2008 1787       askripsky   Initial Creation
 * Apr 18, 2013 1790       rferrel     Code clean up with non-blocking dialogs.
 * May 1,  2014 17096      xwei        Updated the filter list SQL statement
 * 
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */

public class DataIngestFilterDataManager {
    private static DataIngestFilterDataManager manager = new DataIngestFilterDataManager();

    private List<DataIngestFilterData> ingestFilterData = null;

    /**
     * Private constructor.
     */
    private DataIngestFilterDataManager() {
    }

    /**
     * Singleton pattern of data manager.
     * 
     * @return manager
     */
    public static synchronized DataIngestFilterDataManager getInstance() {
        return manager;
    }

    /**
     * Retrieves the Shef Durations from the DB
     * 
     * @return The duration from the DB
     * @throws VizException
     */
    public List<String> getShefDur() throws VizException {
        List<String> rval = new ArrayList<String>();

        String durQuery = "SELECT name, dur FROM shefdur ORDER BY dur";

        QueryResult data = HydroDBDataManager.getInstance().runMappedQuery(
                durQuery);

        if (data != null) {
            for (QueryResultRow currNet : data.getRows()) {
                String name = (String) currNet.getColumn(data.getColumnNames()
                        .get("name"));
                int dur = (Integer) currNet.getColumn(data.getColumnNames()
                        .get("dur"));
                rval.add(String.format("%s (%s)", name, dur));
            }
        }

        return rval;
    }

    /**
     * Retrieves the Shef Type Sources from the DB
     * 
     * @return The type sources from the DB
     * @throws VizException
     */
    public List<String> getShefTs() throws VizException {
        List<String> rval = new ArrayList<String>();

        String tsQuery = "SELECT name, ts FROM shefts ORDER BY ts";

        QueryResult data = HydroDBDataManager.getInstance().runMappedQuery(
                tsQuery);

        if (data != null) {
            for (QueryResultRow currTs : data.getRows()) {
                String name = (String) currTs.getColumn(data.getColumnNames()
                        .get("name"));
                String ts = (String) currTs.getColumn(data.getColumnNames()
                        .get("ts"));
                rval.add(String.format("%s (%s)", name, ts));
            }
        }

        return rval;
    }

    /**
     * Retrieves the Shef Extremum from the DB
     * 
     * @return The extremum from the DB
     * @throws VizException
     */
    public List<String> getShefExtremum() throws VizException {
        List<String> rval = new ArrayList<String>();

        String extQuery = "SELECT name, extremum FROM shefex ORDER BY extremum";

        QueryResult data = HydroDBDataManager.getInstance().runMappedQuery(
                extQuery);

        if (data != null) {
            for (QueryResultRow currExt : data.getRows()) {
                String name = (String) currExt.getColumn(data.getColumnNames()
                        .get("name"));
                String extremum = (String) currExt.getColumn(data
                        .getColumnNames().get("extremum"));
                rval.add(String.format("%s (%s)", name, extremum));
            }
        }

        return rval;
    }

    /**
     * Gets the ingest filter data from the database cache.
     * 
     * @param filterByPE
     *            Whether or not to filter by the Physical element.
     * @param selectedPE
     *            The Physical Elements to filter by.
     * @param filterByLocation
     *            Whether or not to filter by the location.
     * @param selectedLocation
     *            The location string to filter by.
     * @param filterBySwitches
     *            Whether or not to filter by the ingest switches.
     * @param filterByIngest
     *            Whether master ingest is enabled or disabled.
     * @param filterByOFS
     *            Whether OFS ingest is enabled or disabled.
     * @param filterByMPE
     *            Whether MPE ingest is enabled or disabled.
     * @param filterByTS
     *            Whether or not to filter by the TypeSource
     * @param selectedTS
     *            The TS to filter by
     * @return The filtered ingest filter data.
     * @throws VizException
     */
    public List<DataIngestFilterData> getIngestFilter(boolean filterByPE,
            List<String> selectedPE, boolean filterByLocation,
            String selectedLocation, boolean filterBySwitches,
            boolean filterByIngest, boolean filterByOFS, boolean filterByMPE,
            boolean filterByTS, String selectedTS) throws VizException {
        return getIngestFilter(filterByPE, selectedPE, filterByLocation,
                selectedLocation, filterBySwitches, filterByIngest,
                filterByOFS, filterByMPE, filterByTS, selectedTS, false);
    }

    /**
     * Gets the ingest filter data from the database.
     * 
     * @param filterByPE
     *            Whether or not to filter by the Physical element.
     * @param selectedPE
     *            The Physical Elements to filter by.
     * @param filterByLocation
     *            Whether or not to filter by the location.
     * @param selectedLocation
     *            The location string to filter by.
     * @param filterBySwitches
     *            Whether or not to filter by the ingest switches.
     * @param filterByIngest
     *            Whether master ingest is enabled or disabled.
     * @param filterByOFS
     *            Whether OFS ingest is enabled or disabled.
     * @param filterByMPE
     *            Whether MPE ingest is enabled or disabled.
     * @param filterByTS
     *            Whether or not to filter by the TypeSource
     * @param selectedTS
     *            The TS to filter by
     * @param forceLoad
     *            True to force to refresh cache.
     * @return The filtered ingest filter data.
     * @throws VizException
     */
    public List<DataIngestFilterData> getIngestFilter(boolean filterByPE,
            List<String> selectedPE, boolean filterByLocation,
            String selectedLocation, boolean filterBySwitches,
            boolean filterByIngest, boolean filterByOFS, boolean filterByMPE,
            boolean filterByTS, String selectedTS, boolean forceLoad)
            throws VizException {
        if ((ingestFilterData == null) || forceLoad) {
            DataIngestFilterData seedData = new DataIngestFilterData();

            StringBuffer whereClause = new StringBuffer();
            if (filterByLocation) {
                whereClause.append( "lid='" + selectedLocation + "'" );
            }

            if (filterByPE && (selectedPE.size() > 0)) {
                if (!whereClause.toString().equals("")) {
                    whereClause.append(" AND ");
                }

                whereClause.append("pe in (");

                for (String currPE : selectedPE) {
                    whereClause.append("'" + currPE + "',");
                }

                // Remove the extra ,
                whereClause.setLength(whereClause.length() - 1);

                whereClause.append(")");
            }

            if (filterBySwitches) {
                if (!whereClause.toString().equals("")) {
                    whereClause.append(" AND ");
                }

                whereClause.append("ingest='");
                whereClause.append(filterByIngest ? "T" : "F");
                whereClause.append("' AND ofs_input='");
                whereClause.append(filterByOFS ? "T" : "F");
                whereClause.append("' AND stg2_input='");
                whereClause.append(filterByMPE ? "T" : "F");
                whereClause.append("'");
            }

            if (filterByTS) {
                if (!whereClause.toString().equals("")) {
                    whereClause.append(" AND ");
                }

                whereClause.append("ts='" + selectedTS + "'");
            }

            if (!whereClause.toString().equals("")) {
                seedData.setWhereClause(" WHERE " + whereClause.toString());
            }

            ingestFilterData = HydroDBDataManager.getInstance().getData(
                    seedData);
        }

        return ingestFilterData;
    }

    /**
     * Returns the display string for the whole db record.
     * 
     * @param currData
     *            The record to display.
     * @return The display string for the data.
     */
    public String getIngestFilterString(DataIngestFilterData currData) {
        String dataFormat = "%-9S %-4S %-6S %-7S %-6S %-7S %-5S %-5S %-5S";

        return String.format(dataFormat, currData.getLid(), currData.getPe(),
                getDisplayString(currData.getDuration()),
                currData.getTypeSource(), currData.getExtremum(),
                getDisplayString(currData.getTsRank()), currData.getIngest(),
                currData.getOfsInput(), currData.getStg2Input());
    }

    /**
     * Returns the string corresponding to the DB value. Takes the MISSING_VALUE
     * into account.
     * 
     * @param val
     *            The double to get a display string for.
     * @return The corresponding string or "" if the value is MISSING_VALUE
     */
    public String getDisplayString(Double val) {
        String temp = (Double.compare(val,
                Double.valueOf(HydroConstants.MISSING_VALUE)) != 0) ? Double
                .toString(val) : "";

        return temp;
    }

    /**
     * Returns the string corresponding to the DB value. Takes the MISSING_VALUE
     * into account.
     * 
     * @param val
     *            The int to get a display string for.
     * @return The corresponding string or "" if the value is MISSING_VALUE
     */
    public String getDisplayString(int val) {
        String temp = (val != HydroConstants.MISSING_VALUE) ? Integer
                .toString(val) : "";

        return temp;
    }

    /**
     * Returns the object that is currently selected in the dialog.
     * 
     * @param selectedIndex
     *            The currently selected index.
     */
    public DataIngestFilterData getSelectedFilterData(int selectedIndex) {
        return ingestFilterData.get(selectedIndex);
    }

    /**
     * Sets the ingest switches for all of the currently displayed data.
     * 
     * @param masterIngest
     *            Whether or not to ingest
     * @param ofsIngest
     *            Whether or not to ingest OFS
     * @param mpeIngest
     *            Whether or not to ingest MPE or STG2
     * @throws VizException
     */
    public void setSwitches(boolean masterIngest, boolean ofsIngest,
            boolean mpeIngest) throws VizException {
        for (DataIngestFilterData currData : ingestFilterData) {
            currData.setIngest((masterIngest) ? "T" : "F");
            currData.setOfsInput((ofsIngest) ? "T" : "F");
            currData.setStg2Input((mpeIngest) ? "T" : "F");

            HydroDBDataManager.getInstance().putData(currData);
        }
    }
}
