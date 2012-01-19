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

import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.data.DataAdjustFactorData;
import com.raytheon.viz.hydrocommon.util.HydroDataUtils;

/**
 * Class for managing database query calls for Data Adjustment dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 15, 2008 1787       askripsky   Initial Creation
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */

public class DataAdjustFactorDataManager {
    private static DataAdjustFactorDataManager manager = null;

    private ArrayList<DataAdjustFactorData> adjustFactorData = null;

    /**
     * Private constructor.
     */
    private DataAdjustFactorDataManager() {
    }

    /**
     * Singleton pattern of data manager.
     * 
     * @return manager
     */
    public static synchronized DataAdjustFactorDataManager getInstance() {
        if (manager == null) {
            manager = new DataAdjustFactorDataManager();
        }

        return (DataAdjustFactorDataManager) manager;
    }

    /**
     * Retrieves the Shef Durations from the DB
     * 
     * @return The duration from the DB
     * @throws VizException
     */
    public ArrayList<String> getShefDur() throws VizException {
        ArrayList<String> rval = new ArrayList<String>();

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
    public ArrayList<String> getShefTs() throws VizException {
        ArrayList<String> rval = new ArrayList<String>();

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
    public ArrayList<String> getShefExtremum() throws VizException {
        ArrayList<String> rval = new ArrayList<String>();

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

    public ArrayList<DataAdjustFactorData> getAdjustFactorData()
            throws VizException {
        return getAdjustFactorData(false);
    }

    public ArrayList<DataAdjustFactorData> getAdjustFactorData(boolean forceLoad)
            throws VizException {
        if (adjustFactorData == null || forceLoad) {
            adjustFactorData = HydroDBDataManager.getInstance().getData(
                    DataAdjustFactorData.class);
        }

        return adjustFactorData;
    }

    /**
     * Returns the display string for the whole db record.
     * 
     * @param currData
     *            The record to display.
     * @return The display string for the data.
     */
    public String getAdjustFactorString(DataAdjustFactorData currData) {
        StringBuffer data = new StringBuffer();

        // LID
        data.append(String.format("%-8s ", currData.getLid()));

        // PE
        data.append(String.format("%2s ", currData.getPe()));

        // Duration
        data.append(String.format("%s    ", HydroDataUtils.getDisplayString(
                "%4s", "%d", currData.getDuration())));

        // TS
        data.append(String.format("%2s  ", currData.getTypeSource()));

        // Extremum
        data.append(String.format("%2s  ", currData.getExtremum()));

        // Divisor
        data.append(String.format("%s   ", HydroDataUtils.getDisplayString(
                "%11s", "%.3f", currData.getDivisor())));

        // Base
        data.append(String.format("%s  ", HydroDataUtils.getDisplayString(
                "%11s", "%.3f", currData.getBase())));

        // Multiplier
        data.append(String.format("%s    ", HydroDataUtils.getDisplayString(
                "%11s", "%.3f", currData.getMultiplier())));

        // Adder
        data.append(String.format("%s  ", HydroDataUtils.getDisplayString(
                "%11s", "%.3f", currData.getAdder())));

        return data.toString();
    }

    /**
     * Returns the object that is currently selected in the dialog.
     * 
     * @param selectedIndex
     *            The currently selected index.
     */
    public DataAdjustFactorData getSelectedData(int selectedIndex) {
        return adjustFactorData.get(selectedIndex);
    }
}
