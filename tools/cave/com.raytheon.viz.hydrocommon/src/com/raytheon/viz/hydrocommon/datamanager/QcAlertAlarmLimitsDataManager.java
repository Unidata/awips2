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
import com.raytheon.viz.hydrocommon.data.DataLimitData;
import com.raytheon.viz.hydrocommon.data.LocationDataLimitData;
import com.raytheon.viz.hydrocommon.util.HydroDataUtils;

/**
 * Class for managing database query calls. QcAlertAlarmLimitsDataManager.java
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 8, 2008 1697       askripsky   Initial Creation
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */

public class QcAlertAlarmLimitsDataManager {
    protected static QcAlertAlarmLimitsDataManager manager = null;

    private ArrayList<DataLimitData> defaultData = null;

    private ArrayList<DataLimitData> defaultDataFiltered = null;

    private ArrayList<LocationDataLimitData> locationData = null;

    private ArrayList<LocationDataLimitData> locationDataFiltered = null;

    /**
     * Private constructor.
     */
    private QcAlertAlarmLimitsDataManager() {
    }

    /**
     * Singleton pattern of data manager.
     * 
     * @return manager
     */
    public static synchronized QcAlertAlarmLimitsDataManager getInstance() {
        if (manager == null) {
            manager = new QcAlertAlarmLimitsDataManager();
        }

        return (QcAlertAlarmLimitsDataManager) manager;
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

    public ArrayList<DataLimitData> getDefaultLimits(boolean filterByPE,
            ArrayList<String> selectedPE) throws VizException {
        return getDefaultLimits(filterByPE, selectedPE, false);
    }

    public ArrayList<DataLimitData> getDefaultLimits(boolean filterByPE,
            ArrayList<String> selectedPE, boolean forceLoad)
            throws VizException {
        if (defaultData == null || forceLoad) {
            defaultData = HydroDBDataManager.getInstance().getData(
                    DataLimitData.class);
        }

        if (filterByPE) {
            filterDefaultByPE(selectedPE);
        } else {
            if (defaultDataFiltered == null) {
                defaultDataFiltered = new ArrayList<DataLimitData>();
            }
            defaultDataFiltered.clear();

            for (DataLimitData currData : defaultData) {
                defaultDataFiltered.add(currData);
            }
        }

        return defaultDataFiltered;
    }

    public String getDefaultLimitString(DataLimitData currData) {
        StringBuffer defaultString = new StringBuffer();

        // PE Dur MonthStart MonthEnd
        defaultString.append(String.format("%13s%7s%7s%8s  ", currData.getPe(),
                currData.getDur(), currData.getMonthDayStart(), currData
                        .getMonthDayEnd()));

        // Gross Min/Max
        defaultString.append(HydroDataUtils.getDisplayString("%9s", "%9.1f",
                currData.getGrossRangeMin()));
        defaultString.append(HydroDataUtils.getDisplayString("%11s", "%10.1f",
                currData.getGrossRangeMax()));

        // Reason Min/Max
        defaultString.append(HydroDataUtils.getDisplayString("%8s", "%8.1f",
                currData.getReasonRangeMin()));
        defaultString.append(HydroDataUtils.getDisplayString("%9s", "%8.1f",
                currData.getReasonRangeMax()));

        // ROC
        defaultString.append(HydroDataUtils.getDisplayString("%7s", "%6.1f",
                currData.getRocMax()));

        // ALERT LIMIT
        defaultString.append(HydroDataUtils.getDisplayString("%8s", "%7.1f",
                currData.getAlertUpperLimit()));
        defaultString.append(HydroDataUtils.getDisplayString("%8s", "%7.1f",
                currData.getAlertLowerLimit()));

        // Alert ROC Limit
        defaultString.append(HydroDataUtils.getDisplayString("%7s", "%6.1f",
                currData.getAlertRocLimit()));

        // Alert Diff Limit
        defaultString.append(HydroDataUtils.getDisplayString("%5s", "%5.1f",
                currData.getAlertDiffLimit()));

        // Alert Upper Limit
        defaultString.append(HydroDataUtils.getDisplayString("%7s", "%6.1f",
                currData.getAlarmUpperLimit()));

        // Alarm Lower Limit
        defaultString.append(HydroDataUtils.getDisplayString("%7s", "%5.1f",
                currData.getAlarmLowerLimit()));

        // Alarm ROC limit
        defaultString.append(HydroDataUtils.getDisplayString("%8s", "%6.1f",
                currData.getAlarmRocLimit()));

        // Alarm Diff Limit
        defaultString.append(HydroDataUtils.getDisplayString("%6s", "%6.1f",
                currData.getAlarmDiffLimit()));

        return defaultString.toString();
    }

    public ArrayList<LocationDataLimitData> getLocationLimits(
            boolean filterByLID, String lidFilter, boolean filterByPE,
            ArrayList<String> selectedPE) throws VizException {
        return getLocationLimits(filterByLID, lidFilter, filterByPE,
                selectedPE, false);
    }

    public ArrayList<LocationDataLimitData> getLocationLimits(
            boolean filterByLID, String lidFilter, boolean filterByPE,
            ArrayList<String> selectedPE, boolean forceLoad)
            throws VizException {
        if (locationData == null || forceLoad) {
            locationData = HydroDBDataManager.getInstance().getData(
                    LocationDataLimitData.class);
        }

        // Copy locationData to locationDataFiltered and then filter and return
        // locationDataFiltered
        if (locationDataFiltered == null) {
            locationDataFiltered = new ArrayList<LocationDataLimitData>();
        }
        locationDataFiltered.clear();

        for (LocationDataLimitData currData : locationData) {
            locationDataFiltered.add(currData);
        }

        if (filterByPE) {
            filterLocationByPE(selectedPE);
        }

        if (filterByLID) {
            filterLocationByLID(lidFilter);
        }

        return locationDataFiltered;
    }

    public String getLocationLimitString(LocationDataLimitData currData) {
        StringBuffer defaultString = new StringBuffer();

        // PE Dur MonthStart MonthEnd
        defaultString.append(String.format("%-10s%3s%7s%7s%8s  ", currData
                .getLid(), currData.getPe(), currData.getDur(), currData
                .getMonthDayStart(), currData.getMonthDayEnd()));

        // Gross Min/Max
        defaultString.append(HydroDataUtils.getDisplayString("%9s", "%9.1f",
                currData.getGrossRangeMin()));
        defaultString.append(HydroDataUtils.getDisplayString("%11s", "%10.1f",
                currData.getGrossRangeMax()));

        // Reason Min/Max
        defaultString.append(HydroDataUtils.getDisplayString("%8s", "%8.1f",
                currData.getReasonRangeMin()));
        defaultString.append(HydroDataUtils.getDisplayString("%9s", "%8.1f",
                currData.getReasonRangeMax()));

        // ROC
        defaultString.append(HydroDataUtils.getDisplayString("%7s", "%6.1f",
                currData.getRocMax()));

        // ALERT LIMIT
        defaultString.append(HydroDataUtils.getDisplayString("%8s", "%7.1f",
                currData.getAlertUpperLimit()));
        defaultString.append(HydroDataUtils.getDisplayString("%7s", "%7.1f",
                currData.getAlertLowerLimit()));

        // Alert ROC Limit
        defaultString.append(HydroDataUtils.getDisplayString("%8s", "%6.1f",
                currData.getAlertRocLimit()));

        // Alert Diff Limit
        defaultString.append(HydroDataUtils.getDisplayString("%5s", "%5.1f",
                currData.getAlertDiffLimit()));

        // Alert Upper Limit
        defaultString.append(HydroDataUtils.getDisplayString("%7s", "%6.1f",
                currData.getAlarmUpperLimit()));

        // Alarm Lower Limit
        defaultString.append(HydroDataUtils.getDisplayString("%7s", "%5.1f",
                currData.getAlarmLowerLimit()));

        // Alarm ROC limit
        defaultString.append(HydroDataUtils.getDisplayString("%7s", "%6.1f",
                currData.getAlarmRocLimit()));

        // Alarm Diff Limit
        defaultString.append(HydroDataUtils.getDisplayString("%7s", "%6.1f",
                currData.getAlarmDiffLimit()));

        return defaultString.toString();
    }

    private void filterDefaultByPE(ArrayList<String> selectedPE) {
        if (defaultDataFiltered == null) {
            defaultDataFiltered = new ArrayList<DataLimitData>();
        }

        defaultDataFiltered.clear();

        for (String peFilter : selectedPE) {
            for (DataLimitData currData : defaultData) {
                if (peFilter.contains(currData.getPe().toUpperCase())) {
                    defaultDataFiltered.add(currData);
                }
            }
        }
    }

    private void filterLocationByPE(ArrayList<String> selectedPE) {
        // Temp array to hold values that will stay
        ArrayList<LocationDataLimitData> temp = new ArrayList<LocationDataLimitData>();

        for (String peFilter : selectedPE) {
            for (LocationDataLimitData currData : locationDataFiltered) {
                if (peFilter.contains(currData.getPe().toUpperCase())) {
                    temp.add(currData);
                }
            }
        }

        locationDataFiltered = temp;
    }

    private void filterLocationByLID(String lidFilter) {
        // Temp array to hold values that will stay
        ArrayList<LocationDataLimitData> temp = new ArrayList<LocationDataLimitData>();

        for (LocationDataLimitData currData : locationDataFiltered) {
            if (currData.getLid().toUpperCase().contains(
                    lidFilter.toUpperCase())) {
                temp.add(currData);
            }
        }

        locationDataFiltered = temp;
    }

    public LocationDataLimitData getSelectedLocationData(int selectedIndex) {
        return locationDataFiltered.get(selectedIndex);
    }

    public DataLimitData getSelectedDefaultData(int selectedIndex) {
        return defaultDataFiltered.get(selectedIndex);
    }
}
