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

import org.apache.commons.collections.Predicate;

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
 * Jul 21, 2015 4500       rjpeter     Use Number in blind cast.
 * Feb 17, 2016 14607      amoore      Add WFO Filter
 * Jan 03, 2018  6806      mduff       Optimized to reduce db queries and cache data.
 * Apr 18, 2018 DCS19644   jwu         Add "NA" row in shefts for default location data limits.
 * </pre>
 * 
 * @author askripsky
 */

public class DataIngestFilterDataManager {
    private static DataIngestFilterDataManager manager = new DataIngestFilterDataManager();

    private List<DataIngestFilterData> ingestFilterData = null;

    private List<DataIngestFilterData> displayFilterData = new ArrayList<>();

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
        List<String> rval = new ArrayList<>();

        String durQuery = "SELECT name, dur FROM shefdur ORDER BY dur";

        QueryResult data = HydroDBDataManager.getInstance()
                .runMappedQuery(durQuery);

        if (data != null) {
            for (QueryResultRow currNet : data.getRows()) {
                String name = (String) currNet
                        .getColumn(data.getColumnNames().get("name"));
                int dur = ((Number) currNet
                        .getColumn(data.getColumnNames().get("dur")))
                                .intValue();
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
        List<String> rval = new ArrayList<>();

        String tsQuery = "SELECT name, ts FROM shefts WHERE ts != '"
                + HydroConstants.DEFAULT_TS + "' ORDER BY ts";

        QueryResult data = HydroDBDataManager.getInstance()
                .runMappedQuery(tsQuery);

        if (data != null) {
            for (QueryResultRow currTs : data.getRows()) {
                String name = (String) currTs
                        .getColumn(data.getColumnNames().get("name"));
                String ts = (String) currTs
                        .getColumn(data.getColumnNames().get("ts"));
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
        List<String> rval = new ArrayList<>();

        String extQuery = "SELECT name, extremum FROM shefex ORDER BY extremum";

        QueryResult data = HydroDBDataManager.getInstance()
                .runMappedQuery(extQuery);

        if (data != null) {
            for (QueryResultRow currExt : data.getRows()) {
                String name = (String) currExt
                        .getColumn(data.getColumnNames().get("name"));
                String extremum = (String) currExt
                        .getColumn(data.getColumnNames().get("extremum"));
                rval.add(String.format("%s (%s)", name, extremum));
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
    public List<String> getWFOs() throws VizException {
        List<String> rval = new ArrayList<>();

        String extQuery = "SELECT wfo FROM wfo ORDER BY wfo";

        QueryResult data = HydroDBDataManager.getInstance()
                .runMappedQuery(extQuery);

        if (data != null) {
            for (QueryResultRow currExt : data.getRows()) {
                String wfo = (String) currExt
                        .getColumn(data.getColumnNames().get("wfo"));
                rval.add(wfo);
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
     * @param filterByWFO
     *            Whether or not to filter by the WFO.
     * @param selectedWFOs
     *            The WFO strings to filter by.
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
            String selectedLocation, boolean filterByWFO,
            List<String> selectedWFOs, boolean filterBySwitches,
            boolean filterByIngest, boolean filterByOFS, boolean filterByMPE,
            boolean filterByTS, String selectedTS) throws VizException {
        return getIngestFilter(filterByPE, selectedPE, filterByLocation,
                selectedLocation, filterByWFO, selectedWFOs, filterBySwitches,
                filterByIngest, filterByOFS, filterByMPE, filterByTS,
                selectedTS, false);
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
     * @param filterByWFO
     *            Whether or not to filter by the WFO.
     * @param selectedWFOs
     *            The WFO strings to filter by.
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
            String selectedLocation, boolean filterByWFO,
            List<String> selectedWFOs, boolean filterBySwitches,
            boolean filterByIngest, boolean filterByOFS, boolean filterByMPE,
            boolean filterByTS, String selectedTS, boolean forceLoad)
            throws VizException {
        if ((ingestFilterData == null) || forceLoad) {
            ingestFilterData = HydroDBDataManager.getInstance()
                    .getData(new DataIngestFilterData());
        }

        displayFilterData.clear();

        if (!filterByLocation && !filterByWFO && !filterByTS && !filterByPE
                && !filterBySwitches) {
            displayFilterData.addAll(ingestFilterData);
        } else {
            // Filter the data if requested
            String switchFilterString = null;

            if (filterBySwitches) {
                StringBuilder buffer = new StringBuilder(
                        filterByIngest ? "T" : "F");
                buffer.append(" ").append(filterByOFS ? "T" : "F");
                buffer.append(" ").append(filterByMPE ? "T" : "F");
                switchFilterString = buffer.toString();
            }

            // List of predicates to check based on user selections
            List<Predicate> predicateList = new ArrayList<>();

            LocationPredicate locPredicate = new LocationPredicate(
                    selectedLocation);
            PhysicalElementPredicate pePredicate = new PhysicalElementPredicate(
                    selectedPE);
            SwitchPredicate switchPredicate = new SwitchPredicate(
                    switchFilterString);
            TypeSourcePredicate tsPredicate = new TypeSourcePredicate(
                    selectedTS);
            WfoPredicate wfoPredicate = new WfoPredicate(selectedWFOs);
            if (filterByLocation) {
                predicateList.add(locPredicate);
            } else {
                /*
                 * not filtering by location, so allow filtering by WFO if
                 * enabled
                 */
                if (filterByWFO && !selectedWFOs.isEmpty()) {
                    predicateList.add(wfoPredicate);
                }
            }

            if (filterByPE && (!selectedPE.isEmpty())) {
                predicateList.add(pePredicate);
            }

            if (filterBySwitches) {
                predicateList.add(switchPredicate);
            }

            if (filterByTS) {
                predicateList.add(tsPredicate);
            }

            for (DataIngestFilterData data : ingestFilterData) {
                /*
                 * This is an "and" filter. If all that are being checked pass
                 * then the record should be displayed.
                 */
                boolean passesFilter = true;
                for (Predicate p : predicateList) {
                    if (!p.evaluate(data)) {
                        passesFilter = false;
                        break;
                    }
                }

                if (passesFilter) {
                    displayFilterData.add(data);
                }
            }
        }
        return displayFilterData;

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
                Double.valueOf(HydroConstants.MISSING_VALUE)) != 0)
                        ? Double.toString(val) : "";

        return temp;
    }

    /**
     * Returns the object that is currently selected in the dialog.
     * 
     * @param selectedIndex
     *            The currently selected index.
     */
    public DataIngestFilterData getSelectedFilterData(int selectedIndex) {
        return displayFilterData.get(selectedIndex);
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
        for (DataIngestFilterData currData : displayFilterData) {
            currData.setIngest((masterIngest) ? "T" : "F");
            currData.setOfsInput((ofsIngest) ? "T" : "F");
            currData.setStg2Input((mpeIngest) ? "T" : "F");

            HydroDBDataManager.getInstance().putData(currData);
        }
    }

    protected static class LocationPredicate implements Predicate {
        private String lid;

        public LocationPredicate(String lid) {
            this.lid = lid;
        }

        @Override
        public boolean evaluate(Object object) {
            DataIngestFilterData data = (DataIngestFilterData) object;

            return data.getLid().equals(this.lid);
        }

    }

    protected static class WfoPredicate implements Predicate {
        private List<String> wfos;

        public WfoPredicate(List<String> wfos) {
            this.wfos = wfos;
        }

        @Override
        public boolean evaluate(Object object) {
            DataIngestFilterData data = (DataIngestFilterData) object;

            return wfos.contains(data.getWfo());
        }
    }

    protected static class TypeSourcePredicate implements Predicate {
        private String ts;

        public TypeSourcePredicate(String ts) {
            this.ts = ts;
        }

        @Override
        public boolean evaluate(Object object) {
            DataIngestFilterData data = (DataIngestFilterData) object;

            return data.getTypeSource().equalsIgnoreCase(this.ts);
        }
    }

    protected static class PhysicalElementPredicate implements Predicate {
        private List<String> pes;

        public PhysicalElementPredicate(List<String> pes) {
            this.pes = pes;
        }

        @Override
        public boolean evaluate(Object object) {
            DataIngestFilterData data = (DataIngestFilterData) object;

            return pes.contains(data.getPe());
        }
    }

    protected static class SwitchPredicate implements Predicate {
        private String switchFilterString;

        public SwitchPredicate(String switchFilterString) {

            this.switchFilterString = switchFilterString;
        }

        @Override
        public boolean evaluate(Object object) {
            DataIngestFilterData data = (DataIngestFilterData) object;
            String checkStr = data.getIngest() + " " + data.getOfsInput() + " "
                    + data.getStg2Input();

            return checkStr.equalsIgnoreCase(switchFilterString);
        }
    }
}
