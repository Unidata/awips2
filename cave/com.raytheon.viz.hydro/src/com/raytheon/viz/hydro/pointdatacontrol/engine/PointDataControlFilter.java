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
package com.raytheon.viz.hydro.pointdatacontrol.engine;

import java.util.ArrayList;
import java.util.Map;

import com.raytheon.viz.hydro.pointdatacontrol.PDCConstants;
import com.raytheon.viz.hydro.pointdatacontrol.PDCConstants.QueryMode;
import com.raytheon.viz.hydro.pointdatacontrol.data.LocPDC;
import com.raytheon.viz.hydro.pointdatacontrol.db.PDCDataManager;
import com.raytheon.viz.hydro.pointdatacontrol.util.PDCUtils;
import com.raytheon.viz.hydrocommon.data.GageData;
import com.raytheon.viz.hydrocommon.pdc.PDCOptionData;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 24, 2008            mpduff      Initial creation.
 * Oct 14, 2009 2916       mpduff      Fixed problem with elevation filter.
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0 
 */

public class PointDataControlFilter {
    private static Map<String, LocPDC> locPDCMap = null;

    /**
     * Filters the report list as per the user instructions.
     * Reports are filtered by station and/or by value.
     *
     * This function also conveniently filters out data that is for
     * an area, and not for a location.
     * 
     * @param repList
     *     The report list
     */
    public static ArrayList<GageData> filterReportsAndAddInfo(ArrayList<GageData> repList) {
        PDCOptionData pcOptions = PDCOptionData.getInstance();
        PDCDataManager manager = PDCDataManager.getInstance();
        ArrayList<Integer> removeList = new ArrayList<Integer>();
        boolean useReport = true;
        
        /* Continue only if there are data to be processed */
        if (repList == null) {
            // TODO add log message here:  
            //In routine 'filter_reports_and_add_info':\n"
            //"Filtering not performed, no data reports.\n
            return repList;
        }
        
        /* Load the information from the LocPDC view the first time this
        routine is called.  This information is required for the data source
        (telem_type column) and service backup filters (hsa column).  It is
        also required for providing the station name, latitude/longitude, 
        and display class for each of the stations in the report list. */
        
        if (locPDCMap == null) {
            /* Set up the where clause so that only stations with a nonzero
            post value are loaded. */
            String where = "WHERE post != 0";// ORDER BY lid";
            
            locPDCMap = manager.getLocPDC(where);
            
            if ((locPDCMap == null) || (locPDCMap.size() < 1)) {
                // TODO log error
//                In routine 'filter_reports_and_add_info':\n"
//                "No records retrieved from the LocPDC view
                return repList;
            }
        }
        
        int countUse = 0;
        int countNotUse = 0;
        
        /* Loop on the reports and filter out those as per user specifications.
        At the same time, add the necessary information from the
        LocPDC view. */
        for (int i = 0; i < repList.size(); i++) {
            /* initialize convenient variable. assume okay */
            useReport = true;
            LocPDC locpdc = locPDCMap.get(repList.get(i).getLid());

            if (locpdc != null) {
                /* add elevation info  */
                repList.get(i).setElevation(locpdc.getElev());

                // in TIME STEP Mode only,
                // do a filter by typesource.  Ad Hoc mode does this in SQL
                if ((pcOptions.getQueryMode() == QueryMode.TIME_STEP_MODE.getQueryMode()) &&
                        (pcOptions.getFilterByTypeSource() == 1)) {
                    //assume it is not found
                    useReport = false;

                    // if any TS matches, then display the station
                    for (int k = 0; k < pcOptions.getTypeSourceChosenCount(); k++) {
                        if (pcOptions.getTypeSourceChosenList().get(k).contains(repList.get(i).getTs())) {
                            useReport = true;
                            break;
                        }
                    }
                }

                /* if the user requests that the stations are to be filtered by the
                    station data source, then apply the filter. */
                if ((pcOptions.getQueryMode() == QueryMode.AD_HOC_MODE.getQueryMode()) &&
                        (pcOptions.getFilterByDataSource() == 1)) {
                    useReport = false;

                    for (int k = 0; k < pcOptions.getDataSourceChosenCount(); k++) {
                        if (pcOptions.getDataSourcesChosen()[k].equalsIgnoreCase(PDCConstants.OBSERVER_STRING)) {
                            if (locpdc.isDcp()) {
                                useReport = true;
                            }
                        } else if (pcOptions.getDataSourcesChosen()[k].equalsIgnoreCase(PDCConstants.DCP_STRING)) {
                            if (locpdc.isDcp()) {
                                useReport = true;
                            }
                        } else if (pcOptions.getDataSourcesChosen()[k].equalsIgnoreCase(PDCConstants.UNDEFINED_STRING)) {
                            if (locpdc.isDcp() &&
                                    locpdc.isObserver() &&
                                    ((locpdc.getTelemType() != null) && (locpdc.getTelemType().length() == 0))) {
                                useReport = true;
                            }                                       
                        } else {
                            if (pcOptions.getDataSourcesChosen()[k].equalsIgnoreCase(locpdc.getTelemType())) {
                                useReport = true;
                            }
                        }
                    }
                }  /* end of if checking whether to filter by data source */

                /* if the supress non-fcst point options is enabled,
                    then filter out any stations that are not forecast points. */
                if (pcOptions.getFcstptsOnly() == 1) {
                    if (!locpdc.getDispClass().contains(PDCConstants.FCSTPT_STNCLASS)) {
                        useReport = false;
                    }
                }

                /* Apply the service backup filter if enabled.  This test compares
                    the hsa defined for the current location (as given in
                    the locPDCPtr) with that selected from the serivce backup option
                    on the pointcontrol gui. Multiple HSAs may be selected from this
                    option and are represented as a comma separated list in the
                    pc_options->hsa_list. */
                if ((pcOptions.getFilterByHSA() == 1) || !useReport) {
                    if (useReport) {
                        if (!pcOptions.getHsaList().contains(PDCConstants.ALL_AREAS) &&
                                !pcOptions.getHsaList().contains(locpdc.getHsa())) {
                            useReport = false;
                        }
                    }
                }

                /* now suppress missing data and zero data if the
                    suppress option is enabled */
                if ((pcOptions.getSupressMissing() == 0) || !useReport) {
                    if (repList.get(i).getValue() == PDCConstants.MISSING_VALUE) {
                        useReport = false;
                    }
                }

                //pass through value filter
                if (isFilterdOutByValue(pcOptions.getValueFilterOperation(),
                        pcOptions.getValueFilterValue(),
                        repList.get(i).getValue()) || !useReport) {
                    useReport = false;
                }

                //pass through elevation filter
                if (isFilterdOutByValue(pcOptions.getElevFilterOperation(), 
                        pcOptions.getElevFilterValue(),
                        repList.get(i).getElevation()) || !useReport) {
                    useReport = false;
                }

                repList.get(i).setUse(useReport);

                /* if timestep mode we still need to get the display class set */
                if (useReport || (pcOptions.getQueryMode() == 1)) {
                    countUse++;

                    /* Copy the latitude and longitude into the current report
                        structure being processed. */
                    repList.get(i).setLat(locpdc.getLat());
                    repList.get(i).setLon(locpdc.getLon());
                    repList.get(i).setDispClass(locpdc.getDispClass());
                    
                    /* Copy the station name into the current report structure being
                        processed. */
                    repList.get(i).setName(locpdc.getName());
                } else {
                    countNotUse++;
                }
            } else {
                removeList.add(i);
            }
            // TODO - log message here
            //"Total of %d reports; %d filtered out.\n",
            //count_use + count_notuse, count_notuse);
        }
        int totalStations = countUse + countNotUse;

        for (int i = removeList.size() - 1; i >= 0; i--) {
            repList.remove((int) removeList.get(i));
        }
        
        return repList;
    }
    
    /**
     * Determines if a value FAILS the test, so if a value does NOT 
     * meet the criteria, the result is TRUE.
     * 
     * @param filterOperation
     *     The filter operation 
     *          SHOW_ALL ("Any Value") 0,
     *          SHOW_EQUAL ("Value =") 1,
     *          SHOW_NOT_EQUAL ("Value Not =") 2,
     *          SHOW_GREATER_EQUAL ("Value >=") 3,
     *          SHOW_LESS_EQUAL ("Value <=") 4,
     *          SHOW_GREATER ("Value >") 5,
     *          SHOW_LESS ("Value <") 6;
     * @param comparisonValue
     *     The value to compare against
     * @param valueToExamine
     *     The value to examine
     * @return
     *     true if the value passes the test
     */
    private static boolean isFilterdOutByValue(int filterOperation, double comparisonValue, double valueToExamine) {
        boolean filteredOut = false;
        boolean passesTest = false;
        
        if (filterOperation != 0) { // SHOW_ALL
            // round to 2 decimal places
            valueToExamine = PDCUtils.round(valueToExamine, 2);
            comparisonValue = PDCUtils.round(comparisonValue, 2);
        }
        
        if (filterOperation == 0) {
            passesTest = true;
        }
        
        if (filterOperation == 1) {  // SHOW_EQUAL
            if (valueToExamine == comparisonValue) {
                passesTest = true;
            }
        } else if (filterOperation == 2) {  // SHOW_NOT_EQUAL
            if (valueToExamine != comparisonValue) {
                passesTest = true;
            }
        } else if (filterOperation == 3) {  // SHOW_GREATER_EQUAL
            if  (valueToExamine >= comparisonValue) {
                passesTest = true;
            }
        } else if (filterOperation == 5) {  // SHOW_GREATER
            if (valueToExamine > comparisonValue) {
                passesTest = true;
            }
        } else if (filterOperation == 4) {  // SHOW_LESS_EQUAL
            if (valueToExamine <= comparisonValue) {
                passesTest = true;
            }
        } else if (filterOperation == 6) {  // SHOW_LESS
            if (valueToExamine < comparisonValue) {
                passesTest = true;
            }
        }
        
        /*
         * filteredOut is the opposite of passesTest
         * 
         * filteredOut = !(passesTest)
         * 
         * I write it out explicitly for ease of understanding.
         */
        if (!passesTest) {
            filteredOut = true;
        } else {  // passes
            filteredOut = false;
        }
        
        return filteredOut;
    }
}