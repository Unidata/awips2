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
package com.raytheon.viz.hydrocommon.textreport;

import com.raytheon.viz.hydrocommon.HydroConstants;

/**
 * Constants for the Text Report Dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 18, 2009 2260       mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class TextReportConstants extends HydroConstants {
    public static final String E19_HDR_COVER = "NWS FORM E-19 (COVER)";
    public static final String E19_HDR_MAPPAGE    = "                              MAP OF GAGE LOCATION";
    public static final String E19_HDR_BENCHMARKS = "                                   BENCHMARKS";
    public static final String E19_HDR_GAGES      = "                                     GAGES";
    public static final String E19_HDR_HISTORY    = "                                     HISTORY";
    public static final String E19_HDR_CRESTS     = "                                     CRESTS";
    public static final String E19_HDR_LOWWATER   = "                                LOW WATER RECORDS";
    public static final String E19_HDR_CONDITIONS = "                            CONDITIONS AFFECTING FLOW";
    public static final String E19_HDR_DAMAGE     = "                                     DAMAGE";
    public static final String E19_HDR_STAFFGAGE  = "                                RIVER STAGE DATA";
    public static final String E19_HDR_CONTACTS   = "                                    CONTACTS";

    /* "ServBkup Page" Types (for printing also) */
    public static final int SERVBKUP_SORTBY_LID = 0;    
    public static final int SERVBKUP_SORTBY_WFO = 1;
    public static final int SERVBKUP_SORTBY_HSA = 2;

    /* "StaList Page" Types (for printing also) */
    public static final int STALIST_SORTBY_LID = 0;   
    public static final int STALIST_SORTBY_NAME = 1;
    public static final int STALIST_SORTBY_COUNTY = 2;
    public static final int STALIST_SORTBY_BASIN = 3;
    public static final int STALIST_SORTBY_OBSERVER = 4;
    
    public static enum TextReportType {
        E19("E-19"),
        E19A("E-19A (Summary)"),
        B44A("B-44A (Cooperative)"),
        SORTED_STATION_LIST("Sorted Station List"),
        STATION_CLASS("Station Class"),
        SERVICE_BACKUP("Service Backup");
        
        private final String reportType;
        
        TextReportType(String value) {
            reportType = value;
        }
        
        public String getReportName() {
            return reportType;
        }        
    }
    
    public static enum E19Pages {
        COVER("Cover"),
        MAP_GAGE_LOCATION("Map of Gage Location"),
        BENCHMARKS("Benchmarks"),
        GAGES("Gages"),
        HISTORY("History"),
        CRESTS("Crests"),
        LOW_WATER_RECORDS("Low Water Records"),
        CONDITIONS_AFFECTING_FLOW("Conditions Affecting Flow"),
        DAMAGE("Damage"),
        RIVER_STAGE_DATA("River Stage Data"),
        CONTACTS("Contacts");

        private final String page;
        
        E19Pages(String page) {
            this.page = page;
        }
        
        public String getPageName() {
            return page;
        }
    }
    
    public static enum StationListSort {
        LOCATION_ID("Location Id"),
        NAME("Name"),
        COUNTY("County"),
        BASIN("Basin"),
        OBSERVER("Observer");
        
        private final String sortType;
        
        StationListSort(String sortType) {
            this.sortType = sortType;
        }
        
        public String getStationSortType() {
            return sortType;
        }
    }
    
    public static enum ServiceBackupSort {
        STATION("Station"),
        WFO("WFO"),
        HSA("HSA");
        
        private final String serviceSort;
        
        ServiceBackupSort(String serviceSort) {
            this.serviceSort = serviceSort;
        }
        
        public String getServiceSort() {
            return serviceSort;
        }
    }
}
