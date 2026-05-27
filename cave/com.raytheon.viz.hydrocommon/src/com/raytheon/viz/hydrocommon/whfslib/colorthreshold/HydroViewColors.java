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
package com.raytheon.viz.hydrocommon.whfslib.colorthreshold;

import com.raytheon.viz.hydrocommon.pdc.PDCOptionData;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 10, 2008            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class HydroViewColors {
    /**
     * Named color set group object.
     */
    private static NamedColorSetGroup pColorSetGroup = new NamedColorSetGroup();

    private static final String[] ELEMENT_USE_ARRAY = {
            // RIVER
            "HEIGHT", // STAGE_POOL_TSDE,
            "HEIGHT", // FLOW_STORAGE_TSDE,
            "FS_DEPARTURE", // DEPTH_ABOVE_FS_TSDE,
            "PERCENT_FLOOD_Q", // PERCENT_FLOOD_FLOW_TSDE,

            // RAIN
            "PRECIP",// INSTANTANEOUS_PRECIP_TSDE,
            "PRECIP", // HOURLY_PRECIP_TSDE,
            "PRECIP", // THREE_HOUR_PRECIP_TSDE,
            "PRECIP", // SIX_HOUR_PRECIP_TSDE,
            "PRECIP", // DAILY_PRECIP_TSDE,

            // SNOW
            "SWE", // SNOW_WATER_EQUIV_TSDE,
            "SWE-CHANGE", // SWE_24_HOUR_CHANGE_TSDE,

            // TEMPERATURE
            "TEMPERATURE", // TEMPERATURE_TSDE,
            "TEMP-CHANGE", // TEMP_24_HOUR_CHANGE_TSDE,
            "TEMPERATURE", // TEMP_MAX_TSDE,
            "TEMPERATURE", // TEMP_MIN_TSDE,

            // HUMIDITY
            "DEWPOINT", // DEWPOINT_TSDE,
            "DEWPOINT-CHANGE",// DEWPOINT_24_HOUR_CHANGE_TSDE,
            "REL-HUMIDITY", // RELATIVE_HUMIDITY_TSDE,

            // WIND
            "WIND-SPEED", // WIND_SPEED_TSDE,
            "WIND-DIRECTION" // WIND_DIRECTION_TSDE,
    };
    
    private static final int[] ELEMENT_DURATION_ARRAY = {
        //RIVER
        0, //STAGE_POOL_TSDE,
        0, //FLOW_STORAGE_TSDE,
        0, //DEPTH_ABOVE_FS_TSDE,
        0, //PERCENT_FLOOD_FLOW_TSDE,
   
        // RAIN  
        1,//INSTANTANEOUS_PRECIP_TSDE,
        1,//HOURLY_PRECIP_TSDE,
        3,//THREE_HOUR_PRECIP_TSDE,
        6,//SIX_HOUR_PRECIP_TSDE,
        24, //DAILY_PRECIP_TSDE,
   
        // SNOW 
        0, //SNOW_WATER_EQUIV_TSDE,
        24, //SWE_24_HOUR_CHANGE_TSDE,
   
        // TEMPERATURE
        0,  //TEMPERATURE_TSDE,
        24, //TEMP_24_HOUR_CHANGE_TSDE,
        0,  //TEMP_MAX_TSDE,
        0, //TEMP_MIN_TSDE,
   
        // HUMIDITY 
        0, //DEWPOINT_TSDE,
        24,// DEWPOINT_24_HOUR_CHANGE_TSDE,
        0,  //RELATIVE_HUMIDITY_TSDE,
   
        // WIND
        0, //WIND_SPEED_TSDE,
        0, //WIND_DIRECTION_TSDE,
    };

    /**
     * Returns the list of default color sets for HydroView
     * 
     * @return
     */
    public static NamedColorSetGroup buildHvColors() {
        return pColorSetGroup;
    }

    /**
     * Sets the NamedColorSetGroup to the correct color/threshold set based on
     * the data type selection.
     * 
     * @param pcOptions
     *            The PDCOptionData object holding the PDC selections
     */
    public static String getColorUseNameFromPcOptions(
            PDCOptionData pcOptions) {

        String colorUseName = null;
        int selectedTimeStepElement = pcOptions.getTsDataElement();

        colorUseName = ELEMENT_USE_ARRAY[selectedTimeStepElement];
        int dur = ELEMENT_DURATION_ARRAY[selectedTimeStepElement];
        
        return colorUseName + "|" + dur;
    }

}
