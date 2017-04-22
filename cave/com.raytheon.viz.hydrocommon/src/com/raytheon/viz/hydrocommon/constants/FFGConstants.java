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
package com.raytheon.viz.hydrocommon.constants;

import com.raytheon.viz.hydrocommon.HydroConstants;

/**
 * Flash Flood Guidance Constants.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 14, 2009 2256       mpduff      Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */

public class FFGConstants extends HydroConstants {
    /** FFG colors */
    public static final String FFG[] = { "GRAY30", "RED",
        "ORANGE", "YELLOW", "GREEN", "BLUE", "PURPLE" };

    /** FFG Color levels */
    public static final double FFG_LEVELS[] = { -1, 0, 
        1.0, 2.0, 3.0, 4.0, 5.0  };
    
    /** Areal Display Mode */
    public static enum ArealDisplayMode {
        PRECIP_MODE,
        FFG_MODE,
        COMPARISON_MODE;
    }
    
    /** Precipitation Type */
    public static enum PrecipType {
        STAGE1_PRECIP,
        STAGE2_GAGE_ONLY_PRECIP,
        STAGE2_GAGE_RADAR_PRECIP,
        QPF_PRECIP;
    }
    
    /** Resolution Level */
    public static enum ResolutionLevel {
        GRID("Grid"),
        COUNTY("County"), 
        ZONE("Zone"), 
        BASIN("Basin"),
        ALL("All");
        
        private final String resolution;
        
        ResolutionLevel(String value) {
            resolution = value;
        }
        
        public String getResolution() {
            return resolution;
        }
    }
    
    /** Areal Data Status */
    public static enum ArealDataStatus {
        MISSING_STATUS,
        ALL_ZERO,
        NON_ZERO,
        NOT_APPLICABLE;
    }
}
