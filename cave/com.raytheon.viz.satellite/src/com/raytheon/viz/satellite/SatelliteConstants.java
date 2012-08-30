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
package com.raytheon.viz.satellite;

import java.util.HashMap;

/**
 * Constants used in the Satellite Resource.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 02/26/09                 jsanchez    Initial Creation.
 * 03/16/09           800   jsanchez    Updated legend.
 * 
 * </pre>
 */
public class SatelliteConstants {
	
    private static final String GRID_CLOUD_AMOUNT_LEGEND = "GOES Sounder DPI Cloud Amount (%)";
    
    private static final String GRID_CLOUD_TOP_PRESSURE_OR_HEIGHT_LEGEND = "GOES Sounder DPI Cloud Top Height (ft/100 MSL)";
    
    private static final String IMAGER_11U_IR_LEGEND = "IR Satellite ( C )";
    
    private static final String IMAGER_12U_IR_LEGEND = "IR 12u Satellite";
    
    private static final String IMAGER_13U_IR_LEGEND = "IR 13u Satellite";
    
    private static final String IMAGER_3P9U_IR_LEGEND = "IR 3.9u Satellite ( C )";
    
    private static final String IMAGER_6P7_6P5U_IR_WV_LEGEND = "Water Vapor Satellite";
    
    private static final String IMAGER_VISIBLE_LEGEND = "Visible Satellite";
    
    private static final String RAIN_FALL_RATE_LEGEND = "Rainfall Rate (mm/hr)";
    
    private static final String SOUNDER_11U_IMAGER_LEGEND = "GOES Sndr IR Satellite ( C )";
    
    private static final String SOUNDER_14U_IMAGERY_LEGEND = "GOES Sndr IR 14.1u Satellite ( C )";
    
    private static final String SOUNDER_4U_IMAGERY_LEGEND = "GOES Sndr IR 4.0u Satellite ( C )";
    
    private static final String SOUNDER_4P5U_IMAGERY_LEGEND = "GOES Sndr IR 4.5u Satellite ( C )";
    
    private static final String SOUNDER_6P5U_IMAGERY_LEGEND = "GOES Sndr Water Vapor 6.5u Satellite (Counts)";
    
    private static final String SOUNDER_7U_IMAGERY_LEGEND = "GOES Sndr Water Vapor 7.0u Satellite (Counts)";
	
    private static final String SOUNDER_7P4U_IMAGERY_LEGEND = "GOES Sndr Water Vapor 7.4u Satellite (Counts)";
	
    private static final String SOUNDER_BASED_DERIVED_LI_LEGEND = "GOES Sounder DPI Lifted Index ( C )";
	
    private static final String SOUNDER_BASED_DERIVED_PW_LEGEND = "Total Precip Water (mm)";
	
    private static final String SOUNDER_BASED_DERIVED_SFC_SKIN_LEGEND = "GOES Sounder DPI Skin Temperature ( C )";
	
    private static final String SOUNDER_VIS_IMAGERY_LEGEND = "GOES Sndr Visible Satellite (Counts)";
    
    private static final String IMAGER_11U_39U_IR_LEGEND = "11u-3.9u Satellite (counts)";
    
    private static final String IMAGER_11U_13U_IR_LEGEND = "11u-13u Satellite (counts)";
    
    private static final String IMAGER_11U_12U_IR_LEGEND = "11u-12u Satellite (counts)"; 
    
    private static final String IR_IN_WV__LEGEND = "IR in WV Satellite (C)"; 
    
    private static final String POES_IR_LEGEND = "POES IR Satellite (C)";
    
    private static final String POES_VIS_LEGEND = "POES Visible Satellite";
    
    private static final String POES_3P7U_IR_LEGEND = "POES IR 3.7u Satellite (C)";
    
    private static final String POES_11U_37U_IR_LEGEND = "POES 11u-3.7u Satellite (counts)";
    
    private static final String PERCENT_OF_NORMAL_LEGEND = "Percent of Normal TPW (%)";
    
    public static final String DMSP = "DMSP";
    
    public static final String POES = "POES-NPOESS";
    
    public static final String MISC = "Miscellaneous";
    
    public static final String COMP = "Composite";
    
    public static final String PRECIP = "Sounder Based Derived Precipitable Water (PW)";
    
    public static final String RAIN = "Rain fall rate";
	
	private static final HashMap<String, String> LEGEND_MAP = new HashMap<String, String>();
	
	static {
        LEGEND_MAP.put("Gridded Cloud Amount",GRID_CLOUD_AMOUNT_LEGEND);
		LEGEND_MAP.put("Gridded Cloud Top Pressure or Height",GRID_CLOUD_TOP_PRESSURE_OR_HEIGHT_LEGEND);
		LEGEND_MAP.put("Imager 11 micron IR",IMAGER_11U_IR_LEGEND);
		LEGEND_MAP.put("Imager 12 micron IR",IMAGER_12U_IR_LEGEND);
		LEGEND_MAP.put("Imager 13 micron (IR)",IMAGER_13U_IR_LEGEND);
		LEGEND_MAP.put("Imager 3.9 micron IR",IMAGER_3P9U_IR_LEGEND);
        LEGEND_MAP.put("Imager 6.7-6.5 micron IR (WV)",IMAGER_6P7_6P5U_IR_WV_LEGEND);
        LEGEND_MAP.put("Imager Visible",IMAGER_VISIBLE_LEGEND);
        LEGEND_MAP.put(RAIN,RAIN_FALL_RATE_LEGEND);
        LEGEND_MAP.put("Sounder 11.03 micron imagery",SOUNDER_11U_IMAGER_LEGEND);
        LEGEND_MAP.put("Sounder 14.06 micron imagery",SOUNDER_14U_IMAGERY_LEGEND);
        LEGEND_MAP.put("Sounder 3.98 micron imagery",SOUNDER_4U_IMAGERY_LEGEND);
        LEGEND_MAP.put("Sounder 4.45 micron imagery",SOUNDER_4P5U_IMAGERY_LEGEND);
        LEGEND_MAP.put("Sounder 6.51 micron imagery",SOUNDER_6P5U_IMAGERY_LEGEND);
        LEGEND_MAP.put("Sounder 7.02 micron imagery",SOUNDER_7U_IMAGERY_LEGEND);
        LEGEND_MAP.put("Sounder 7.43 micron imagery",SOUNDER_7P4U_IMAGERY_LEGEND);
        LEGEND_MAP.put("Sounder Based Derived Lifted Index (LI)",SOUNDER_BASED_DERIVED_LI_LEGEND);
        LEGEND_MAP.put(PRECIP,SOUNDER_BASED_DERIVED_PW_LEGEND);
        LEGEND_MAP.put("Sounder Based Derived Surface Skin Temp (SFC Skin)",SOUNDER_BASED_DERIVED_SFC_SKIN_LEGEND);
        LEGEND_MAP.put("Sounder Visible imagery",SOUNDER_VIS_IMAGERY_LEGEND);
        LEGEND_MAP.put("Percent of Normal TPW",PERCENT_OF_NORMAL_LEGEND);
        LEGEND_MAP.put("Polar IR",POES_IR_LEGEND);
        LEGEND_MAP.put("Polar Vis",POES_VIS_LEGEND);        
        LEGEND_MAP.put("Polar 3.7u",POES_3P7U_IR_LEGEND);
        
        //Derived Parameters
        LEGEND_MAP.put("satDif11u3_9uIR",IMAGER_11U_39U_IR_LEGEND);
        LEGEND_MAP.put("satDif11u13uIR",IMAGER_11U_13U_IR_LEGEND);
        LEGEND_MAP.put("satDif11u12uIR",IMAGER_11U_12U_IR_LEGEND);
        LEGEND_MAP.put("satDivWVIR",IR_IN_WV__LEGEND);
        LEGEND_MAP.put("poesDif11u3_7uIR",POES_11U_37U_IR_LEGEND);
	}
	
	public static String getLegend(String productName, String creatingEntity) {
		String rVal  = LEGEND_MAP.get(productName);
		if(productName.equals(PRECIP) || productName.equals(RAIN)){
		    if(creatingEntity.equals(COMP)){
		     rVal = "GOES Sounder DPI " + rVal;
		    } else if(creatingEntity.equals(DMSP)){
		        rVal = "DMSP SSM/I " + rVal;
		    } else if(creatingEntity.equals(POES)){
		    	if (productName.equals("Rain fall rate")) {
		    		rVal = "Blended Rain Rate (mm/hr)";
		    	}
	    		else {
	    			rVal = "POES AMSU " + rVal;
	    		}
            } else if(creatingEntity.equals(MISC)){
                rVal = "Blended " + rVal;
            }
		}
		if(rVal == null || rVal.length() == 0) {
			rVal = productName;
		}
		return rVal;
		
	}

}
