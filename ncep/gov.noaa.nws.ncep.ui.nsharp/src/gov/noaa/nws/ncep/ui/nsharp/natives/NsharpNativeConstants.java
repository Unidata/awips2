/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.natives.NsharpNativeConstants
 * 
 * This java class performs the NSHARP NsharpNativeConstants functions.
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 03/23/2010	229			Chin Chen	Initial coding
 * 7/2012					T. Lee		Changed Rogash QPF to Rainfall Rate
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nsharp.natives;

import java.util.HashMap;
import java.util.Map;

import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;

public class NsharpNativeConstants {
	public static final float NSHARP_NATIVE_INVALID_DATA = NcSoundingLayer.MISSING; //-9999f
	//Note: legacy NSHARP lib return -999 as invalid data
	public static final float NSHARP_LEGACY_LIB_INVALID_DATA = NcSoundingLayer.MISSING;  //-9999f
	public static final float PI = 3.14159265F;
	/*
	 * PAGE1 CANVAS1 string definitions
	 */
	public static final String PAGE1TEXT1_SB_STR   ="SB PARCEL   ";
	public static final String PAGE1TEXT1_ML_STR   ="ML PARCEL   ";
	public static final String PAGE1TEXT1_FCST_STR ="FCST PARCEL";
	public static final String PAGE1TEXT1_MU_STR   ="MU PARCEL   ";
	public static final String PAGE1TEXT1_USER_STR ="USER PARCEL";
	public static final String PAGE1TEXT1_EFF_STR  ="EFF PARCEL  ";
	//PARCELTYPES_STR array: order of array element should follow PARCELTYPE_* definitions below.
	//public static final String[]  PARCELTYPES_STR = {"", PAGE1TEXT1_SB_STR, PAGE1TEXT1_FCST_STR,PAGE1TEXT1_ML_STR,
	//	PAGE1TEXT1_MU_STR, PAGE1TEXT1_USER_STR, PAGE1TEXT1_EFF_STR
	//};
	/*
	 * PAGE1 Panel2 string definitions
	 */
	public static final String[] STORM_MOTION_TYPE_STR1 = { "SFC-1km", "SFC_2km",
		"SFC_3km", "Eff Inflow Layer"
	};
	public static final String[] STORM_MOTION_TYPE_STR2 = { "SFC-6km","SFC-8km", "LCL-EL(Cloud Layer)",
		"Lower Half Storm Depth"	
	};
	public static final float[][] STORM_MOTION_HEIGHT1 = { {0,1000}, {0,2000}, {0,3000}, {0,0}
	};
	public static final float[][] STORM_MOTION_HEIGHT2 = { {0,6000},
		{0, 8000}, {0,0},{0,0}
	};
	/*
	 * PARCEL DATA string definitions
	 */
	//PARCEL type flags to be used for define_parcel()
	public static final short PARCELTYPE_CUIRRENT_SELECTION = -1;
	public static final short PARCELTYPE_OBS_SFC = 1;
	public static final short PARCELTYPE_FCST_SFC = 2;
	public static final short PARCELTYPE_MOST_UNSTABLE = 3;//BigNsharp
	public static final short PARCELTYPE_MEAN_MIXING = 4;  //BigNsharp
	public static final short PARCELTYPE_USER_DEFINED = 5;
	public static final short PARCELTYPE_EFF = 6; //BigNsharp
	public static final short PARCEL_MAX = PARCELTYPE_EFF;
	
	//default pressure for parcel as defined in BigNsharp
	public static final float OBS_LAYER   = 0.0f; 
	public static final float FCST_LAYER = 0.0f; 
	public static final float MML_LAYER  = 100.0f;   /* mean-mixed layer */
	public static final float MU_LAYER   = 400.0f;   /* most-unstable layer */
	public static final float USER_LAYER = 850.0f;   /* default user-defined level */
	public static final float EFF_LAYER   = MU_LAYER; //Chin, need to check with John
	/*1 = Observed sfc parcel                       */
	/*             2 = Fcst sfc parcel                           */
	/*             3 = Most unstable parcel                      */
	/*             4 = Mean mixlyr parcel                        */
	/*             5 = User defined parcel                       */
	/*             6 = Mean Effective Layer parcel  
	 * 
	 */
	//parcel header string
	public static final String PARCEL_DATA_STR = "\t\t\t\tPARCEL DATA    \r\n";
	public static final String PARCEL_OBS_SFC_STR = "\t\t*** SFC PARCEL ***\r\n";
	public static final String PARCEL_FORECAST_SFC_STR =  "\t\t*** FCST SFC PARCEL ***\r\n";
	public static final String PARCEL_MEAN_MIXING_STR =  "\t\t*** MEAN MIXING LAYER PARCEL ***\r\n";
	public static final String PARCEL_MOST_UNSTABLE_STR =  "\t\t*** MOST UNSTABLE PARCEL ***\r\n";
	public static final String PARCEL_MEAN_EFF_STR =  "\t\t*** MEAN EFFECTIVE PARCEL ***\r\n";
	public static final String PARCEL_USR_DEFINED_STR =  "\t\t*** %.1f mb PARCEL ***\r\n";
	//parcel lines
	public static final String PARCEL_LPL_LINE = "LPL:\t%dmb\t%dC/%dC\t%dF/%dF\r\n\r\n";
	public static final String PARCEL_LPL_LINE_ = "LPL:_%dmb_%dC/%dC_%dF/%dF";
	public static final String PARCEL_CAPE_LINE = "CAPE =  %.0f  J/Kg";
	public static final String PARCEL_CAPE_MISSING = "CAPE =  M";
	public static final String PARCEL_LI_LINE = "LI(500mb) =%5.0fC\r\n";
	public static final String PARCEL_LI_MISSING = "LI(500mb) =  M\r\n";
	public static final String PARCEL_BFZL_LINE = "BFZL =  %.0f J/Kg";
	public static final String PARCEL_BFZL_MISSING = "BFZL =  M";
	public static final String PARCEL_LIMIN_LINE = "LImin =  %4.0fC /%4.0fmb\r\n";
	public static final String PARCEL_LIMIN_MISSING = "LImin =  M / M\r\n";
	public static final String PARCEL_CINH_LINE = "CINH =  %.0f J/Kg";
	public static final String PARCEL_CINH_MISSING = "CINH =  M";
	public static final String PARCEL_CAP_LINE = "Cap =  %4.0fC /%4.0fmb\r\n\r\n";
	public static final String PARCEL_CAP_MISSING = "Cap =  M / M\r\n\r\n";
	public static final String PARCEL_LEVEL_LINE = "LEVEL\t\tPRES\t\tHGT(AGL)\t\tTEMP\r\n____________________________________________________\r\n";
	public static final String PARCEL_LEVEL_LINE_ = "LEVEL_PRES_HGT(AGL)_TEMP";
	public static final String PARCEL_LCL_LINE = "LCL\t\t%5.0fmb\t\t%7.0fft\r\n";
	public static final String PARCEL_LCL_LINE_ = "LCL_%5.0fmb_%7.0fft_ ";
	public static final String PARCEL_LCL_MISSING = "LCL\t\tM      \t\tM\r\n";
	public static final String PARCEL_LCL_MISSING_ = "LCL_M_M_ ";
	public static final String PARCEL_LFC_LINE = "LFC\t\t%5.0fmb\t\t%7.0fft\t%6.0fC\r\n";
	public static final String PARCEL_LFC_LINE_ = "LFC_%5.0fmb_%7.0fft_%6.0fC";
	public static final String PARCEL_LFC_MISSING = "LFC\t\tM      \t\tM     \t\t\tM\r\n";
	public static final String PARCEL_LFC_MISSING_ = "LFC_M_M_M";
	public static final String PARCEL_EL_LINE = "EL \t\t%5.0fmb\t\t%7.0fft\t%6.0fC\r\n";
	public static final String PARCEL_EL_LINE_ = "EL_%5.0fmb_%7.0fft_%6.0fC";
	public static final String PARCEL_EL_MISSING = "EL \t\tM      \t\tM     \t\t\tM\r\n";
	public static final String PARCEL_EL_MISSING_ = "EL_M_M_M";
	public static final String PARCEL_MPL_LINE = "MPL\t\t%5.0fmb\t\t%7.0fft\r\n";
	public static final String PARCEL_MPL_LINE_ = "MPL_%5.0fmb_%7.0fft_ ";
	public static final String PARCEL_MPL_MISSING = "MPL\t\tM      \t\tM\r\n";
	public static final String PARCEL_MPL_MISSING_ = "MPL_M_M_ ";
	
	/*
	 * THERMODYNAMIC DATA string definitions
	 */
	// THERMODYNAMIC HEADER STR
	public static final String THERMO_DATA_STR = "\t\t\t\tTHERMODYNAMIC DATA    \r\n";
	public static final String THERMO_MOISTURE_STR = "\t\t------ AVAILABLE MOISTURE ------\r\n";
	public static final String THERMO_INSTABILITY_STR = "\t\t------ CONDITIONAL INSTABILITY ------\r\n";
	public static final String THERMO_MISC_PARMS_STR = "\t\t------ MISC PARAMETERS ------\r\n";
	// THERMODYNAMIC LINE STRING
	public static final String THERMO_PWATER_LINE = "P. Water = %2.2f in\t\t";
	public static final String THERMO_PWATER_MISSING = "P. Water = M   \t\t";
	public static final String THERMO_MEANRH_LINE = "Mean RH  = %.0f %c \r\n";
	public static final String THERMO_MEANRH_MISSING = "Mean RH  = M\r\n";
	public static final String THERMO_MEANW_LINE = "Mean W   = %.1fg/Kg\t\t";
	public static final String THERMO_MEANW_MISSING = "Mean W   = M\t\t\t";
	public static final String THERMO_MEANLRH_LINE = "Mean LRH = %.0f %c \r\n";
	public static final String THERMO_MEANLRH_MISSING = "Mean LRH = M\r\n";
	public static final String THERMO_TOP_LINE = "Top of Moist Lyr =  %.0f mb / %.0f ft\r\n";
	public static final String THERMO_TOP_MISSING = "Top of Moist Lyr =  \t\tM / M\r\n\r\n";
	public static final String THERMO_700500mb_LINE = "700-500mb Lapse Rate =  %.0f C / %.1f  C/Km\r\n";
	public static final String THERMO_700500mb_MISSING = "700-500mb Lapse Rate =  M / M\r\n\r\n";
	public static final String THERMO_850500mb_LINE = "850-500mb Lapse Rate =  %.0f C / %.1f  C/Km\r\n";
	public static final String THERMO_850500mb_MISSING = "850-500mb Lapse Rate =  M / M\r\n\r\n";
	public static final String THERMO_TOTAL_LINE = "Total Totals = %.0f\t\t";
	public static final String THERMO_TOTAL_MISSING = "Total Totals = M \t\t";
	public static final String THERMO_KINDEX_LINE = "K-Index   = %.0f\r\n";
	public static final String THERMO_KINDEX_MISSING = "K-Index   = M\r\n";
	public static final String THERMO_SWEAT_LINE = "SWEAT Index  = %.0f\t\t";
	public static final String THERMO_SWEAT_MISSING = "SWEAT Index  = M\t\t";
	public static final String THERMO_MAXT_LINE = "Max Temp  = %.0fF\r\n";
	public static final String THERMO_MAXT_MISSING = "Max Temp  = M\r\n";
	public static final String THERMO_THETAE_LINE = "ThetaE Diff  = %.0fC\t\t";
	public static final String THERMO_THETAE_MISSING = "ThetaE Diff  = M \t\t";
	public static final String THERMO_CONVT_LINE = "Conv Temp = %.0fF\r\n";
	public static final String THERMO_CONVT_MISSING = "Conv Temp = M\r\n";
	public static final String THERMO_FGZ_MISSING = "FGZ level = M\r\n";
	public static final String THERMO_FGZ_LINE = "FGZ level = %.0fft\r\n";
	public static final String THERMO_WBZ_MISSING = "WBZ level    = M \t\t";
	public static final String THERMO_WBZ_LINE = "WBZ level    = %3.0fft\t";
	
	/*
	 * OPC LOW LEVEL STABILITY data string
	 */
	public static final String OPC_LOW_LEVEL_STR = "\t\t\t    OPC LOW LEVEL STABILITY    \r\n";

	//OPC page header string
	public static final String OPC_SURFACE975_STR = " ----- SURFACE-975 hPa TEMP GRADIENT -----\r\n";
	public static final String OPC_LOWEST_INV_STR = "\r\n -------- LOWEST INVERSION HEIGHT --------\r\n";
	public static final String OPC_MIXING_HGT_STR = " ------------- MIXING HEIGHT -------------\r\n";
	//OPC SURFACE975 line strings
	public static final String OPC_LEVEL_LINE = "LEVEL\t\tPRES\t\tHEIGHT\t\tTEMP\r\n_______________________________________________\r\n";
	public static final String OPC_LEVEL_LINE_ = "LEVEL_PRES_HEIGHT_TEMP";
	public static final String OPC_975_LINE = 	"975 hPa\t\t 975 mb\t\t%4.0f m\t\t%.2f C\r\n";
	public static final String OPC_975_LINE_ = 	"975 hPa_975 mb_%4.0f m_%.2f C";
	public static final String OPC_975_LINE_MISSING = 	"975 hPa\t\t 975 mb\t\tM \t\t\tM \r\n";
	public static final String OPC_975_LINE_MISSING_ = 	"975 hPa_975 mb_M_M";
	public static final String OPC_SURFACE_LINE = 	"Surface\t\t%4.0f mb\t\t%4.0f m\t\t%.2f C\r\n\r\n";
	public static final String OPC_SURFACE_LINE_ = 	"Surface_%4.0f mb_%4.0f m_%.2f C";
	public static final String OPC_SURFACE_MISSING = 	"Surface\t\t M    mb\t\t M   m\t\t M\r\n";
	public static final String OPC_SURFACE_MISSING_ = 	"Surface_M_M_M";
	public static final String OPC_975_SURFACE_LINE = 	"975-Sfc Gradient = %.2f C\r\n";
	public static final String OPC_975_SURFACE_MISSING = 	"975-Sfc Gradient =  M\r\n";
	// lowest inversion height line strings
	public static final String OPC_BASEHEIGHT_LINE = 	"Base Height      =    %.0f m\r\n";
	public static final String OPC_BASEHEIGHT_MISSING = "Base Height      =    M\r\n";
	public static final String OPC_BASEPRESSURE_LINE = 	"Base Pressure    =    %.0f mb\r\n";
    public static final String OPC_BASEPRESSURE_MISSING = "Base Pressure    =    M\r\n";
    public static final String OPC_CHANGE_IN_TEMP_LINE = 	"Change in Temp   =    %.2f C\r\n";
	public static final String OPC_CHANGE_IN_TEMP_MISSING = "Change in Temp   =    M\r\n";
	
	// mixing height line strings
	public static final String OPC_LAYER_BASED_STR = "\t\t\t\t*** Layer Based ***\r\n";
	public static final String OPC_SURFACE_BASED_STR = "\t\t\t\t*** Surface Based ***\r\n";
   public static final String OPC_DRY_AD_LINE = 	    "Dry Ad Lapse Rate      _=    9.8 C/Km\r\n";
	public static final String OPC_THRESH_LAPSE_LINE = 	"Thresh Lapse Rate      _=    8.3 C/Km\r\n";
	public static final String OPC_MIXINGHEIGHT_LINE = 	"Mixing Height          _=    %.0f m\r\n";
	public static final String OPC_MIXINGPRESSURE_LINE ="Mixing Pressure        _=    %.0f mb\r\n";
	public static final String OPC_TOPMIXLAYER_LINE = 	"Top Mix Layer Wind     _=    %d%c/%d kt\r\n";
	public static final String OPC_MIXLAYERMAX_LINE = 	"Mix Layer Max Wind     _=    %d%c/%d kt\r\n";
	public static final String OPC_MIXINGHEIGHT_MISSING="Mixing Height          _=    M\r\n";
public static final String OPC_MIXINGPRESSURE_MISSING = "Mixing Pressure        _=    M\r\n";
	public static final String OPC_TOPMIXLAYER_MISSING ="Top Mix Layer Wind     _=    M / M\r\n";
	public static final String OPC_MIXLAYERMAX_MISSING ="Mix Layer Max Wind     _=    M / M r\n";
	public static final String OPC_LAYER_LAPSE_LINE = 	"Layer Lapse Rate       _=    %.2f C/%.1fC/Km\r\n";
	public static final String OPC_LAYER_LAPSE_MISSING ="Layer Lapse Rate       _=    M / M\r\n";
	
	/*
	 * STORM RELATIVE data string
	 */
	public static final String STORM_RELATIVE_STR = "\t\t\tSTORM RELATIVE    \r\n";
	// SR header strings
	public static final String STORM_HELICITY_STR = "\t\t------ SR HELICITY ------\r\n";
	public static final String STORM_WIND_STR = "\r\n \t\t------ SR WINDS ------\r\n";
	//SR Helicity line strings
	public static final String STORM_MOTION_LINE = "%3.0f%c  / %3.0f kt        ( %.0f m/s )\r\n";
	public static final String STORM_MOTION_MISSING = "M /  M    (M)\r\n";
	public static final String STORM_LAYER_POS_STR = "LAYER_POS_NEG_TOT\r\n";
	public static final String STORM_SFC2KM_LINE = "Sfc - 2 km_%.0f_%4.0f_%.0f m%c/s%c\r\n";
	public static final String STORM_SFC3KM_LINE = "Sfc - 3 km_%.0f_%4.0f_%.0f m%c/s%c\r\n";
	public static final String STORM_LPL_LFC_LINE = "LPL - LFC_%.0f_%4.0f_%.0f m%c/s%c\r\n";
	public static final String STORM_SFC3KM_MISSING = "Sfc - 3 km_M  _M   _M\r\n";
	public static final String STORM_SFC2KM_MISSING = "Sfc - 2 km_M  _M   _M\r\n";
	public static final String STORM_LPL_LFC_MISSING= "LPL - LFC _M  _M   _M\r\n";
	public static final String STORM_LAYER_VECTOR_STR = "LAYER_VECTOR\r\n";
	public static final String STORM_SFC2KM_VECT_LINE =    "Sfc - 2 km_%3.0f /  %3.0f kt  (%3.0f m/s)\r\n";
	public static final String STORM_SFC2KM_VECT_MISSING = "Sfc - 2 km_  M /  M   (M)\r\n";
	public static final String STORM_4_6KM_VECT_LINE = "4 - 6 km  _%3.0f /  %3.0f kt  (%3.0f m/s)\r\n";
	public static final String STORM_4_6KM_VECT_MISSING = "4 - 6 km_  M /  M   (M)\r\n";
	public static final String STORM_9_11KM_VECT_LINE = "9 - 11 km_%3.0f /  %3.0f kt  (%3.0f m/s)\r\n";
	public static final String STORM_9_11KM_VECT_MISSING = "9 - 11 km_  M /  M   (M)\r\n";
	
	/*
	 * MEAN WIND data string
	 */
	public static final String MEAN_WIND_STR = "\t\t\t\tMEAN WIND    \r\n";
	//Mean Wind line strings
	public static final String MEANWIND_SFC6KM_LINE =    "Sfc - 6 km_%3.0f /\t%3.0f kt\t(%.0f m/s)\r\n";
	public static final String MEANWIND_LFC_EL_LINE =    "LFC - EL_%3.0f /\t%3.0f kt\t(%.0f m/s)\r\n";
	public static final String MEANWIND_850_200MB_LINE = "850 - 200 mb_%3.0f /\t%3.0f kt\t(%.0f m/s)\r\n";
	public static final String MEANWIND_SFC6KM_MISSING = "Sfc - 6 km_M   / \tM \t(M)\r\n";
	public static final String MEANWIND_LFC_EL_MISSING = "LFC - EL_M   / \tM \t(M)\r\n";
	public static final String MEANWIND_850_200MB_MISSING = "850 - 200 mb_M   / \tM \t(M)\r\n";

	/*
	 * ENVIRONMENTAL SHEAR data string
	 */
	public static final String ENVIRONMENTAL_SHEAR_STR = "\t\t\tENVIRONMENTAL SHEAR    \r\n";
	//ENVIRONMENTAL SHEAR line strings
	public static final String SHEAR_LAYER_DELTA_STR = "LAYER_DELTA V_TOT SHR\r\n";
	public static final String SHEAR_LOW_3KM_LINE =    "Low - 3 km_%3.0f kt (%3.0f m/s)_%.0f\r\n";
	public static final String SHEAR_LOW_3KM_MISSING = "Low - 3 km_M (   M   )_M\r\n";
	public static final String SHEAR_SFC_2KM_LINE = "Sfc - 2 km_%3.0f kt (%3.0f m/s)_%.0f\r\n";
	public static final String SHEAR_SFC_2KM_MISSING = "Sfc - 2 km_M (   M   )_M\r\n";
	public static final String SHEAR_SFC_6KM_LINE = "Sfc - 6 km_%3.0f kt (%3.0f m/s)_%.0f\r\n";
	public static final String SHEAR_SFC_6KM_MISSING = "Sfc - 6 km_M (   M   )_M\r\n";
	public static final String SHEAR_SFC_12KM_LINE = "Sfc - 12 km_%3.0f kt (%3.0f m/s)_%.0f\r\n";
	public static final String SHEAR_SFC_12KM_MISSING = "Sfc - 12 km_M  (   M   )  _M\r\n";
	
	/*
	 * CONVECTIVE INITIATION data strings
	 */
	public static final String CONVECTIVE_INITIATION_STR = "\t\t\t\tCONVECTIVE INITIATION    \r\n";
	public static final String CONVECTIVE_CINH_LINE =   "CINH_=    %6.0f J/Kg";
	public static final String CONVECTIVE_KINDEX_LINE = "K-Index_=    %6.0f";
	public static final String CONVECTIVE_CINH_MISSING =   "CINH_=    M";
	public static final String CONVECTIVE_KINDEX_MISSING = "K-Index_=    M";
	public static final String CONVECTIVE_CAP_LINE =    "Cap_=    %.0fC / %.0fmb\n";
	public static final String CONVECTIVE_MEANRH_LINE = "Mean RH_=    %.0f %c";
	public static final String CONVECTIVE_CAP_MISSING =    "Cap_=    M / M\r\n";
	public static final String CONVECTIVE_MEANRH_MISSING = "Mean RH_=    M\r\n";
	public static final String CONVECTIVE_TOP_LINE =    "Top of Moist Lyr  =    %.0f mb / %.0f ft\r\n";
	public static final String CONVECTIVE_TOP_MISSING = "Top of Moist Lyr  =    M / M\r\n";
	public static final String CONVECTIVE_LFC_LINE =    "LFC Height         =    %.0f mb / %.0f ft\r\n";
	public static final String CONVECTIVE_LFC_MISSING = "LFC Height         =    M / M\r\n";

	/*
	 * STORM TYPE data strings
	 */
	public static final String STORM_TYPE_STR = "\t\t\t\t    STORM TYPE    \r\n";
	public static final String STORM_TYPE_CAPE_LINE = "CAPE _=    %6.0f  J/Kg\t";
	public static final String STORM_TYPE_EHI_LINE =  "EHI  _=    %6.1f     \t";
	public static final String STORM_TYPE_BRN_LINE =     "BRN  _=    %6.0f     \t";
	public static final String STORM_TYPE_CAPE_MISSING = "CAPE _=        M";
	public static final String STORM_TYPE_EHI_MISSING =  "EHI  _=        M";
	public static final String STORM_TYPE_BRN_MISSING =  "BRN  _=        M";
	public static final String STORM_TYPE_EFF_LINE = "Eff. SREH _=    %.0f m%c/s%c\r\n";
	public static final String STORM_TYPE_EFF_MISSING = "Eff. SREH _=    M\r\n";
	public static final String STORM_TYPE_3KM_LINE = "3km Shear _=    %.0f m/s\r\n";
	public static final String STORM_TYPE_3KM_MISSING = "3km Shear _=    M\r\n";
	public static final String STORM_TYPE_BRNSHEAR_LINE = "BRN Shear _=    %.0f m%c/s%c\r\n";
	public static final String STORM_TYPE_BRNSHEAR_MISSING = "BRN Shear _=    M\r\n";
	/*
	 * SEVERE POTENTIAL data strings
	 */
	public static final String SEVERE_POTENTIAL_STR = "\t\t\t\tSEVERE POTENTIAL    \r\n";
	// SEVERE POTENTIAL HEADER STR
	public static final String SEVERE_HAIL_POTENTIAL_STR = "\t\t------ HAIL POTENTIAL ------\r\n";
	public static final String SEVERE_TORNADO_POTENTIAL_STR = "\t\t------ TORNADO POTENTIAL ------\r\n";
	// SEVERE POTENTIAL LINE STRING
	public static final String SEVERE_CAPE_LINE =  "CAPE_=    %5.0f J/Kg";
	public static final String SEVERE_MIDRH_LINE = "Mid Lvl RH_=    %5.0f %c";
	public static final String SEVERE_CHI1_LINE =  "CHI1_=    %5.1f     ";
	public static final String SEVERE_CAPE_MISSING =  "CAPE_=        M";
	public static final String SEVERE_MIDRH_MISSING = "Mid Lvl RH_=        M";
	public static final String SEVERE_CHI1_MISSING =  "CHI1_=        M";
	public static final String SEVERE_WBZ_LINE = "WBZ level_=  %.0fft\r\n";
	public static final String SEVERE_FGZ_LINE = "FGZ level_=  %.0fft\r\n";
	public static final String SEVERE_CHI2_LINE ="CHI2_=  %.1f\r\n";
	public static final String SEVERE_WBZ_MISSING = "WBZ level_=  M\r\n";
	public static final String SEVERE_FGZ_MISSING = "FGZ level_=  M\r\n";
	public static final String SEVERE_CHI2_MISSING ="CHI2_=  M\r\n";
	public static final String SEVERE_ELSTORM_LINE = "EL Storm Relative Wind Speed =    %.0f kt \r\n";
	public static final String SEVERE_ELSTORM_MISSING = "EL Storm Relative Wind Speed =    M \r\n";
	public static final String SEVERE_AVGBL_LINE = "Avg BL Wetbulb Temp =    %.1f %cC \r\n";
	public static final String SEVERE_AVGBL_MISSING = "Avg BL Wetbulb Temp =    M \r\n";
	public static final String SEVERE_LOWSRWSFC_LINE =    "Low SRW (Sfc - LFC)     _=    %.0f kt\r\n";
	public static final String SEVERE_MIDSRW_LINE =       "Mid SRW (LFC - LFC+4km) _=    %.0f kt\r\n";
	public static final String SEVERE_LOWSRWEL_LINE =     "Low SRW (EL-4km - EL)   _=    %.0f kt\r\n";
	public static final String SEVERE_LOWSRWSFC_MISSING = "Low SRW (Sfc - LFC)     _=    M \r\n";
	public static final String SEVERE_MIDSRW_MISSING =    "Mid SRW (LFC - LFC+4km) _=    M \r\n";
	public static final String SEVERE_LOWSRWEL_MISSING =  "Low SRW (EL-4km - EL)   _=    M \r\n";

	/*
	 * PRECIPITATION TYPE data strings
	 */
	public static final String PRECIPITATION_TYPE_STR = "\t\t\t\t    PRECIPITATION TYPE    \r\n";
	public static final String PRECIPITATION_MELTING_LINE = "Melting Level =    %.0f ft / %.0f mb\r\n";
	public static final String PRECIPITATION_MELTING_MISSING = "Melting Level =    M \r\n";

	/*
	 * HEAVY RAINFALL data strings
	 */
	public static final String HEAVY_RAINFALL_STR = "\t\t\t\t    HEAVY RAINFALL    \r\n";
	// change Rogash QPF to Rainfall Rate (TL)
	public static final String HEAVY_ROGASH_LINE = "Rogash Rainfall Rate =    %.2f in/hr\r\n";
	public static final String HEAVY_ROGASH_MISSING = "Rogash Rainfall Rate =    M \r\n";

	
	// use parcel type to retrieve parcel header string for display
	public static final Map<Short, String> parcelToHdrStrMap = new HashMap<Short, String>(){
		private static final long serialVersionUID = 1L;

		{
            put(NsharpNativeConstants.PARCELTYPE_OBS_SFC, PARCEL_OBS_SFC_STR);
            put(NsharpNativeConstants.PARCELTYPE_FCST_SFC, PARCEL_FORECAST_SFC_STR);
            put(NsharpNativeConstants.PARCELTYPE_MEAN_MIXING, PARCEL_MEAN_MIXING_STR);
            put(NsharpNativeConstants.PARCELTYPE_MOST_UNSTABLE, PARCEL_MOST_UNSTABLE_STR);
            put(NsharpNativeConstants.PARCELTYPE_USER_DEFINED, PARCEL_USR_DEFINED_STR);
            put(NsharpNativeConstants.PARCELTYPE_EFF, PARCEL_MEAN_EFF_STR);
        }
    };
    
    public static final Map<Short, String> parcelToTypeStrMap = new HashMap<Short, String>(){
 		private static final long serialVersionUID = 1L;

		{
            put(NsharpNativeConstants.PARCELTYPE_OBS_SFC, PAGE1TEXT1_SB_STR);
            put(NsharpNativeConstants.PARCELTYPE_FCST_SFC, PAGE1TEXT1_FCST_STR);
            put(NsharpNativeConstants.PARCELTYPE_MEAN_MIXING, PAGE1TEXT1_ML_STR);
            put(NsharpNativeConstants.PARCELTYPE_MOST_UNSTABLE, PAGE1TEXT1_MU_STR);
            put(NsharpNativeConstants.PARCELTYPE_USER_DEFINED, PAGE1TEXT1_USER_STR);
            put(NsharpNativeConstants.PARCELTYPE_EFF,PAGE1TEXT1_EFF_STR );
        }
    };
    
	public static final Map<Short, Float> parcelToLayerMap = new HashMap<Short, Float>(){
		private static final long serialVersionUID = 1L;

		{
            put(NsharpNativeConstants.PARCELTYPE_OBS_SFC, OBS_LAYER);
            put(NsharpNativeConstants.PARCELTYPE_FCST_SFC, FCST_LAYER);
            put(NsharpNativeConstants.PARCELTYPE_MEAN_MIXING, MML_LAYER);
            put(NsharpNativeConstants.PARCELTYPE_MOST_UNSTABLE, MU_LAYER);
            put(NsharpNativeConstants.PARCELTYPE_USER_DEFINED, USER_LAYER);
            put(NsharpNativeConstants.PARCELTYPE_EFF, EFF_LAYER);
        }
    };
	public static final String TEMP_TRACE = "Temperature Trace"; 
	public static final String DEWP_TRACE = "Dewpoint Trace";
	public static final String PARCEL_VT_TRACE= "Parcel Tv Trace";
	public static final String PARCEL_T_TRACE= "Parcel Trace";
	public static final String DCAPE_TRACE = "DCAPE Trace";
	public static final String VTEMP_TRACE = "Virtual Temperature Trace";
	public static final String WETBULB_TRACE=  "WetbulbTrace";
	public static final String MIXING_RATIO=  "Mixing Ratio";
	public static final String DRY_ADIABAT=  "Dry Adiabat";
	public static final String MOIST_ADIABAT=  "Moist Adiabat";
	public static final String OMEGA =  "Omega";
	public static final String MEAN_WIND_VECTOR =  "Mean Wind Vector";
	public static final String STORM_MOTION_VECTOR_3075 = "30/75 Storm Motion Vector";
	public static final String STORM_MOTION_VECTOR_1585 = "15/85 Storm Motion Vector";
	public static final String STORM_MOTION_VECTOR_BUNKERS_R = "Bunkers R Storm Motion Vector";
	public static final String STORM_MOTION_VECTOR_BUNKERS_L = "Bunkers L Storm Motion Vector";
	public static final String CORFIDI_VECTORS = "Corfidi Vectors";
	public static final String HODOGRAPH = "Hodograph Trace";
	public static final String EFFECTIVE_LAYER = "Effective Layer";
	public static final String CLOUD = "Cloud";
	public static final String WINDBARB="Wind Barb";
	public static final float ENTRAIN_DEFAULT = 0.0f;
}
