package gov.noaa.nws.ncep.viz.localization.impl;

import java.io.File;

public class LocalizationResourcePathConstants {
	/*
	 * Some of these directories are not necessarily 'resource' 
	 */
	public static final String SPF_GROUPS_DIR = "resources"+File.separator+"spfs"; 
	public static final String RESOURCES_BASE_DIR = "resources"; 
//	public static final String RESOURCE_DEFNS_DIR = "resources"+File.separator+"resourceDefns"; 
//	public static final String RESOURCES_IMPLS_DIR = "resources"+File.separator+"resourcesImpls"; 
//	public static final String BNDL_TEMPLATES_DIR = 
//                     "resources"+File.separator+"resourcesImpls"+File.separator+"bundleTemplates";  
	public static final String OVERLAY_DATA_DIR = 
		             "resources"+File.separator+"resourceImpls"+File.separator+"overlayData";  
	public static final String PREDEFINED_AREAS_DIR = "resources"+File.separator+"predefinedAreas";
	public static final String STATION_RESOURCES_DIR = "stns";
	public static final String PLOTMODELS_DIR = "plotModels";
	public static final String PLOTMODELPARAMETERS_DIR = "plotModels"+File.separator+"plotParameters"; 
	public static final String CURSOR_IMAGES_DIR = "cursor"+File.separator+"images"; 
	public static final String LOGO_IMAGES_DIR = "logo"+File.separator+"images"; 
	public static final String PROJECTION_RESOURCES_DIR = "resources"+File.separator+"projection"; 
	public static final String LUTS_RESOURCES_DIR = "luts";
	public static final String RADAR_RESOURCES_DIR = "radar";
//	public static final String SPF_GROUPS_DIR = "resources"+File.separator+"spfs"; 
//	public static final String SPF_RESOURCES_DIR = "resources"+File.separator+"spfs"; 
}
