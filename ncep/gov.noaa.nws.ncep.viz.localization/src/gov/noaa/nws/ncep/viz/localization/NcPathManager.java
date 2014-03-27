package gov.noaa.nws.ncep.viz.localization;

import java.io.File;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.viz.core.localization.CAVELocalizationAdapter;
import com.raytheon.uf.viz.core.localization.LocalizationManager;


/**
 * A Facade over the PathManager. This was initially created to create a pathMngr with
 * a NatlCntrsLocalizationAdapter but now is just a convenience wrapper around the 
 * same PathManager as the rest of CAVE.
 *
 Would it be ok/better to derive from PathManager directly and bypass the PathManagerFactory?
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 07/22/11       #450      Greg Hull    Created
 * 12/16/11	      #579      J. Zeng      Add PGENXMLOverlay path
 * 12/19/11                 Q.Zhou       Added PGEN_FILTER_HOUR
 * 03/06/11       #707      Q.Zhou       Added PGEN_FORECASTER
 * 03/15/2012     #621      S. Gurung    Added LOCKED_CMAP_TBL
 * 04/12/2012     #615      S. Gurung    Added CONDITIONAL_FILTERS_DIR, CONDITIONAL_FILTER_HELP_FILE, 
 * 										 CONDITIONAL_FILTER_MINUS_IMG and CONDITIONAL_FILTER_PLUS_IMG
 * 05/07/2012     #615      S.Gurung     Modified constants CONDITIONAL_FILTER_HELP_FILE,CONDITIONAL_FILTER_MINUS_IMG
 *                                       and CONDITIONAL_FILTER_PLUS_IMG
 * 05/24/2012     #606      Greg Hull    Added NCINVENTORY_DEFINITIONS_DIR
 * 06/01/2012     #815      Greg Hull    rm NatlCntrsLocalizationAdapter and use CAVELocalizationAdapter
 * 06/07/2012     #717      Archana      Added the constants STYLE_RULES_DIR,
 *                                       MCIDAS_IMG_STYLE_RULES and GINI_IMG_STYLE_RULES                                 
 * 06/21/2012     #825      Greg Hull    rm mosaicInfo.txt
 * 10/18/2012     #431      S. Gurung    Added constant ADVANCED_ICON_IMG
 * 12/17/2012     #861      Greg Hull    rm PGEN_XML_OVERLAYS
 * 02/22/2013     #972      Greg Hull    add DFLT_NTRANS_RBD, DFLT_SOLAR_RBD
 * 02/26/2013     #936      Archana      Added FONT_FILES_DIR  
 * 04/15/2013     #864      Greg Hull    RESOURCE_FILTERS
 * 04/17/2013     #863      Greg Hull    AREA_MENUS_FILE    
 * 04/10/2013     #958      qzhou        Added SOLAR_IMG_STYLE_RULES
 * 05/15/2013     #862      Greg Hull    AreaMenus tbl to xml
 * 11/15/2013     #1051     Greg Hull    createDeskLevel() called from NmapCommon and triggered by spring.
 * 
 * </pre>
 * 
 * @author ghull 
 * @version 1
 */
public class NcPathManager {

	private static NcPathManager ncPathMngr = null;
	private static IPathManager pathMngr = null;

	// we could instead read from the extension point to find the relative path for a 'type' of file.
	// 	
	public static class NcPathConstants {

		public static final String DESK_LEVEL  = "DESK";
		
		// the root of NCEP file hierarchy (below base/user/site/desk)
		public static final String NCEP_ROOT   = "ncep"+File.separator;

		// Note that these files are in STATIC_COMMON  
		public static final String NCINVENTORY_DEFNS_DIR = NCEP_ROOT + "NcInventoryDefinitions";

		// static directories. 
		public static final String SPFS_DIR            = NCEP_ROOT + "SPFs"; // the Groups dir
		public static final String RSC_TMPLTS_DIR      = NCEP_ROOT + "resourceTemplates";
		public static final String RSC_DEFNS_DIR       = NCEP_ROOT + "ResourceDefns";
		public static final String ATTR_SET_GROUPS_DIR = NCEP_ROOT + "AttributeSetGroups";
		public static final String PREDEFINED_AREAS_DIR= NCEP_ROOT + "PredefinedAreas";
		public static final String COLORBARS_DIR       = NCEP_ROOT + "ColorBars";
		public static final String COLORMAPS_DIR       = NCEP_ROOT + "ColorMaps";
		public static final String STATIONS_DIR        = NCEP_ROOT + "Stations";
		public static final String LOGOS_DIR           = NCEP_ROOT + "Logos";
		public static final String CURSORS_DIR         = NCEP_ROOT + "Cursors";
		public static final String PLOT_MODELS_DIR     = NCEP_ROOT + "PlotModels";
		public static final String PLOT_PARAMETERS_DIR = PLOT_MODELS_DIR + File.separator+"PlotParameters";
		public static final String FONT_FILES_DIR      = NCEP_ROOT + "fontFiles" + File.separator ;
		public static final String LOCATOR_SOURCE_DIR  = NCEP_ROOT+"LocatorDataSources";

		public static final String RESOURCE_FILTERS    = RSC_DEFNS_DIR+ File.separator+"ResourceFilters.xml";
		public static final String AREA_MENUS_FILE     = PREDEFINED_AREAS_DIR + File.separator+
														   "menus"+ File.separator+"AreaMenus.xml";
		
// No longer used. location is now a parameter for the StaticPgenOverlayResource.
//		public static final String PGEN_XML_OVERLAYS   = NCEP_ROOT + "StaticPgenOverlays";
		public static final String STYLE_RULES_DIR = NCEP_ROOT + "styleRules" + File.separator;
		// lpi,spi files for overlays
		public static final String BASEMAPS_DIR        = NCEP_ROOT + "basemaps";
		
		// static files
		public static final String DFLT_RBD        = NCEP_ROOT + "DefaultRBDs" + File.separator+"defaultRBD.xml";
		public static final String DFLT_NTRANS_RBD = NCEP_ROOT + "DefaultRBDs" + File.separator+"defaultNTransRBD.xml";
		public static final String DFLT_SOLAR_RBD   = NCEP_ROOT + "DefaultRBDs" + File.separator+"defaultSolarRBD.xml";
		
		public static final String LOCATOR_TBL   = NCEP_ROOT+"Locator"+File.separator+"locator_tbl.xml";
	    public static final String LOGOS_TBL     = NCEP_ROOT + "Logos" + File.separator + "logos.tbl";
		public static final String LOOP_SPEEDS_TBL= NCEP_ROOT + "LoopControls"+
															File.separator+"loopSpeeds.tbl";
		public static final String LOCKED_CMAP_TBL = NcPathConstants.COLORMAPS_DIR + File.separator + "lockedColorMaps.tbl";
		public static final String CURSOR_REFS_TBL= CURSORS_DIR + File.separator + "cursorref_tbl.xml";

		public static final String CLOUD_HEIGHT_SOUNDING_MODELS = 
			                              NCEP_ROOT+"CloudHeight"+File.separator+"SoundingModels.xml";
		
		public static final String GEOG_TBL = PREDEFINED_AREAS_DIR + File.separator+
												     "gempak"+File.separator+"geog.xml";
		public static final String SFSTNS_TBL = STATIONS_DIR+File.separator+"sfstns.xml";
		
		public static final String CONDITIONAL_FILTERS_DIR      = PLOT_MODELS_DIR + File.separator+"ConditionalFilters";
		public static final String CONDITIONAL_FILTER_HELP_FILE = NCEP_ROOT + File.separator + "conditionalFilter" + File.separator + "ConditionalFilterHelp.txt";
		public static final String CONDITIONAL_FILTER_MINUS_IMG = NCEP_ROOT + File.separator + "conditionalFilter" + File.separator + "minus_red.gif";
		public static final String CONDITIONAL_FILTER_PLUS_IMG  = NCEP_ROOT + File.separator + "conditionalFilter" + File.separator + "plus_green.gif";
		public static final String ADVANCED_ICON_IMG  = NCEP_ROOT + File.separator + "advanced" + File.separator + "adv_icon.jpg";
		
		// migrating code which looked for these filenames
		public static final String VORS_STN_TBL  = STATIONS_DIR + File.separator+"vors.xml";
	    public static final String VOLCANO_STN_TBL  = STATIONS_DIR + File.separator+"volcano.xml";
	    public static final String COUNTY_STN_TBL  = STATIONS_DIR + File.separator+ "county.xml";
	    public static final String FFG_ZONES_STN_TBL  = STATIONS_DIR + File.separator+"ffgZones.xml";
	    public static final String SPCWATCH_STN_TBL  = STATIONS_DIR + File.separator+"spcwatch.xml";
	    public static final String SHAPEFILES_DIR  = NCEP_ROOT + "Shapefiles";

	    public static final String SEEK_STN_TBL  = NCEP_ROOT + "Seek"+File.separator+"seekStns.xml";

//	    public static final String GRID_DATATYPE_TBL  = NCEP_ROOT + "grid"+File.separator+"datatype.tbl";
//	    public static final String ENSEMBLE_MODELS_TBL= NCEP_ROOT + "grid"+File.separator+"ensemble_models.tbl";
	    public static final String GEMPAK_MARKER_TYPE = NCEP_ROOT + "Gempak"+File.separator+"gempakMarkerType.tbl";
	    
	    // Note: These are read by Raytheon's code which just takes the directory as input and 
	    // assumes the filename. So don't change the fileNames. 
	    // the files.
	    public static final String RADAR_INFO   = NCEP_ROOT + "Radar"+File.separator+"radarInfo.txt";
//	    public static final String MOSAIC_INFO  = NCEP_ROOT + "Radar"+File.separator+"mosaicInfo.txt";
	    public static final String MCIDAS_IMG_STYLE_RULES = STYLE_RULES_DIR + "mcidasSatelliteImageryStyleRules.xml";
	    public static final String GINI_IMG_STYLE_RULES = STYLE_RULES_DIR + "giniSatelliteImageryStyleRules.xml";	
	    public static final String SOLAR_IMG_STYLE_RULES = STYLE_RULES_DIR + "solarImageryStyleRules.xml";
	    // PGEN Files 
	    public static final String PGEN_ROOT            = NCEP_ROOT + "pgen"+File.separator;
	    public static final String PGEN_SETTINGS_TBL    = PGEN_ROOT + "settings_tbl.xml";
	    public static final String PGEN_LINE_PATTERNS   = PGEN_ROOT + "linePatterns.xml";
	    public static final String PGEN_SYMBOL_PATTERNS = PGEN_ROOT + "symbolPatterns.xml";
	    public static final String PGEN_PRODUCT_TYPES   = PGEN_ROOT + "productTypes.xml";
	    public static final String PGEN_HELP_FILE       = PGEN_ROOT + "PgenHelp.txt";
	    public static final String PGEN_TCA_ATTR_INFO   = PGEN_ROOT + "TCAinfo.xml";
	    public static final String PGEN_GFA_ATTR_FILE   = PGEN_ROOT + "gfa.xml";	    
	    public static final String PGEN_GFA_PROD_XSL    = PGEN_ROOT + "xslt"+File.separator+
	    													"airmet"+File.separator+"gfa_product.xsl";
	    public static final String PGEN_AIRMET_CYCLE_TBL= PGEN_ROOT + "airmetcycle.xml";
	    public static final String PGEN_ISLND_BRKPTS_TBL= PGEN_ROOT + "IslandBreakpoints.xml";
	    public static final String PGEN_WATER_BRKPTS_TBL= PGEN_ROOT + "WaterBreakpoints.xml";
	    public static final String PGEN_COAST_BRKPTS_TBL= PGEN_ROOT + "CoastBreakpoints.xml";
	    public static final String PGEN_SPC_ANCHOR_TBL  = SPCWATCH_STN_TBL;
	    public static final String PGEN_FIR_BOUNDS      = SHAPEFILES_DIR +File.separator+ "firbnds"+File.separator+"firbnds.shp";
	    public static final String PGEN_CCFP_TIMES      = PGEN_ROOT + "ccfpTimes.xml";
	    public static final String PGEN_CCFP_XSLT       = PGEN_ROOT + "xslt"+File.separator+
	     													"ccfp"+File.separator+"ccfpXml2Txt.xslt";
	    public static final String PGEN_VAA_FILE        = PGEN_ROOT + "vaa.xml";
	    public static final String PGEN_VAA_XSLT       = PGEN_ROOT + "xslt"+File.separator+
															"vaa"+File.separator+"vaaXml2Txt.xslt";
	    public static final String PGEN_PHENOMENONS     = PGEN_ROOT + "phenomenons.xml";
	    
	    // 
	    public static final String PGEN_PROD_SCHEMA     = PGEN_ROOT + "product.xsd";
	    public static final String PGEN_RED_CROSS_IMG   = PGEN_ROOT + "red_cross.png";
	    public static final String PGEN_OUTLOOK_TYPE    = PGEN_ROOT + "outlooktype.xml";
	    public static final String PGEN_OUTLOOK_SETTINGS= PGEN_ROOT + "outlooksettings.xml";
	    public static final String PGEN_OUTLOOK_TIMES   = PGEN_ROOT + "outlooktimes.xml";
	    public static final String PGEN_MNTN_OBSC_STATES= PGEN_ROOT + "mt_obsc_states.xml";
	    public static final String PGEN_CONTOURS_INFO   = PGEN_ROOT + "contoursInfo.xml";
	    public static final String PGEN_G2G_GRPHGD      = PGEN_ROOT + "grphgd.tbl";	    
	    public static final String PGEN_FILTER_HOUR     = PGEN_ROOT + "filterHour.xml";
	    public static final String PGEN_FORECASTER      = PGEN_ROOT + "forecasters.xml";
	    
	    public static final String NSHARP_NLIST_FILE    = NCEP_ROOT + "nsharp"+File.separator+"nlist.txt";
	    public static final String NSHARP_SUP_FILE      = NCEP_ROOT + "nsharp"+File.separator+"sup.txt";    
		//nsharp configuration
		public static final String NSHARP_CONFIG        = NCEP_ROOT + "nsharp"+ File.separator+"nsharpConfig.xml";

	}
	
	public static synchronized NcPathManager getInstance() {
		if( ncPathMngr == null ) {
			ncPathMngr = new NcPathManager();
		}
		return ncPathMngr;
	}
	
	private NcPathManager() {
		
		// Uses the same CAVELocalizationAdapter.
		pathMngr = PathManagerFactory.getPathManager( new CAVELocalizationAdapter() );
		
	}
	
	public void createDeskLevelLocalization( String deskName ) {
		// SITE < DESK < USER
		
		// NOTE : order of 650 is between SITE(order=500) and USER(order=1000). 
		LocalizationLevel DESK = LocalizationLevel.createLevel(
				NcPathConstants.DESK_LEVEL, 650 ); 

		// sanity check to make sure the order is correct
		//
		if( LocalizationLevel.SITE.compareTo( DESK ) >= 0 ) {
			System.out.println("WARNING: the SITE level order >= the DESK???? ");
		}
		if( LocalizationLevel.USER.compareTo( DESK ) <= 0 ) {
			System.out.println("WARNING: the USER level order <= the DESK???? ");
		}

		LocalizationManager.getInstance();

		LocalizationManager.registerContextName( DESK, deskName );		
	}
	
	public LocalizationLevel getDeskLevel() {
		return LocalizationLevel.valueOf( NcPathConstants.DESK_LEVEL );
	}
	
	public LocalizationContext getDeskContext() {
		return getContext( LocalizationType.CAVE_STATIC, getDeskLevel() );
	}

	// same thing as calling PathManagerFactory.getPathManager();
	public IPathManager getPathManager() {
		return pathMngr;		
	}
	
	// Use this method if we don't care or need to know which context the file comes from.
	public File getStaticFile( String fname ) {
		return pathMngr.getStaticFile( fname );
	}

	// Use this method if we don't care which context the file comes from but we
	// need to know which one it was from.
	//
	public LocalizationFile getStaticLocalizationFile( String name ) {
		return pathMngr.getStaticLocalizationFile( name );
	}
	
    // This can be used to create a new LocalizationFile.
    // 
    public LocalizationFile getLocalizationFile( LocalizationContext context,
            String name) {
    	return pathMngr.getLocalizationFile( context, name);
    }
    
    // only include 1 version of each filename. Assume CAVE_STATIC (can change this later)
    // 
    public Map<String, LocalizationFile>  listFiles( 
            String name, String[] filter, boolean recursive, boolean filesOnly ) {
    	LocalizationContext[] contexts = getLocalSearchHierarchy( LocalizationType.CAVE_STATIC );
    	
    	return listFiles( contexts, name, filter, recursive, filesOnly );
    }

    // created to allow listFiles for COMMON_STATIC contexts
    //
    public Map<String, LocalizationFile>  listFiles( LocalizationContext[] contexts,
            String name, String[] filter, boolean recursive, boolean filesOnly ) {
 
        Map<String, LocalizationFile> lFileMap = new HashMap<String, LocalizationFile>();

    	List<LocalizationFile> lFilesList = 
    			Arrays.asList(
    					pathMngr.listFiles( contexts, name, filter, recursive, filesOnly ) );

    	//  loop thru the files and add them to the map if there is not already a file
    	//  present from a higher level.
    	//
    	for( LocalizationFile lFile : lFilesList ) {
    		String lName = lFile.getName();
    		LocalizationLevel lLvl = lFile.getContext().getLocalizationLevel();
    		
    		if( !lFileMap.containsKey( lName ) ||
    			(lFileMap.get( lName ).getContext().getLocalizationLevel().compareTo( lLvl ) < 0)  ) { 
    			//System.out.println("listFiles "+lFile.getFile().getAbsolutePath());
    			lFileMap.put( lFile.getName(), lFile );
    		}
    		
    	}
    	
    	return lFileMap;
	}

    // convienence method to get all the versions of a file. Assume CAVE_STATIC.
    public Map<LocalizationLevel, LocalizationFile> getTieredLocalizationFile( String name) {
    	return pathMngr.getTieredLocalizationFile( LocalizationType.CAVE_STATIC, name );
	}
	
    public LocalizationContext getContext( LocalizationType type,
            LocalizationLevel level) {
        return pathMngr.getContext(type, level);
    }

    public String[] getContextList(LocalizationLevel level) {
    	return pathMngr.getContextList(level);
    }
    
    public LocalizationContext[] getLocalSearchHierarchy(LocalizationType type) {
        return pathMngr.getLocalSearchHierarchy( type );
    }

    // return a map of all files from all contexts
    // this would make it easier to 'revert' files
    // 
//    public Map<String, LocalizationFile> listFilesFromAllContexts(  
//            // getTieredLocalizationFile(
//            // LocalizationType type, 
//            String name, String[] filter, boolean recursive, boolean filesOnly ) {
//        Map<String, LocalizationFile> map = new HashMap<String, LocalizationFile>();
//
//        for( LocalizationLevel level : ncLevels ) {
//            LocalizationContext context = getContext( LocalizationType.CAVE_STATIC, level );
//            
//            LocalizationFile[] lFiles = 
//            	 pathMngr.listFiles( getLocalSearchHierarchy( LocalizationType.CAVE_STATIC ), 
//            			 name, filter, recursive, filesOnly );
////            LocalizationFile lf = getLocalizationFile( context, name );
//            
//            if( lf.exists() ) {
//                map.put(, lf);
//            }
//        }
//
//        return map;
//    }

    
    // delete the file and also return a superceding file if one exists.
    // 
//    public LocalizationFile revert( File file, LocalizationContext context, String fileName, boolean findReplacement )
//            throws LocalizationOpFailedException {
//    	super.delete(file, context, fileName);
//    	
//    	if( findReplacement ) {
////    		LocalizationContext superContext = new LocalizationContext( 
////    				context.getLocalizationType(),  
////          LocalizationManager.getInstance().get    		
//    	}
//    	
//    	return null;
//    }

}
