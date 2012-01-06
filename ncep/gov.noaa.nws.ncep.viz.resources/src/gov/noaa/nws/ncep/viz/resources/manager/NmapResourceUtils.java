package gov.noaa.nws.ncep.viz.resources.manager;

import java.util.Arrays;
import java.util.Vector;
import java.io.File;
import java.io.FilenameFilter;

import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.viz.core.exception.VizException;

import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.localization.impl.LocalizationManager;
import gov.noaa.nws.ncep.viz.localization.impl.LocalizationResourcePathConstants;
import gov.noaa.nws.ncep.viz.ui.display.PredefinedArea;

/**
 * Common class for constants, utility methods ...
 *  * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 02/24/10		  #226		Greg Hull    Break out from NmapCommon
 * 03/04/10       #226      Greg Hull    special case for PGEN
 * 06/22/10       #273      Greg Hull    move some methods to ResourceDefnsMngr
 *
 * </pre>
 * 
 * @author 
 * @version 1
 */
public class NmapResourceUtils {	

	static File spfGroupsDir = null;
	static File predefinedAreasDir = null;
	
    // commands associated with GUI Elements that can be updated/refreshed.
    public final static String[] guiUpdateElementCommands = {
//    	"gov.noaa.nws.ncep.viz.tools.pan",
    	"gov.noaa.nws.ncep.viz.ui.options.SyncPanes",
    	"com.raytheon.viz.ui.tools.looping.loop"
    	// ? frameTool, looping
    };
    

	// make sure that the default map is first.
	// TODO : if it doesn't take too long; unmarshall the
	// jaxb files as a validation check.
	//
	public static String[] getAvailPredefinedAreas() {
		if( predefinedAreasDir == null || !predefinedAreasDir.exists() ) {
			/*
			 * Start of M. Gao's change
			 */
//			predefinedAreasDir = LocalizationManager.getInstance().getLocalizationFileDirectory(
//	   				LocalizationResourcePathConstants.PREDEFINED_AREAS_DIR, 
//	   				     LocalizationConstants.LOCALIZATION_BASE_LEVEL);	
			predefinedAreasDir = LocalizationManager.getInstance().getLocalizationFileDirectory(
	   				LocalizationResourcePathConstants.PREDEFINED_AREAS_DIR);	
			/*
			 * End of M. Gao's change
			 */
			if( predefinedAreasDir == null || !predefinedAreasDir.exists() ) {
				System.out.println("Error opening predefined Areas directory");
				predefinedAreasDir = null;
				return new String[]{};
			}
		}

//   		File map_dir = new File(getResoursePathByLoadingManager("mapResourcesDir"));
   		 
   		String xml_files[] = predefinedAreasDir.list( new FilenameFilter() {		
   			public boolean accept(File dir, String name) {
   				File f = new File( dir, name );
   				return (!f.isDirectory() && f.getName().endsWith(".xml") ? true : false );
   			}
   		});
   		
   		Arrays.sort( xml_files );

   		Vector<String> maps_vect = new Vector<String>();
   		maps_vect.add( NmapCommon.getDefaultMap() );

   		for( int i=0 ; i< xml_files.length ; i++ ) {
   			String map=xml_files[i];
   			map = map.substring(0, map.indexOf(".xml") );
   			
   			if( !map.equals( NmapCommon.getDefaultMap() ) ) {
   	   			maps_vect.add( map );   				
   			}
   		}
   		String[] maps_list = maps_vect.toArray(new String[0]);
   		// Arrays.sort( maps_list );
        // find the dflt map in the sorted list and set selected.
   		return maps_list;
   	}

//	static public File getPredefinedAreaFile( String predefinedArea ) {
//		
//	}
	
//	// it might be nice create a class specifically to store the predefined 
//	// area but for now this will just be the Display (we need the zoomLevel and
//	// mapCenter as well as the gridGeometry)
//	// 
	public static PredefinedArea getPredefinedArea( String areaName ) throws VizException {
		if( predefinedAreasDir == null )  {
			getAvailPredefinedAreas();
		}
		
   		//File areaFile new File( NmapCommon.getMapResourcesDir() + areaName + ".xml" );
		try {
			Object xmlObj = SerializationUtil.jaxbUnmarshalFromXmlFile( 
					predefinedAreasDir.getAbsolutePath()+ File.separator + areaName + ".xml" );

			if( !(xmlObj instanceof PredefinedArea) ) {				
   	   			VizException ve = new VizException( 
   	   					"Error reading PredefinedArea: "+areaName );
   	   			throw ve;
			}
			return (PredefinedArea)xmlObj;
			
		} catch (SerializationException e) {
			throw new VizException( e );
		}
	}
//	
    // return an array of all the sub directories in the spf groups dir.
    public static String[] getAvailSPFGroups() {
//   		File group_dir = new File( NmapCommon.getSPFGroupsDir() );  comment out by M. Gao
//   		File group_dir = new File(getResoursePathByLoadingManager("spfGroupsDir"));
    	if( spfGroupsDir == null ) {
			/*
			 * Start of M. Gao's change
			 */
//    		spfGroupsDir = LocalizationManager.getInstance().getLocalizationFileDirectory(
//    				LocalizationResourcePathConstants.SPF_GROUPS_DIR, 
//    				LocalizationConstants.LOCALIZATION_BASE_LEVEL);    	    
    		spfGroupsDir = LocalizationManager.getInstance().getLocalizationFileDirectory(
    				LocalizationResourcePathConstants.SPF_GROUPS_DIR);    	    
			/*
			 * Start of M. Gao's change
			 */

    		if( spfGroupsDir == null || !spfGroupsDir.exists() ) {
    			spfGroupsDir = null;
    			return new String[]{};
    		}
    	}

   		String[] avail_groups = spfGroupsDir.list( new  FilenameFilter() {
   			public boolean accept(File dir, String name) {
   				File f = new File( dir, name );
   				return ( f.isDirectory() && 
   					    !f.getAbsolutePath().contains(".svn") ? true : false );
   			}
   		});
   		
   		Arrays.sort( avail_groups );
   		return avail_groups;
   	}
    
    // return an array of all the spf (.xml) files in the given spf group dir.
    //
	public static String[] getSpfNamesForGroup( String spf_group ) {
   		if( spf_group == null || spf_group.isEmpty() ) {
   			return new String[0];
   		}
   		
   		File spfGrpDir = new File( spfGroupsDir, spf_group ); 
   		
   		if( spfGrpDir.exists() ) {
   			String spfNames[] = spfGrpDir.list( new FilenameFilter() {
   				public boolean accept( File dir, String name ) {
   					File f = new File( dir, name );
   					return f.isDirectory();
   				}
   			});

   			if( spfNames == null ) {
   				return new String[0];
   			}
   			 
   	   		Arrays.sort( spfNames );

   	   		return spfNames;
   		}
   		else {
   			return new String[0];
   		}
	}

	// return an array of all the rbd (.xml) files in the given spf dir.
    //
	public static String[] getRbdNamesForSPF( String seldSpfGroup, String seldSpfName ) {
   		File spfGrpDir = new File( spfGroupsDir, seldSpfGroup ); 
   		File spfDir = new File( spfGrpDir, seldSpfName );
			
		if( spfDir == null || !spfDir.isDirectory() ) {
			return new String[0];
		}

		String rbd_names[] = spfDir.list( new FilenameFilter() {
			public boolean accept( File dir, String name ) {
				File f = new File( dir, name );
				return (!f.isDirectory() && f.getName().endsWith(".xml") ? true : false );
			}
		});

		if( rbd_names == null ) {
			return new String[0];
		}

		Arrays.sort( rbd_names );
		Vector<String> maps_vect = new Vector<String>();

		for( int i=0 ; i< rbd_names.length ; i++ ) {
			rbd_names[i] = rbd_names[i].substring( 0, rbd_names[i].indexOf(".xml") );   			
		}
		return rbd_names;
	}    
    		
	static public File getSpfGroupsDir( ) {
		if( spfGroupsDir == null || !spfGroupsDir.exists() ) {
			getAvailSPFGroups();
		}
		return spfGroupsDir;
	}
    // all the overlay xml bundle files with the lat/lon at the top of the list
//    public static IStructuredContentProvider createOverlaysContentProvider() {
//    	return NmapCommon.createFileContentProvider( new String[]{".xml"}, 
//    			new Comparator<File>( ) {
//    		@Override
//    		public int compare( File f1, File f2 ) {
//    			if( f1.getName().equals("latlon.xml" ) ) {
//    				return -1;
//    			}
//    			else if( f2.getName().equals("latlon.xml" ) ) {
//    				return 1;
//    			} 
//    			else return f1.compareTo(f2);
//    		}
//    	});
//    }
    
//    public static String getResoursePathByLoadingManager(String rscPath) {
//    	return LocalizationManager.getInstance().getFilename(rscPath); 
//    }
}
