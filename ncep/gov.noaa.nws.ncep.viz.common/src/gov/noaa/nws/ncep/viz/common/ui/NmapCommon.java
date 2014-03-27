package gov.noaa.nws.ncep.viz.common.ui;

import gov.noaa.nws.ncep.viz.common.Activator;
import gov.noaa.nws.ncep.viz.common.preferences.NcepGeneralPreferencesPage;
import gov.noaa.nws.ncep.viz.common.preferences.NcepPreferences;
import gov.noaa.nws.ncep.viz.localization.NcPathManager;

import java.io.File;
import java.io.FileFilter;
import java.text.NumberFormat;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.TimeZone;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.Viewer;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.application.ProgramArguments;

/**
 * Common class for constants, utility methods ...
 *  * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 11/26/08		  #24		Greg Hull		Created		
 * 12/15/08       #43      	Greg Hull    	Added methods for Content & Label Providers and File Filters
 * 01/07/09       #22        M. Li		    Add getLocatorTable()
 * 03/12/09       #80       Greg Hull       Migrate to TO10
 * 03/26/09       #85       Greg Hull       moved parseRscAttrsFile from RBDMngr
 * 04/06/09       #22       J. Wu		    Add getSettingsTable()
 * 04/08/09       #83       Greg Hull       Add getCountyZonesFile() && filter out .svn directories
 * 05/18/09		  #108		M. Li			Add getDwellRateTable() 
 * 05/13/09       #42       S. Gilbert      Add getLinePatternsFile() and getSymbolPatternsFile()
 * 05/19/09       #106      Greg Hull       Added getSoundingModelsTable()
 * 06/01/09		  #105		M. Li			Added getLogoTable()
 * 06/17/09       #115      Greg Hull       remove parseRscAttrsFile and allow sort on contentProviders
 * 06/18/09		  #109		M. Li			Add getCursorRefTable & getCursorTypeTable
 * 07/31/09	                Greg Hull       Add getDefaultRBD()
 * 08/05/09                 Greg Hull       add getAttrSetFilename() to map attrSetNames to their files.
 * 08/17/09                 B. Hebbard      Add getStnsDir()
 * 09/22/09       #151      J. Wu		    Add getProductTypes()
 * 10/02/09       #151      J. Wu		    Add getPgenHelpFile()
 * 10/06/09       #169      Greg Hull       getGUIUpdateElementCommands()
 * 10/14/09       #170      M. Gao          Add getProjectionResourcesDir() and static String ProjectionRscDir
 * 10/26/09       #180      Greg Hull       SPF Groups Directory. Rm RBDGroupsDir, NatlCntrsPerspectiveID
 * 12/02/09                 Greg Hull       Overlays data directory.
 * 12/04/09                 Greg Hull       plotModels directory
 * 11/30/09       #197      Q.Zhou          Added LutsDir and getLutsDir() 
 * 12/05/09		  #159		B. Yin			added VorDir
 * 01/06/09       #217      Greg Hull       moved ceateQualifiedResourceName() here and added
 *                                          getAllResourcesForCategory()
 * 01/13/10		  #204		M. Li		    Add radarDir
 * 02/24/10       #226      Greg Hull       moved methods to NmapResourceUtils class                                         
 * 03/10/10       #228      Archana         Added the overloaded method createSubDirContentProvider()
 * 04/12/10		  #165		G. Zhang		Added the dirs and filenames for fir shapefile and vaa xslt file
 * 08/08/10       #273      G. Hull         Remove much commented out code. This class used to be a common place to
 *                                          access filenames/dirs for the system.
 * 09/09/10       #137      Q. Zhou         Added standalone condition to get spcAnchorTable for watch box     
 * 09/16/10       #307      G. Hull         added getCycleTimeStringFromDataTime()                 
 * 10/09/10       #137      Q. Zhou         Added standalone condition to get vaaXmlFile for volcano   
 * 10/20/10                 X. Guo          Rename getCycleTimeStringFromDataTime to getTimeStringFromDataTime
 * 11/29/10		  			m.gamazaychikov Added createDatatypeMap, dbtimeToFilename, getDatatypeTblColumnValue, 
 * 											getNavigationFromGempak, getAvailableTimesStringFromGempak, 
 * 											getCycleTimeStringFromGempak
 * 12/03/10		  			m.gamazaychikov Removed createDatatypeMap, dbtimeToFilename, getDatatypeTblColumnValue, 
 * 											getNavigationFromGempak, getAvailableTimesStringFromGempak, 
 * 											getCycleTimeStringFromGempak
 * 12/20/10       #137      Q. Zhou         Modified standalone condition for watch box & volcano
 * 07/28/11       #450      G. Hull         move pgen localization calls to pgen
 * 08/09/11       #450      G. Hull         get/set for pgen working directory.
 * 07/31/12       #631      G. Hull         getNcepPreferenceStore()
 * 11/10/12                 G. Hull         onlyShowResourcesWithData
 * 11/14/13       #1051     G. Hull         add pref for desk, set from command line. now gets triggered
 *                                          from spring bean.
 * 
 * </pre>
 * 
 * @author 
 * @version 1
 */

public class NmapCommon {
	
	// The desk is stored as a preference but is actually set from the 
	// command line.
	//
	public static final String DESK_ARG = "-desk"; 
	
	private static IPreferenceStore ncPrefStore = Activator.getDefault().getPreferenceStore();
	{
		ncPrefStore.setDefault( NcepGeneralPreferencesPage.PromptOnDisplayClose, false );
		ncPrefStore.setDefault( NcepGeneralPreferencesPage.ShowLatestResourceTimes, true );
		ncPrefStore.setDefault( NcepGeneralPreferencesPage.OnlyShowResourcesWithData, true );

		String desk = ProgramArguments.getInstance().getString( DESK_ARG ); 

		if( desk != null && !desk.trim().isEmpty() ) {
			desk = desk.trim().toUpperCase();
			System.out.println("Setting Desk to "+ desk+" from Program Arguement.");
		}
		else {
			desk = "NONE";
		}

		ncPrefStore.setDefault( NcepPreferences.DeskNamePref, desk );
		
		NcPathManager.getInstance().createDeskLevelLocalization(  desk );
	}
		    
    private final static String BaseOverlay = "GeoPolitical";
//    private final static String DefaultMap = "BasicWX_US";

    // commands associated with GUI Elements that can be updated/refreshed.
    public final static String[] guiUpdateElementCommands = {
//    	"gov.noaa.nws.ncep.viz.tools.pan",
    	"gov.noaa.nws.ncep.viz.ui.options.SyncPanes",
    	"com.raytheon.viz.ui.tools.looping.loop"
    	// ? frameTool, looping
    };

    public final static String NatlCntrsPerspectiveID = "gov.noaa.nws.ncep.viz.ui.NCPerspective";

    private static String pgenWorkingDirectory=".";
    
    public static String getPgenWorkingDirectory() {
		return pgenWorkingDirectory;
	}

	public static void setPgenWorkingDirectory(String pgenWorkingDirectory) {
		NmapCommon.pgenWorkingDirectory = pgenWorkingDirectory;
	}

	public static String getBaseOverlay() {
    	return new String( BaseOverlay );
    }

    // Added this to make it more clear that this plugin's store is used for 
    // all Ncep preferences
    // 
    public static IPreferenceStore getNcepPreferenceStore() {
    	// 
    	return ncPrefStore;
    }
        
    // Only non-svn directories.
	public static FileFilter createDirFilter() {
		return new FileFilter() {
			public boolean accept( File f ) {
				return ( f.isDirectory() &&
						 !f.getAbsolutePath().contains(".svn") ? true : false );
			}
		};
	}

	// filter out directories and files that don't end in one of the file extensions given.
	public static FileFilter createFileFilter( String[] fileExts ) {
		class FileExtFilter implements FileFilter {
			String[] file_exts=null;
			public FileExtFilter( String[] exts ) {
				file_exts = exts;
			}
			public boolean accept( File f ) {
				if( f.isDirectory() ) 
					return false;
				else if( file_exts.length == 0 )
					return true;
				else { 
					for( int i=0 ; i<file_exts.length ; i++ ) {
						if( f.getName().endsWith(file_exts[i]) ) {
							return true;
						}
					}
					return false;
				}
			}
		}

		return new FileExtFilter( fileExts );
	}
    
    
	public static LabelProvider createFileLabelProvider( ) {
		return createFileLabelProvider( new String[]{} );
	}

	public static LabelProvider createFileLabelProvider( String[] fileExts ) {
		// If the element is a directory then return the name of the directory. 
		// If it is a file then return the filename w/o extension.
		class FileLabelProvider extends LabelProvider {
			String[] file_exts = null;
			FileLabelProvider( String[] exts ) {
				file_exts = exts;
			}
			
	    	public String getText( Object element ) {
	    		if( !(element instanceof File) )
	    			return new String("Error: "+element.toString() );
	    		
	    		File f=(File)element;

	    		if( !f.exists() ) { // sanity check
	    			return new String("BadFileOrDirError");
	    		}
	    		if( f.isDirectory() || file_exts == null || file_exts.length == 0 ) {
	    			return f.getName();
	    		}
	    		else { // if the file name ends with one of the file extentions then return minus the extension.
	    			for( int i=0 ; i<file_exts.length ; i++ ) {
	    				String l = f.getName(); 
	    				if( l.endsWith( file_exts[i] ) ) {
	    					return l.substring(0, l.length()-file_exts[i].length() );
	    				}
	    			}
	    		}
	    		return f.getName();
	    	}
	    }

		return new FileLabelProvider( fileExts );
    }
    // Input is a File and the Elements are the sub-directories
    // This is used by the RscType and RscGroup listViewers for the Rsc Bndl Defn Mngr and the Group list 
	// on the Load Rsc Bundle window.
    //
	public static IStructuredContentProvider createSubDirContentProvider() {
		return createSubDirContentProvider(null);
	}
	
	public static IStructuredContentProvider createSubDirContentProvider(final Comparator<File> dirSortComparator) {
		class SubDirContentProvider implements IStructuredContentProvider {
			public void dispose() { }
			public void inputChanged(Viewer viewer, Object oldInput,
					Object newInput) {
			}
			// the input element is a File and the elements are the sub-directories
			public Object[] getElements(Object inputElement) {
				File dir= (File)inputElement;
				if( !dir.exists() ) 
				{
					return new File[]{}; //BadFileOrDirError
				}else{
					//Object[] subdirs = (Object[])dir.listFiles( NmapCommon.createDirFilter() );
					File[] subdirs = dir.listFiles( NmapCommon.createDirFilter() );
					
					if( (dirSortComparator == null)) {
						
					    	//if no comparator is defined the default comparator for files is used
					     	Arrays.sort( subdirs );
					}
					else {
						    List<File> dirList = Arrays.asList(subdirs);
						    Collections.sort( dirList, dirSortComparator );
					}
					return subdirs;
				}
				
			}
		}
		
		return new SubDirContentProvider();		
		
	}
	public static IStructuredContentProvider createFileContentProvider( String[] exts ) {
		return createFileContentProvider( exts, null );
	}

	public static IStructuredContentProvider createFileContentProvider( String[] exts, 
														final Comparator<File> fileSortComparator ) {
		class FileContentProvider implements IStructuredContentProvider {
			String[] file_exts = null; 

			FileContentProvider( String[] ext ) {
				file_exts = (ext == null ? new String[]{} : ext );
			}
			public void dispose() { }
			public void inputChanged(Viewer viewer, Object oldInput,
					Object newInput) {
				
			}
			public Object[] getElements(Object inputElement) {
				File dir= (File)inputElement;
				if( !dir.exists() ) {
					return new File[]{}; // BadFileOrDirError
				}
				//Object[] files = (Object[])dir.listFiles( NmapCommon.createFileFilter( file_exts ) );
				File[] files = dir.listFiles( NmapCommon.createFileFilter( file_exts ) );
				
				if( fileSortComparator == null ) {
					Arrays.sort( files );
				}
				else {
					List<File> filesList = Arrays.asList( files );
					Collections.sort( filesList, fileSortComparator );
				}
				return files;
			}
		}
		return new FileContentProvider( exts );
	}
	
	public static String[] getGUIUpdateElementCommands() {
		return guiUpdateElementCommands;
	}	
		
	// DataTime refTime -> YYMMDD"connStr"HHMM --- connStr = '_', '/', ':' or....
	public static String getTimeStringFromDataTime( DataTime dt, String connStr ) {
        NumberFormat nf = NumberFormat.getInstance();
        nf.setMinimumIntegerDigits(2);
        nf.setMinimumFractionDigits(0);
        nf.setMaximumFractionDigits(2);
        nf.setMaximumIntegerDigits(2);

        Calendar cal = Calendar.getInstance( TimeZone.getTimeZone("GMT") );
        cal.setTime( dt.getRefTime() );
        int yy = cal.get(Calendar.YEAR)%100;
        String yyStr = nf.format( yy );
        String mon = nf.format( cal.get(Calendar.MONTH)+1 );
        String dd = nf.format(cal.get(Calendar.DAY_OF_MONTH));
        String hh = nf.format(cal.get(Calendar.HOUR_OF_DAY));
        String min = nf.format(cal.get(Calendar.MINUTE));

		return String.format("%s%s%s%s%s%s", yyStr, mon, dd, connStr, hh, min);
	}
	
	// YYMMDD/HHMM -> DataTime
	public static DataTime parseDataTimeFromCycleTimeString( String cycTimeStr ) {
		if( cycTimeStr == null || cycTimeStr.isEmpty() ) {
			return null;
		}
		else if( cycTimeStr.length() != 2+2+2+1+2+2 ) {
			System.out.println("Can't parse cycle time:"+cycTimeStr );
			return null;
		}
		
		int yy = Integer.parseInt( cycTimeStr.substring(0,2) );
		yy = ( yy > 60 ? 1900 + yy : 2000 + yy );
		int mon = Integer.parseInt( cycTimeStr.substring(2,4))-1;
		int dd  = Integer.parseInt( cycTimeStr.substring(4, 6));
		int hh  = Integer.parseInt( cycTimeStr.substring(7, 9));
		int min = Integer.parseInt( cycTimeStr.substring(9,11));

        Calendar cal = Calendar.getInstance( TimeZone.getTimeZone("GMT") );
        
        cal.set( yy, mon, dd, hh, min, 0 );
        cal.set( Calendar.MILLISECOND, 0 );
        
        return new DataTime( cal );
	}	
}
