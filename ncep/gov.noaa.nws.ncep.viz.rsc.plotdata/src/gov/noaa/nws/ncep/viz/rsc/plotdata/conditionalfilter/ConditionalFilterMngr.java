package gov.noaa.nws.ncep.viz.rsc.plotdata.conditionalfilter;

import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.viz.core.exception.VizException;


/**
 * 
 * This class reads and writes conditional filters. 
 * (It initially reads all the xml files in the ConditionalFilters directory 
 * and unmarshals them as ConditionalFilters.)
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/2012      #615       S. Gurung   Initial Creation
 * 04/2012      #606       Greg Hull   
 * 12/2012      #947       Greg Hull   add pluginName to localization path
 *                       
 * </pre>
 * 
 * @author sgurung
 * @version 1
 */
public class ConditionalFilterMngr {
	
	private static HashMap<String,ConditionalFilter> conditionalFilters = null;

	private static ConditionalFilterMngr instance = null;

	public static final String NullFilterName = "NoFilter";
	
	private ConditionalFilterMngr() {
	}

	public static synchronized ConditionalFilterMngr getInstance() {		
		if( instance == null ) 
			 instance = new ConditionalFilterMngr();
		return instance;
	}
	
	// read in all the xml files in the conditionalFilters directory.
	synchronized private void readConditionalFilters() {
		if( conditionalFilters == null ) {
			conditionalFilters = new HashMap<String,ConditionalFilter>();

			// get all of the xml (ConditionalFilter) files in the CONDITIONAL_FILTERS_DIR directory.
			// This will return files from all context levels.
			Map<String,LocalizationFile> condFilterLclFiles = NcPathManager.getInstance().listFiles( 
        				NcPathConstants.CONDITIONAL_FILTERS_DIR, 
        				       new String[]{ ".xml" }, true, true );
			
			// we are expecting the files to be under a sub-directory that is the name of the plugin.
			// if this is not the case then display a warning msg.
			//
			for( LocalizationFile lFile : condFilterLclFiles.values() ) {
				try {
					ConditionalFilter ConditionalFilter = null;
					Object xmlObj = SerializationUtil.jaxbUnmarshalFromXmlFile( 
							lFile.getFile().getAbsolutePath() );

					if( xmlObj instanceof ConditionalFilter ) {
						ConditionalFilter = (ConditionalFilter)xmlObj;
					
						ConditionalFilter.setLocalizationFile( lFile );
					
						if( ConditionalFilter.getPlugin() == null ) {
							continue;
						}
						else if( !lFile.getName().equals( ConditionalFilter.createLocalizationFilename() ) ) {
							// This will only cause a problem if the user creates a USER-level (uses naming convention) and
							// then reverts back to the base version by deleting the user level file. The code will 
							// look for the base version using the naming convention and so won't find the file.
							System.out.println("Warning: ConditionalFilter file doesn't follow the naming convention.\n");
							System.out.println( lFile.getName()+" should be "+ConditionalFilter.createLocalizationFilename() );
						}

						conditionalFilters.put( ConditionalFilter.getPlugin()+ConditionalFilter.getName(), ConditionalFilter );	
					
					}
				} catch (SerializationException e) {
					System.out.println("Error unmarshalling file: " + lFile.getFile().getAbsolutePath() );
					System.out.println( e.getMessage() );
				}	
			}
			if( conditionalFilters.size() == 0 ) 
				conditionalFilters = null;
							
		}
	}

	public ArrayList<String> getPlugins( ) {
		readConditionalFilters();
		ArrayList<String> pluginList = new ArrayList<String>();
		
		for( ConditionalFilter pm : conditionalFilters.values() ) {
			if( !pluginList.contains( pm.getPlugin() ) ) {
				pluginList.add( pm.getPlugin() );
			}
		}
		return pluginList;
	}
	
	// if null category then get all the conditionalFilters
	public HashMap<String,ConditionalFilter> getConditionalFiltersByPlugin( String plgn ) {
		
		readConditionalFilters();
		
		HashMap<String,ConditionalFilter> conditionalFiltersByPlugin = new HashMap<String,ConditionalFilter>();
		
		for( ConditionalFilter pm : conditionalFilters.values() ) {
			if( plgn == null ||
				plgn.equalsIgnoreCase( pm.getPlugin() ) ) {
				conditionalFiltersByPlugin.put( pm.getName(), pm );
			}
		}
		
		return conditionalFiltersByPlugin;
	}
	
	public String[] getAllConditionalFiltersByPlugin( String plgn ) {
		
		if( plgn == null || plgn.isEmpty() ) {
			return new String[0];
		}

		readConditionalFilters();
		
		ArrayList<String> cfList = new ArrayList<String>();
		for( ConditionalFilter pm : conditionalFilters.values() ) {
			if( plgn.equalsIgnoreCase( pm.getPlugin() ) ) {
				cfList.add(pm.getName());
			}
		}	

		String[] condFiltersArray = cfList.toArray( new String[0] );
		
		Arrays.sort(condFiltersArray);
		
		return condFiltersArray;
	}
	
	/**
	 * Reads the contents of the table file
	 * @param xmlFilename - full path of the xml table name
	 * @return - a list of stations
	 * @throws JAXBException
	 */
	public ConditionalFilter getConditionalFilter( String plgn, String ConditionalFilterName  ) {		
		readConditionalFilters();
		
		ConditionalFilter ConditionalFilter = getConditionalFiltersByPlugin(plgn).get( ConditionalFilterName );
		return ConditionalFilter;
	}   

	/*
	 *  Writes a JAXB-based object into the xml file and updates the map.
	 *  
	 */
	public void saveConditionalFilter( ConditionalFilter condFilter ) throws VizException { 
		
		readConditionalFilters();
		
		if( condFilter == null || condFilter.getSize() == 0 || 
			condFilter.getName() == null || condFilter.getName().isEmpty() ) {
			
			throw new VizException( "saveConditionalFilter: ConditionalFilter is null or doesn't have a name?");
		}
		if( condFilter.getName().equals( NullFilterName ) ) {
			if( condFilter.getSize() != 0 ) {
				throw new VizException( "Can't save a non-null filter as "+NullFilterName  );				
			}
		}
		
		// create a localization file for the ConditionalFilter
		LocalizationContext userCntxt = NcPathManager.getInstance().getContext( 
				LocalizationType.CAVE_STATIC, LocalizationLevel.USER );

		LocalizationFile lFile = NcPathManager.getInstance().getStaticLocalizationFile(
				condFilter.createLocalizationFilename() );
		//NcPathConstants.CONDITIONAL_FILTERS_DIR + File.separator + condFilter.getName());
      	
		// if the file exists overwrite it.
		if( lFile == null || 
		    lFile.getContext().getLocalizationLevel() != LocalizationLevel.USER ) {
		    lFile = NcPathManager.getInstance().getLocalizationFile( userCntxt,
		    		condFilter.createLocalizationFilename() ); 
		}
			
		condFilter.setLocalizationFile( lFile );		
		File condFilterFile = lFile.getFile();
		
		try {
			SerializationUtil.jaxbMarshalToXmlFile( condFilter, 
					condFilterFile.getAbsolutePath() );

			lFile.save();
			
			// update this ConditionalFilter in the map
			conditionalFilters.put( condFilter.getPlugin()+condFilter.getName(), condFilter );		
						
		} catch (LocalizationOpFailedException e) {
			throw new VizException( e );
		} catch (SerializationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	public void deleteConditionalFilter( String pluginName, String condFilterName ) 
						throws VizException {
		
		ConditionalFilter delCondFilter = getConditionalFilter( pluginName, condFilterName );
		
		if( delCondFilter == null ) {
			throw new VizException("Could not find conditional filter, "+condFilterName+
					", for plugin, "+pluginName );
		}
		
		LocalizationFile lFile = delCondFilter.getLocalizationFile();
		
		if( lFile == null ||
			!lFile.getFile().exists() ||
			lFile.getContext().getLocalizationLevel() != LocalizationLevel.USER ) { 
			throw new VizException( "File "+delCondFilter.createLocalizationFilename() +
					" doesn't exist or is not a User Level Conditional Filter.");
		}
		
		try {
			String lFileName = lFile.getName();
			
			lFile.delete(); 
			conditionalFilters.remove( pluginName+condFilterName );

			lFile = NcPathManager.getInstance().getStaticLocalizationFile( lFileName ); 
				
			// If there is another file of the same name in the BASE/SITE/DESK then
			// update the conditionalFilters with this version.
			if( lFile != null) {
				if( lFile.getContext().getLocalizationLevel() == LocalizationLevel.USER ) {
					System.out.println("Delete Conditional Filter successful but unexplained error occurred.");
					return;
					//throw new VizException( "Unexplained error deleting Conditional Filter.");
				}				
				try {
					ConditionalFilter ConditionalFilter = null;
					Object xmlObj = SerializationUtil.jaxbUnmarshalFromXmlFile( 
							lFile.getFile().getAbsolutePath() );

					if( xmlObj instanceof ConditionalFilter ) {
						ConditionalFilter = (ConditionalFilter)xmlObj;
					
						ConditionalFilter.setLocalizationFile( lFile );
					
						if( ConditionalFilter.getPlugin() != null ) {
							conditionalFilters.put( ConditionalFilter.getPlugin()+ConditionalFilter.getName(), ConditionalFilter );		
						}
					}
				} catch (SerializationException e) {
					System.out.println("Error unmarshalling file: " + lFile.getFile().getAbsolutePath() );
					System.out.println( e.getMessage() );
				}	
				
			}

			// TODO : check if there is a base or site level file of the same name and 
			// update with it....
		} catch ( LocalizationOpFailedException e ) {
			throw new VizException( "Error Deleting Conditional Filter, "+condFilterName+
					", for plugin, "+pluginName +"\n"+e.getMessage() );
		}				
	}
		
	public ConditionalFilter getDefaultConditionalFilter( String plugin ) {
	    ConditionalFilter dfltPM = new ConditionalFilter();
	    dfltPM.setName( NullFilterName );
	    dfltPM.setPlugin( plugin );
	    dfltPM.setDescription("");
	    dfltPM.getConditionalFilterElements(); 
	    return dfltPM;
	}
	
	// TODO Add logic for if the conditionalFilter is in the base/site level and provide appropriate confirmation message
    public static boolean conditionalFilterFileExists(String plugin, String name ) {
		String fname = name;
		if( !name.endsWith(".xml")) {
			fname = fname + ".xml";
		}
    	File f = NcPathManager.getInstance().getStaticFile( 
    			NcPathConstants.CONDITIONAL_FILTERS_DIR + File.separator +
    							plugin + File.separator+fname );

        return ( f != null && f.exists() );
    }
    
}