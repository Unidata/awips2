/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.NsharpConfigManager
 * 
 * This java class performs the surface station locator functions.
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 * This class handle Nsharp configuration saving and retrieving from file system. It also manages 
 * current configuration information.
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 03/13/2012   			Chin Chen	Initial coding
 * 										
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nsharp;

import gov.noaa.nws.ncep.ui.nsharp.palette.NsharpParametersSelectionConfigDialog;
import gov.noaa.nws.ncep.ui.nsharp.palette.NsharpDataDisplayConfigDialog;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;

import java.io.File;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.viz.core.exception.VizException;

public class NsharpConfigManager {
	
	private NsharpConfigStore nsharpConfigStore=null;
	private static NsharpConfigManager instance=null;
	
	private NsharpConfigManager() {
		instance = this;
	}
	
	public static NsharpConfigManager getInstance() {
		if(instance==null){
			new NsharpConfigManager();
		}
		return instance;
	}

	public NsharpConfigStore getNsharpConfigStore() {
		return nsharpConfigStore;
	}

	public void setNsharpConfigStore(NsharpConfigStore nsharpConfigStore) {
		this.nsharpConfigStore = nsharpConfigStore;
	}
	private  Map<String, LocalizationFile>  listFiles( IPathManager pthmgr,
	            String name, String[] filter, boolean recursive, boolean filesOnly ) {
	    	LocalizationContext[] contexts = pthmgr.getLocalSearchHierarchy( LocalizationType.CAVE_STATIC );
	    	
	        Map<String, LocalizationFile> lFileMap = new HashMap<String, LocalizationFile>();

	    	List<LocalizationFile> lFilesList = 
	    			Arrays.asList(
	    					pthmgr.listFiles( contexts, name, filter, recursive, filesOnly ) );

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
	//retrieve from file system
	public NsharpConfigStore retrieveNsharpConfigStoreFromFs() {
		if(nsharpConfigStore == null){ 
			// get configuration from nsharpConfig.xml 
			IPathManager pthmgr =  PathManagerFactory.getPathManager();
			Map<String,LocalizationFile> nsharpFiles =listFiles( pthmgr, 
					NcPathConstants.NSHARP_CONFIG, 
					new String[]{ ".xml" }, true, true );

			for( LocalizationFile lFile : nsharpFiles.values() ) {
				try {
					//System.out.println("lFile name="+ lFile.getName());

					Object xmlObj = SerializationUtil.jaxbUnmarshalFromXmlFile( 
							lFile.getFile().getAbsolutePath() );

					if( xmlObj instanceof NsharpConfigStore ) {
						nsharpConfigStore = (NsharpConfigStore)xmlObj;
						break;

					}
				} catch (SerializationException e) {
					System.out.println("Error unmarshalling file: " + lFile.getFile().getAbsolutePath() );
					System.out.println( e.getMessage() );
				}	
			}
		}
		
		if(nsharpConfigStore== null){
			// could not find configuration nsharpConfig.xml file 
			nsharpConfigStore = new NsharpConfigStore();
			nsharpConfigStore = NsharpParametersSelectionConfigDialog.setDefaultGraphConfig(nsharpConfigStore);
			nsharpConfigStore=NsharpDataDisplayConfigDialog.setDefaultLineConfig(nsharpConfigStore);
		}	
		return nsharpConfigStore;
	}
	//save to file system
	public boolean saveConfigStoreToFs(NsharpConfigStore nsharpConfigStore) throws VizException{
		this.nsharpConfigStore = nsharpConfigStore;
		// create a localization file for the plotModel
		//
		IPathManager pthmgr =  PathManagerFactory.getPathManager();
		LocalizationContext userCntxt = pthmgr.getContext( 
				LocalizationType.CAVE_STATIC, LocalizationLevel.USER );

		LocalizationFile 
		    lFile = pthmgr.getLocalizationFile( userCntxt,
		    		NcPathConstants.NSHARP_CONFIG ); 
		
		
		File configFile = lFile.getFile();
		
		try {
			SerializationUtil.jaxbMarshalToXmlFile( nsharpConfigStore, 
					configFile.getAbsolutePath() );

			lFile.save();
			
		} catch (LocalizationOpFailedException e) {
			throw new VizException( e );
		} catch (SerializationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		return true;
	}
}
