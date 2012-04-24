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

import gov.noaa.nws.ncep.ui.nsharp.palette.NsharpGraphConfigDialog;
import gov.noaa.nws.ncep.ui.nsharp.palette.NsharpLineConfigDialog;
import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;

import java.io.File;
import java.util.Map;

import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
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
	//retrieve from file system
	public NsharpConfigStore retrieveNsharpConfigStoreFromFs() {
		if(nsharpConfigStore == null){ 
			// get configuration from nsharpConfig.xml 
			Map<String,LocalizationFile> nsharpFiles = NcPathManager.getInstance().listFiles( 
					NcPathConstants.NSHARP_CONFIG, 
					new String[]{ ".xml" }, true, true );

			for( LocalizationFile lFile : nsharpFiles.values() ) {
				try {
					System.out.println("lFile name="+ lFile.getName());

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
			nsharpConfigStore = NsharpGraphConfigDialog.setDefaultGraphConfig(nsharpConfigStore);
			nsharpConfigStore=NsharpLineConfigDialog.setDefaultLineConfig(nsharpConfigStore);
		}	
		return nsharpConfigStore;
	}
	//save to file system
	public boolean saveConfigStoreToFs(NsharpConfigStore nsharpConfigStore) throws VizException{
		this.nsharpConfigStore = nsharpConfigStore;
		// create a localization file for the plotModel
		//
		LocalizationContext userCntxt = NcPathManager.getInstance().getContext( 
				LocalizationType.CAVE_STATIC, LocalizationLevel.USER );

		LocalizationFile 
		    lFile = NcPathManager.getInstance().getLocalizationFile( userCntxt,
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
