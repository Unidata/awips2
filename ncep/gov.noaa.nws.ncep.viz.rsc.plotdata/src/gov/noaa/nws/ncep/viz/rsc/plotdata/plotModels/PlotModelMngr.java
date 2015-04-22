package gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels;


import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;
import gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels.elements.PlotModel;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

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
 * This class reads and writes plotModels. It initially reads all the xml files in the plotModels directory 
 * and unmarshals them as plotModels. 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 12/15  		 #217	   	Greg Hull   Initial Creation
 * 03/21        R1G2-9      Greg Hull   synchronized readPlotModels()     
 * 03/04/11      425        Greg Hull   change category to plugin
 * 03/08/11      425        Greg Hull   add deletePlotModel
 * 08/15/11      450        Greg Hull   NcPathManager and save LocalizationFiles;
 *                                      use SerializationUtil instead of JaxBContext
 *                       
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
public class PlotModelMngr {
	
	private static HashMap<String,PlotModel> plotModels = null;

	private static PlotModelMngr instance = null;
	
	final String DFLT_SVG_TEMPLATE_FILE = "standardPlotModelTemplate.svg";


	private PlotModelMngr() {
	}

	public static synchronized PlotModelMngr getInstance() {		
		if( instance == null ) 
			 instance = new PlotModelMngr();
		return instance;
	}
	
	// read in all the xml files in the plotModels directory.
	synchronized private void readPlotModels() {
		
		if( plotModels == null ) {
			plotModels = new HashMap<String,PlotModel>();

			// get all of the xml (plotModel) files in the PLOT_MODELS directory.
			// This will return files from all context levels.
			// This is recursive to pick up files in the 'plugin' subdirectories but as
			// a result the PlotParameterDefns in PlotModels/PlotParameters will also be
			// picked up so we have to ignore them.
			Map<String,LocalizationFile> pmLclFiles = NcPathManager.getInstance().listFiles( 
        				NcPathConstants.PLOT_MODELS_DIR, 
        				       new String[]{ ".xml" }, true, true );
			
			for( LocalizationFile lFile : pmLclFiles.values() ) {
				try {
					PlotModel plotModel = null;
					Object xmlObj = SerializationUtil.jaxbUnmarshalFromXmlFile( 
							lFile.getFile().getAbsolutePath() );

					if( xmlObj instanceof PlotModel ) {
						plotModel = (PlotModel)xmlObj;
					
						plotModel.setLocalizationFile( lFile );
					
						if( plotModel.getPlugin() == null ) {
							continue;
						}
						else if( !lFile.getName().equals( plotModel.createLocalizationFilename() ) ) {
							// This will only cause a problem if the user creates a USER-level (uses naming convention) and
							// then reverts back to the base version by deleting the user level file. The code will 
							// look for the base version useing the naming convention and so won't find the file.
							System.out.println("Warning: PlotModel file doesn't follow the naming convention.\n");
							System.out.println( lFile.getName()+" should be "+plotModel.createLocalizationFilename() );
						}

						plotModels.put( plotModel.getPlugin()+plotModel.getName(), plotModel );		
					}
				} catch (SerializationException e) {
					System.out.println("Error unmarshalling file: " + lFile.getFile().getAbsolutePath() );
					System.out.println( e.getMessage() );
				}	
			}
			if( plotModels.size() == 0 ) 
				plotModels = null;
		}
	}

	public ArrayList<String> getPlugins( ) {
		readPlotModels();
		ArrayList<String> pluginList = new ArrayList<String>();
		
		for( PlotModel pm : plotModels.values() ) {
			if( !pluginList.contains( pm.getPlugin() ) ) {
				pluginList.add( pm.getPlugin() );
			}
		}
		return pluginList;
	}
	
	// if null cat then get all the plotModels
	public HashMap<String,PlotModel> getPlotModelsByPlugin( String plgn ) {
		
		readPlotModels();
		
		HashMap<String,PlotModel> plotModelsByPlugin = new HashMap<String,PlotModel>();
		
		for( PlotModel pm : plotModels.values() ) {
			if( plgn == null ||
				plgn.equalsIgnoreCase( pm.getPlugin() ) ) {
				// should we make a copy here instead?
				plotModelsByPlugin.put( pm.getName(), pm );
			}
		}
		
		return plotModelsByPlugin;
	}
	
	/**
	 * Reads the contents of the table file
	 * @param xmlFilename - full path of the xml table name
	 * @return - a list of stations
	 * @throws JAXBException
	 */
	public PlotModel getPlotModel( String plgn, String plotModelName  ) {		
		readPlotModels();
		
		PlotModel plotModel = getPlotModelsByPlugin(plgn).get( plotModelName );
		return plotModel;
	}   

	/*
	 *  Writes a JAXB-based object into the xml file and updates the map.
	 *  
	 */
	public void savePlotModel( PlotModel plotModel ) throws VizException { 
		
		readPlotModels();
		
		if( plotModel == null ||
			plotModel.getName() == null ) {
			throw new VizException( "savePlotModel: PlotModel is null or doesn't have a name?");
		}
		
		// create a localization file for the plotModel
		//
		LocalizationContext userCntxt = NcPathManager.getInstance().getContext( 
				LocalizationType.CAVE_STATIC, LocalizationLevel.USER );

		LocalizationFile lFile = plotModel.getLocalizationFile();
		
		// if the file exists overwrite it.
		if( lFile == null || 
		    lFile.getContext().getLocalizationLevel() != LocalizationLevel.USER ) {
		    lFile = NcPathManager.getInstance().getLocalizationFile( userCntxt,
								plotModel.createLocalizationFilename() ); 
		}
			
		plotModel.setLocalizationFile( lFile );
		
		File plotModelFile = lFile.getFile();
		
		try {
			SerializationUtil.jaxbMarshalToXmlFile( plotModel, 
					plotModelFile.getAbsolutePath() );

			lFile.save();
			
			// update this PlotModel in the map
			plotModels.put( plotModel.getPlugin()+plotModel.getName(), plotModel );		
						
		} catch (LocalizationOpFailedException e) {
			throw new VizException( e );
		} catch (SerializationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	public void deletePlotModel( String pluginName, String pltMdlName ) 
						throws VizException {
		
		PlotModel delPltMdl = getPlotModel( pluginName, pltMdlName );
		
		if( delPltMdl == null ) {
			throw new VizException("Could not find plot model, "+pltMdlName+
					", for plugin, "+pluginName );
		}
		
		LocalizationFile lFile = delPltMdl.getLocalizationFile();
		
		if( lFile == null ||
			!lFile.getFile().exists() ||
			lFile.getContext().getLocalizationLevel() != LocalizationLevel.USER ) { // sanity check
			throw new VizException( "File "+delPltMdl.createLocalizationFilename() +
					" doesn't exist or is not a User Level Plot Model.");
		}
		
		try {
			String lFileName = lFile.getName();
			
			lFile.delete(); 
			plotModels.remove( pluginName+pltMdlName );

			lFile = NcPathManager.getInstance().getStaticLocalizationFile( lFileName ); 

			// If there is another file of the same name in the BASE/SITE/DESK then
			// update the plotmodels with this version.
			if( lFile != null ) {
				if( lFile.getContext().getLocalizationLevel() == LocalizationLevel.USER ) {
					System.out.println("Huh, What?");
					throw new VizException( "Unexplained error deleting PlotModel.");
				}
				try {
					PlotModel plotModel = null;
					Object xmlObj = SerializationUtil.jaxbUnmarshalFromXmlFile( 
							lFile.getFile().getAbsolutePath() );

					if( xmlObj instanceof PlotModel ) {
						plotModel = (PlotModel)xmlObj;
					
						plotModel.setLocalizationFile( lFile );
					
						if( plotModel.getPlugin() != null ) {
							plotModels.put( plotModel.getPlugin()+plotModel.getName(), plotModel );		
						}
					}
				} catch (SerializationException e) {
					System.out.println("Error unmarshalling file: " + lFile.getFile().getAbsolutePath() );
					System.out.println( e.getMessage() );
				}					
			}
//			else { // otherwise delete the PlotModel from the map.
//				plotModels.remove( pluginName+pltMdlName );
//			}

			// TODO : check if there is a base or site level file of the same name and 
			// update with it....
		} catch ( LocalizationOpFailedException e ) {
			throw new VizException( "Error Deleting PlotModel, "+pltMdlName+
					", for plugin, "+pluginName +"\n"+e.getMessage() );
		}				
	}
		
	public PlotModel getDefaultPlotModel() {
	    PlotModel dfltPM = new PlotModel();
	    dfltPM.setName("default");
	    dfltPM.setPlugin("none");
	    dfltPM.setSvgTemplate( getDefaultSvgTemplate() );
	    dfltPM.getAllPlotModelElements(); 
	    return dfltPM;
	}
	
	public String getDefaultSvgTemplate() {
		return DFLT_SVG_TEMPLATE_FILE;
	}
}