package gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels;


import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.localization.impl.LocalizationConstants;
import gov.noaa.nws.ncep.viz.localization.impl.LocalizationManager;
import gov.noaa.nws.ncep.viz.localization.impl.LocalizationResourcePathConstants;
import gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels.elements.PlotModelElement;
import gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels.elements.PlotModel;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

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
 *                       
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
public class PlotModelMngr {

	//
	private static HashMap<String,PlotModel> plotModels = null;

	private static PlotModelMngr instance = null;
	
	final String DFLT_SVG_TEMPLATE_FILE = "standardPlotModel.svg";
	
	private static File plotModelsDir = null;

	/**
	 * Singleton Constructor. 
	 */
	private PlotModelMngr() {
	}

	public static synchronized PlotModelMngr getInstance() {		
		if ( instance == null ) 
			 instance = new PlotModelMngr();
		return instance;
	}
	
	// read in all the xml files in the plotModels directory.
	synchronized private void readPlotModels() {
				
		if( plotModels == null ) {
			plotModels = new HashMap<String,PlotModel>();

			plotModelsDir = LocalizationManager.getInstance().getLocalizationFileDirectory(
					LocalizationResourcePathConstants.PLOTMODELS_DIR); 

			File xmlFiles[] = plotModelsDir.listFiles( new FilenameFilter() {
				@Override
				public boolean accept( File dir, String name) {
					return (name.endsWith(".xml"));
				}			
			});
			
			for( File xmlFile : xmlFiles ) {
				try {
					PlotModel plotModel = null;
					JAXBContext context = JAXBContext.newInstance(
							PlotModel.class.getPackage().getName() );
					Unmarshaller unmarshaller = context.createUnmarshaller();
					plotModel = (PlotModel)unmarshaller.unmarshal(
							new FileReader(xmlFile));
					
					// enforce the filenaming convention.
					File genFile = new File( createPlotModelFilename( 
												plotModel.getPlugin(), plotModel.getName() ) );

					if( plotModel.getPlugin() == null ) {
						continue;
					}
					
					if( !xmlFile.equals( genFile ) ) {
						System.out.println("The PlotModel filename "+xmlFile.getAbsolutePath() + 
								" doesn't follow the proper filename convention\nRenaming the file to "+
								 genFile.getAbsolutePath() );
						xmlFile.renameTo( genFile );
					}
					
					plotModels.put( plotModel.getPlugin()+plotModel.getName(), plotModel );		
					
				} catch (JAXBException jbe ) {
					System.out.println("Error unmarshalling file: " + xmlFile.getAbsolutePath() );
					System.out.println( jbe.getMessage() );
				} catch (FileNotFoundException e1) {
					e1.printStackTrace();
				} catch (NullPointerException e2) {
					e2.printStackTrace();		
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
	 *  This will overwrite an existing file so check before calling.
	 */
	public void savePlotModel( PlotModel plotModel ) { 
		readPlotModels();
		
		if( plotModel == null ||
			plotModel.getName() == null ) {
			System.out.println("savePlotModel: PlotModel is null or doesn't have a name?");
			return;
		}
		
		// convention is camelcase category and plotmodel name
		String pluginName = plotModel.getPlugin();
		String name = plotModel.getName();
		File plotModelFile = new File( createPlotModelFilename( pluginName, name ) );
		
		try {
			JAXBContext context = JAXBContext.newInstance(PlotModel.class);
			Marshaller marshaller = context.createMarshaller();
			marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);  
			marshaller.marshal( plotModel, new FileWriter( plotModelFile ) ); 

			// update this PlotModel in the map
			plotModels.put( plotModel.getPlugin()+plotModel.getName(), plotModel );				
		} catch (JAXBException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}		
	}
	
	
	public void deletePlotModel( String pluginName, String pltMdlName ) 
						throws VizException { 
//		readPlotModels();
				
		File plotModelFile = new File( createPlotModelFilename( pluginName, pltMdlName ) );
		
		if( !plotModelFile.exists() ) {
			throw new VizException( "File "+plotModelFile.getAbsolutePath() +
					" doesn't exist.");
		}
		else if( !plotModelFile.delete() ) {
			throw new VizException( "Error Deleting File "+plotModelFile.getAbsolutePath());
		}		
		else if( plotModels.remove( pluginName+pltMdlName) == null ) {
			// ???
		}
	}
	
	// convention is camelcase category and plotmodel name
	public String createPlotModelFilename( String pluginName, String name ) {
		String xmlFname = //pluginName.substring(0,1).toUpperCase() + pluginName.substring(0).toLowerCase() +
//						  name.substring(0,1).toUpperCase() +  name.substring(1).toLowerCase() + ".xml";
			plotModelsDir.getAbsolutePath()+File.separator+pluginName+"_"+name+".xml";
		return xmlFname;
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