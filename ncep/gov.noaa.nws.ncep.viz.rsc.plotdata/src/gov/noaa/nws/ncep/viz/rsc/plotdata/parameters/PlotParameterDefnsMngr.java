package gov.noaa.nws.ncep.viz.rsc.plotdata.parameters;

import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.NcUnits;
import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;

import java.io.File;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBException;

import org.w3c.dom.Element;

import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.serialization.SerializationUtil;

/**
 * This class reads parameter list from the xml file. And set the parameter property if needed.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 10/09  		172    	   	M. Li       Initial Creation
 * 06/10		291			G. Zhang	Added support for synop/ship/ship6hr
 * 03/04/11     425         G. Hull     renamed from ParmList; create filename from plugin name,
 *                                      use SerializationUtil to un/marshal
 * 05/27/11     441         G. Hull     register NcUnits; Determine windBarb params from the plotMode 
 * 07/31/11     450         G. Hull     Make singleton. Use NcPathManager                 
 *                       
 * </pre>
 * 
 * @author mli
 * @version 1
 */
public class PlotParameterDefnsMngr{
		
	private Map<String,LocalizationFile> locFilesMap=null;
		
	// map from the pluginName to the PlotParameterDefns
	private Map<String,PlotParameterDefns> plotPrmDefnsMap=null;
	
	private static PlotParameterDefnsMngr instance=null;
	
	public static PlotParameterDefnsMngr getInstance() {
		if( instance == null ) {
			instance = new PlotParameterDefnsMngr();			
		}
		
		return instance;
	}
	
    private PlotParameterDefnsMngr( ) { 
        
    	NcUnits.register(); // moved from PlotResource.initResource

    	// get all of the xml (PlotParameterDefns) files in the PlotParameters dir.
    	//
    	plotPrmDefnsMap = new HashMap<String,PlotParameterDefns>();
    	
    	locFilesMap = NcPathManager.getInstance().listFiles(
    				NcPathConstants.PLOT_PARAMETERS_DIR, 
    				 	new String[]{".xml"}, false, true );

    	for( LocalizationFile lFile : locFilesMap.values() ) {
    	
    		File plotParamsFile = lFile.getFile();
    		    	
    		try {
    			if( plotParamsFile != null && plotParamsFile.exists() ) {
    				
    				PlotParameterDefns paramDefnTable = readParameterFile( plotParamsFile  );
    			
    				if( plotPrmDefnsMap.containsKey( paramDefnTable.getPlugin() ) ) {
    					System.out.println("Warning : More than one PlotParameterDefns file "+
    							"found for plugin "+ paramDefnTable.getPlugin() );    					
    				}
    				
    				plotPrmDefnsMap.put( paramDefnTable.getPlugin(), paramDefnTable );
    				
    			}
    		}
    		catch ( JAXBException exp ){
    			System.out.println("Error parsing PlotParameterDefns file: "+
    					lFile.getName()+" : "+ exp.getMessage() );
    			continue;
    		}    	
    	}
	}

	private PlotParameterDefns readParameterFile( File xmlFile ) throws JAXBException{

        PlotParameterDefns parmsDefnsList = null;
        
		try {			
            FileReader fr = new FileReader(xmlFile);
            char[] b = new char[(int) xmlFile.length()];
            fr.read(b);
            fr.close();
            String str = new String(b);

			Object xmlObj = SerializationUtil.unmarshalFromXml( str.trim()  );

			parmsDefnsList = (PlotParameterDefns)xmlObj;
						
			return  parmsDefnsList;
			
        } catch (Exception e) {
        	System.out.println( "unmarshall error: "+e.getMessage() );
        }
		
		return null;              
    }   
	
	public PlotParameterDefns getPlotParamDefns( String pluginName ) {
		
		if( plotPrmDefnsMap.containsKey( pluginName ) ) {
			return plotPrmDefnsMap.get( pluginName );//.getParameterDefns();
		}
		else {
			return null;
		}
	}
		



}