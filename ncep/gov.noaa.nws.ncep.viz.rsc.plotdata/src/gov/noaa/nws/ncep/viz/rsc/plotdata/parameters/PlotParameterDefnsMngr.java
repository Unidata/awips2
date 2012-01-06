package gov.noaa.nws.ncep.viz.rsc.plotdata.parameters;

import gov.noaa.nws.ncep.viz.localization.impl.LocalizationManager;
import gov.noaa.nws.ncep.viz.localization.impl.LocalizationResourcePathConstants;
import gov.noaa.nws.ncep.metParameters.parameterConversion.NcUnits;

import java.io.File;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import javax.xml.bind.JAXBException;

import org.w3c.dom.Element;

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
 *                       
 * </pre>
 * 
 * @author mli
 * @version 1
 */

public class PlotParameterDefnsMngr{
		
	private List<PlotParameterDefn> paramDefnTable;
		
    public PlotParameterDefnsMngr( String pluginName ) { 
        
    	NcUnits.register(); // moved from PlotResource.initResource

		File plotModelsDir = LocalizationManager.getInstance().getLocalizationFileDirectory(
				LocalizationResourcePathConstants.PLOTMODELS_DIR); 
		File plotParamsFile = new File( plotModelsDir, "plotParameters"+File.separator+
				"plotParameters_"+pluginName+".xml" );
    	
    	try{
    		if( plotParamsFile == null ||
    		   !plotParamsFile.exists() ) {
    			
    			System.out.println("Unable to find plotParameter file for plugin:"+pluginName );
    			return;
    		}
    		paramDefnTable = readParameterFile( plotParamsFile  );
    	}
    	catch ( JAXBException exp ){
    		paramDefnTable = null;
    		exp.printStackTrace();
    	}    	
    	
	}

	private List<PlotParameterDefn> readParameterFile( File xmlFile ) throws JAXBException{

        PlotParameterDefns parmsDefnsList = null;
        
		try {			
            FileReader fr = new FileReader(xmlFile);
            char[] b = new char[(int) xmlFile.length()];
            fr.read(b);
            fr.close();
            String str = new String(b);

			Object xmlObj = SerializationUtil.unmarshalFromXml( str.trim()  );

			parmsDefnsList = (PlotParameterDefns)xmlObj;
			
			List<PlotParameterDefn> listOfItems = getRscParams( parmsDefnsList.getParameterDefns());			
			
			return  listOfItems;
			
        } catch (Exception e) {
        	System.out.println( "unmarshall error: "+e.getMessage() );
        }
		
		return null;              
    }   
	
	public List<PlotParameterDefn> getPlotParamDefns() { //String pluginName ){
		return paramDefnTable;
	}
	
	public PlotParameterDefn getPlotParamDefn( String plotParmName ) {//TODO: Map<String,HashMap<String,String>> (<sfcobs,<SKYC,skyCover>>) 20100716
		if (paramDefnTable == null || paramDefnTable.isEmpty()) 
			return null;
		
		for (PlotParameterDefn p : paramDefnTable){
			if (p.getPlotParamName().equalsIgnoreCase(plotParmName)) {
				return p;
			}
		}
		
		//System.out.println("Can't find mapping for Parameter " + parmName );
		
		return null;
	}	
	
	private List<PlotParameterDefn> getRscParams(List<PlotParameterDefn> list){
		return list;		
	}
	
	public String[] getAllParameterNames( boolean includeSkyC, boolean includeWndBrb ) {
		List<String> list = new ArrayList<String>();
		
		for(PlotParameterDefn p : paramDefnTable ){
			
			if( p.getPlotMode() != null && p.getPlotMode().equals( "barb" ) ) {
				if( includeWndBrb ) {
					list.add( p.getPlotParamName() );
				}
			}
			else if( p.getPlotMode() != null && p.getPlotMode().equals( "table" ) &&
				 	 p.getSymbolFont() != null    && p.getSymbolFont().equals("SpecialSymbolFont") ) {					
				if( includeSkyC ) {
					list.add( p.getPlotParamName() );
				}
			}
			else {
				list.add( p.getPlotParamName() );
			}

		}		
		
		return list.toArray(new String[]{});

	}

	public ArrayList<String> getWindBarbParams(){ 
		ArrayList<String> list = new ArrayList<String>();
		
		for(PlotParameterDefn p : paramDefnTable ){
			if( p.getPlotMode() != null && p.getPlotMode().equals( "barb" ) &&
				p.getPlotParamName() != null ) {
				
				list.add( p.getPlotParamName() );				
			}
		}		
		
		return list;
	}
	
	public ArrayList<String> getSpecialTableParams() {
		ArrayList<String> list = new ArrayList<String>();
		
		for(PlotParameterDefn p : paramDefnTable ){
			if( p.getPlotMode() != null && p.getPlotMode().equals( "table" ) &&
				p.getSymbolFont() != null    && p.getSymbolFont().equals("SpecialSymbolFont") &&
				p.getPlotParamName() != null ) {
				
				list.add( p.getPlotParamName() );
			}
		}		
		
		return list;
	}

// there may be more than one Defn for a dbParam	
//	public PlotParameterDefn getParamDefnForDbName( String dbParamName ) {
//		if (paramDefnTable == null || paramDefnTable.isEmpty()) 
//			return null;
//		
//		for (PlotParameterDefn p : paramDefnTable){
//			if (p.getDbParamName().equalsIgnoreCase(dbParamName)) {
//				return p;
//			}
//		}
//		
//		//System.out.println("Can't find mapping for Parameter " + parmName );
//		
//		return null;
//	}	
	// return  a list of all the Defns with the given metParameter.
	public ArrayList<PlotParameterDefn> getPlotParamDefnsForMetParam( String metParam ) {
		ArrayList<PlotParameterDefn> retList = new ArrayList<PlotParameterDefn>();
		for(PlotParameterDefn pd : paramDefnTable ){
			if( pd.getMetParamName().equals( metParam ) ) {
				retList.add( pd );				
			}
		}		
		return  retList;
	}

}