/*
 * gov.noaa.nws.ncep.ui.pgen.graphToGrid.GraphToGrid
 * 
 * January 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.graphToGrid;

import java.io.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Scanner;

//import org.apache.log4j.Logger;

import gov.noaa.nws.ncep.gempak.parameters.core.categorymap.CatMap;
import gov.noaa.nws.ncep.ui.pgen.PgenStaticDataProvider;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;


/**
 * Abstract super class for Graph-to-Grid processing.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 01/10		#215		J. Wu   	Initial Creation.
 * 07/11        #450        G. Hull     use localization names from NcPathConstants
 * 10/11         	        J. Wu	    handle non-existing table file entries.
 * 
 * </pre>
 * 
 * @author	J. Wu
 */

public abstract class GraphToGrid {
	
//	private final static Logger logger = Logger.getLogger( GraphToGrid.class);

	AbstractDrawableComponent currentGraph;	     
    
    protected LinkedHashMap<String, String>  gridParameters = null;
    
    private static ArrayList<String> parameterNames = null;
       
    private static String[] gridCalcParams = {    		 
    		"PROJ", 	"GRDAREA",
    		"KXKY", 	"GGLIMS",	
    		"DISCRETE",	"DLINES",
    		"EDGEOPTS", "BOUNDS",	
    		"CATMAP" }; 

   
    private static String[] gridDisplayParams = { 
		   "CINT",		"LINE",
		   "FINT", 		"FLINE"
    };

	private static String[] gridOutputParams = {  
			"HISTGRD", 	"PATH",	
			"GDOUTF", 	"MAXGRD", 	
			"GDATTIM", 	"GVCORD",	
			"GLEVEL", 	"GFUNC",
			"GPARM"
	};
    	
	public abstract void makeGrid();
	
    /**
     * Constructor
     */
	public GraphToGrid(AbstractDrawableComponent currentGraph,
			           HashMap<String, String> gridParameters ) {
		super();		
 
		this.currentGraph = currentGraph;
		
		setGridParameters( gridParameters );
	    
   }

	/**
	 * @return the parameterNames
	 */
	public final static ArrayList<String> getParameterNames() {
		initializeParameterNames();
		return parameterNames;
	}
	
	/*
	 * @return the parameterNames
	 */
	private final static void initializeParameterNames() {
	    
		if ( parameterNames == null ) {
	    	parameterNames = new ArrayList<String>();
	    	for ( String str : gridCalcParams ) {
	    		parameterNames.add( str );
	    	}
	    	
	    	for ( String str : gridDisplayParams ) {
	    		parameterNames.add( str );
	    	}
	    	
	    	for ( String str : gridOutputParams ) {
	    		parameterNames.add( str );
	    	}
	    	
    		parameterNames.add( "DISPOPT" );	    	
	    }
		
	}

	/**
	 * @return the currentGraph
	 */
	public AbstractDrawableComponent getCurrentGraph() {
		return currentGraph;
	}

	/**
	 * @param currentGraph the currentGraph to set
	 */
	public void setCurrentGraph(AbstractDrawableComponent currentGraph) {
		this.currentGraph = currentGraph;
	}

	/**
	 * @return the gridParameters
	 */
	public LinkedHashMap<String, String> getGridParameters() {
		
		if ( this.gridParameters == null ) {
			initializeGridParameters();
		}
		
		return gridParameters;
	}

	/**
	 * @param gridParameters the gridParameters to set
	 */
	public void setGridParameters( HashMap<String, String> gridParameters) {
        
		initializeParameterNames();
		
		if ( this.gridParameters == null ) {
			initializeGridParameters();
		}
	
		if ( gridParameters != null ) {
		    for ( String str : gridParameters.keySet() ) {
			    for ( String pstr : parameterNames ) {
			    	if ( pstr.equals( str ) ) {
			    		this.gridParameters.put( str, gridParameters.get(str) );
			    	    break;
			    	}
			    }
		    }
		}

	}
	
	/**
	 * @return the gridCalcParams
	 */
	public static String[] getGridCalcParams() {
		return gridCalcParams;
	}

	/**
	 * @return the gridDisplayParams
	 */
	public static String[] getGridDisplayParams() {
		return gridDisplayParams;
	}

	/**
	 * @return the gridOutputParams
	 */
	public static String[] getGridOutputParams() {
		return gridOutputParams;
	}

	/**
	 * @param gridParameter the gridParameter to update
	 */
	public void setGridParameter( String name,  String value ) {
		this.gridParameters.put( name, value );
	}
		

	/**
	 * @param gridParameter the gridParameter to update
	 */
	public String getGridParameter( String name ) {
		if ( this.gridParameters.containsKey( name ) ) {
			return this.gridParameters.get( name );
		}
		else {
			return null;
		}
	}
	
    /**
     *  Initialize the parameter list.
     */
    private void initializeGridParameters() {
    	
		initializeParameterNames();
		
    	gridParameters = new LinkedHashMap<String, String>( parameterNames. size() );
    	
    	for ( String name : parameterNames ) {
    		gridParameters.put( name,	"" );
    	}

    }       

    
    /**
     *  Initialize the parameter list.
     */
    public final static LinkedHashMap<String, String> loadParameters( String localizationName ) {
    	
    	LinkedHashMap<String, String> params = new LinkedHashMap<String, String>();
		
    	//Check if the given file exists and readable.
        String fname = null;
        if ( PgenStaticDataProvider.getProvider().getStaticFile( localizationName ) != null ) {
            fname = PgenStaticDataProvider.getProvider().getStaticFile( localizationName ).getAbsolutePath();
        }
 
        File thisFile = null;                
        if ( fname != null ) {
             thisFile = new File( fname );
             if ( !thisFile.exists() || !thisFile.canRead() ) {
        	    thisFile = null;
             }
        }
        
        if ( thisFile == null ) {
        	return params;
        }
      
   	    try {
       	    
         	Scanner fileScanner = new Scanner( thisFile );       	 
            Scanner lineScanner = null;
            
            try {
                //first use a Scanner to get each line
                while ( fileScanner.hasNextLine() ) {
                    String nextLine = fileScanner.nextLine().trim();
                
                    //process each line
                    if ( ! (nextLine.startsWith("!")) ) {
            	        lineScanner = new Scanner( nextLine );
                
                        if ( lineScanner.hasNext() ){
                            String name = lineScanner.next();
                            String value = "";
                        
                            if ( lineScanner.hasNext() ) {
                                value = lineScanner.next();
                            }
                        
                            params.put( name, value );
                        }
                
                        lineScanner.close();

                    }
                }
            }
            finally {
                fileScanner.close();
            }
    	}
        catch ( IOException e ) {
        	e.printStackTrace();
        }
        
   	    return params;
   	
    } 


    /**
     * Retrieve the value from a HashMap, if not found, return an empty string.
     * @param map
     * @param paramName
     * @return
     */
    public static String getParamValues( HashMap<String,String> map, String paramName ) {

	    String mapVals = null;
	    if ( map != null && paramName != null ) {
	        mapVals = map.get( paramName );
	    }
	    
	    if ( mapVals != null ) {
	        return mapVals;
	    }
	    else {
//	        logger.debug( "Cannot find input for " +  paramName );
	        return new String("");
	    }
	
	}

    /**
     * Retrieve the value from a CatMap. If not found, return RMISSD.
     * @param cmap
     * @param label
     * @return lblValue
     */
    public static float getValueForLabel( CatMap cmap, String label ) {

    	float lblValue = G2GCommon.RMISSD;
    	
    	if ( label != null && cmap != null ) {
	    	
    		Float cmapValue= cmap.getMatchingValueForLabel( label );
	    	if ( cmapValue.equals( Float.NaN ) ) {
        		
	    		try {
    			    lblValue = Float.parseFloat( label );
    		    }
    		    catch( Exception e) {		    		
	    		    lblValue = G2GCommon.RMISSD;
    		    }
	    	} 
	    	else {
	    		lblValue = cmapValue;
	    	}
        }   	
    	
    	return lblValue;
	
	}
    
    /**
     * Retrieve the value from a CatMap. If not found, return RMISSD.
     * @param cmap
     * @param label
     * @return lblValue
     */
    public static String getLabelForValue( CatMap cmap, float value ) {

    	String lbl = Float.toString( value );
    	
    	if ( cmap != null ) {
	    	
    		String cmapLabel = cmap.getMatchingLabelForValue( value );
	    	if (  cmapLabel != null ) {
	    		lbl = cmapLabel;
	    	} 
        }
    	    	
    	return lbl;
	
	}

}


