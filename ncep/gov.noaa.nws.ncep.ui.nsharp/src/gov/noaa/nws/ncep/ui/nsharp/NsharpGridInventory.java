package gov.noaa.nws.ncep.ui.nsharp;

import static java.lang.System.out;
import gov.noaa.nws.ncep.edex.common.ncinventory.ManageNcInventoryMsg;
import gov.noaa.nws.ncep.edex.common.ncinventory.NcInventoryDefinition;
import gov.noaa.nws.ncep.edex.common.ncinventory.NcInventoryRequestMsg;

import java.util.ArrayList;
import java.util.HashMap;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;

/**
 * A class to create and query an NcInventory for Nsharp grid data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    		Description
 * ------------ ---------- ----------- --------------------------
 * 08/19/12       #845     Greg Hull     Created.
 * 08/23/12				   Chin Chen     Added ref time for Nsharp and remove event time as 
 * 										 it is not used.
 * 09/04/13       #1031    Greg Hull     Make directory request to find the grid resource's inventory.
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1.0
 */
public class NsharpGridInventory {

	// This will use one NcInventory to store grid metadata needed for NSharp. 
	// currently this is just the modelNames and dataTimes. This will use
	// the same inventory as the NcGrid resource.
	//
	public static String nsharpGridInventoryName = "";//"NcGridModelTimes";
	
	private static final ArrayList<String> inventoryParamNames = new ArrayList<String>(); {
		/*
		 * Chin note: the order of adding parameters is important. Do not change them, otherwise,
		 * the code calling isearchInventory()  from ModelSoundingDialogContents class
		 * will have to change as well.
		 */
		inventoryParamNames.add( "pluginName" );
		inventoryParamNames.add( "info.datasetId" ); //model type
		inventoryParamNames.add( "dataTime" );//available grid files 
		//inventoryParamNames.add( "dataTime.refTime" );//available grid files 
		//inventoryParamNames.add( "dataTime.fcstTime");
	}
	
    private static NsharpGridInventory instance = null;
    
	public static NsharpGridInventory getInstance() {
		if( instance == null ) {
			instance = new NsharpGridInventory();	
			nsharpGridInventoryName = "";
		}
		return instance;
	}
	
	public Boolean isInitialized() {
		return !nsharpGridInventoryName.isEmpty();
	}

	// get a list of the inventories on the server and save the name of
	// the one that NSharp will query
	public void initialize() throws VizException {

		if( isInitialized() ) {
			return;
		}
		String errMsg = "";
		HashMap<String, RequestConstraint> baseConstraints = 
				new HashMap<String,RequestConstraint>();
		baseConstraints.put( "pluginName", new RequestConstraint( "grid" ) );

		//inventoryConstraints.put( "pluginName", new RequestConstraint( getPluginName() ) );			
		NcInventoryDefinition invDefn = 
				new NcInventoryDefinition( "NSharpGridModels", //  
						baseConstraints, inventoryParamNames );

		NcInventoryRequestMsg dirRequest = NcInventoryRequestMsg.makeDirectoryRequest();

		try {
			Object rslts = ThriftClient.sendRequest( dirRequest );

			if( rslts instanceof String ) {
				errMsg = rslts.toString();
			}
			if( !(rslts instanceof ArrayList<?>) ) {
				errMsg = "Grid Inventory Directory Request Error: expecting ArrayList<NcInventoryDefinition>.";
			}
			else if( ((ArrayList<?>)rslts).isEmpty() ) {
				errMsg = "Grid Inventory Directory Request Warning: No Inventories initialized.???";
			}
			else if( !(((ArrayList<?>)rslts).get(0) instanceof NcInventoryDefinition) ) {
				errMsg = "Grid Inventory Directory Request Error: expecting ArrayList<NcInventoryDefinition>.";
			}
			else {
				// used to set the inventory initialized flag
				ArrayList<NcInventoryDefinition> invDefnsList = (ArrayList<NcInventoryDefinition>)rslts;

				// it would be nice to use the supportsQuery() method instead of equals but we'd have to 'assume' the constraints  
				// used for queries and I'd rather not do that.
				// instead just check for 1 'grid' base constraint and that the needed parameters
				// are in the inventory.
				for( NcInventoryDefinition id : invDefnsList ) {
					//if( id.supportsQuery( , inventoryParamNames ))
					if( id.getBaseConstraints().keySet().size() > 1 ||
					   !id.getBaseConstraints().containsKey("pluginName") ||
					   !id.getBaseConstraints().get("pluginName").getConstraintValue().equals("grid")) {
						continue;
					}
					Boolean invFound = true;
					for( String invPrm : inventoryParamNames ) {
						if( !id.getInventoryParameters().contains( invPrm ) ) {
							invFound = false;
							break;
						}
					}
					if( invFound ) {
						nsharpGridInventoryName = id.getInventoryName();
						System.out.println("Found Inventory, "+ nsharpGridInventoryName + 
								", to be used by the NsharpGridInventory class");				
						break;
					}
				}
				
				if( nsharpGridInventoryName.isEmpty() ) {
					errMsg = "Could not find usable inventory for NSharp Grid Models";
				}
			}
		}
		catch( VizException e ) {
			errMsg = "Error getting inventory directory: "+e.getMessage();
		}
		
		if( !isInitialized() ) {
			System.out.println(errMsg);
			throw new VizException( errMsg );
		}
	}
	
	// Note: this should not be necessary since edex should have already initialized the 
	// NcGridSoundingInventoryDefinition 
	// 
	// if the grid inventory is not found on the server, we
	// can create it from here. 
	public static void createInventory() throws VizException {
			HashMap<String, RequestConstraint> baseConstraints = 
									new HashMap<String,RequestConstraint>();
			baseConstraints.put( "pluginName", new RequestConstraint( "grid" ) );
			
		nsharpGridInventoryName = "NSharpGridModels";
		
			//inventoryConstraints.put( "pluginName", new RequestConstraint( getPluginName() ) );			
	    	NcInventoryDefinition invDescr = 
    				new NcInventoryDefinition( nsharpGridInventoryName, 
    							baseConstraints, inventoryParamNames );

	    	ManageNcInventoryMsg createReqMsg = 
    			       ManageNcInventoryMsg.makeCreateDirective();
	    	createReqMsg.setInventoryDefinition( invDescr );
//	    	createReqMsg.setReInitInventory( reinit );

	    	long t01 = System.currentTimeMillis();

	    	Object rslts = ThriftClient.sendRequest( createReqMsg );

	    	if( !(rslts instanceof String) ) {
    		nsharpGridInventoryName = "";
	    		throw new VizException("initInventory failed: response not of type String???");	    		
	    	}
	    	String response = (String)rslts;

    	if( !response.equals( ManageNcInventoryMsg.CREATE_SUCCESS_RESPONSE ) ) {	    		
    		nsharpGridInventoryName = "";
	    		throw new VizException( response );
	    	}

	    	long t02 = System.currentTimeMillis();

	    	out.println("Inventory loaded for "+ nsharpGridInventoryName+" in "+ (t02-t01)+ "msecs" );
		}
		
	// 
	public ArrayList<String> searchInventory( 
				HashMap<String, RequestConstraint> searchConstraints, String reqParam ) {
		
		if( !isInitialized() ) {
			System.out.println("Nsharp searchInventory failed because the inventory has not been initialized.");
			return null;
		}

	 	try {    		
			NcInventoryRequestMsg reqMsg = NcInventoryRequestMsg.makeQueryRequest();

	 		reqMsg.setInventoryName( nsharpGridInventoryName );
	 		reqMsg.setRequestedParams( new String[]{reqParam} );
	 		reqMsg.setReqConstraintsMap( searchConstraints );
	 		reqMsg.setUniqueValues( true );

	 		Object rslts;

	 		//long t01 = System.currentTimeMillis();

	 		rslts = ThriftClient.sendRequest( reqMsg );

//	 		out.println("inv request returned "+rslts.getClass().getCanonicalName() );

	 		if( rslts instanceof String ) {
	 			throw new VizException("Inventory Request Failed: "+ rslts.toString() ); 				
 			}
	 		
	 		if( !(rslts instanceof String[]) ) {
	 			throw new VizException("Inventory Request Failed: expecting String[] instead of "+
	 					rslts.getClass().getName() );
	 		}

	 		//long t02 = System.currentTimeMillis();

	 		String[] rsltsList = (String[]) rslts;

//	 		out.println("Inventory DataTime Query for "+ ncGridInventoryName+ 
//	 				" took "+ (t02-t01)+ "msecs for "+ rsltsList.length + " results." );

	 		ArrayList<String> retArray = new ArrayList<String>();
	 		
	 		for( int i=0 ; i<rsltsList.length ; i++ ) {	 			
	 			retArray.add( (String)rsltsList[i] );
//	 			out.println( (String)rsltsList[i] );
	 		}
	 		
	 		return retArray;
	 	}
	 	catch ( VizException vizex ) {
	 		System.out.println("Error searching NsharpGridInventory: "+vizex.getMessage() );
	 		return null;
	 	}
	}	
	
	// TODO : add search constraints to the dump...
	//
	public String dumpNsharpGridInventory() throws VizException {

		// Create an NcInventoryRequest script to get query the ncInventory for the types.
    	// 
    	NcInventoryRequestMsg reqMsg = NcInventoryRequestMsg.makeDumpRequest();
    	
		reqMsg.setInventoryName( nsharpGridInventoryName );
//		reqMsg.setRequestedParam( 
//				inventoryParamNames.get( inventoryParamNames.size()-1) );
	//	reqMsg.setDumpToFile( true );
		
		Object rslts;

		//long t01 = System.currentTimeMillis();

		rslts = ThriftClient.sendRequest( reqMsg );

		if( rslts instanceof String ) {
//			out.println("NcGribInventoryDump returned "+rslts );
		}
		else {
//			out.println("Inventory Dump Error: expecting String return." );
			throw new VizException("Inventory Dump Request Error: String response expecting instead of "+
					rslts.getClass().getName() );
		}

		//long t02 = System.currentTimeMillis();

//		out.println("Inventory Dump for "+ ncGridInventoryName+ 
//				" took "+ (t02-t01)+ "msecs " );
		return (String)rslts;
	}
}

