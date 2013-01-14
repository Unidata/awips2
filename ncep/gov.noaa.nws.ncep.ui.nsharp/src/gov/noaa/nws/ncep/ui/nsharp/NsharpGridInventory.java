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
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1.0
 */
public class NsharpGridInventory {

	// This will use one NcInventory to store grid metadata needed for NSharp. 
	// currently this is just the modelNames and dataTimes. 
	//
	// NOTE : If more data is needed we may want to consider merging this
	// with the NcGridInventory which stores the parm/vcord/levels but not 
	// the dataTimes.
	// 
	public static final String nsharpGridInventoryName = "NSharpGridInventory";
	
	private static final ArrayList<String> inventoryParamNames = new ArrayList<String>(); {
		/*
		 * Chin note: the order of adding parameters is important. Do not change them, otherwise,
		 * the code calling isearchInventory()  from ModelSoundingDialogContents class
		 * will have to change as well.
		 */
		inventoryParamNames.add( "pluginName" );
		inventoryParamNames.add( "info.datasetId" );
		inventoryParamNames.add( "dataTime.refTime" );
		//inventoryParamNames.add( "dataTime.fcstTime");
	}
	
	private boolean isInventoryInited = false;
		
	//private static final transient IUFStatusHandler statusHandler = 
	//			UFStatus.getHandler(NsharpGridInventory.class);

    private static NsharpGridInventory instance = null;
    
	public static NsharpGridInventory getInstance() {
		if( instance == null ) {
			instance = new NsharpGridInventory();	
		}
		return instance;
	}
	
	public Boolean isInitialized() {
		return isInventoryInited;
	}
	
	// Note: this should not be necessary since edex should have already initialized the 
	// NcGridSoundingInventoryDefinition 
	// 
	public void initInventory( boolean reinit ) throws VizException {
		
		if( !isInventoryInited || reinit) {

			HashMap<String, RequestConstraint> baseConstraints = 
									new HashMap<String,RequestConstraint>();
			baseConstraints.put( "pluginName", new RequestConstraint( "grid" ) );
			
			//inventoryConstraints.put( "pluginName", new RequestConstraint( getPluginName() ) );			
	    	NcInventoryDefinition invDescr = 
    				new NcInventoryDefinition( nsharpGridInventoryName, 
    							baseConstraints, inventoryParamNames );

	    	ManageNcInventoryMsg createReqMsg = 
	    		( reinit ? ManageNcInventoryMsg.makeReinitDirective() :
	    			       ManageNcInventoryMsg.makeCreateDirective() );
	    	createReqMsg.setInventoryDefinition( invDescr );
//	    	createReqMsg.setReInitInventory( reinit );

	    	long t01 = System.currentTimeMillis();

	    	Object rslts = ThriftClient.sendRequest( createReqMsg );

	    	if( !(rslts instanceof String) ) {
	    		throw new VizException("initInventory failed: response not of type String???");	    		
	    	}
	    	String response = (String)rslts;

	    	if( response.equals( ManageNcInventoryMsg.CREATE_SUCCESS_RESPONSE ) ) {	    		
	    		isInventoryInited = true;
	    	}
	    	else {
	    		throw new VizException( response );
	    	}

	    	long t02 = System.currentTimeMillis();

	    	out.println("Inventory loaded for "+ nsharpGridInventoryName+" in "+ (t02-t01)+ "msecs" );
		}
	}
		
	// 
	public ArrayList<String> searchInventory( 
				HashMap<String, RequestConstraint> searchConstraints, String reqParam ) {
		
		if( !isInventoryInited ) {
			return null;
		}

	 	try {    		
			NcInventoryRequestMsg reqMsg = NcInventoryRequestMsg.makeQueryRequest();

	 		reqMsg.setInventoryName( nsharpGridInventoryName );
	 		reqMsg.setRequestedParam( reqParam );
	 		reqMsg.setReqConstraintsMap( searchConstraints );

	 		Object rslts;

	 		//long t01 = System.currentTimeMillis();

	 		rslts = ThriftClient.sendRequest( reqMsg );

//	 		out.println("inv request returned "+rslts.getClass().getCanonicalName() );

	 		if( !(rslts instanceof String[]) ) {
//	 			out.println("Inventory Request Failed: expecting String[] return." + rslts.toString());
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
		reqMsg.setRequestedParam( 
				inventoryParamNames.get( inventoryParamNames.size()-1) );
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

