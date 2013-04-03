package gov.noaa.nws.ncep.viz.gempak.grid.inv;

import java.util.ArrayList;
import java.util.HashMap;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import static java.lang.System.out;

import gov.noaa.nws.ncep.edex.common.ncinventory.ManageNcInventoryMsg;
import gov.noaa.nws.ncep.edex.common.ncinventory.NcInventoryDefinition;
import gov.noaa.nws.ncep.edex.common.ncinventory.NcInventoryRequestMsg;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    		Description
 * ------------ ---------- ----------- --------------------------
 * Oct 03, 2011            gamazaychikov    Initial creation
 * Nov 17, 2011            Xguo             Fixed getRequestContrainsMap problem
 * Mar 20, 2012   #606     Greg Hull        update inventory by implementing IAlertObserver
 * May 08, 2012   #606     Greg Hull        Renamed to NcGridInventory. Use NcInventory on Edex 
 * May 16, 2012   #606     Greg Hull        change glevel1 to modelInfo.level.levelonevalue (and glevel2)
 * Aug 27,2012             Xilin Guo        used new unified grid plugin
 * 
 * </pre>
 * 
 * @author gamazaychikov
 * @version 1.0
 */
public class NcGridInventory {
	
	// This will use one NcInventory to store all of the ncgrib data (ie all modelNames).
	// Another alternative is to create one NcInventory for each modelName. Or even better would be
	// to tie this to the GRID Resource Definitions so we don't need to store data for models that
	// can't be displayed (w/o a RD)
	// 
	public static final String ncGridInventoryName = "NcGridInventory";
	
	private static final ArrayList<String> inventoryParamNames = new ArrayList<String>(); {
		inventoryParamNames.add( GridConstants.PLUGIN_NAME );
		inventoryParamNames.add( GridConstants.DATASET_ID ); // source
		inventoryParamNames.add( GridConstants.SECONDARY_ID ); //
		inventoryParamNames.add( GridConstants.ENSEMBLE_ID ); //
		inventoryParamNames.add( GridConstants.PARAMETER_ABBREVIATION );      
		inventoryParamNames.add( GridConstants.MASTER_LEVEL_NAME );
		inventoryParamNames.add( GridConstants.LEVEL_ONE );
		inventoryParamNames.add( GridConstants.LEVEL_TWO );
	}
	
	private boolean isInventoryInited = false;
		
	private static final transient IUFStatusHandler statusHandler = 
				UFStatus.getHandler(NcGridInventory.class);

    private static NcGridInventory instance = null;
    
	public static NcGridInventory getInstance() {
		if( instance == null ) {
			instance = new NcGridInventory();	
		}
		return instance;
	}
	
	public Boolean isInitialized() {
		return isInventoryInited;
	}
	
	// 
	public void initInventory( boolean reinit ) throws VizException {
		
		if( !isInventoryInited || reinit) {

			// TODO : limit this to only the modelNames we are interested in.
			//
			HashMap<String, RequestConstraint> baseConstraints = 
									new HashMap<String,RequestConstraint>();
			baseConstraints.put( "pluginName", new RequestConstraint( GridConstants.GRID ) );
			
			//inventoryConstraints.put( "pluginName", new RequestConstraint( getPluginName() ) );			
	    	NcInventoryDefinition invDescr = 
    				new NcInventoryDefinition( ncGridInventoryName, 
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

//	    	out.println("Inventory loaded for "+ ncGridInventoryName+" in "+ (t02-t01)+ "msecs" );
		}
	}
		
	// 
	public ArrayList<String> searchNcGridInventory( 
				HashMap<String, RequestConstraint> searchConstraints, String reqParam ) {
		
		if( !isInventoryInited ) {
			return null;
		}

	 	try {    		
			NcInventoryRequestMsg reqMsg = NcInventoryRequestMsg.makeQueryRequest();

	 		reqMsg.setInventoryName( ncGridInventoryName );
	 		reqMsg.setRequestedParam( reqParam );
	 		reqMsg.setReqConstraintsMap( searchConstraints );

	 		Object rslts;

	 		long t01 = System.currentTimeMillis();

	 		rslts = ThriftClient.sendRequest( reqMsg );

//	 		out.println("inv request returned "+rslts.getClass().getCanonicalName() );

	 		if( !(rslts instanceof String[]) ) {
//	 			out.println("Inventory Request Failed: expecting String[] return." + rslts.toString());
	 			throw new VizException("Inventory Request Failed: expecting String[] instead of "+
	 					rslts.getClass().getName() );
	 		}

	 		long t02 = System.currentTimeMillis();

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
	public String dumpNcGribInventory() throws VizException {

//		System.out.println("dumpNcGribInventory not implemented");

		// Create an NcInventoryRequest script to get query the ncInventory for the types.
    	// 
    	NcInventoryRequestMsg reqMsg = NcInventoryRequestMsg.makeDumpRequest();
    	
		reqMsg.setInventoryName( ncGridInventoryName );
		reqMsg.setRequestedParam( 
				inventoryParamNames.get( inventoryParamNames.size()-1) );
	//	reqMsg.setDumpToFile( true );
		
		Object rslts;

		long t01 = System.currentTimeMillis();

		rslts = ThriftClient.sendRequest( reqMsg );

		
		if( rslts instanceof String ) {
//			out.println("NcGribInventoryDump returned "+rslts );
		}
		else {
//			out.println("Inventory Dump Error: expecting String return." );
			throw new VizException("Inventory Dump Request Error: String response expecting instead of "+
					rslts.getClass().getName() );
		}

		long t02 = System.currentTimeMillis();

//		out.println("Inventory Dump for "+ ncGridInventoryName+ 
//				" took "+ (t02-t01)+ "msecs " );
		return (String)rslts;
	}
}


