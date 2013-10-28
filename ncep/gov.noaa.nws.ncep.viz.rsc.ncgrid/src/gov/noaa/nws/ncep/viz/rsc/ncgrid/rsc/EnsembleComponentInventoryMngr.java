package gov.noaa.nws.ncep.viz.rsc.ncgrid.rsc;

import static java.lang.System.out;
import gov.noaa.nws.ncep.edex.common.ncinventory.NcInventoryDefinition;
import gov.noaa.nws.ncep.edex.common.ncinventory.NcInventoryRequestMsg;
import gov.noaa.nws.ncep.viz.resources.manager.InventoryLoaderJob;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceDefnsMngr;
import gov.noaa.nws.ncep.viz.rsc.ncgrid.dgdriv.GridDBConstants;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.requests.ThriftClient;

public class EnsembleComponentInventoryMngr {

	private static Map<String,String> ensCompInvNamesMap=new HashMap<String,String>();

	// the components are the model names. The inventories will need to store the 
	// ensembleId and the dataTimes.
	//     
	public static void initInventoriesForEnsComponents( List<String> ensCompsList ) {
		
		try {		
			HashMap<NcInventoryDefinition,NcInventoryDefinition> currInvDefnsMap = 
				ResourceDefnsMngr.getInstance().getInventoryDefinitions( true );
			
		    List<NcInventoryDefinition> invDefnsToCreate = new ArrayList<NcInventoryDefinition>();
		    List<NcInventoryDefinition>  errList = new ArrayList<NcInventoryDefinition>();

			// loop thru the ens components and create an Inventory Definition 
			// to store the times for this component
			for( String ensCompModel : ensCompsList ) {
				NcInventoryDefinition invDefn;
			
				// if there is already an inventory for this component
				//	
				if( !ensCompInvNamesMap.containsKey( ensCompModel ) ) {

					for( NcInventoryDefinition id : currInvDefnsMap.values() ) {
						if( !id.getPluginName().equals(GridDBConstants.GRID_TBL_NAME ) ) {
							continue;
						}

						HashMap<String,RequestConstraint> constraints = id.getBaseConstraints();
						ArrayList<String> invParams = id.getInventoryParameters();

						if( constraints.containsKey( GridDBConstants.MODEL_NAME_QUERY ) &&
							constraints.get( GridDBConstants.MODEL_NAME_QUERY ).getConstraintValue().equals( ensCompModel ) &&
							invParams.contains(GridDBConstants.ENSEMBLE_ID_QUERY) &&
							invParams.contains(GridDBConstants.DATA_TIME_QUERY) ) {

							ensCompInvNamesMap.put( ensCompModel, id.getInventoryName() );
							break;
						}					
					}
				}
				
				// if a suitable inv defn not found then we will need to create one.
				//
				if( !ensCompInvNamesMap.containsKey( ensCompModel ) ) {
					
					HashMap<String,RequestConstraint> baseConstr = 
												new HashMap<String,RequestConstraint>();
					baseConstr.put("pluginName", new RequestConstraint(GridDBConstants.GRID_TBL_NAME) );
					baseConstr.put(GridDBConstants.MODEL_NAME_QUERY, new RequestConstraint( ensCompModel) );
					
					ArrayList<String> inventoryParamNames = new ArrayList<String>();
					inventoryParamNames.add("pluginName" );
					inventoryParamNames.add(GridDBConstants.ENSEMBLE_ID_QUERY);
					inventoryParamNames.add(GridDBConstants.DATA_TIME_QUERY );
					
					invDefn = new NcInventoryDefinition( 
							LocalizationManager.getInstance().getCurrentUser() + ":EnsembleComponents:" + ensCompModel,
							baseConstr, inventoryParamNames );	
					
					invDefnsToCreate.add( invDefn );
					ensCompInvNamesMap.put( ensCompModel, invDefn.getInventoryName() );
				}
			}

		    if( !invDefnsToCreate.isEmpty() ) {
		    	InventoryLoaderJob invLoader = 
		    		     new InventoryLoaderJob( invDefnsToCreate, false );
		    	
		    	invLoader.schedule();

		    	// TODO : update the progress monitor
		    	while( invLoader.getNumberOfInventoriesLeftToLoad() > 0 ) {
		    		System.out.println("Inventories left to load = "+invLoader.getNumberOfInventoriesLeftToLoad() );
		    	
		    		try { Thread.sleep(400); } catch (InterruptedException e) { }
		    	}

		    	errList = Arrays.asList( invLoader.getUninitializedInventoryDefns() );

		    	if( !errList.isEmpty() ) {
		    		System.out.println("Error initializing inventory defns for ensemble components");
		    		ensCompInvNamesMap.clear();
		    		ensCompInvNamesMap = null;
		    	}
		    }			
		} catch (VizException e) {
		}	
	}
	
	// 
	static DataTime[] queryEnsComponentCycleTimes( String model, String pertNum  ) throws VizException {
		
		
		if( !ensCompInvNamesMap.containsKey( model ) ) {
			System.out.println("Ens Comp inventory not found. Will create for ens comp model:"+model);
			
			ArrayList<String> compList = new ArrayList<String>();
			compList.add( model );
		
			initInventoriesForEnsComponents( compList );		
		}
		if( !ensCompInvNamesMap.containsKey( model ) ) {
			System.out.println("Unable to create inventory for ens comp model:"+model );
			return new DataTime[0];
		}		

		try {
			HashMap<String, RequestConstraint> reqConstraints = 
				new HashMap<String, RequestConstraint>();
			reqConstraints.put( "pluginName", new RequestConstraint( GridDBConstants.GRID_TBL_NAME ) );
			
			if( !pertNum.isEmpty() ) {
				reqConstraints.put( GridDBConstants.ENSEMBLE_ID_QUERY, new RequestConstraint( pertNum ) );
			}

			NcInventoryRequestMsg reqMsg = NcInventoryRequestMsg.makeQueryRequest();    			
			reqMsg.setInventoryName( ensCompInvNamesMap.get( model ) );
			reqMsg.setRequestedParam( GridDBConstants.DATA_TIME_QUERY );
			reqMsg.setReqConstraintsMap( reqConstraints );

			Object rslts;

			long t01 = System.currentTimeMillis();

			rslts = ThriftClient.sendRequest( reqMsg );

			if( !(rslts instanceof String[]) ) {
				out.println("Inventory Request Failed:"+rslts.toString() );

				throw new VizException("Inventory Request Failed: "+rslts.toString() );
			}

			long t02 = System.currentTimeMillis();

			String[] rsltsList = (String[]) rslts;

			ArrayList<DataTime> dataTimeArr = new ArrayList<DataTime>();
			
			//		out.println("Inventory DataTime Query for "+ resourceDefnName+ 
			//				" took "+ (t02-t01)+ "msecs for "+ rsltsList.length + " results." );
			//    		out.println("    DataTimes are " + rsltsArray.toString() );

			for( int i=0 ; i<rsltsList.length ; i++ ) {

				String rsltStr = (String)rsltsList[i];
				String[] queryResults = rsltStr.split("/");
				DataTime dt = new DataTime( queryResults[ queryResults.length-1 ] );
				dt = new DataTime( dt.getRefTime() );
//				
				if( !dataTimeArr.contains( dt ) ) {
					dataTimeArr.add( dt );
				}
			}
			
			DataTime dtArr[] = dataTimeArr.toArray( new DataTime[0] );
			Arrays.sort( dtArr );
			
			return dtArr;
		}
		catch( VizException e ) {			
			throw e;
		}
	}

}
