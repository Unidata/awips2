package gov.noaa.nws.ncep.viz.resources.manager;

import static java.lang.System.out;

import gov.noaa.nws.ncep.edex.common.ncinventory.ManageNcInventoryMsg;
import gov.noaa.nws.ncep.edex.common.ncinventory.NcInventoryDefinition;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ConcurrentLinkedQueue;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;

/**
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 *  03/10/12      #606       Greg Hull   Created
 *  05/25/12      #606       Greg Hull   Changed to use NcInventoryDefinition 
 *                                       and load/create here instead of ResourceDefn.
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */

public class InventoryLoaderJob extends Job {

	private final ConcurrentLinkedQueue<NcInventoryDefinition> invToLoad;
	private final ConcurrentLinkedQueue<NcInventoryDefinition> errInvList;

	private Boolean reloadInventory = false;
	
	private int numInvsToLoad = 0;
	private int invCount = 0;	
	private final int MAX_NUM_LOADER_THREADS = 10;
	
	public InventoryLoaderJob( List<NcInventoryDefinition> invList, Boolean reload ) {
		super("Inventory Loader");

		invToLoad = new ConcurrentLinkedQueue<NcInventoryDefinition>( invList );			
		errInvList = new ConcurrentLinkedQueue<NcInventoryDefinition>();			
		reloadInventory = reload;
	}

	public InventoryLoaderJob( NcInventoryDefinition inv, Boolean reload ) {
		super("Inventory Loader");

		invToLoad = new ConcurrentLinkedQueue<NcInventoryDefinition>();
		invToLoad.add( inv );
		errInvList = new ConcurrentLinkedQueue<NcInventoryDefinition>();			
		reloadInventory = reload;
	}

	public int getNumberOfInventoriesLeftToLoad() {
		return numInvsToLoad;
	}
	
	public NcInventoryDefinition[] getUninitializedInventoryDefns() {
		return errInvList.toArray( new NcInventoryDefinition[0] );
	}
	
	@Override
	protected IStatus run( IProgressMonitor monitor ) {
		//monitor.beginTask("Loading Inventories", invToLoad.size());

		invCount = invToLoad.size();
		numInvsToLoad = invCount;
    	long t01 = System.currentTimeMillis();
    	int numThreads = (MAX_NUM_LOADER_THREADS < invCount ?
    					  MAX_NUM_LOADER_THREADS : invCount );
    	
		for( int t=0 ; t<numThreads ; t++  ) {
			new Thread() {
				public void run(){
					
					while( !invToLoad.isEmpty() ) {						
						NcInventoryDefinition invDefn = invToLoad.poll();		
						
						try {
							ManageNcInventoryMsg createReqMsg = 
								( reloadInventory ? ManageNcInventoryMsg.makeReinitDirective() :
													ManageNcInventoryMsg.makeCreateDirective() );
							createReqMsg.setInventoryDefinition( invDefn );

							long t01 = System.currentTimeMillis();

							Object rslts = ThriftClient.sendRequest( createReqMsg );

							if( !(rslts instanceof String) ) {
								throw new VizException("initInventory failed: "+rslts.toString() );	    		
							}
							String response = (String)rslts;

							if( !response.equals( ManageNcInventoryMsg.CREATE_SUCCESS_RESPONSE ) &&
								!response.equals( ManageNcInventoryMsg.REINIT_SUCCESS_RESPONSE ) ) {	    		
								
								throw new VizException( response );
							}

							long t02 = System.currentTimeMillis();

							out.println("Inventory loaded for "+ 
									invDefn.getInventoryName()+" in "+ (t02-t01)+ "msecs" );

						} catch (VizException e) {
							//		out.println("Inventory failed to load for "+ resourceDefnName+
							//					" .... "+ e.getMessage() );
							out.println("Error Loading Inventory " +
									invDefn.getInventoryName()+ ": "+ e.getMessage() );
							//rscDefn.
							errInvList.add( invDefn );
						}
					//							rscDefn.initInventory( reloadInventory );
						numInvsToLoad--;
					}
				}
			}.start();
		}
		
    	long t02 = System.currentTimeMillis();

//		out.println("Took "+ (t02-t01) + "ms to start "+ numInvsToLoad+ " LoadInventory Threads");
		
		// TODO : update the progress monitor
		while( numInvsToLoad > 0 ) {
//			out.println("Inventories left to load = "+invToLoad.size() );
			try {
				Thread.sleep(500);
			} 
			catch (InterruptedException e) {				
			}
		}
		
		t02 = System.currentTimeMillis();
		out.println("Took "+ (t02-t01) + "ms to load "+ invCount + " Inventories");
		
		
		if( !errInvList.isEmpty() ) {
			out.println( errInvList.size()+" Inventories failed to initialize: ");
		}
		return Status.OK_STATUS;
	}
}

