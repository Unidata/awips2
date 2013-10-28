package gov.noaa.nws.ncep.edex.common.ncinventory;
/**
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 02/14/2012	606			Greg Hull	Initial coding
 * 
 * @author Greg Hull
 * @version 1.0
 */
import static java.lang.System.out;

import gov.noaa.nws.ncep.edex.common.ncinventory.NcInventoryRequestMsg.NcInventoryRequestType;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Date;
import java.util.HashMap;
import java.util.List;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 *  02/20/12      #606       Greg Hull   Created
 *  04/13/12      #606       Greg Hull   add dumpInventory()
 *  05/18/12      #606       Greg Hull   add directory and summary
 *  11/15/12      #950       Greg Hull   Don't treat empty inventory as an error.
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
public class NcInventoryRequestMsgHandler implements IRequestHandler<NcInventoryRequestMsg> {
	
	@Override
	public Object handleRequest( NcInventoryRequestMsg request) throws Exception {
		try {
			if( request.getRequestType() == NcInventoryRequestType.QUERY ) {
				return (Object)handleQueryRequest( request );
			}
			else if( request.getRequestType() == NcInventoryRequestType.DUMP ) {
				return (Object)handleDumpRequest( request );
			}
			else if( request.getRequestType() == NcInventoryRequestType.DIRECTORY ) {
				return (Object)handleDirectoryRequest( request );
			}
			else if( request.getRequestType() == NcInventoryRequestType.SUMMARY ) {
				return (Object)handleSummaryRequest( request );
			}
			else {
				return "Unimplemented Request Type";
			}
		}
		catch (Exception e ) {
			return e.getMessage();
		}
	}
	
	public String[] handleQueryRequest( NcInventoryRequestMsg queryRequest) throws Exception {

		String inventoryName = queryRequest.getInventoryName();

		NcInventory inv = NcInventory.getInventory( inventoryName );

		if( inv == null ) {
			String errStr = "Error requesting NcInventory data. NcInventory "+
			inventoryName+" has not been created on "+InetAddress.getLocalHost().getHostName();
			throw new Exception( errStr );
		}

		String invContents[] = inv.search( 
				queryRequest.getReqConstraintsMap(), queryRequest.getRequestedParam() );

		return invContents;
	}

	private Object handleDumpRequest( NcInventoryRequestMsg dumpRequest ) throws Exception {
		try {
			long t0 = System.currentTimeMillis();

			String invContents[] = handleQueryRequest( dumpRequest );
		
			String inventoryName = dumpRequest.getInventoryName();

			File dumpFile = File.createTempFile( "NcInvDump-"+inventoryName+"-", ".xml" );
	        FileWriter dumpWriter = new FileWriter( dumpFile );

	        // TODO : get edex request service instance
	        dumpWriter.write("Dumping Inventory "+inventoryName+ " with Description: " +
	        		NcInventory.getInventory( inventoryName ).getInventoryDefinition().toString() +"\n");
			if( !dumpRequest.getReqConstraintsMap().isEmpty() ) {
				dumpWriter.write( "search constraints are:\n"+
						dumpRequest.getReqConstraintsMap().toString()+"\n" );
			}

			dumpWriter.write( invContents.length + " entries found in " + (System.currentTimeMillis()-t0) + " msecs.\n"); 

			for( String invEntry : invContents ) {
				dumpWriter.write( invEntry+"\n" );
			}
			dumpWriter.close();

    		String hostname = InetAddress.getLocalHost().getHostName();
    		
    		if( invContents.length == 0 ) {
    			return (Object)"Inventory "+inventoryName+" contains no data.";
    		}
    		else {
    			return (Object)("Inventory "+inventoryName+" contains "+invContents.length + " entries.\n"+
					"Contents Dumped to file:\n"+dumpFile.getAbsolutePath() + " on host "+hostname);
    		}
		} catch (IOException e) {
			return (Object)"Error creating temporary Dump file.";			
		}				
	}
	
	// return an array of the InventoryDefnitions.
	//
	public Object handleDirectoryRequest( NcInventoryRequestMsg dirRequest) throws Exception {

		// can be empty but not null.
		List<NcInventory> invList = NcInventory.getAllNcInventories();
		
		NcInventoryDefinition invDefns[] = new NcInventoryDefinition[ invList.size() ];
		
		for( int s=0 ; s<invList.size() ; s++ ) {
			invDefns[s] = invList.get(s).getInventoryDefinition();
		}

		return invDefns;
	}
	
	// for now this is just the inventory description.
	//
	public String handleSummaryRequest( NcInventoryRequestMsg summaryRequest) throws Exception {

		String inventoryName = summaryRequest.getInventoryName();
		NcInventory inv = NcInventory.getInventory( inventoryName );

		if( inv == null ) {
			String errStr = "Error requesting NcInventory Summary. NcInventory "+
				inventoryName+" has not been created on "+InetAddress.getLocalHost().getHostName();
			throw new Exception( errStr );
		}
		String summaryStr = inv.getInventoryDefinition().toString()+
					"\n"+inv.getBranchCount() + " Entries stored in "+
					inv.getNodeCount()+" Nodes\n"+
					"Initialized at "+new Date(inv.getLastLoadTime()).toString();
		return summaryStr;
	}
}
