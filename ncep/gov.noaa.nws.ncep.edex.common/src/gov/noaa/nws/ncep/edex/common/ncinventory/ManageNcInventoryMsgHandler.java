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
import java.io.File;
import java.io.FileReader;

import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;

/**
 * Called by ThriftClient on one EdexRequest. This will create the inventory
 * for this Edex Request and send a message to topic for all EdexRequests.
 *    The load time is stored in the inventory and will be used to determine
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 *  04/04/12      #606       Greg Hull   Created
 *  05/20/12      #606       Greg Hull   Reinit so the description is not needed.
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
public class ManageNcInventoryMsgHandler implements IRequestHandler<ManageNcInventoryMsg> {
	
    public static final String MANAGE_INVENTORY_TOPIC = "jms-generic:topic:manageInventory";

	@Override
	public Object handleRequest(ManageNcInventoryMsg mngrMsg) throws Exception {
//		out.println("NcInventoryCreateHandler handleRequest called for "+
//				createMsg.getInventoryDescr().getInventoryName() );
		String directive = mngrMsg.getDirectiveType();
		
		if( directive.equals( ManageNcInventoryMsg.CREATE_INVENTORY_DIRECTIVE ) ) {
			return initInventory( mngrMsg, false );
		}
		else if( directive.equals( ManageNcInventoryMsg.REINIT_INVENTORY_DIRECTIVE ) ) {
			
			// the Description is taken from the existing inventory instead of the msg.
			//	
			NcInventory inv = NcInventory.getInventory( mngrMsg.getInventoryName() );

			if( inv == null ) {
				return "Inventory " + mngrMsg.getInventoryName()+" has not been created.";
			}
			else if( mngrMsg.getInventoryDefinition() != null ) {
				if( !inv.getInventoryDefinition().equals( mngrMsg.getInventoryDefinition() ) ) {
					return "???Existing inventory "+mngrMsg.getInventoryName()+
					    " has different description than that given in the ReInit msg???";
				}
			}
			
			mngrMsg.setInventoryDefinition( inv.getInventoryDefinition() );
			
			return initInventory( mngrMsg, true );
		}
		else if ( directive.equals( ManageNcInventoryMsg.DELETE_INVENTORY_DIRECTIVE ) ) {
			return deleteInventory( mngrMsg );
		}
		else {
			return (Object)"Unrecognized NcInventory Directive";
		}
	}
		
	public Object initInventory( ManageNcInventoryMsg initMsg, boolean reload ) {
		
		try {
			NcInventory.initInventory( initMsg.getInventoryDefinition(), reload );

		// if the inventory created successfully on this EDEX, send
		//   a msg to all EDEX Requests to create their own inventory.
		//
			File tmpFile = File.createTempFile("LoadInventoryRequest-", ".xml" );

			SerializationUtil.jaxbMarshalToXmlFile( 
					initMsg, tmpFile.getAbsolutePath() );

			FileReader freader = new FileReader( tmpFile );
			char[] xmlStr = new char[(int)tmpFile.length()];
			freader.read(xmlStr);
			freader.close();

			EDEXUtil.getMessageProducer().sendAsyncUri( MANAGE_INVENTORY_TOPIC,
				( reload ? ManageNcInventoryMsg.REINIT_INVENTORY_DIRECTIVE :
						   ManageNcInventoryMsg.CREATE_INVENTORY_DIRECTIVE ) 
						          		+ new String(xmlStr) );
			
			if( !tmpFile.delete() ) {
				System.out.println("Error deleting temp file:"+tmpFile.getName());
			}
			
		} catch ( Exception ex ) {
			return ex.getMessage();
		}
		
		// msg recieved but no guarantee on whether the inventory was created.
		//
        return ManageNcInventoryMsg.CREATE_SUCCESS_RESPONSE;
	}
	
	// this should be quick so don't bother deleting here for this Edex.
	// Just put the msg on the topic and delete the inventory when the msg is
	// recieved.
	//
	public Object deleteInventory( ManageNcInventoryMsg deleteMsg ) {
		
		try {
			String invName = deleteMsg.getInventoryName();
			
			EDEXUtil.getMessageProducer().sendAsyncUri( MANAGE_INVENTORY_TOPIC, 
					  ManageNcInventoryMsg.DELETE_INVENTORY_DIRECTIVE + invName );

			// not much of a success msg since we aren't really deleting yet,
			// so we'll try for at least a little feedback by checking if the inventory exists
//			if( NcInventory.getInventory( invName ) == null ) {
//				
//			}

			return ManageNcInventoryMsg.DELETE_SUCCESS_RESPONSE;

		} catch (EdexException e) {
			return e.getMessage();
		}
	}
}
