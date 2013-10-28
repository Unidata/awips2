package gov.noaa.nws.ncep.edex.common.ncinventory;

import java.io.File;
import java.io.FileWriter;
import java.net.InetAddress;

import com.raytheon.uf.common.serialization.SerializationUtil;

/**
 * Recieve msgs from the jms-generic:topic:manageInventory topic which are sent
 * out when ManageNcInventoryMsgHandler receives a msg from the ThriftClient.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 *  04/04/12      #606       Greg Hull   Created
 *  05/11/12      #606       Greg Hull   renamed to manager and implement delete
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
public class NcInventoryMngr {
		
	public void manageInventory(String manageInvMsg ) throws Exception {

        if( manageInvMsg == null ) {
        	NcInventory.logError("NULL message received by NcInventoryMngr on "+
        			InetAddress.getLocalHost().getHostName() );
            return;
        }        
        else if( manageInvMsg.startsWith( ManageNcInventoryMsg.DELETE_INVENTORY_DIRECTIVE ) ) {
        	deleteInventory( manageInvMsg.substring( 
        			 ManageNcInventoryMsg.DELETE_INVENTORY_DIRECTIVE.length() ) );
        }
        else if( manageInvMsg.startsWith( ManageNcInventoryMsg.REINIT_INVENTORY_DIRECTIVE ) ) {
        	createInventory( manageInvMsg.substring( 
        			ManageNcInventoryMsg.REINIT_INVENTORY_DIRECTIVE.length() ), true );
        }
        else if( manageInvMsg.startsWith( ManageNcInventoryMsg.CREATE_INVENTORY_DIRECTIVE ) ) {
        	createInventory( manageInvMsg.substring( 
        			ManageNcInventoryMsg.CREATE_INVENTORY_DIRECTIVE.length() ), false );
        }
	}
	
	public void deleteInventory( String inventoryName ) throws Exception {
		
		try {
			NcInventory.deleteInventory( inventoryName );
		}
		catch ( Exception e ) {
			NcInventory.logError( e.getMessage() );
		}		
	}
	
	public void createInventory( String createInvMsg, Boolean reload ) {

		try {
			File tmpFile = File.createTempFile("CreateInventoryRequest-", ".xml" );
			FileWriter fwriter = new FileWriter( tmpFile );
			fwriter.write( createInvMsg );
			fwriter.close();
			
			Object createMsgObj =  SerializationUtil.jaxbUnmarshalFromXmlFile( tmpFile );
			
			tmpFile.delete();
			
			if( !(createMsgObj instanceof ManageNcInventoryMsg) ) {
				throw new Exception("NcInventoryMngr create Error: msg object is not ManageNcInventoryMsg");
			}
			
			ManageNcInventoryMsg invCreateMsg = (ManageNcInventoryMsg)createMsgObj;
						
			NcInventory inv = NcInventory.getInventory( invCreateMsg.getInventoryName() );
			
			// if there is already an inventory and if it was created recently (ie. by the 
			// CreateMsgHandler that sent this createInventory msg) then don't bother creating
			// the inventory again.
			//
			if( inv != null ) {
				if( System.currentTimeMillis() - inv.getLastLoadTime() < 20*1000 ) {
		        	NcInventory.logInfo( "NcInventoryMngr.creator: "+invCreateMsg.getInventoryName()+
		        			" was loaded less than 20 seconds ago. Not re-initializing"	);
		        	return;
				}
				else if( System.currentTimeMillis() - inv.getLastLoadTime() < 60*1000 ) {
		        	System.out.println( "NcInventoryMngr.creator: "+invCreateMsg.getInventoryName()+
		        			" was loaded less than 60 seconds ago. still creating inventory."	);
				}
			}
			
			String stsStr = NcInventory.initInventory( invCreateMsg.getInventoryDefinition(), 
					reload );
			 
		} catch ( Exception ex ) {
        	NcInventory.logError( ex.getMessage() );
		}
	}	
}
