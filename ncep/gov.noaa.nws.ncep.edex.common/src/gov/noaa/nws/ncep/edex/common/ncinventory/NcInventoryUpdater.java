package gov.noaa.nws.ncep.edex.common.ncinventory;
/**
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 02/14/2012	#606		Greg Hull	Initial coding
 * 05/29/2012   #606        Greg Hull   replace URI '_'s with spaces
 * 
 * @author Greg Hull
 * @version 1.0
 */
import static java.lang.System.out;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.msg.DataURINotificationMessage;
import com.raytheon.edex.uengine.tasks.query.MetadataCatalogQuery;
import com.raytheon.edex.uengine.tasks.query.TableQuery;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginProperties;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.edex.core.dataplugin.PluginRegistry;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.status.StatusConstants;

/**
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 *  02/20/12      #606       Greg Hull   Created
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
public class NcInventoryUpdater  {
	
	// TODO : add statistics for how many URIs are being recieved.
	
	public NcInventoryUpdater( ) {
	}

	public Object updateInventoryFromURIs( Object obj ) {

//		out.println("updateInventoryFromURIs");

		if( !(obj instanceof DataURINotificationMessage) ){
			NcInventory.logError("Received msg that is not a DataURINotificationMessage? msg is "+
					obj.getClass().getName() );
			return null;			
		}
		
		DataURINotificationMessage uriMsg = (DataURINotificationMessage)obj;

		String[] dataURIs = uriMsg.getDataURIs();

		// loop thru the URIs in the msg and create a map of the 
		// field/attrib/parameter (whatever we are calling them) values
		//
		for( String dataURI : dataURIs ) {
		//	out.println("NcInventory Updating for URI "+ dataURI );
					
			Map<String, Object> attrsMap = new HashMap<String, Object>();

			String[] tokens = dataURI.replaceAll("_", " ").split(DataURI.SEPARATOR);
			String pluginName = tokens[1];

			attrsMap.put( "pluginName",  pluginName );
			PluginDataObject pdo = null;

			try {
		        PluginRegistry reg = PluginRegistry.getInstance();		       
		        PluginProperties props = reg.getRegisteredObject(pluginName);

		        if( props != null && props.getRecord() != null) {		        		
		        	pdo = props.getRecord().newInstance();		        	
		        }

		        if( pdo == null ) {
		        	throw new Exception( "Can't find PDO for plugin.");
		        }

				for( int i=2 ; i < tokens.length ; i++ ) {
					if( !tokens[i].equals("%") && !tokens[i].trim().isEmpty() ) {
						String fld = PluginDataObject.getDataURIFieldName( pdo.getClass(), i-2 );
					
						if( fld == null ) {
							throw new Exception("Unable to get field name");
						}
						
						Object value = pdo.getDataURIFieldValue( i-2, tokens[i] );
						
						attrsMap.put( fld, value );
					} 
					else if( tokens[i].equals("%") ) {
						String fld = PluginDataObject.getDataURIFieldName( pdo.getClass(), i-2 );
						attrsMap.put( fld, "%");
					}
				}
				
				attrsMap.put( "dataURI", dataURI );
				
				
	// HACK alert! Currently the URI for the radar plugin does not have all the fields 
	// that we need to maintain in the inventory and so we will use the URI to query  
	// the record directly and then create the attrsMap from it.
	//
				if( pluginName.equals("radar") ) {

					TableQuery query;
					List<PluginDataObject> recList = new ArrayList<PluginDataObject>(); ;

					try {
						query = new TableQuery("metadata", pdo.getClass().getName());
						query.addParameter("dataURI", dataURI );
						recList = (List<PluginDataObject>)query.execute();	
						
						if( recList.size() != 1 ) {
							out.println("??? radar query for "+dataURI+ " returned size of "+
									recList.size() );
						}
						else {
							RadarRecord radRec = (RadarRecord)recList.get(0);
							
							String elevNum = radRec.getElevationNumber().toString();
							attrsMap.put("elevationNumber", elevNum );

							String format = radRec.getFormat().toString();
							attrsMap.put("format", format );

							// Note : won't need these now but leaving here since it can't hurt.
							String operMode = radRec.getOperationalMode().toString();							
							attrsMap.put("operationalMode", operMode );
							
							String primElevAngle = radRec.getPrimaryElevationAngle().toString();
							attrsMap.put("primaryElevationAngle", primElevAngle );
						}
					}
					catch ( DataAccessLayerException daex ) {
						NcInventory.logError(
								"NcInventoryUpdate: Error updating radar plugin. Failed TableQuery to get URI: "+ dataURI+
								"\n errror is: "+ daex.getMessage() );
						continue;
					}
				}
				
				// loop thru all of the inventories that apply for this plugin and 
				// update with the URI
				//
				List<NcInventoryDefinition> invDescList = NcInventory.getInventoriesForPlugin( pluginName );
				
				for( NcInventoryDefinition invDescr : invDescList ) {
					NcInventory inv = NcInventory.getInventory(invDescr);
					try {
						if( inv != null &&
							inv.updateInventory( attrsMap ) ) {
							// add to statistics 
						}
						else {
							
						}
					} catch( Exception e ) {
						NcInventory.logError("Failed to update NcInventory. "+e.getMessage() );
					}					
				}

			} catch ( Exception e ) {
				NcInventory.logError("NcInventoryUpdate: Unable to create attr map for URI: "+ dataURI+
						"\n errror is: "+ e.getMessage() );
				continue;
			}
		}
		
		return obj; // don't think this returned obj is actually used anywhere?
	}

	public void purgePlugin(String message) throws Exception {

        if (message == null) {
        	NcInventory.logError("NULL message received by Purge Service" );
            return;
        }
 
        NcInventory.reinitInventories( message );
    }


}
