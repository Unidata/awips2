package gov.noaa.nws.ncep.edex.common.ncinventory;

import java.util.HashMap;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 *  04/13/12      #606       Greg Hull   created
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
@DynamicSerialize
public class NcInventoryRequestMsg implements IServerRequest {
	
	public static enum NcInventoryRequestType {
		QUERY, 		
		DUMP,
		DIRECTORY, // list of current inventory names
		SUMMARY,
		STATISTICS // Not Implemented
	}
	
	@DynamicSerializeElement
	private NcInventoryRequestType requestType;  // Query, DeleteRequest, Dump, Summary, Statistics...
	
	@DynamicSerializeElement
	private String inventoryName;  // the userName+rscDefnName
	
	@DynamicSerializeElement
	private String requestedParam; 

	@DynamicSerializeElement
	private HashMap<String,RequestConstraint> reqConstraintsMap;

	
	public static NcInventoryRequestMsg makeQueryRequest( ) {
		NcInventoryRequestMsg msg = new NcInventoryRequestMsg();
		msg.setRequestType( NcInventoryRequestType.QUERY );
		return msg;
	}

	public static NcInventoryRequestMsg makeDumpRequest( ) {
		NcInventoryRequestMsg msg = new NcInventoryRequestMsg();
		msg.setRequestType( NcInventoryRequestType.DUMP );
		return msg;
	}
	
	public static NcInventoryRequestMsg makeDirectoryRequest( ) {
		NcInventoryRequestMsg msg = new NcInventoryRequestMsg();
		msg.setRequestType( NcInventoryRequestType.DIRECTORY );
		return msg;
	}

	public static NcInventoryRequestMsg makeSummaryRequest( ) {
		NcInventoryRequestMsg msg = new NcInventoryRequestMsg();
		msg.setRequestType( NcInventoryRequestType.SUMMARY );
		return msg;
	}

	public String getInventoryName() {
		return inventoryName;
	}

	public void setInventoryName(String inventoryName) {
		this.inventoryName = inventoryName;
	}

	public String getRequestedParam() {
		return requestedParam;
	}

	public void setRequestedParam(String requestedParam) {
		this.requestedParam = requestedParam;
	}

	public HashMap<String, RequestConstraint> getReqConstraintsMap() {
		if( reqConstraintsMap == null ) {
			return new HashMap<String,RequestConstraint>();
		}
		else {
			return reqConstraintsMap;
		}
	}

	public void setReqConstraintsMap(
			HashMap<String, RequestConstraint> reqConstraintsMap) {
		this.reqConstraintsMap = reqConstraintsMap;
	}
	
	public NcInventoryRequestType getRequestType() {
		return requestType;
	}

	public void setRequestType(NcInventoryRequestType requestType) {
		this.requestType = requestType;
	}
}
