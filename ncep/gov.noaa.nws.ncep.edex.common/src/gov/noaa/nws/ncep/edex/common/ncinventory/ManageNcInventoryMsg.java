package gov.noaa.nws.ncep.edex.common.ncinventory;

import java.util.ArrayList;
import java.util.HashMap;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestableMetadataMarshaller;
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
@XmlRootElement(name = "ManageNcInventoryMsg")
@XmlAccessorType(XmlAccessType.NONE)
public class ManageNcInventoryMsg implements IServerRequest {
	
	// the directiveType and also the prefix of a message put on the
	// manageInventory topic for all edexes.
	//
	public static final String CREATE_INVENTORY_DIRECTIVE = "CREATE:";

	public static final String REINIT_INVENTORY_DIRECTIVE = "REINIT:";

	public static final String DELETE_INVENTORY_DIRECTIVE = "DELETE:";
	
	public static final String CREATE_SUCCESS_RESPONSE = "Inventory Created";
	
	public static final String REINIT_SUCCESS_RESPONSE = "Inventory Reinitialized";
	
	public static final String DELETE_SUCCESS_RESPONSE = "Inventory Deleted";

	/// ... REBUILD...SYNC...???
	
	@DynamicSerializeElement
	@XmlElement
	private String directiveType = CREATE_INVENTORY_DIRECTIVE; // Create or Delete

	@DynamicSerializeElement
	@XmlElement
	private String inventoryName; // the resourceDefnName

	@DynamicSerializeElement
	@XmlElement
	private ArrayList<String> inventoryParamsList;
	
	// all data that goes into the inventory must pass these constraints.
	// inventory parameters may have a constraint in which case these
	// will be returned separately by getInventoryQueryParams.
	//
	@DynamicSerializeElement
	@XmlElement
    @XmlJavaTypeAdapter(value = RequestableMetadataMarshaller.class)
	private HashMap<String,RequestConstraint>  baseConstraints;

	public String getDirectiveType() {
		return directiveType;
	}

	public void setDirectiveType(String directiveType) {
		this.directiveType = directiveType;
	}
	
	public static ManageNcInventoryMsg makeCreateDirective() {
		ManageNcInventoryMsg msg = new ManageNcInventoryMsg();
		msg.setDirectiveType(CREATE_INVENTORY_DIRECTIVE);
		return msg;
	}

	public static ManageNcInventoryMsg makeReinitDirective() {
		ManageNcInventoryMsg msg = new ManageNcInventoryMsg();
		msg.setDirectiveType(REINIT_INVENTORY_DIRECTIVE);
		return msg;
	}

	public static ManageNcInventoryMsg makeDeleteDirective() {
		ManageNcInventoryMsg msg = new ManageNcInventoryMsg();
		msg.setDirectiveType(DELETE_INVENTORY_DIRECTIVE);
		return msg;
	}

	public NcInventoryDefinition getInventoryDefinition() {
		if( inventoryName == null || inventoryName.isEmpty() ||
			baseConstraints == null || baseConstraints.isEmpty() ||
			inventoryParamsList == null || inventoryParamsList.isEmpty() ) {
			return null;
		}
		else {
			return new NcInventoryDefinition( inventoryName, baseConstraints, inventoryParamsList );
		}
//		return inventoryDescr;
	}

	public void setInventoryDefinition(NcInventoryDefinition inventoryDescr) {
		this.inventoryName =       inventoryDescr.getInventoryName();
		this.baseConstraints =     inventoryDescr.getBaseConstraints();
		this.inventoryParamsList = inventoryDescr.getInventoryParameters();
	}	

	public String getInventoryName() {
		return inventoryName;
	}

	public void setInventoryName(String inventoryName) {
		this.inventoryName = inventoryName;
	}

	public ArrayList<String> getInventoryParamsList() {
		return inventoryParamsList;
	}

	public void setInventoryParamsList(ArrayList<String> inventoryParamsList) {
		this.inventoryParamsList = inventoryParamsList;
	}

	public HashMap<String, RequestConstraint> getBaseConstraints() {
		return baseConstraints;
	}

	public void setBaseConstraints(
			HashMap<String, RequestConstraint> baseConstraints) {
		this.baseConstraints = baseConstraints;
	}

}
