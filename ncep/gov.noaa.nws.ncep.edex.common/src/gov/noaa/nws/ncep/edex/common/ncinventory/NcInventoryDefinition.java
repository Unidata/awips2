package gov.noaa.nws.ncep.edex.common.ncinventory;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.raytheon.uf.common.dataquery.db.QueryParam;
import com.raytheon.uf.common.dataquery.db.QueryParam.QueryOperand;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestableMetadataMarshaller;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 *   An ncInvetory is defined by a set of base constraints which every entry 
 * (from a query or dataURI) must pass, and a set of inventory parameters which
 * will define the levels in the inventory tree. 
 *    Two inventories are the same if they have the same base and inventory constraints.
 * The inventoryName is just used to know which 'user/rscDefn' created the inventory.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 *  01/20/12      #606       Greg Hull   Created
 *  05/03/12      #606       Greg Hull   make ISerializable; changed to store all constraints as
 *                                       RequestConstraints in baseConstraints and access as QueryParam
 *  05/23/12      #606       Greg Hull   Save jaxb files in static_common for edex to read on startup
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
@DynamicSerialize
@XmlRootElement(name = "NcInventoryDefinition")
@XmlAccessorType(XmlAccessType.NONE)
public class NcInventoryDefinition implements ISerializableObject {
	
	@DynamicSerializeElement
	@XmlElement
	private String inventoryName; // the resourceDefnName
	
	// the names of parameters that are kept in the inventory. These are the 
	// levels in the inventory tree. There may be a
	//
	@DynamicSerializeElement
	@XmlElement
	@XmlJavaTypeAdapter(value = InvParmListAdapter.class)
	private ArrayList<String>  inventoryParameters;
	
	// all data that goes into the inventory must pass these constraints.
	// inventory parameters may have a constraint in which case these
	// will be returned separately by getInventoryQueryParams.
	//
	@DynamicSerializeElement
	@XmlElement
    @XmlJavaTypeAdapter(value = RequestableMetadataMarshaller.class)
	private HashMap<String,RequestConstraint>  baseConstraints;

	// no-arg constructor required for serialization
	public NcInventoryDefinition() {
		inventoryName = "none";
		inventoryParameters = new ArrayList<String>();
		baseConstraints  = new HashMap<String,RequestConstraint>();
	}
	
	public NcInventoryDefinition( String rscName ) {
		inventoryName = rscName;
		inventoryParameters = new ArrayList<String>();
		baseConstraints  = new HashMap<String,RequestConstraint>();
	}
	
	public NcInventoryDefinition( String rscName,
			HashMap<String,RequestConstraint>  baseConstrMap,
			ArrayList<String>      invPrmsList ) {
		inventoryName = rscName;
		inventoryParameters = new ArrayList<String>( invPrmsList );
		baseConstraints  = new HashMap<String,RequestConstraint>( baseConstrMap );
	}

	public void setInventoryName(String inventoryName) {
		this.inventoryName = inventoryName;
	}

	public void setInventoryParameters(ArrayList<String> invParamsList) {
		this.inventoryParameters = invParamsList;
	}

	public ArrayList<String> getInventoryParameters() {
		return inventoryParameters;
	}
	
	public String getInventoryName( ) {
		return inventoryName;
	}
	
	public HashMap<String, RequestConstraint> getBaseConstraints() {
		return baseConstraints;
	}

	public void setBaseConstraints(
			HashMap<String, RequestConstraint> baseConstraints) {
		this.baseConstraints = baseConstraints;
	}
	
	public String getPluginName() {
		return baseConstraints.get("pluginName").getConstraintValue().toString();
	}
	
	public RequestConstraint getBaseRequestConstraint( String paramName ) {
		return baseConstraints.get( paramName );		
	}
	
	// if there are constraints for a parameter stored in the inventory the NcInventory
	// needs to know them when making the catalog query for that parameter.
	//
	/// This was used by queryChildNodes() before it was replaced
//	public QueryParam getQueryParamFromConstraints( String paramName ) {
//		if( baseConstraints.containsKey( paramName ) ) {
//			return new QueryParam( paramName, 
//					baseConstraints.get( paramName ).getConstraintValue(),
//							getQueryOperandFromRequestConstraintType(
//									baseConstraints.get( paramName ).getConstraintType() ) );
//		}
//
//		return null;		
//	}
	
	// return a version of the baseConstraints using QueryParams instead of 
	// RequestConstraints. (Should we leave out the constraints for the inventory parameters?) 
	// 
	public HashMap<String,QueryParam> getBaseQueryParamsMap() {
		HashMap<String,QueryParam> queryParamsMap = new HashMap<String,QueryParam>();
		
		for( String prm : baseConstraints.keySet() ) {
			queryParamsMap.put( prm,
					new QueryParam( prm, baseConstraints.get(prm).getConstraintValue(),
							getQueryOperandFromRequestConstraintType(
									baseConstraints.get(prm).getConstraintType() ) ) );
		}

		return queryParamsMap;
	}
	
	public QueryOperand getQueryOperandFromRequestConstraintType( ConstraintType cType ) {
		if( cType == ConstraintType.EQUALS ) {
			return QueryOperand.EQUALS;
		}
		else if( cType == ConstraintType.NOT_EQUALS ) {
			return QueryOperand.NOTEQUALS;
		}
		else if( cType == ConstraintType.LESS_THAN ) {
			return QueryOperand.LESSTHAN;
		}
		else if( cType == ConstraintType.LESS_THAN_EQUALS ) {
			return QueryOperand.LESSTHANEQUALS;
		}
		else if( cType == ConstraintType.GREATER_THAN ) {
			return QueryOperand.GREATERTHAN;
		}
		else if( cType == ConstraintType.GREATER_THAN_EQUALS ) {
			return QueryOperand.GREATERTHANEQUALS;
		}
		else if( cType == ConstraintType.IN ) {
			return QueryOperand.IN;
		}
		else if( cType == ConstraintType.LIKE ) {
			return QueryOperand.LIKE;
		}
		else if( cType == ConstraintType.ILIKE ) {
			return QueryOperand.ILIKE;
		}
		else if( cType == ConstraintType.ISNULL ) {
			return QueryOperand.ISNULL;
		}
		else if( cType == ConstraintType.BETWEEN ) {
			return QueryOperand.BETWEEN;
		}
		else {
			return QueryOperand.EQUALS;
		}
	}
	
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((baseConstraints == null) ? 0 : baseConstraints.toString().hashCode());
		result = prime * result
				+ ((inventoryParameters == null) ? 0 : inventoryParameters.toString().hashCode());
		return result;
	}

	// Note that inventoryName is NOT part of the equals 
	// 2 users can create 2 resourceDefns with different names but as long as the constraints
	// are the same then they are the same inventory
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		NcInventoryDefinition other = (NcInventoryDefinition) obj;
		if (baseConstraints == null) {
			if (other.baseConstraints != null)
				return false;
		} else if (!baseConstraints.toString().equals( other.baseConstraints.toString() ))
			return false;
		if (inventoryParameters == null) {
			if (other.inventoryParameters != null)
				return false;
		} else if (!inventoryParameters.toString().equals(other.inventoryParameters.toString()))
			return false;
		return true;
	}
	
	public String toString() {
		return  "InventoryName="+ inventoryName + 
				"\nBaseConstraints="+baseConstraints.toString() +
				"\nInventoryConstraints=" + inventoryParameters.toString();
	}
	
	public static class InvParmListAdapter extends XmlAdapter<String, ArrayList<String>> {
		@Override
		public String marshal(ArrayList<String> v) throws Exception {
			if( v.isEmpty() ) {
				return new String("");
			}

			StringBuffer strb = new StringBuffer(v.get(0).toString());

			for( int i=1 ; i<v.size() ; i++ ) {
				strb.append( ","+v.get(i).toString());
			}
			return strb.toString();			
		}

		// assume that the input string is from the marshal method
		@Override
		public ArrayList<String> unmarshal(String v) throws Exception {
			ArrayList<String> intlist = new ArrayList<String>();
			if( v != null && !v.isEmpty() && !v.equals("[]") ) {
				String istrs[] = v.split(",");
				for( int i=0 ; i<istrs.length ; i++ ) {
					intlist.add( istrs[i].trim() );
				}
			}

			return intlist;
		}
	}
}
