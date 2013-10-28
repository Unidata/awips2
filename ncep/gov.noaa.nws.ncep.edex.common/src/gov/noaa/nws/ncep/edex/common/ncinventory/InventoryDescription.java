package gov.noaa.nws.ncep.edex.common.ncinventory;

import java.util.ArrayList;
import java.util.HashMap;

import com.raytheon.uf.common.dataquery.db.QueryParam;

/**
 *   An ncInvetory is defined by a set of base constraints which every entry 
 * (from a query or dataURI) must pass, and a set of inventory constraints which
 * will define the levels in the inventory tree. 
 *    Two inventories are the same if they have the same base and inventory constraints.
 * The inventoryName is just used to know which 'user/rscDefn' created the inventory.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 *  01/20/12      #606       Greg Hull   Created
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
public class InventoryDescription {
	//private String rscDefnName;
	private String inventoryName; // the resourceDefnName
	
	private ArrayList<QueryParam>      inventoryParamsList;
	private HashMap<String,QueryParam> baseQueryParamsMap;

	public InventoryDescription( String rscName ) {
		inventoryName = rscName;
		inventoryParamsList = new ArrayList<QueryParam>();
		baseQueryParamsMap  = new HashMap<String,QueryParam>();
	}
	
	public InventoryDescription( String rscName,
			HashMap<String,QueryParam>  basePrmsMap,
			ArrayList<QueryParam>      invPrmsList ) {
		inventoryName = rscName;
		inventoryParamsList = new ArrayList<QueryParam>( invPrmsList );
		baseQueryParamsMap  = new HashMap<String,QueryParam>( basePrmsMap );
	}

	public void addBaseParameter( String prmName, String prmValue, String op ) {
		baseQueryParamsMap.put( prmName, new QueryParam( prmName, prmValue, op) );
	}

	// the order that this is called is important since it will define the 
	// levels of the tree
	public void addInventoryParameter( String prmName, String prmValue, String op ) {
		inventoryParamsList.add( new QueryParam( prmName, prmValue, op) );
	}

	public HashMap<String,QueryParam> getBaseQueryParamsMap() {
		return baseQueryParamsMap;
	}
	
	public ArrayList<QueryParam> getInventoryParamsList() {
		return inventoryParamsList;
	}
	
	public String getInventoryName( ) {
		return inventoryName;
	}
	
	public String getPluginName() {
		return baseQueryParamsMap.get("pluginName").getValue().toString();
	}
	
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((baseQueryParamsMap == null) ? 0 : baseQueryParamsMap.toString().hashCode());
		result = prime * result
				+ ((inventoryParamsList == null) ? 0 : inventoryParamsList.toString().hashCode());
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
		InventoryDescription other = (InventoryDescription) obj;
		if (baseQueryParamsMap == null) {
			if (other.baseQueryParamsMap != null)
				return false;
		} else if (!baseQueryParamsMap.toString().equals( other.baseQueryParamsMap.toString() ))
			return false;
		if (inventoryParamsList == null) {
			if (other.inventoryParamsList != null)
				return false;
		} else if (!inventoryParamsList.toString().equals(other.inventoryParamsList.toString()))
			return false;
		return true;
	}
	
	public String toString() {
		return  "InventoryName="+ inventoryName + 
				"\nBaseConstraints="+baseQueryParamsMap.toString() +
				"\nInventoryConstraints=" + inventoryParamsList.toString();
	}
}
