package gov.noaa.nws.ncep.edex.common.ncinventory;

import static java.lang.System.out;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.uengine.tasks.query.MetadataCatalogQuery;
import com.raytheon.uf.common.dataquery.db.QueryParam;
import com.raytheon.uf.common.dataquery.db.QueryParam.QueryOperand;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.plugin.PluginFactory;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

/**
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 *  02/20/12      #606       Greg Hull   Created
 *  04/20/12      #606       Greg Hull   Init using one distinct query
 *  05/28/12      #606       Greg Hull   store actual db values (w/o underscores)
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
public class NcInventory {

	// map of all ncInventories by description. 
	//
	private static Map<NcInventoryDefinition,NcInventory> inventoriesMap = new HashMap<NcInventoryDefinition,NcInventory>();

	// map of all ncInventories by plugin
	// (We could do without this if it becomes too tedious to maintain. If just helps speed up
	// the inventory updates, but we could always just attempt to update all inventories with each URI)
	//
	private static Map<String,List<NcInventoryDefinition>> inventoriesByPlugin = 
					new HashMap<String,List<NcInventoryDefinition>>();

	// a map from the inventory name (userName+rscDefnName) to the inventoryDescription.
	// 
	private static Map<String,NcInventoryDefinition>  inventoryAliasMap= new HashMap<String,NcInventoryDefinition>();
	
	private static Object inventoryLock = new Object();
	
	// the descriptor for this inventory. This is the baseConstraints (which all nodes must
	// match and the inventory constraints which defines the parameter and a constraint for
	// each node level.
	private NcInventoryDefinition inventoryDefn;
		
	// This defines the levels of the inventory tree. The first level will be the pluginName
	// and the last param will be the dataTimes. 
	//
	private ArrayList<String> paramNames;

	public int getInventoryDepth() {
		return paramNames.size();
	}

	// the top level node.
	private InventoryNode treeTopNode;
	
	private long lastLoadTime = 0;

	// all nodes, not just leafs
	private long nodeCount=0;
	private long branchCount=0;
	
	// temporary holding place for a search.
	private ArrayList<InventoryNode> searchResults = new ArrayList<InventoryNode>();
	
    private final static Log logger = LogFactory.getLog(NcInventory.class);
	
	public static  NcInventory getInventory( String invName ) {

		synchronized( inventoryLock ) {
			if( !inventoryAliasMap.containsKey( invName ) ||
					inventoryAliasMap.get( invName ) == null ) {

				logError("getInventory() can't find inventory or alias for "+ invName );
				return null;
			}

			return getInventory( inventoryAliasMap.get( invName ) );
		}
	}

	public static List<NcInventory> getAllNcInventories() {
		return new ArrayList<NcInventory>( inventoriesMap.values() );
	}
	
	public static NcInventory getInventory( NcInventoryDefinition invDescr ) {

		synchronized( inventoryLock ) {
			if( !inventoriesMap.containsKey( invDescr ) ) {
				return null;
			}

			return inventoriesMap.get( invDescr );
		}
	}

	public static List<NcInventoryDefinition> getInventoriesForPlugin( String plugin ) {
		List<NcInventoryDefinition> invList = new ArrayList<NcInventoryDefinition>();
		
		synchronized( inventoryLock ) {
			if( inventoriesByPlugin.containsKey( plugin ) ) {
				return inventoriesByPlugin.get( plugin );
			}
			else {
				return invList;
			}
		}
	}
	
	//
	private NcInventory( NcInventoryDefinition id ) {

		inventoryDefn = id;

		paramNames = new ArrayList<String>( inventoryDefn.getInventoryParameters() );
		
		treeTopNode = new InventoryNode( null, "pluginName", inventoryDefn.getPluginName() );
	}

	// not synchronized since we don't want to block if re are re-initializing
	//
	public static String reinitInventories( String plugin ) {

		List<NcInventoryDefinition> invDescList = NcInventory.getInventoriesForPlugin( plugin );
		
		for( NcInventoryDefinition invDescr : invDescList ) {			
			try {
				NcInventory.initInventory( invDescr, true );
			} catch( Exception e ) {
				NcInventory.logError("Failed to re-init NcInventory "+
						invDescr.getInventoryName()+e.getMessage() );
			}					
		}
		return "N/A";
	}	
	
	public static String initInventory( NcInventoryDefinition invDescr, 
									    Boolean reload ) throws Exception {
		
		// if there is a matching inventoryDescription then get the inventory.
		// 
		NcInventory inv = getInventory( invDescr );
		NcInventory newInv = null;
		
		long t0 = System.currentTimeMillis();
		
		if( inv == null || reload ) {
			newInv = new NcInventory( invDescr );

			try {
				newInv.loadInventory();
			}
			catch( Exception e ) {
				logError( "Failed to Create Inventory for "+invDescr.toString() +
						"\nError is : " + e.getMessage() );
				
				// TODO : send an alertViz msg to cave				
				throw new Exception( "Failed to Create Inventory for "+invDescr.toString() +
							"\nError is : " + e.getMessage() );
			}
		}

		synchronized( inventoryLock ) {
		
			// if creating a new Inventory add it to the map  
			//
			if( newInv != null ) {

				if( inv != null ) {
					inv.dispose();
					inventoriesMap.remove( inv.getInventoryDefinition() );
				}

				inventoriesMap.put( invDescr, newInv );

				if( !inventoriesByPlugin.containsKey( invDescr.getPluginName() ) ) {
					inventoriesByPlugin.put( invDescr.getPluginName(), 
							new ArrayList<NcInventoryDefinition>() );
				}
				
				List<NcInventoryDefinition> idList =
								inventoriesByPlugin.get( invDescr.getPluginName() );
				
				if( !idList.contains( newInv.getInventoryDefinition() ) ) {
					idList.add( newInv.getInventoryDefinition() );
				}

				logInfo("Inventory "+invDescr.getInventoryName() + " loaded "+
						newInv.getNodeCount() + " nodes and "+ 
						newInv.getBranchCount() + " branches in "+
							(System.currentTimeMillis()-t0) +" msecs" );
			}

			// make sure that there is an alias for this inventory
			//
			//if( !inventoryAliasMap.containsKey( invDescr.getInventoryName() ) ) {
				inventoryAliasMap.put( invDescr.getInventoryName(), invDescr );		

				if( inv != null && 
				   !invDescr.getInventoryName().equals( 
						      inv.getInventoryDefinition().getInventoryName() )) {						
					logInfo("Inventory "+ 
							invDescr.getInventoryName()+" aliased to inventory "+
							inv.getInventoryDefinition().getInventoryName() );
				}
			//}
		}
		
		return "Inventory Initialized";
	}
	
	public static void deleteInventory( String invName ) throws Exception {

		synchronized( inventoryLock ) {
			if( !inventoryAliasMap.containsKey( invName ) ||
				 inventoryAliasMap.get( invName ) == null ) {
				throw new Exception("deleteInventory() can't find alias for "+ invName );
			}

			NcInventoryDefinition invDescr = inventoryAliasMap.get( invName ); 
			
			if( !inventoriesMap.containsKey( invDescr ) ||
				 inventoriesMap.get( invDescr ) == null ) {
				throw new Exception("deleteInventory() can't find inventory for "+ invName );
			}
			
			// first remove all aliases to this inventory
			//
			ArrayList<String> invAliases = new ArrayList<String>( inventoryAliasMap.keySet() );
			
			for( String invAlias : invAliases )  {
				
				if( inventoryAliasMap.get( invAlias ).equals( invDescr ) ) {
					inventoryAliasMap.remove( invAlias );
				}
			}
			
			inventoriesMap.remove( invDescr );			
			
			logInfo("Inventory "+ 
					invDescr.getInventoryName()+" has been Deleted and all aliases removed." );
		}

	}

	public NcInventoryDefinition getInventoryDefinition() {
		return inventoryDefn;
	}
	
	public long getNodeCount() {
		return nodeCount;
	}

	public long getBranchCount() {
		return branchCount;
	}

	private boolean loadInventory() throws Exception {
		synchronized ( this ) {	
			
	        // get the dao and the record class for this plugin and use them
			// to create a DatabaseQuery.
	        //	        
	        CoreDao dao = PluginFactory.getInstance().getPluginDao( 
	        		inventoryDefn.getPluginName() );
	        String recordClassStr = PluginFactory.getInstance().getPluginRecordClassName( 
	        		inventoryDefn.getPluginName() );

	        DatabaseQuery dbQuery = new DatabaseQuery( recordClassStr );

	        List<String> distinctFields = new ArrayList<String>(
	        		       inventoryDefn.getInventoryParameters() );

	        // don't need pluginName as a field.
	        //
	        distinctFields.remove(0); 
 
	        dbQuery.addOrder( distinctFields.get( 0 ), true );
	        dbQuery.addDistinctParameter( distinctFields );

	        // get each of the base constraints and add them to the dbQuery
	        for( QueryParam  queryPrm : inventoryDefn.getBaseQueryParamsMap().values() ) {
	        	if( !queryPrm.getField().equals("pluginName" ) ) {
	        		dbQuery.addQueryParam( queryPrm );	
	        	}
	        }	        
	        
	        List<?> queryResults = dao.queryByCriteria( dbQuery );
	        
	        if( !queryResults.isEmpty() ) {
	        	
	            for( Object queryRslt : queryResults ) {
	            	// if there is only one parameter then the results will be an
	            	// Object, otherwise it will be an array of Objects.
	            	//
	                HashMap<String,Object> rsltMap = new HashMap<String,Object>();
	                rsltMap.put( "pluginName", inventoryDefn.getPluginName() );
	                
	            	if( distinctFields.size() == 1 ) {
	            	   	rsltMap.put( distinctFields.get(0), queryRslt );
	            	}
	            	else {
	            		if( !queryRslt.getClass().isArray() ) {
	            			throw new Exception(
	                	          "dao.queryByCriteria returned unexpected non-array type???");
	            		}
		                Object rsltArray[] = (Object[])queryRslt;

		                for( int p=0 ; p<distinctFields.size() ; p++ ) {
		                	rsltMap.put( distinctFields.get(p), rsltArray[p] );
		                }
		            	
	            	}
//	                ArrayList<Object> rsltArray = new ArrayList<Object>(
//	                			            Arrays.asList( (Object[]) queryRslt ) );
	                
	            	// if updateNode returns false then the rsltMap was not added to the
	            	// tree. Since our query should only return distinct results this is a sanity check.
	                if( !treeTopNode.updateNode( rsltMap ) ) {
	                	logWarning("NcInventory:loadInventory sanity check; distinct query result ("+
	                			queryRslt.toString()+ ") did not add a new node to the tree???" );
	                }
	            }
	        }
	
//			treeTopNode.queryChildNodes();
			lastLoadTime = System.currentTimeMillis();
			return true;
		}
	}
		
	public long getLastLoadTime() {
		return lastLoadTime;
	}
	
    public class InventoryNode {
    	private String paramName; 
    	private String paramValue; // straight from the DB and may contain spaces. 
    							  // when returning the value from a search or when 
    	                         // updating from a URI the spaces will be replaced by '_'s.    	
    	private InventoryNode parentNode;
    	
    	// map from paramvalues to a node that may store    	
    	private HashMap<String,InventoryNode> childNodes;

//    	private int nodeDepth;
    	
    	public InventoryNode( InventoryNode parent, String pName, String pValue ) { 
    		parentNode = parent;
    		paramName  = pName;
    		
    		paramValue = pValue;
//    		nodeCount++; // not the top node
    	}
    	
    	public InventoryNode getParamNode() {
    		return parentNode;
    	}

    	public String getParamName() {
    		return paramName;
    	}
    	
    	public String getParamValue() {
    		return paramValue;
    	}
    	
    	public String getParamValueNoSpaces() {
    		return paramValue.replaceAll(" ", "_");
    	}

    	// used to index into paramNames
    	public int getNodeDepth() {
    		return (parentNode == null ? 0 : parentNode.getNodeDepth()+1 );
    	}
    	
    	// add a constraint for this node to the requestConstraint for its parent.
    	// This is used when querying the DB (ie initializing the inventory)
    	//
    	public HashMap<String, QueryParam> getRequestConstraintsForNode() {
    		
    		if( parentNode == null ) {
    			return new HashMap<String,QueryParam>( inventoryDefn.getBaseQueryParamsMap() );
    		}
    		HashMap<String, QueryParam> nodeConstraints = 
    								parentNode.getRequestConstraintsForNode();
    	
    		// use the actual paramValue (as queried from the db) which may contain spaces
    		nodeConstraints.put( paramName, new QueryParam( paramName, paramValue ) ); 
    		
    		return nodeConstraints;        		
    	}

    	// go up the tree and get
    	public String[] getBranchValue() {
    		if( parentNode == null ) {
    			String[] brVals = new String[ getInventoryDepth() ];
    			brVals[0] = getParamValueNoSpaces(); 
    			return brVals;
    		}
    		else {
    			String[] brVals = parentNode.getBranchValue();
    			brVals[ getNodeDepth() ] = getParamValueNoSpaces();
    			return brVals;
    		}
    	}

    	private void getBranchValueAsString( StringBuffer sbuf, String delimiter ) {
    		if( parentNode == null ) {    			
    			sbuf.append( getParamValueNoSpaces() ); // with a preceding '/' the string.split will not be indexed correctly
    			return ;
    		}
    		else {
    			parentNode.getBranchValueAsString(sbuf, delimiter);
    			sbuf.append( delimiter+getParamValueNoSpaces() );
    		}
    	}

    	// 
    	public InventoryNode createChildNode( String prmName, String prmValue ) {
    		
    		if( childNodes == null ) {    			
    			childNodes = new HashMap<String,InventoryNode>();
    		}
    		else { // sanity check that the prmName matches other child nodes
    			assert( !childNodes.isEmpty() );
    			assert( childNodes.values().iterator().next().getParamName().equals( prmName ) );
    		}
    		
    		if( childNodes.containsKey( prmValue ) ) {
    			return childNodes.get( prmValue );
    		}
    		else {
    			InventoryNode newNode = new InventoryNode( this, prmName, prmValue );
    			
    			String keyStr = newNode.getParamValue();//.replaceAll(" ","_");
    			
    			childNodes.put( keyStr, newNode );
    		
    			nodeCount++;
//        		out.println("NcInventory param "+prmName+" node depth = "+getNodeDepth() + " inv depth "+getInventoryDepth() );
        		
    			if( newNode.getNodeDepth() == getInventoryDepth()-1 ) {
        			branchCount++;
        		}
        		
    			return newNode;
    		}    		    		
    	}
    	
    	// NOTE : this will use the '_' version of the prmValue to look up 
    	// the child node in the map.
    	public InventoryNode getChildNode( String prmName, String prmValue ) {    		
    		if( childNodes == null || childNodes.isEmpty() ) {
    			return null;
    		}
//    		String keyStr = prmValue.replaceAll(" ", "_");
    		InventoryNode childNode = childNodes.get( prmValue );//keyStr );
    		
    		if( childNode != null &&
    			childNode.getParamName().equals( prmName ) ) { // sanity check    			
        		return childNode;
    		}
    		
    		if( childNode != null ) {
    			logWarning( "getChildNode(): ??? sanity check childNode prmname "+childNode.getParamName()+
    					" doesn't match requested prm " + prmName );
    		}

    		return null;
    	}
    	
    	// add a constraint for this paramName/Value and query for child nodes
    	//
    	/*********
 This was used before loadInventory was changed to query all distinct
 parameters with one query.
    	private void queryChildNodes() throws Exception { 
//    		if( paramName.startsWith( "satelliteName") ) {
//    			out.println("GINI");
//    		}
    		
    		if( getNodeDepth() == getInventoryDepth()-1 ) {
    			return;
    		}
    		
    		String childParamName = paramNames.get( getNodeDepth()+1 );

    		try {    			
    			MetadataCatalogQuery catQuery = new MetadataCatalogQuery( inventoryDescr.getPluginName() );
                catQuery.setDistinctField( childParamName );

                HashMap<String, QueryParam> nodeConstraints = getRequestConstraintsForNode();
                
                for( QueryParam qParam : nodeConstraints.values() ) {
                	if( !qParam.getField().equals("pluginName") ) {
                		catQuery.addParameter( qParam.getField(), 
                				qParam.getValue().toString(), qParam.getOperand());
                	}
                }

                // check that the queried values pass the constraintsForChildNodes      
                QueryParam childQueryParam = 
                	inventoryDescr.getQueryParamFromConstraints( childParamName ); 
                	//constraintsForChildNodes.get( childParamName ); 

                if( childQueryParam != null ) {
                	catQuery.addParameter( childQueryParam.getField(), 
                			childQueryParam.getValue().toString(), childQueryParam.getOperand() );
                }
                	
                String[] childParamValues = catQuery.execute().getValues();
                
    			for( String childPrmVal : childParamValues ) {
    				InventoryNode childNode = createChildNode( childParamName, childPrmVal );
    				childNode.queryChildNodes();
    			}

    		} catch (Exception e1) {
//    			out.println("CatalogQuery Error while initializing rscInventory "+ 
//    					": "+e1.getMessage() );
    			throw e1;
    		}		
    	}
    	*****************************/
    	    	
    	private void searchForNodes( 
    			HashMap<String,RequestConstraint> searchConstraints, String searchPrm ) {
    		
    		// if this node doesn't match the constraints, return without adding
    		// anything to the searchResults
    		if( searchConstraints.containsKey( paramName ) ) {
    			RequestConstraint reqCon = searchConstraints.get( paramName );
    			
    			if( !reqCon.evaluate( getParamValue() ) ) { // getParamValueNoSpaces() ) ) {
    				return;
    			}
    		}
    		else {
    			// true or false; if there is no constraint for this level?
    			
    		}
    		
    		// if this is the last level or if this is the requested search parameter.
    		//
			if( getNodeDepth() == getInventoryDepth()-1 ) {
				searchResults.add( this );
    		}
			// if this is the parameter being requested add it to the searchResults
			else if( !searchPrm.isEmpty() && 
					 paramName.equals( searchPrm ) ) {
    			searchResults.add( this );
    		}
    		else if( childNodes != null ) { // else search each of the child nodes
    			
    			for( InventoryNode child : childNodes.values() ) {
    				child.searchForNodes( searchConstraints, searchPrm );
    			}
    		}
    		
    		return;
    	}
    	
    	// 
    	private boolean updateNode( Map<String, Object> paramValues ) throws Exception {
			
			// if there is a constraint for this node, make sure that 
			//    the parameter passes it.  
			if( !evaluateParameterConstraint( paramName, paramValues.get( paramName ) ) ) {
				return false;
			}

//			out.println(paramValues.toString() + " passed constraint for node "+paramName );
			
			if( getNodeDepth() == getInventoryDepth()-1 ) {
    			return true;
    		}
			
			String chldPrmName = paramNames.get( getNodeDepth()+1 );

			// sanity check. this is checked for in updateInventory
			if( chldPrmName == null || 
			   !paramValues.containsKey( chldPrmName ) ) {
				 throw new Exception("sanity check!!! : param "+  chldPrmName + " not found in URI map" );
			}			
    		
			Object chldPrmValue = paramValues.get( chldPrmName );
			String chldPrmStrVal = chldPrmValue.toString();

    		InventoryNode chldNode = getChildNode( chldPrmName, chldPrmStrVal );
    		
    		if( chldNode == null ) {
        		
    			chldNode = createChildNode( chldPrmName, chldPrmStrVal );
        		
        		if( !chldNode.updateNode( paramValues ) ) {
        			childNodes.remove( chldPrmStrVal );
        			return false;
        		}
        		return true;
//        		out.println("creating child node for "+ chldPrmName+"="+chldPrmStrVal);
    		}
    		else {
    			return chldNode.updateNode( paramValues );
    		}			
    	}    
    }
    
    // 
    // the constraints here should have had any spaces replaced with '_'s.    
    //
	public String[] search( 
			HashMap<String,RequestConstraint> searchConstraints, String searchPrm ) {
	
		// lock this ncInventory so that it is not updated in the middle of a search
		//
		synchronized( this ) {
			searchResults.clear();

			if( searchPrm != null && !searchPrm.isEmpty() &&
				!paramNames.contains( searchPrm ) ) {
				out.println("Error searching inventory: param "+ searchPrm+" doen'nt exist");
				return null;
			}

			treeTopNode.searchForNodes( searchConstraints, searchPrm );

			String[] retRslts = new String[ searchResults.size() ];

			int r=0;
			StringBuffer sbuf = new StringBuffer( );

			for( InventoryNode inode : searchResults ) {
				inode.getBranchValueAsString( sbuf, "/" );
				retRslts[r++] = sbuf.toString();
				sbuf.setLength(0);
			}

			return retRslts;
		}
	}
    
	// 
	public boolean updateInventory( Map<String, Object> paramValues ) throws Exception {
		
		synchronized( this ) {			
			// check that all of the base constraints are met by the paramValues
			//
			for( String  constraintPrmName : inventoryDefn.getBaseQueryParamsMap().keySet() ) {
				// if there is no value for a constraint then we fail
				// the dataURIs for each plugin must have the information needed for all the constraints 
				// for an inventory.
				if( !paramValues.containsKey( constraintPrmName ) ) {
					throw new Exception( "dataURI has no attribute/field for inventory "+
							inventoryDefn.getInventoryName() + "'s base constraint, "+ constraintPrmName );				
				}

				if( !evaluateParameterConstraint( constraintPrmName,
								paramValues.get( constraintPrmName ) ) ) {
					return false;
				}
			}

//			out.println( paramValues.toString()+" passed base constraints for "+inventoryDescr.getInventoryName() );

			// confirm that there are param values given for all of the inventory parameters
			// 
			for( String invParam : paramNames ) {
				if( !paramValues.containsKey( invParam ) ) {
					throw new Exception( "dataURI has no attribute/field for "+
							inventoryDefn.getInventoryName() + "'s inventory constraint, "+ invParam );				
				}
			}

			// 
			return treeTopNode.updateNode( paramValues );
		}
	}

	private boolean evaluateParameterConstraint( String prmName, Object prmVal ) {
		
		// if there is no constraint for this parameter then return true.
		//
		if( !inventoryDefn.getBaseConstraints().containsKey( prmName ) ) {
			return true;
		}
		
		RequestConstraint reqConstr = inventoryDefn.getBaseRequestConstraint( prmName );
		
		// No longer need to do this since we are storing the actual db value. 
		// if the prmVal is coming from a URI then it will have already replaced '_'s with spaces.		
//		reqConstr = new RequestConstraint( 
//				reqConstr.getConstraintValue().replaceAll(" ", "_"),
//				reqConstr.getConstraintType() );					
//		if( prmVal instanceof String ) {
//			prmVal = prmVal.toString().replaceAll(" ", "_");
//		}
		
		return reqConstr.evaluate( prmVal );		
	}
	
	private void dispose() {
		// TODO : 
	}
	
	public static void logInfo( String info ) { logger.info( "NcInventory: "+info ); }
	public static void logWarning( String warn ) { logger.warn( "NcInventory: "+warn ); }
	public static void logError( String err ) { logger.error( "NcInventory: "+err ); }
}
