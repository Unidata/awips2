package gov.noaa.nws.ncep.viz.resources.manager;

import static java.lang.System.out;
import gov.noaa.nws.ncep.edex.common.ncinventory.NcInventoryDefinition;
import gov.noaa.nws.ncep.edex.common.ncinventory.NcInventoryRequestMsg;
import gov.noaa.nws.ncep.viz.common.SelectableFrameTimeMatcher;
import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceExtPointMngr;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.FileUpdatedMessage.FileChangeType;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
/**
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 06/09/10		  #273		Greg Hull	 Created
 * 08/31/10       #303	    Greg Hull    Update PGEN AttrSetGroups
 * 09/07/10       #307      Greg Hull    add dfltTimeRange, timelineGenMethod
 * 10/14/10	      #227		M. Li		 add EnsembleRscCategory
 * 11/27/10       #365      Greg Hull    dynamically generated resource types and sub-types
 * 02/08/11       #365      Greg Hull    dynamically generated local radars.
 * 02/28/11       #408      Greg Hull    Replace Forecast/Observed with a filter
 * 06/07/11       #445      Xilin Guo    Data Manager Performance Improvements
 * 07/15/11       #450      Greg Hull    refactor; support Localization using NcPathManager
 * 07/15/11       #450      Greg Hull    Break resourceDefns.xml into multiple files,
 *                                       Break AttrSetGroup files into multiple files
 * 07/15/11       #450      Greg Hull    rm .prm files and put into ResourceDefn
 * 10/25/11       #467      Greg Hull    recreate AttrSet on edit.
 * 10/26/11                 Xilin Guo    Added dynamicUpdateNcGribInventoryDB to update
 *                                       Ncgrid inventory
 * 11/14/11                 Xilin Guo    Fixed stringIndexOutOfBoundsExpection problem in 
 *                                       NcGrib inventory
 * 11/11/11                 Greg Hull    don't store AttrSetGroupNames in the ResourceDefns
 * 12/01/11      #518       Greg Hull    add getDefaultFrameTimesSelections()
 * 12/08/11                 Shova Gurung Modified dynamicUpdateRadarDataResource() to fix a bug (Local Radar
 * 										 data resource names not showing up correctly after being populated, if database was empty at CAVE startup)
 * 01/09/11      #561       Greg Hull    save the Locator Resource
 * 01/20/12      #606       Greg Hull    use '@' to reference plotModel files.
 * 04/23/12      #606       Greg Hull    use '@' to reference conditional filters for plot data
 * 05/27/12      #606       Greg Hull    get a list of inventoryDefinitions from Edex and save the alias in the ResourceDefn.
 * 06/05/12      #816       Greg Hull    return RD comparator. Change method to get RDs by
 *                                       category to return RD instead of type name. 
 * 11/2012		 #885		T. Lee		 processed unmapped satellite projection                                
 * 11/15/12      #950       Greg Hull    don't fail if an empty list of inventorys is returned
 * 12/16/12      #957       Greg Hull    change getAttrSetsForResource to return AttributeSets list
 * 12/18/12      #957       Greg Hull    patch the bug when deleting a localization file.
 *
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
public class ResourceDefnsMngr implements ILocalizationFileObserver {
	
	// one instance per user. (Currently only the 'base' used)
	//
    private static Map<String, ResourceDefnsMngr> instanceMap = 
    	   new HashMap<String, ResourceDefnsMngr>();

    // TODO : fold this into NcPathMngr
    private NcPathManager pathMngr;
    
	private HashMap<String,ResourceDefinition> resourceDefnsMap = null;
		
	// a list of inventories definitions available on edex.
	// used to set the inventoryInitialized flag.
	private HashMap<NcInventoryDefinition,NcInventoryDefinition> invDefnsMap = null; 
	
	// a map from either the rscType or the rscImpl (depending on if AttrSetGroups apply)
	// to a list of available Attribute Set Files returned from Localization.
	private Map<String,Map<String,AttributeSet>> attrSetMap;
	
	// map from the rscType+groupName to the AttrSetGroup that 
	// holds the list of attrSets
	private Map<String,AttrSetGroup> attrSetGroupsMap;
	
	// for parameters which reference a file under localization.
	// (ie. ColorBars and PlotModels and the plot model conditional filters)
	// NOTE that Conditional Filters dir is under the Plot Models so 
	// this is not strictly necessary.
	private static final String[] refdParamDirectories = { 
		NcPathConstants.PLOT_MODELS_DIR,
		NcPathConstants.CONDITIONAL_FILTERS_DIR,
		NcPathConstants.COLORBARS_DIR
	};
	
	private static Map<String,LocalizationFile> refdParamFilesMap;
	
//  read dynamically based on what is in the resourceDefinitions.xml
	private static String[] AvailResourceCategories = {
		ResourceName.SatelliteRscCategory, 
		ResourceName.RadarRscCategory, 
		ResourceName.SurfaceRscCategory,  
		ResourceName.UpperAirRscCategory,
		ResourceName.GridRscCategory, 
		ResourceName.PGENRscCategory, 
		ResourceName.MiscRscCategory,	
		ResourceName.EnsembleRscCategory,
		ResourceName.OverlayRscCategory,
		// These are meant to allow the user to create categories like NMAP w/o a code change if they 
		// don't like the delivered version using SurfaceRscCategory & UpperAirRscCategory
		ResourceName.SurfaceFcstRscCategory,
		ResourceName.SurfaceObsRscCategory,
		ResourceName.UpperAirFcstRscCategory,
		ResourceName.UpperAirObsRscCategory
	};	

	public static final HashMap<String,String> paramInfoForRscDefnParamsMap = new HashMap<String,String>(); 
	{
		paramInfoForRscDefnParamsMap.put("frameSpan", "Integer" );
		paramInfoForRscDefnParamsMap.put("timeMatchMethod", "TimeMatchMethod" );
		paramInfoForRscDefnParamsMap.put("dfltNumFrames", "Integer");
		paramInfoForRscDefnParamsMap.put("dfltTimeRange", "Integer" );
		paramInfoForRscDefnParamsMap.put("timelineGenMethod",  "TimelineGenMethod" );
		paramInfoForRscDefnParamsMap.put("isForecast", "Boolean" );
	}

	private static List<String> availResourceCategoriesList = Arrays.asList( AvailResourceCategories );

	private static String ATTR_SET_FILE_EXT =  ".attr";

	private static ResourceDefinition locatorRscDefn = null;
	
	private static List<VizException> badRscDefnsList = new ArrayList<VizException>();
	
	public static synchronized ResourceDefnsMngr getInstance() throws VizException {
		return getInstance("base");
	}

	public static synchronized ResourceDefnsMngr getInstance(String user) throws VizException {
		ResourceDefnsMngr instance = instanceMap.get(user);
		
        if( instance == null ) {
        	try {
        		instance = new ResourceDefnsMngr(user);        		
        		instance.readResourceDefns(); 
        	} catch( VizException ve ) {
        		throw ve;
        	}
            instanceMap.put(user, instance);
        }
        //instance.initializeInventory();
        
        return instance;
	}
	
	private ResourceDefnsMngr( String user ) {
		
		// check for an environment variable to override the inventory strategies.
		//
//		String inventoryLoadStrategy = System.getenv().get("INVENTORY_STRATEGY");
//		out.println("INVENTORY_STRATEGY is " + (inventoryLoadStrategy != null ? inventoryLoadStrategy : " Not set ") );
		
		pathMngr = NcPathManager.getInstance();		        
//        searchContexts = pathMngr.getLocalSearchHierarchy( LocalizationType.CAVE_STATIC );
	}
	
	public List<VizException> getBadResourceDefnsErrors() {
		return badRscDefnsList;
	}
	
	public ArrayList<String> getAvailResourceCategories() {
		return (ArrayList<String>) Arrays.asList( AvailResourceCategories );
	}
	
	public List<ResourceDefinition> getAllResourceDefinitions() {
		return new ArrayList<ResourceDefinition>( resourceDefnsMap.values() );
	}
	
	//
	private void readResourceDefns() throws VizException {
		if( resourceDefnsMap != null ) {
			return;
		}

		// This would read the inventoryDefns from localizations (ie what edex uses)
		// to initialize but instead we will query edex to see what's there and 
		// only create inventories that don't exist.	    
//	    checkAndSaveNcInventories();

		invDefnsMap = getInventoryDefinitions( true );
		
		// this was used to maintain the order in the resourceDefnsTable but now that 
		// these are separate files, I don't know that this will work. Need to 
		// find another way to get these in the right order for the GUI.
		long t0 = System.currentTimeMillis();

        Map<String,LocalizationFile> lFiles = pathMngr.listFiles( //searchContexts,
        	     NcPathConstants.RSC_DEFNS_DIR, new String[]{  "xml" }, true, true );

		List<String> availCategories = Arrays.asList( AvailResourceCategories );
		resourceDefnsMap = new HashMap<String,ResourceDefinition>();

		for( LocalizationFile lFile : lFiles.values() ) {
			
			try {
				readResourceDefn( lFile );
			}
			catch ( VizException e ) {
				out.println("Error creating ResourceDefn from file: "+lFile.getName() );
				out.println(" --->"+e.getMessage() );
				badRscDefnsList.add( e );
			}
		}
		
		long t1 = System.currentTimeMillis();
		out.println("Time to read "+lFiles.values().size() +" Resource Definitions: " + (t1-t0) + " ms");
		
		// Note: Temporary solution. Would prefer to fold this into the referencing
		// attr set file when changed to xml format.
		// 
		t0 = System.currentTimeMillis();
	    
		readRefParamFiles();
	    
	    t1 = System.currentTimeMillis();
	    out.println("Time to read colorbars: " + (t1-t0) + " ms");
	    
		// read in the attrSetGroupsMap (this needs the resourceDefnsMap
	    // to be set.)
		//
	    readAttrSets( );
	    long t2 = System.currentTimeMillis();
	    
	    out.println("Time to read Attr Sets: " + (t2-t1) + " ms");
		   
	    // loop thru the ResourceDefns and enable those that have been initialized and
	    // find any inventories that don't exist and create them
	    //
	    List<NcInventoryDefinition> createInvDefns = new ArrayList<NcInventoryDefinition>();
	    List<NcInventoryDefinition>  errList = new ArrayList<NcInventoryDefinition>();
	    
	    for( ResourceDefinition rd : resourceDefnsMap.values() ) {
	    	NcInventoryDefinition invDefn = null;
	    	try {
	    		invDefn = rd.createNcInventoryDefinition();
	    	}
	    	catch ( VizException e ) {
				out.println("Error creating ResourceDefn from file: "+rd.getLocalizationFile().getName() );
				out.println(" --->"+e.getMessage() );
				badRscDefnsList.add( e );				
	    	}
	    	
			if( invDefnsMap.containsKey( invDefn ) ) {
				rd.setInventoryAlias( invDefnsMap.get( invDefn ).getInventoryName() );
			}

	    	if( rd.usesInventory() && 
	    		rd.getInventoryEnabled() &&
	    	   !rd.isInventoryInitialized() ) {
	    		
	    		createInvDefns.add( rd.createNcInventoryDefinition() );
	    	}
	    }

	    if( !createInvDefns.isEmpty() ) {
	    	InventoryLoaderJob invLoader = new InventoryLoaderJob( createInvDefns, false );

	    	invLoader.schedule();

	    	// TODO : update the progress monitor
	    	while( invLoader.getNumberOfInventoriesLeftToLoad() > 0 ) {
	    		//			out.println("Inventories left to load = "+invLoader.getNumberOfInventoriesLeftToLoad() );
	    		try {
	    			Thread.sleep(400);
	    		} catch (InterruptedException e) {				
	    		}
	    	}

	    	errList = Arrays.asList( invLoader.getUninitializedInventoryDefns() );	    	
	    }

	    // for the rscDefns that just had an inventory created for them
		// enable or disable based on whether there was an error.
		//
	    for( ResourceDefinition rd : resourceDefnsMap.values() ) {
	    	
	    	if( rd.usesInventory() ) {
	    		
	    		NcInventoryDefinition invDefn = rd.createNcInventoryDefinition();

	    		// if created successfully set the inventoryName/Alias
	    		if( createInvDefns.contains( invDefn ) &&
	    		   !errList.contains( invDefn ) ) {	    			
	    		
	    			rd.setInventoryAlias( invDefn.getInventoryName() );
	    		}
	    		
	    		// if there is an inventory and if it is enabled 
	    		if( rd.getInventoryEnabled() &&
	    			!errList.contains( invDefn ) ) {
	    			
	    			rd.enableInventoryUse(); // remove the ProductAlertObserver
	    		}
	    		else {
	    			rd.disableInventoryUse(); // add the ProductAlertObserver and query types/subTypes
	    		}
	    	}
	    }
	    
//		if( errList.length == 1 ) {
//			throw new VizException("There was an error Initializing an Inventory for "+
//					errList[0].getInventoryName() );
//		}
//		else if( errList.length > 0 ) {
//			StringBuffer errStr = new StringBuffer( "There were errors Initializing "+
//					errList.length + " Inventories :\n");
//			for( int i=0 ; i<errList.length ; i++ ) {
//				errStr.append( errList[i].getInventoryName()+"\n");
//				if( i > 20 ) {
//					errStr.append("... and "+(errList.length-20)+" others...");
//					break;
//				}
//			}
//			throw new VizException( errStr.toString() );
//		}
	    
	    // validate that attrSetGroups in the ResourceDefinitions are actually present
	    //
//	    for( ResourceDefinition rd : resourceDefnsMap.values() ) {
//	    	if( rd.applyAttrSetGroups() ) {
//	    		for( String asgName : new ArrayList<String>( rd.getAttrSetGroupNames() ) ) {	    			
//	    			if( !attrSetGroupsMap.containsKey( 
//	    					rd.getResourceDefnName()+File.separator+ asgName ) ) {
//	    				rd.removeAttrSetGroup( asgName );
//	    				out.println("attrSetGroup "+ asgName+" for resourceDefn "+
//	    					rd.getResourceDefnName()+" doesn't exist???" );
//	    			}
//	    		}
//	    	}
//	    }
	    
	    // query the database to generate dynamic resource names.
	    //
	    //generateDynamicResources( );
	}

	private void readResourceDefn( LocalizationFile lFile ) throws VizException {
		List<String> availCategories = Arrays.asList( AvailResourceCategories );

		File rscDefnFile = lFile.getFile();
		
		Object xmlObj;
		try {
			xmlObj = SerializationUtil.jaxbUnmarshalFromXmlFile( 
					rscDefnFile.getAbsolutePath() );

			if( !(xmlObj instanceof ResourceDefinition) ) {
//				out.println("sanity check: "+rscDefnFile.getAbsolutePath()+
//						" is not a ResourceDefinition xml file");
				throw new VizException("sanity check: "+rscDefnFile.getAbsolutePath()+
					" is not a ResourceDefinition xml file");		
			}

			// copy the resourceDefn to the resourceDefnsMap and then
			//  verify the implementation, and add the resource Definitions to the map.
			//
			ResourceDefinition rscDefn = (ResourceDefinition)xmlObj;

			// TODO : If the definitions are modified and written out, this will drop any invalid resourceDefns. 
	    	// Should we save these write them out anyway? Make them disabled?

	    	// Validate the category
	    	if( !availCategories.contains( rscDefn.getResourceCategory() ) ) {
//	    		out.println( "Error configuring resource: " +
//	    				rscDefn.getResourceDefnName()+" : Invalid Resource Category." ); 
	    		throw new VizException( "resource: " +
	    				rscDefn.getResourceDefnName()+" has an unknown Category." ); 
	    	}
	    
	    	// Validate that the resource implementation is present and that the   
	    	// parameters are defined
	    	//  
	    	String rscImpl = rscDefn.getRscImplementation();
	    	
	    	if( !ResourceExtPointMngr.getInstance().getAvailResources().contains( rscImpl ) ) {
	    		throw new VizException("The Resource implementation: "+ rscImpl +" for "+
	    				rscDefn.getResourceDefnName()+" is not " +
    				"specified in a NC-Resource extention point" );
	    	}
	    	else {
	    		rscDefn.validateResourceParameters();
	    			    		
	    		if( resourceDefnsMap.containsKey( rscDefn.getResourceDefnName() ) ) {
	    			throw new VizException("Failed to create Rsc Defn '"+rscDefn.getResourceDefnName()+
	    					"' from file: "+rscDefnFile.getAbsolutePath()+ " because there is another Rsc Defn with this name.");
	    		}
    			resourceDefnsMap.put( rscDefn.getResourceDefnName(), rscDefn );
    			
    			if( rscImpl.equals( "Locator" ) ) {
    				locatorRscDefn = rscDefn;
    			}
    			
    			// TODO : Change this to set the LocalizationFile or the context
    		    rscDefn.setLocalizationFile( lFile );
    		    
    			if( rscDefn.usesInventory() ) {
    				
    				NcInventoryDefinition invDefn = 
    							rscDefn.createNcInventoryDefinition();
    				
    				// throws exception on error creating a defn
    				if( invDefnsMap.containsKey( invDefn ) ) {
    					
    					rscDefn.setInventoryAlias( 
							invDefnsMap.get( invDefn ).getInventoryName() );
    				}
				}
	    	}
		} 
		catch (SerializationException e) {
			throw new VizException("Error parsing "+rscDefnFile.getAbsolutePath() +" - " + e.getMessage() );
		} 		
	}

	private void readRefParamFiles( ) {
		refdParamFilesMap = new HashMap<String,LocalizationFile>();

		// This will find all .xml (must be AttrSetGroup xml files) for all
		// of the Resource implementations.
		//
		
		for( String refDir : refdParamDirectories ) {
			
			Map<String,LocalizationFile> lclFiles = pathMngr.listFiles(
					refDir, new String[]{ ".xml" }, true, false );

			if( lclFiles.isEmpty() ) {
				out.println("Error finding Files in "+refDir+" for parameter references?");
				continue;
			}

			// 
			for( LocalizationFile lclFile : lclFiles.values() ) {

				// get the resource implementation from the path.
//				String lName = lclFile.getName();
//				lName = lName.substring( NcPathConstants.NCEP_ROOT.length() );
				//			cbarName = cbarName.substring( 0, cbarName.length()-".xml".length() );

				lclFile.addFileUpdatedObserver( this );

				if( !lclFile.isDirectory() ) {
					// 	TODO : should we unmarsh here to validate?
					// 
					refdParamFilesMap.put( lclFile.getName(), lclFile );				
				}
//				else {
//					out.println("Adding observer to LFile "+lclFile.getContext()+":"+lclFile.getName() );
//				}
			}
		}
	}
	
	// initialize the attrSetMap and the attrSetGroupsMap
	//
	private void readAttrSets( ) throws VizException {

		attrSetGroupsMap = new HashMap<String,AttrSetGroup>();
		
		// This will find all .xml (must be AttrSetGroup xml files) for all
		// of the Resource implementations.
		//
		Map<String,LocalizationFile> attrSetGrpLclFiles = pathMngr.listFiles( 
					NcPathConstants.ATTR_SET_GROUPS_DIR, 
								new String[]{ ".xml" },	true, true );

        if( attrSetGrpLclFiles.isEmpty() ) {
        	out.println("Error finding AttrSetGroup Files?");
        	return;
        }
      
		// the sub-dirs under the attrSetGroups dir must match a resourceImplClass 
		// 
        // check that the naming convention is used. If not then there can be a potential problem if the  
        // group is edited since it will be given a different localization Name. 
        
		for( LocalizationFile lclFile : attrSetGrpLclFiles.values() ) {
			
			File asgFile = lclFile.getFile();
			
			if( !asgFile.exists() ) {
				out.println( "Can't open AttrSetGroup file: "+asgFile.getAbsolutePath() );
				continue;
			}

			Object asgObj;
			try {
				asgObj = SerializationUtil.jaxbUnmarshalFromXmlFile( 
						asgFile.getAbsolutePath( ) );

				if( !(asgObj instanceof AttrSetGroup) ) { // AttrSetGroupList
					out.println( asgFile.getAbsolutePath()+
							" file is expected to be an attrSetGroup file." );
					continue;
				}

			} catch (SerializationException e) {
				throw new VizException("Error Parsing file "+asgFile.getAbsolutePath( ) +"\n"+e.getMessage());
			} 

			// add the ASG's in the list to the map. (PGEN is a special case since
			// 1 'default' ASG applies to all PGEN resources.)
			AttrSetGroup asg = (AttrSetGroup) asgObj;
			
			asg.setLocalizationFile( lclFile );
			
			String rscImpl="";
			
			if( asg.getResource().equals("PGEN") ) {
				attrSetGroupsMap.put( asg.getMapKey(), asg ); // rscImpl, asg );
			}
			else {
				ResourceDefinition asgRscDefn = getResourceDefinition( asg.getResource() );
				if( asgRscDefn == null ) {
					out.println("AttrSetGroup file "+
							asgFile.getName() +" has a unknown resource:"+
							asg.getResource() );
					continue;
				}

				rscImpl = asgRscDefn.getRscImplementation();

				// validate that there is a resourceImpl for this attrSetGroup
				//
				if( !ResourceExtPointMngr.getInstance().
						getAvailResources().contains( rscImpl ) ) {
					out.println( "Can't  find Resource Implementation (class) for " +
							"attrSetGroup : " + rscImpl );

					out.println( "The Resource implementation should be specified in " +
					                    "a NC-Resource extention point" );
					// don't fail; go ahead and put the attrset in the map 
				}
				
				// check that the file name follows the convention otherwise there could be a 
				// problem if the user edits it since the name for the USER-level file will be 
				// different and not recognized as the same group.
				// 
				String lFileName = NcPathConstants.ATTR_SET_GROUPS_DIR + File.separator + rscImpl +
				            File.separator + asg.getResource()+"-"+asg.getAttrSetGroupName()+".xml";
				
				if( !lFileName.equals( lclFile.getName() ) ) 	{
					out.println("Warning: Localization file for AttrSetGroup, "+ lclFile.getName() +
							" doesn't follow the naming convention.("+lFileName+")" );
				}
				            
				attrSetGroupsMap.put( asg.getMapKey(), asg );
			}			
		}
		
		// Next set the attrSetMap. 
		
		// This is a map from the resource type or rsc impl to a map of the available attribute sets
		attrSetMap = new HashMap<String,Map<String,AttributeSet>>();
		
		// first get the attrSets for the AttrSetGroups.
		// In this case the key is the resource Implementation instead of the resource type
		//			
		Map<String,LocalizationFile> attrSetLclFiles = 
			pathMngr.listFiles( 
					NcPathConstants.ATTR_SET_GROUPS_DIR, 
							new String[]{ ATTR_SET_FILE_EXT }, true, true );

		if( attrSetLclFiles.isEmpty() ) {
			out.println("Error finding AttrSets (for AttrSetGroups) Files?");
			return;
		}

		// the sub-dirs under attrSetGroups must match a resourceImplClass 
		// 
		for( LocalizationFile asLclFile : attrSetLclFiles.values() ) {

			// get the resource implementation from the path.
//			String asLclName = asLclFile.getName().substring( ATTR_SET_GROUPS_DIR.length()+1 );
//			String rscImpl = asLclName.substring( 0,
//					asLclName.indexOf( File.separator ) );
//			String attrSetName = asLclName.substring(rscImpl.length()+1, 
//					                          asLclName.length()-ATTR_SET_FILE_EXT.length() );
			String rscImpl = asLclFile.getFile().getParentFile().getName();
//			String attrSetName = asLclFile.getFile().getName();
//			attrSetName = attrSetName.substring(0, 
//					                          attrSetName.length()-ATTR_SET_FILE_EXT.length() );
			
//			ArrayList<String> rscTypes = getRscTypesForRscImplementation( rscImpl ); 		

			if( !attrSetMap.containsKey( rscImpl ) ) {
				attrSetMap.put( rscImpl, new HashMap<String,AttributeSet>() );
			}
			try {
				AttributeSet aSet = AttributeSet.createAttributeSet(rscImpl, asLclFile );
				
				attrSetMap.get( rscImpl ).put( aSet.getName(), aSet );
			} 
			catch ( VizException e ) {
				out.println("Error Creating AttributeSet "+ asLclFile.getName() + 
						": "+ e.getMessage() ); 						
			}
		}

		// Next  get the attrSets for other resources which have attribute sets of their own.
		// In this case the key is the resource type name.
		//			
		attrSetLclFiles = pathMngr.listFiles( NcPathConstants.RSC_DEFNS_DIR, 
				new String[]{ ATTR_SET_FILE_EXT }, true, true );

		if( attrSetLclFiles.isEmpty() ) {
			out.println("Error finding AttrSet Files?");
			return;
		}

		for( LocalizationFile asLclFile : attrSetLclFiles.values() ) {

//			if( asLclFile.getContext().getLocalizationLevel() == LocalizationLevel.USER ) {
//				out.println(" USER");
//			}
			// Some resources may have more organizational directories than others. The resource
			// type is the lowest directory.
			String dirs[] = asLclFile.getName().split( File.separator );
			if( dirs == null || dirs.length < 3 ) {
				continue; // ?????
			}
			
//			String attrSetName = dirs[ dirs.length-1 ];
//			attrSetName = attrSetName.substring( 0, attrSetName.length()-ATTR_SET_FILE_EXT.length() );
			
			String rscType = dirs[ dirs.length-2 ]; 
			
			if( !attrSetMap.containsKey( rscType ) ) {
				attrSetMap.put( rscType, new HashMap<String,AttributeSet>() );
			}
			
			try {
				AttributeSet aSet = AttributeSet.createAttributeSet(rscType, asLclFile );
				
				attrSetMap.get( rscType ).put( aSet.getName(), aSet );
			} 
			catch ( VizException e ) {
				out.println("Error Creating AttributeSet "+ asLclFile.getName()+ 
						": "+ e.getMessage() ); 						
			}			
		}
		
		// validate that the attrSets referenced from the attrSetGroups actually exist
		// (PGEN is a special case)
		for( AttrSetGroup asg : attrSetGroupsMap.values() ) {
			
			String rscImpl="";
			if( asg.getResource().equals("PGEN") ) {
				rscImpl = "PGEN";
			}
			else {
				String rscType = asg.getResource();

				ResourceDefinition rscDefn = getResourceDefinition( rscType );
				rscImpl = (rscDefn != null ? rscDefn.getRscImplementation() : "");
			}
			
			for( String asName : new ArrayList<String>( asg.getAttrSetNames()) ) {
				if( !attrSetMap.containsKey(rscImpl) ||
					!attrSetMap.get( rscImpl ).containsKey( asName ) ) {
					asg.removeAttrSet(asName);
					out.println("attrSet "+asName+" in attrSetGroup "+
							asg.getResource()+File.separator+asg.getAttrSetGroupName()+" doesn't exist.");
				}
			}
		}
	}
	
	
	private HashMap<NcInventoryDefinition,NcInventoryDefinition> 
					getInventoryDefinitions( boolean reload ) throws VizException {
		
		if( invDefnsMap == null || reload ) {
			
			// query the list of inventories that exist on edex and set the
			// inventoryInitialized flag in the ResourceDefns 
			NcInventoryRequestMsg dirRequest = NcInventoryRequestMsg.makeDirectoryRequest();

			Object rslts = ThriftClient.sendRequest( dirRequest );

			if( rslts instanceof String ) {
				throw new VizException( rslts.toString() );
			}
			if( !(rslts instanceof ArrayList<?>) ) {
				out.println("Inventory Directory Directory Error: expecting NcInventoryDefinition[] return." );
				throw new VizException( "Inventory Directory Request Error: expecting ArrayList<NcInventoryDefinition>." );
			}
			else if( ((ArrayList<?>)rslts).isEmpty() ) {
				out.println("Inventory Directory Request Warning: No Inventories initialized.???" );
			}
			else if( !(((ArrayList<?>)rslts).get(0) instanceof NcInventoryDefinition) ) {
				throw new VizException( "Inventory Directory Request Error: expecting ArrayList<NcInventoryDefinition>." );
			}

			// used to set the inventory initialized flag
			ArrayList<NcInventoryDefinition> invDefnsList = (ArrayList<NcInventoryDefinition>)rslts;
			
			invDefnsMap = new HashMap<NcInventoryDefinition,NcInventoryDefinition>();
			
			for( NcInventoryDefinition invDefn : invDefnsList ) {
				invDefnsMap.put( invDefn, invDefn );
			}
		}
		
		return invDefnsMap;
	}
	
	public boolean isResourceNameValid( ResourceName rscName ) {
		if( rscName == null ||
			rscName.getRscCategory() == null || 
			rscName.getRscCategory().isEmpty() ||
			rscName.getRscType() == null || 
			rscName.getRscType().isEmpty() ||
			rscName.getRscAttrSetName() == null ||
			rscName.getRscAttrSetName().isEmpty() ) {
			
			return false;
		}
		
		ResourceDefinition rd = getResourceDefinition( rscName );

		if( rd == null ) {
			return false;
		}
		
		// if there is a generating type then check for a ':'
		//
		if( !rd.getRscTypeGenerator().isEmpty() ) {
			if( rscName.getRscType().indexOf(
					ResourceName.generatedTypeDelimiter ) == -1 ) {
//				out.println("ResourceName "+rscName.toString() + " is expecting a "+
//						"generated type from the "+ rd.getRscTypeGenerator() );
				return false;
			}
		}
		// if there is no group/subType, make sure there isn't supposed to be one.
		if( rscName.getRscGroup() == null || 
			rscName.getRscGroup().isEmpty() ) {

			if( rd.applyAttrSetGroups() || !rd.getSubTypeGenerator().isEmpty() ) {
//				out.println("ResourceName "+rscName.toString() + " is expecting a "+
//				"generated sub-type from "+ rd.getSubTypeGenerator() );
				return false;				
			}				
		}
		else { // and if there is one 
			
		}

		return true;
	}

	// read all of the NcInventoryDefinitions from COMMON_STATIC and
	// If a resourceDefn doesn't have a matching Inventory then save 
	// one for edex to use on a restart.
	//    NOTE THIS WON"T WORK SINCE EDEX CAN"T READ FROM USER 
	// WE COULD USE THIS AS A WAY TO KNOW WHETHER AN INVENTORY EXISTS BUT
	// INSTEAD WE WILL QUERY THE ACTUAL INVENTORY DENFS FROM EDEX
//	private void checkAndSaveNcInventories() throws VizException {        
//        LocalizationContext lCntxts[] = new LocalizationContext[3]; 
//        lCntxts[0] = pathMngr.getContext( 
//        		LocalizationType.COMMON_STATIC, LocalizationLevel.BASE );
////      lCntxts[1] = pathMngr.getContext( 
////        		LocalizationType.COMMON_STATIC, LocalizationLevel.DESK );
//        lCntxts[1] = pathMngr.getContext( 
//        		LocalizationType.COMMON_STATIC, LocalizationLevel.SITE );
//        lCntxts[2] = pathMngr.getContext( 
//        		LocalizationType.COMMON_STATIC, LocalizationLevel.USER );
//        
//        Map<String, LocalizationFile> invDefnFilesMap =
//        	NcPathManager.getInstance().listFiles( 
//        			lCntxts, NcPathConstants.NCINVENTORY_DEFNS_DIR, 
//        				new String[]{".xml"}, true, true );
//        
//        ArrayList<NcInventoryDefinition> invDefnsArray = 
//        					new ArrayList<NcInventoryDefinition>();
//        
//        for( LocalizationFile lFile : invDefnFilesMap.values() ) {
//        	out.println("invDefn  file is :"+ lFile.getName() );
//        	File invDefnFile = lFile.getFile();
//    		
//    		Object xmlObj;
//    		try {
//    			xmlObj = SerializationUtil.jaxbUnmarshalFromXmlFile( 
//    								invDefnFile.getAbsolutePath() );
//    			if( !(xmlObj instanceof NcInventoryDefinition) ) {    				
//    				throw new VizException("NcInventoryDefinition .xml file is not an NcInventoryDefinition object???");
//    			}
//    			
//    			invDefnsArray.add( (NcInventoryDefinition)xmlObj );    			
//    		}
//    		catch( Exception ex ) {
//    			System.out.println( ex.getMessage() );
//    		}
//        }
//
//        for( ResourceDefinition rscDefn : resourceDefnsMap.values() ) {
//        	if( !rscDefn.usesInventory() ) {
//        		continue;
//        	}
//        	
//        	try {
//        		NcInventoryDefinition invDefn = rscDefn.createNcInventoryDefinition(); 
//
//        		if( !invDefnsArray.contains( invDefn ) ) {
//        		
//        			String invDefnFileName = NcPathConstants.NCINVENTORY_DEFNS_DIR+
//        			 	 	 		File.separator + invDefn.getInventoryName()+".xml";
//        			LocalizationFile lFile = 
//        				NcPathManager.getInstance().getLocalizationFile( 
//        						lCntxts[2], invDefnFileName );
//        			               			
//        			SerializationUtil.jaxbMarshalToXmlFile( invDefn, 
//        									lFile.getFile().getAbsolutePath() );
//        			lFile.save();
//        			out.println("Localizing NcInventory, "+ invDefn.getInventoryName()+
//        					" to LFile:"+lFile.getName() );
//        		}
//        		
//        	} catch( VizException vizex ) {
//        		out.println( vizex.getMessage() );
//        	} catch (SerializationException e) {
//				throw new VizException("Error Serializing NcInventory:"+e.getMessage() );
//			} catch (LocalizationOpFailedException e) {
//				throw new VizException("Error Localizing file:"+e.getMessage() );
//			} 
//        }        
//	}
	
	// similar to validateResourceParameters except we are also checking for the attributes 
//	public void verifyParametersExist( ResourceName rscName ) throws VizException {
//		ResourceDefinition rscDefn = getResourceDefinition( rscName.getRscType() );
//
//		if( rscDefn == null ) {
//			throw new VizException("Unable to find resource definition for "+rscName.toString() );
//		}
//		
//		rscDefn.validateResourceParameters();
//		
//		HashMap<String, ResourceParamInfo> rscImplParams = 
//			ResourceExtPointMngr.getInstance().getResourceParameters( rscDefn.getRscImplementation() );
//
//		// the default values specified in the extention point
//		HashMap<String,String> dfltParamValues = rscDefn.getDefaultParameterValues();
//
//		// the parameters defined by the resource definition
//		HashMap<String,String> paramValues = getAllResourceParameters( rscName );
//
//		// a list of all the generated parameters
//		List<String> genParamsList = new ArrayList<String>( Arrays.asList( rscDefn.getSubTypeGenParamsList() ) ); 
//		if( !rscDefn.getRscTypeGenerator().isEmpty() ) {
//			genParamsList.add( rscDefn.getRscTypeGenerator() );
//		}
//		
//		// check that all the parameters defined for the implmentation either have a 
//		// value given in the rsc params, will be generated, or have a default value 
//		// 
//		for( ResourceParamInfo implPrmInfo : rscImplParams.values() ) {
//			String implPrm = implPrmInfo.getParamName();
//			String prmConstraintName = implPrmInfo.getConstraintName();// same as paramName except for GDFILE and 1 or 2 others. 
//
//			if( implPrmInfo.getParamType() == ResourceParamType.EDITABLE_ATTRIBUTE ||
//				implPrmInfo.getParamType() == ResourceParamType.NON_EDITABLE_ATTRIBUTE ) {
//				
//				// if the needed param is not set in the resource defn or is set to empty 
//				String paramValue = paramValues.get( implPrm );
//
//				if( paramValue == null || paramValue.isEmpty() ) {
//
//					paramValue = dfltParamValues.get( implPrm );
//
//					// if there is no default value specified by the implementation
//					//
//					if( paramValue == null || paramValue.isEmpty() ) {
//						throw new VizException( rscDefn.getResourceDefnName()+ 
//								" is missing a value for the attribute "+implPrm+"." );
//					}
//					else {
//						out.println("Setting attribute "+implPrm+" to default value "+paramValue );
//					}
//				}
//			}
//			
//			// if this is not a generated request constraint, then check the inventory to make
//			// sure that this value is in the database
//			//			
//			else if( implPrmInfo.getParamType() == ResourceParamType.REQUEST_CONSTRAINT ) {
//				// if there is not a value for this parameter
//				// 
//				if( !paramValues.containsKey( implPrm ) ) {
//					throw new VizException( 
//							"The parameter "+implPrm+" is not available for resource "+ rscDefn.getResourceDefnName()+"." );
//				}
//				
//				// if this parameter is in the inventory then see if we need to 
//				//     see if the value (given in the selected attrSet) is in the inventory.
//				//
//				if( rscDefn.getInventoryParameterNames().contains( prmConstraintName ) ) {
//					
//					// if this is not a generated parameter 
//					// (assume that since it was generated then it must be in the inventory,
//					//  currently this is true but may not be if this method is ever
//					//  called from other than updateSelectedResource.)
//					// 
//					if( !genParamsList.contains( prmConstraintName ) ) {
//
//						ArrayList<String> qRslts = rscDefn.queryInventoryParameter( rscName, prmConstraintName );
//
//						if( qRslts.isEmpty() ) {
//							throw new VizException( 
//							   "The parameter value "+implPrm+"="+paramValues.get(implPrm)+
//							   " is not available for resource "+ rscDefn.getResourceDefnName()+"." );
//						}	
//
//						out.println("Inventory has "+ qRslts.size() +" values");
//					}
//				}
//			}
//		}
//		
//		// check that all of the paramValues are specified for the rsc implementation
//		// 
//		for( String prm : paramValues.keySet() ) {
//			if( !prm.equals("pluginName") &&
//				!paramInfoForRscDefnParamsMap.containsKey( prm ) &&
//				!rscImplParams.containsKey( prm ) ) {
//
//				//					out.println("Warning: parameter "+prm+" for "+getResourceDefnName()+
//				//						" is not recognized for the resource implementation "+getRscImplementation() );
//
//				throw new VizException( rscDefn.getResourceDefnName()+" has a parameter, "+prm+
//						", that is not recognized by its implementation: "+ rscDefn.getRscImplementation() );
//			}
//		}
//	}
	
				    			
	public ResourceDefinition getResourceDefinition( ResourceName rscName ) {
		return (rscName != null ? getResourceDefinition( rscName.getRscType() ) : null);
	}
	
	public boolean findResourceDefinition( String rscType ) {
		if( resourceDefnsMap.containsKey( rscType ) ) {
			return true;
		}
		
		// allow for generated types which will have a ':' 
		//
		int indx = rscType.indexOf(":");

		if( indx == -1 ) {
			return false;
		}

		return resourceDefnsMap.containsKey( rscType.substring( 0, indx ) );
	}
	
	public ResourceDefinition getResourceDefinition( String rscType ) {
		if( resourceDefnsMap.containsKey( rscType ) ) {
			return resourceDefnsMap.get( rscType );
		}
		
		// allow for generated types which will have a ':' 
		//
		int indx = rscType.indexOf(":");

		if( indx != -1 ) {			
			if( resourceDefnsMap.containsKey( rscType.substring( 0, indx ) ) ) {
				return resourceDefnsMap.get( rscType.substring( 0, indx ) );
			}
		}

//		out.println("sanity check: can't find ResourceDefinition for: "+rscType );
		return null;
	}

	public String[] getResourceCategories( boolean includeDisabled ) {
		ArrayList<String> catsList = new ArrayList<String>();

    	for( ResourceDefinition rscDefn : resourceDefnsMap.values() ) {
    		if( includeDisabled || rscDefn.getIsEnabled() ) {
        		String rscCat = rscDefn.getResourceCategory();
    			
    			if( !catsList.contains( rscCat ) ) {
    				catsList.add( rscCat );
    			}
    		}
    	}
    	
    	String[] catsArray = catsList.toArray( new String[0] );
    	
    	Arrays.sort( catsArray, new Comparator<String>() {
    		public int compare(String o1, String o2) {
    			int indx1 = availResourceCategoriesList.indexOf( (String)o1 );
    			int indx2 = availResourceCategoriesList.indexOf( (String)o2 );

    			return ((indx1 == indx2 ? 0 : (indx1 < indx2 ? -1 : 1 )));
    		}
		}); 
		
    	return catsArray;
    	
//    	return catsList.toArray( new String[0] );
//		return ResourceCategories;
	}
	
	// map the Full Resource Name to the location of the resource bundle template file 
	// for the resource.
	// The rsc name is the RBD Category/Type/Group/AttributeSet. The fcst/obs is not saved
	// so we try both to find a match.
	public File getRscBundleTemplateFile( String rscType ) {		
		ResourceDefinition rscDefn = getResourceDefinition( rscType );
		
		if( rscDefn == null ) {
			return null;
		}

		// get the name of the NC resource which will implement the resource
		String rscImplName = rscDefn.getRscImplementation();
		
		if( !rscImplName.endsWith(".xml") ) {
			rscImplName = rscImplName + ".xml";
		}
		
		File rscTemplateFile = pathMngr.getStaticFile( 
				NcPathConstants.RSC_TMPLTS_DIR+File.separator+rscImplName );
			
		return (  rscTemplateFile.exists( ) ? rscTemplateFile : null );
	}
	
	// 
	public boolean isResourceEnabled( String rscType ) {
		ResourceDefinition rscDefn = getResourceDefinition( rscType );
		
		return ( rscDefn == null ? false : rscDefn.getIsEnabled() );
	}
	
	public boolean setResourceEnabled( String rscType, boolean isEnabled ) {
		ResourceDefinition rscDefn = getResourceDefinition( rscType );
		
		if( rscDefn == null ) {
			return false;
		}
		
		rscDefn.setEnabled( isEnabled );
		return true;
	}
	
	public boolean doesResourceUseAttrSetGroups( String rscType ) {
		ResourceDefinition rscDefn = getResourceDefinition( rscType );
		
		return ( rscDefn == null ? false : rscDefn.applyAttrSetGroups() );
	}
	
	// 
	//
	public String getDefaultFrameTimesSelections( ResourceName rscName ) throws VizException {		
		ResourceDefinition rscDefn = getResourceDefinition( rscName.getRscType() );

		if( rscDefn == null ) {
			return null;
		}

		HashMap<String,String> paramsMap = new HashMap<String,String>( rscDefn.getResourceParameters(false) );

		AttributeSet attrSet = getAttrSet( rscName );

		if( attrSet != null ) {
			paramsMap.putAll( attrSet.getAttributes() ); 
		}

		if( paramsMap.containsKey("GDATTIM" ) ) {

			// check syntax
			new SelectableFrameTimeMatcher( paramsMap.get("GDATTIM") ); 

			return paramsMap.get("GDATTIM");
		}
		else {
			return null;
		}		
	}
	
	// get the Attribute Set File for the given resource name. This may either 
	// be in the AttrSetGroup directory or directly under the resources config dir.
	
	// Get all parameters needed to instantiate the bundle template
	//   This includes parameters from the ResourceDefinition, attributes and timeMatching/frameCount...
	//
	public HashMap<String,String> getAllResourceParameters( ResourceName rscName ) throws VizException {

		ResourceDefinition rscDefn = getResourceDefinition( rscName.getRscType() );

		if( rscDefn == null ) {
			return null;
		}
		
		// first get the parameters defined by the RscDefn and use the default values
		// for any that are not present. 
		HashMap<String,String> paramsMap = new HashMap<String,String>( rscDefn.getResourceParameters(true) );

		// next get the attributes
		AttributeSet attrSet = getAttrSet( rscName );

		if( attrSet != null ) {
			paramsMap.putAll( attrSet.getAttributes() ); 
		}
		
		if( paramsMap.containsKey("GDATTIM") ) {
			paramsMap.remove("GDATTIM");
		}
		
		// and now create the parameters from the rscDefinitions file.
		// (frameInterval, timeMatchMethod)
		if( !rscDefn.getResourceCategory().equals( ResourceName.OverlayRscCategory ) ) {
			paramsMap.put("frameSpan", Integer.toString( rscDefn.getFrameSpan() ) );
			paramsMap.put("timeMatchMethod", rscDefn.getTimeMatchMethod().toString() );
			paramsMap.put("dfltNumFrames", Integer.toString( rscDefn.getDfltFrameCount()));
			paramsMap.put("dfltTimeRange", Integer.toString( rscDefn.getDfltTimeRange()) );
			paramsMap.put("timelineGenMethod",  rscDefn.getTimelineGenMethod().toString() );
			paramsMap.put("isForecast", (rscDefn.isForecast() ? "true" : "false" ) );
		}

		// if this is a generated type get the parameter value from the type in the ResourceName
		//
		String typeGenParam = rscDefn.getRscTypeGenerator();
		
		if( !typeGenParam.isEmpty() ) {

			String rscType = rscName.getRscType();
			int indx = rscType.indexOf(":");
			if( indx == -1 ) {
				throw new VizException("sanity check: Can't parse generated typ from "+
						"Resource name :"+rscName.toString() );
			}

			String typeName = rscType.substring( indx+1 );  

			paramsMap.put( typeGenParam, typeName );
		}
		
		// If there is a generated sub-type then we will need to set a parameter for this 
		// (In this case the name of the parameter in the paramsMap must be the same as the 
		// name of the variable in the BundleTemplate.) 
		// 
		String[] subTypeGenParams = rscDefn.getSubTypeGenParamsList();
		
		if( subTypeGenParams.length == 1 ) {
			paramsMap.put( subTypeGenParams[0], rscName.getRscGroup() );
		}
		
		// TODO : Note this currently only works for Satellite because its
		// the only resource that uses 2 generating params, (the area and resolution) 
		// A trailing 'km' which means this parsing code is not generic
		//
		else if( subTypeGenParams.length == 2 ) {
			
			String subType = rscName.getRscGroup();
			//if( !subType.endsWith( "km" ) ) {
			//	out.println("Sanity check : SubType "+subType +" is expected to end with 'km'");
			//}

			int indx = subType.lastIndexOf( '_' );
			if( indx != -1 ) {
				String paramVal1 = subType.substring(0, indx);
				String paramVal2 = subType.substring(indx+1, subType.length()-2); // NOTE; "km"

				try {
					int ok = Integer.parseInt(paramVal2);
				} catch (NumberFormatException e ) {
					paramVal2 = "0";
				}

				String subtypeGenParam = subTypeGenParams[0];
				paramsMap.put( subTypeGenParams[0], paramVal1 );
				paramsMap.put( subTypeGenParams[1], paramVal2 );			
			}
		}
		
		// just one more hack since we can't have PGEN save a .prm file for some reason.
		if( rscDefn.isPgenResource() ) {
			if( !rscName.getRscGroup().isEmpty() ) {
				paramsMap.put("productName", rscName.getRscGroup() );
				paramsMap.put("legendString", "PGEN : "+ rscName.getRscGroup() );
			}
		}
				
		return paramsMap;		
	}
					
	// 
	public static HashMap<String,String> readAttrSetFile( File asFile ) throws VizException {
		// parse the attrset file to get the attrs to substitude into the 
		// Bundle Template file.
		HashMap< String, String > rscAttrMap = new HashMap<String,String>();
		
		if( asFile.length() == 0 ) {
			return rscAttrMap;
		}
		
		try {
			FileReader freader = new FileReader( asFile );
			BufferedReader breader = new BufferedReader( freader );
			String prmStr = breader.readLine().trim();

			while( prmStr != null ) {
				if( prmStr.isEmpty() || prmStr.charAt(0) == '!' ) {  // comments
					prmStr = breader.readLine();
					continue;
				}
				
				int eq_indx = prmStr.indexOf('=');
				if( eq_indx == -1 ) {
					throw new VizException("The resource prm file, "+
							asFile.getName()+", has a non-comment line with no '='");
					//prmStr = breader.readLine(); // uncomment if this is not considered a fatal error.
					//continue;
				}
				else {
					String prmKey = prmStr.substring(0,eq_indx).trim();
					String prmVal = prmStr.substring(eq_indx+1).trim();

//					if( prmKey.equals("SKIP" ) ) {
//						out.println("SKIP");
//					}
					// '@' used to be a reference to a file in the same directory but with 
					// the localization, and since this is only used for colorbars, 
					// 					
					if( !prmVal.isEmpty() && prmVal.charAt(0) == '@' ) {
						try {
							String refdLclName = NcPathConstants.NCEP_ROOT+prmVal.substring(1);
											
							if( !refdParamFilesMap.containsKey( refdLclName ) ) {
								throw new VizException("Error reading file: "+asFile.getAbsolutePath() + 
										" : Unable to find file for parameter reference " +prmVal+"'." );
							}
							File lFile = refdParamFilesMap.get( refdLclName ).getFile( true );

							if( !lFile.exists() ) {
								throw new VizException("Error reading file: "+asFile.getAbsolutePath() + 
										" : File for parameter reference " +prmVal+"' doesn't exist." );
							}

							FileReader fr = new FileReader(lFile);
							char[] b = new char[(int) lFile.length()];
							fr.read(b);
							fr.close();

							prmVal = new String(b).trim();
		// remove the xml header
							if( prmVal.startsWith("<?xml") ) {
								if( prmVal.indexOf("?>") != -1 ) {
									prmVal = prmVal.substring(
											prmVal.indexOf("?>")+2 ).trim();
								}
							}
						} catch (FileNotFoundException fnf ) {
							throw new VizException( fnf );
						} catch (IOException ioe ) {
							throw new VizException( ioe );
						} catch (LocalizationException lex ) {
							throw new VizException( lex );
						}
					}

					rscAttrMap.put( prmKey.trim(), prmVal.trim() );					
				}
				prmStr = breader.readLine();
			}
		} catch (FileNotFoundException fnf ) {
			throw new VizException( "Can't find referenced file: "+asFile.getAbsolutePath() );
		} catch (IOException fnf ) {
			throw new VizException( "Can't open referenced file: "+asFile.getAbsolutePath() );
		}		

		return rscAttrMap;
	}
	
	// sort with the Obs types first and then the Fcst, and then alphabetically 
	public Comparator<ResourceDefinition> getDefaultRscDefnComparator() {
		
		return new Comparator<ResourceDefinition>() {
			@Override
			public int compare(ResourceDefinition rscDefn1, ResourceDefinition rscDefn2) {

				if( rscDefn1 == null ) return  1;
				if( rscDefn2 == null ) return -1;

				// categories will be the same for the types but we may want to order them differently
				// based on the category
				//
				// for Surf or UAIR, Obs before Fcst  
				if( rscDefn1.getResourceCategory().equals( ResourceName.SurfaceRscCategory ) ||
					rscDefn1.getResourceCategory().equals( ResourceName.UpperAirRscCategory ) ) {

					if( (!rscDefn1.isForecast() && rscDefn2.isForecast() ) ||
						 (rscDefn1.isForecast() && !rscDefn2.isForecast() ) ) {    				

						return ( rscDefn1.isForecast() ? 1 : -1);
					}
				}
				// for Radar, Mosaics before Local Radar
				else if( rscDefn1.getResourceCategory().equals( ResourceName.RadarRscCategory ) ) {
					if( rscDefn1.getFilterLabels().contains("RadarMosaic") || 
						rscDefn1.getRscImplementation().equals("RadarMosaic") ) {
						return -1;
					}
					else if( rscDefn2.getFilterLabels().contains("RadarMosaic") || 
							 rscDefn2.getRscImplementation().equals("RadarMosaic") ) {
						return 1;
					}    				
				}

					return rscDefn1.getResourceDefnName().compareToIgnoreCase( 
						   rscDefn2.getResourceDefnName() );    						
			}
		};
	}

	// loop thru all the rsc defns for this cat and return a list of all
	// filter labels.
	public List<String> getFilterLabelsForResourceCategory( String rscCat ) {
		
		ArrayList<String> filterLabelsList = new ArrayList<String>();

    	for( ResourceDefinition rscDefn : resourceDefnsMap.values() ) {
    		
    		if( !rscDefn.getIsEnabled() ||
    			!rscDefn.getResourceCategory().equals( rscCat ) ) {
    			continue;
    		}
    		
    		for( String filtStr : rscDefn.getFilterLabels() ) {
    			
    			if( !filterLabelsList.contains( filtStr ) ) {
    				filterLabelsList.add( filtStr );
    			}
    		}
    	}

    	return filterLabelsList;
	}
	
    public  List<ResourceDefinition> getResourceDefnsForCategory( String rscCat ) throws VizException {
    	return getResourceDefnsForCategory(rscCat, "", false, true );
    }

    public  List<ResourceDefinition> getResourceDefnsForCategory( 
    		String rscCat, String filterStr, 
    		Boolean includeGeneratedTypes,
    		Boolean includeDisabledRscDefns ) 
    						throws VizException {
    	List<ResourceDefinition> resourceDefnsList = new ArrayList<ResourceDefinition>();
       	
       	// for other resources, get all of the resources in the given category.
       	//
    	for( ResourceDefinition rscDefn : resourceDefnsMap.values() ) {

    		if( !includeDisabledRscDefns && !rscDefn.getIsEnabled() ) {
    			continue;
    		}
    		
    		if( filterStr == null || filterStr.isEmpty() ||
    			rscDefn.getFilterLabels().contains( filterStr ) ) {
    			
    			if( rscDefn.getResourceCategory().equals( rscCat ) ) {
    				if( rscDefn.getRscTypeGenerator().isEmpty() ) {
    					resourceDefnsList.add( rscDefn );    					
    				}
    				
    				if( includeGeneratedTypes ) {    	
    					// TODO : could(should?) generate these from ResourceDefinition
    					for( String genType : rscDefn.getGeneratedTypesList() ) {
    						ResourceDefinition genRscDefn = new ResourceDefinition( rscDefn );
    						genRscDefn.setResourceDefnName( genType );
    						resourceDefnsList.add( genRscDefn );
    					}
    				}
    				else if( !rscDefn.getRscTypeGenerator().isEmpty() ) {
    					resourceDefnsList.add( rscDefn );
    				}
    			}
//    			List<String> rscCatList = rscDefn.getRscCategories();
//    			if( rscCatList.contains( rscCat ) ) {
//    				resourceTypes.add(rscDefn.getResourceDefnName() );
//    			}
    		}
    	}
    	return resourceDefnsList;
    	//ResourceDefinition typesArray[] = resourceDefnsList.toArray( new ResourceDefinition[0] );
    			
//    	return Arrays.asList( typesArray );
    }

    // if these resource type uses attributeSetGroups then return the specified 
    // attrSetGroups, if not then look for groups which are 'sub-types' which are
    // given as 
    public String[] getResourceSubTypes( String rscType ) throws VizException {

		ResourceDefinition rscDefn = getResourceDefinition( rscType );

		if( rscDefn == null ) {
			return new String[0];
		}
       	
		// generate the subTypes 
		return rscDefn.generatedSubTypesList().toArray( new String[0] );
    }

    // look up using a resource name
    public AttrSetGroup getAttrSetGroupForResource( ResourceName rscName) {
    	String rscType = rscName.getRscType();
    	String asgName = rscName.getRscGroup();
    	
    	// for PGEN the group in the rscName is the PGEN file and we need the 
    	// attrSetGroup name which is 'PGEN'
    	if( rscName.isPgenResource() ) {
        	asgName = "PGEN";
    	}

    	return getAttrSetGroupForResource( rscType, asgName );
    }
    	
    // lookup usging the rscType and the asg name
    public AttrSetGroup getAttrSetGroupForResource( String rscType, String asgName ) {
       	ResourceDefinition rscDefn = getResourceDefinition( rscType );
       	
    	if( rscDefn == null ) {
    		return null;
    	}

    	String asgKey = rscDefn.getResourceDefnName() + File.separator + asgName;

       	// look up the attrSetGroup in the map and return it.
    	// for PGEN the key 
    	if( asgName.equals("PGEN") ) {
    		asgKey = "PGEN";
    	}
       	
       	return attrSetGroupsMap.get( asgKey );       	
    }
    
    
    // this is all of the attribute set groups available for a resource
    // (the returned list references the actual AttrSetGroup objects)
    //
    public ArrayList<AttrSetGroup> getAttrSetGroupsForResource( String rscType ) {
    	// loop thru all the entries in the attrSetGroupsMap and return those
    	// that match the rscImpl
    	ArrayList<AttrSetGroup> attrSetGroupsList = new ArrayList<AttrSetGroup>();
	
    	ResourceDefinition rscDefn = getResourceDefinition( rscType );
    	
    	if( rscDefn== null ) {
    		return attrSetGroupsList;
    	}
    	
    	for( AttrSetGroup asg : attrSetGroupsMap.values() ) {
    		if( asg.getResource().equals(  rscDefn.getResourceDefnName() ) ) {
				attrSetGroupsList.add( asg );    			
    		}    		
    	}

    	return attrSetGroupsList;
    }
    
    public List<String> getAttrSetGroupNamesForResource( String rscType ) {
       	ResourceDefinition rscDefn = getResourceDefinition( rscType );
       	List<String> asgNameList = new ArrayList<String>();
       	
       	ArrayList<AttrSetGroup> asgList = getAttrSetGroupsForResource( rscType );
       	
       	for( AttrSetGroup asg : asgList ) {
       		asgNameList.add( asg.getAttrSetGroupName()  );
       	}

       	return asgNameList;
    	//return ( rscDefn == null ? null :  rscDefn.getAttrSetGroupNames() );    	
    }    

	public AttributeSet getAttrSet( ResourceName rscName ) { //, String asName ) {
		String asName = rscName.getRscAttrSetName();
		ResourceDefinition rscDefn = getResourceDefinition( rscName );
		String asgName = rscName.getRscGroup();

		return getAttrSet( rscDefn, asgName, asName );
	}
	
	// the asgName is not required but if given a sanity check will be done
	// to ensure that the attrSet is actually in the given attrSetGroup.
	//
	public AttributeSet getAttrSet( ResourceDefinition rscDefn, String asName ) {
		return getAttrSet( rscDefn, null, asName );
	}
	
	public AttributeSet getAttrSet( ResourceDefinition rscDefn, 
									String asgName, String asName ) {

		String asMapKey = (rscDefn.applyAttrSetGroups() ? 
				rscDefn.getRscImplementation() : rscDefn.getResourceDefnName() );

		// 
		Map<String,AttributeSet> attrSetFiles = attrSetMap.get( asMapKey );
		
		if( attrSetFiles == null || !attrSetFiles.containsKey( asName ) ) {				
//			out.println("Unable to find attrSet,"+asName+", for "+rscDefn.getResourceDefnName());
			return null;
		}

		// if AttrSetGroups apply for this resource do a sanity check
		// 
		if( rscDefn.applyAttrSetGroups() && !rscDefn.isPgenResource() &&
			asgName != null && !asgName.isEmpty() ) {
			
			String asgMapKey = rscDefn.getResourceDefnName()+File.separator+asgName;
			
			if( !attrSetGroupsMap.containsKey( asgMapKey ) ) {
				out.println("Error: cant find AttrSetGroup name, "+asgName+", for "+ 
						rscDefn.getResourceDefnName() );
				return null;
			}
			
			// Should we check that the asName is actually in the asGroup?
			//
			AttrSetGroup asg = attrSetGroupsMap.get( asgMapKey );
			if( !asg.getAttrSetNames().contains( asName ) ) {
				out.println("Warning: AttrSet, "+asName+", is not in group "+asgName );
				return null;
			}
		}

		return attrSetFiles.get( asName );
	
//		try {
//			asLclFile.getFile( true ); // ?? force to retrieve file?
//			return asLclFile;			
//		}
//		catch( LocalizationException le ) {
//			out.println("AttrSet,"+asName+", for "+rscName.toString()+" doesn't exist?" );
//			out.println( le.getLocalizedMessage() +"\n"+ le.getCause() );
//			return null;			
//		}
	}
	
	
	// get a list of all the available attribute sets for this resource defn.
	//
    public ArrayList<String> getAvailAttrSets( ResourceDefinition rscDefn ) {
    	
    	String asMapKey = ( rscDefn.applyAttrSetGroups() ? 
    							rscDefn.getRscImplementation() : rscDefn.getResourceDefnName() );
    	if( !attrSetMap.containsKey( asMapKey ) ) { // ?????
    		return new ArrayList<String>();
    	}
    	
    	return new ArrayList<String>( attrSetMap.get( asMapKey ).keySet() );
    }
    
    // 
    //
    public ArrayList<String> getAvailAttrSetsForRscImpl( String rscImpl ) {
   		ArrayList<String> attrSetList = new ArrayList<String>();
   		if( attrSetMap.containsKey( rscImpl ) ) {
   			
   			attrSetList.addAll( attrSetMap.get( rscImpl ).keySet() );   			
   		}
   		else {
   			out.println("No available attribute sets for "+rscImpl );
   		}
	    return attrSetList;
    }
    
    
    public List<AttributeSet> getAttrSetsForResource( ResourceName rscName, boolean matchGroup ) {    
       	ResourceDefinition rscDefn = getResourceDefinition( rscName.getRscType() );
       	List<AttributeSet> asList = new ArrayList<AttributeSet>();
       	
    	if( rscDefn == null ) {
    		return null;
    	}
       	
    	if( rscDefn.applyAttrSetGroups( ) ) {
       		AttrSetGroup asg = getAttrSetGroupForResource( rscName );
       		if( asg != null ) {
       			for( String asName : asg.getAttrSetNames() ) {       				 
       				AttributeSet as = getAttrSet( rscDefn, asg.getAttrSetGroupName(), asName );
       				if( as != null ){
       					asList.add( as );
       				}
       			}
       		}
       	}
    	else {
    		// if there is supposed to be a generated group but there is none
    		// then 
    		if( matchGroup &&
    			rscName.getRscGroup().isEmpty() &&
    		   !rscDefn.getSubTypeGenerator().isEmpty() ) {
    		}
    		else {
    			for( String asName : getAvailAttrSets( rscDefn ) ) {       				 
       				AttributeSet as = getAttrSet( rscDefn, asName );
       				if( as != null ){
       					asList.add( as );
       				}
    		}
    	}
    }

    	return asList;
    }
    
    public List<ResourceName> getAllSelectableResourceNamesForResourcDefn(
    								ResourceDefinition rscDefn ) throws VizException {
		List<ResourceName> rscNamesList = new ArrayList<ResourceName>();
		
		// build a list of all the possible requestable resourceNames
//					
		ResourceName rscName = new ResourceName();
		String rscCat = rscDefn.getResourceCategory();
		rscName.setRscCategory( rscCat );

		// TODO : need to improve the way this works. Never liked it.
//           		
		List<String> rscTypes = new ArrayList();
		
//		if( rscDefn.getRscTypeGenerator().isEmpty() ) {
//			rscTypes.add( rscDefn.getResourceDefnName() );
//       	}
//       	else {
//			rscTypes = rscDefn.getGeneratedTypesList();			
//       	}		
		
		rscTypes.add( rscName.getRscType() );
		
		for( String rscType : rscTypes ) {
			ResourceDefinition rd  = getResourceDefinition( rscType ); //: getResourceDefnsForCategory( rscCat, "", true, false ) ) {

//			// check that this is 
//			String rscType = rd.getResourceDefnName();

			rscName.setRscType( rscType );

			List<String> asgList;

			if( rd.applyAttrSetGroups() ) {
				asgList = getAttrSetGroupNamesForResource( rscType );
			}
			else {
				asgList = rd.generatedSubTypesList();				
    }
    
			if( asgList.isEmpty() ) {
				rscName.setRscGroup( "" );

				for( AttributeSet attrSet : getAttrSetsForResource(rscName, false ) ) {
					rscName.setRscAttrSetName( attrSet.getName() );
					rscNamesList.add( new ResourceName( rscName ) );
				}					
			}
			else {
				for( String rscGroup : asgList ) {

					rscName.setRscGroup( rscGroup );

					for( AttributeSet attrSet : getAttrSetsForResource( rscName, false ) ) {
						rscName.setRscAttrSetName( attrSet.getName() );
						rscNamesList.add( new ResourceName( rscName ) );
					}
				}
			}
		}
		
		return rscNamesList;
    }
    
    public String getResourceImplementation( String rscType ) {
       	ResourceDefinition rscDefn = getResourceDefinition( rscType );
       	
       	return ( rscDefn == null ? null : rscDefn.getRscImplementation() );       	
    }
    
    // return true if the resource was replaced with another from a higher context level.
    //
    public Boolean removeResourceDefn( ResourceDefinition rscDefn ) throws VizException {
    	if( rscDefn == null ) {
    		throw new VizException("Resource Defn is null?");
    	}
    	LocalizationFile lFile = rscDefn.getLocalizationFile();
    	
    	if( lFile == null ) {
    		throw new VizException("Resource Defn File is null?");
    	}

    	// sanity check (button should be disabled if BASE/SITE context)
    	if( lFile.getContext().getLocalizationLevel() != LocalizationLevel.USER ) {
    		throw new VizException( "Can't Remove Base or Site Level Resource Types.");
    	}
    	
    	try {
        	String lFileName = lFile.getName();
//    		int rscIndx = rscDefn.getDefinitionIndex();
    		
        	lFile.delete();
//        	PathManagerFactory.getPathManager().

        	rscDefn.dispose();
        	
        	resourceDefnsMap.remove( rscDefn.getResourceDefnName() );
    		
    		// get the BASE, SITE or DESK level file to replace the deleted one.
        	lFile = NcPathManager.getInstance().getStaticLocalizationFile( lFileName );
    		
        	if( lFile != null ) {
        		// sanity check 
        		if( lFile.getContext().getLocalizationLevel() == LocalizationLevel.USER ) {
        			out.println("??? a User-level file still exists??");
        		
                	lFile = NcPathManager.getInstance().getStaticLocalizationFile( lFileName );
                	
                	Thread.sleep(1000);
					
            		// It would be nice to know why this happens sometimes but for now this seems to 
                	// fix the problem.
                	if( lFile.getContext().getLocalizationLevel() == LocalizationLevel.USER ) {
                		throw new VizException("Error Removing Localization File: getStaticLocalizationFile returned a User level File.");
        		}
        		}
        		
        		if( lFile != null ) {

        			readResourceDefn( lFile ); // can throw VizException	        			
        		}        		
        	}

    	} catch( LocalizationOpFailedException e ) {
    		throw new VizException( e );
    	}
    	catch (InterruptedException e) {
    		throw new VizException( e );
		}
		catch( VizException e ) {
			throw e;
		}     

    	
    	return false;
    }
    
    // add/replace the given attrSetGroup in the map. 
    // 
    public void saveAttrSetGroup( AttrSetGroup attrSetGroup ) throws VizException {
    	if( attrSetGroup == null ) {
    		throw new VizException("null attr set group???");
    	}
    	
    	LocalizationFile asgLclFile = attrSetGroup.getLocalizationFile();
    	LocalizationContext asgLclContext = null;
    	String lFileName = null;
    		
//    	if( asgLclFile != null ) {    	
//    		asgLclContext = asgLclFile.getContext();
//    		// if there is an existing file, use the same name. 
//    		lFileName = asgLclFile.getName();
//    	}
//    	else { // other wise create a new name 
    		ResourceDefinition rscDefn = getResourceDefinition( attrSetGroup.getResource() );
    		if( rscDefn == null ) {
    			throw new VizException("Unknown resource " + attrSetGroup.getResource() );
    		}
    		
    		lFileName = NcPathConstants.ATTR_SET_GROUPS_DIR + File.separator + 
    				rscDefn.getRscImplementation() + File.separator +
    					attrSetGroup.getResource()+"-"+
    					attrSetGroup.getAttrSetGroupName() +".xml";
//    	}
    	
    	// if this is not a USER level file, we need to create another 
    	// Localization File at the USER Level
    	// 
    	if( asgLclContext == null ||
    		asgLclContext.getLocalizationLevel() != LocalizationLevel.USER ) {
    		
    		asgLclContext = pathMngr.getContext(
    				LocalizationType.CAVE_STATIC, LocalizationLevel.USER );
		
    		asgLclFile = pathMngr.getLocalizationFile( asgLclContext, lFileName );
    	}
    	
		try {
			SerializationUtil.jaxbMarshalToXmlFile( attrSetGroup, 
					     asgLclFile.getFile().getAbsolutePath() );

			asgLclFile.save();
	    	attrSetGroup.setLocalizationFile( asgLclFile );
			
	    	attrSetGroupsMap.put( attrSetGroup.getMapKey(), attrSetGroup );
	    	
		} catch (SerializationException e) {
			throw new VizException("Error Serializing AttrSetGroup file:"+e.getMessage() );
		} catch (LocalizationOpFailedException e) {
			throw new VizException("Error Localizing file:"+e.getMessage() );
		} 
    }
    
    // remove this from the attrSetGroupsMap and remove the name from 
    // the resource definition.
    public void removeAttrSetGroup( String asgName, String rscType ) throws VizException {
    	// 
       	ResourceDefinition rscDefn = getResourceDefinition( rscType );
       	
    	if( rscDefn == null ) {
    		throw new VizException("Unable to find rscDefn "+ rscType );
    	}

    	String mapKey = rscType+File.separator+asgName;
    	
    	if( asgName.equals("PGEN") ) {
    		mapKey = "PGEN";
    	}
    	
    	AttrSetGroup asg = getAttrSetGroupForResource( rscType, asgName );
    	
    	if( asg == null ) {
    		throw new VizException("Unable to find attrSetGroup "+ rscType+"/"+asgName );
    	}
    	
    	LocalizationFile lFile = asg.getLocalizationFile();
    	
    	if( lFile == null ) {
    		throw new VizException("Error Removing AttrSetGroup: LFile is null" );
    	}
    	else if( lFile.getContext().getLocalizationLevel() != LocalizationLevel.USER ) {
    		throw new VizException( "Can't Remove Base or Site Level Attribute Set Groups.");
    	}

    	String lFileName = lFile.getName();
    	
    	try {
			lFile.delete();

	    	attrSetGroupsMap.remove( mapKey );
	    	
    		// get the BASE, SITE or DESK level file to replace the deleted one.
    		lFile = NcPathManager.getInstance().getStaticLocalizationFile( lFileName );
    		
    		// 
    		if( lFile == null ) {
        		throw new VizException("Can't find a Base or Superceding AttrSetGroup" );
//    			rscDefn.removeAttrSetGroup( asgName );
    		}

    		// sanity check (is failing)
        		if( lFile.getContext().getLocalizationLevel() == LocalizationLevel.USER ) {
    			out.println("??? a User-level ASG file still exists??");
    			
            	try { Thread.sleep(1000); } catch (InterruptedException e) { }
				
            	// TODO : Find out why this is happening and put in a better fix....
        		lFile = NcPathManager.getInstance().getStaticLocalizationFile( lFileName );
        		
        		if( lFile.getContext().getLocalizationLevel() == LocalizationLevel.USER ) {
        			throw new VizException("Error Removing Attr Set Group: Still finding a User-level file?");
        		}
        		}
        		
    			File asgFile = lFile.getFile();
    			
    			if( !asgFile.exists() ) {
    				throw new VizException( "Can't open AttrSetGroup file: "+asgFile.getAbsolutePath() );
    			}

    			Object asgObj;
    			try {
    				asgObj = SerializationUtil.jaxbUnmarshalFromXmlFile( 
    						asgFile.getAbsolutePath( ) );

    				if( !(asgObj instanceof AttrSetGroup) ) { // AttrSetGroupList
    					throw new VizException( asgFile.getAbsolutePath()+
    							" file is expected to be an attrSetGroup file." );
    				}

    			} catch (SerializationException e) {
    				throw new VizException("Error Parsing file "+asgFile.getAbsolutePath( ) +"\n"+e.getMessage());
    			} 

    			// add the ASG's in the list to the map. (PGEN is a special case since
    			// 1 'default' ASG applies to all PGEN resources.)
    			asg = (AttrSetGroup) asgObj;
    			
    			asg.setLocalizationFile( lFile );
    			
    			String rscImpl="";
    			
    			if( asg.getResource().equals("PGEN") ) {
    				attrSetGroupsMap.put( asg.getMapKey(), asg ); // rscImpl, asg );
    			}
    			else {
    				ResourceDefinition asgRscDefn = getResourceDefinition( asg.getResource() );
    				if( asgRscDefn == null ) {
    					throw new VizException("AttrSetGroup file "+
    							asgFile.getName() +" has a unknown resource:"+
    							asg.getResource() );
    				}

    				rscImpl = asgRscDefn.getRscImplementation();

    				// validate that there is a resourceImpl for this attrSetGroup
    				//
    				if( !ResourceExtPointMngr.getInstance().
    						getAvailResources().contains( rscImpl ) ) {
    					throw new VizException(( "Can't  find Resource Implementation (class) for " +
    							"attrSetGroup : " + rscImpl + "\nThe Resource implementation should be specified in " +
    					                    "a NC-Resource extention point" ) );
    					// don't fail; go ahead and put the attrset in the map 
    				}

    				attrSetGroupsMap.put( asg.getMapKey(), asg );
    			}
    	} catch (LocalizationOpFailedException e) {
			throw new VizException( e.getMessage() );
		}    			
    }

    //  
    public void saveAttrSet( ResourceDefinition rscDefn, 
    		String asName, //Map<String,String> attrs,
    		String attrsStr ) throws VizException {
    	
    	String applicableRsc = 
    		( rscDefn.applyAttrSetGroups() ? rscDefn.getRscImplementation() :
    			 						     rscDefn.getResourceDefnName() );
    			
    	AttributeSet aSet = getAttrSet( rscDefn, asName );
		LocalizationFile lFile=null;
		LocalizationContext userCntxt = 
			NcPathManager.getInstance().getContext( 
					LocalizationType.CAVE_STATIC, 
				      LocalizationLevel.USER );

    	boolean newAttrSet = (aSet == null);
    	
    	// create a new LocalizationFile. The path of the file is based on 
    	// the resouce it applies to.
    	if( newAttrSet ) {
    		String attrSetLclName;
    		
    		if( rscDefn.applyAttrSetGroups() ) {
    			attrSetLclName = NcPathConstants.ATTR_SET_GROUPS_DIR+File.separator+
    								rscDefn.getRscImplementation()+
    									File.separator+asName+".attr";
    		}
    		else {
    			attrSetLclName = rscDefn.getLocalizationFile().getName();
    			attrSetLclName = attrSetLclName.substring(0, 
    					attrSetLclName.lastIndexOf(File.separator))+
    								File.separator +asName+".attr";     					
    		}
    		
			// create the path for localization to the config dir for the resource
			lFile = NcPathManager.getInstance().getLocalizationFile( userCntxt, attrSetLclName );
			
//			aSet = AttributeSet.createAttributeSet(applicableRsc, lFile);
//			aSet.setFile( lFile );
    	}
    	else { // if the aSet exists check that the context level is USER and change if needed
    		lFile = aSet.getFile();
    		if( lFile.getContext().getLocalizationLevel() != LocalizationLevel.USER ) {
    			
    			lFile = NcPathManager.getInstance().getLocalizationFile( userCntxt, lFile.getName() );

    			aSet.setFile( lFile );
    		}
    	}
		
		try {
			File newAttrSetFile = lFile.getFile();

			FileWriter fwriter = new FileWriter( newAttrSetFile );
			fwriter.write( attrsStr );
			fwriter.close();	
			
			lFile.save();
		
		} catch (Exception e) {
			throw new VizException( e );
		}
		
		// update the attrSetMap and attrSetGroupsMap with a new/renamed attrSet.
		//
	    aSet = AttributeSet.createAttributeSet(applicableRsc, lFile);
		
		if( !attrSetMap.containsKey( applicableRsc ) ) {
			// sanity check since this shouldn't happen.
			attrSetMap.put( applicableRsc, new HashMap<String,AttributeSet>() );
		}

		Map<String,AttributeSet> asFileMap = attrSetMap.get( applicableRsc );
		
		// add or overwrite the localizationFile for this attrSet
			
		asFileMap.put( asName, aSet );				
    }
    
    // remove the attr set file and remove the attr set name from the attr set groups
    // that reference it
    //
    public boolean removeAttrSet( ResourceName rscName ) throws VizException {

    	ResourceDefinition rscDefn = getResourceDefinition( rscName );
    	if( rscDefn == null ) {
    		throw new VizException("???can't find rscDefn for "+rscName.toString() );
    	}
    	
    	String attrSetName = rscName.getRscAttrSetName();

    	// delete the file and take it out of the rsc dfn list
    	LocalizationFile asLclFile = getAttrSet( rscName ).getFile();//, rscName.getRscAttrSetName() );

    	if( asLclFile == null ) {
    		throw new VizException("Attr Set File: "+asLclFile.getName()+" not found");
    	}

    	// sanity check (button should be disabled if BASE/SITE context)
    	if( asLclFile.getContext().getLocalizationLevel() != LocalizationLevel.USER ) {
    		throw new VizException( "Can't Remove Base or Site Level Attribute Sets.");
    	}
    	
    	try {
    		String lFileName = asLclFile.getName();
    		
    		if( attrSetMap.containsKey( rscDefn.getResourceDefnName() ) ) {
        		attrSetMap.get( rscDefn.getResourceDefnName()).remove( attrSetName );    			
    		}
    		
    		// TODO : call NcPathManager method to delete and return superceding file
    		// from BASE or SITE and add this to the map.
    		asLclFile.delete();
    		
    		// get the BASE, SITE or DESK level file to replace the deleted one.
    		asLclFile = NcPathManager.getInstance().getStaticLocalizationFile( lFileName );
    		
        	if( asLclFile != null ) {
        		// sanity check 
        		if( asLclFile.getContext().getLocalizationLevel() == LocalizationLevel.USER ) {
        			out.println("??? a User-level file still exists??");

        			try { Thread.sleep(1000); } catch (InterruptedException e) { }
    				
                	// TODO : Find out why this is happening and put in a better fix....
        			asLclFile = NcPathManager.getInstance().getStaticLocalizationFile( lFileName );
            		
            		if( asLclFile.getContext().getLocalizationLevel() == LocalizationLevel.USER ) {
            			throw new VizException("Error Removing Attr Set: Still finding a User-level file?");
            		}
        		}
        		
    			String rscImpl = asLclFile.getFile().getParentFile().getName();

    			if( !attrSetMap.containsKey( rscImpl ) ) {
    				attrSetMap.put( rscImpl, new HashMap<String,AttributeSet>() );
    			}
    			try {
    				AttributeSet aSet = AttributeSet.createAttributeSet(rscImpl, asLclFile );
    				
    				attrSetMap.get( rscImpl ).put( aSet.getName(), aSet );
    			} 
    			catch ( VizException e ) {
    				out.println("Error Creating AttributeSet "+ asLclFile.getName() + 
    						": "+ e.getMessage() ); 						
    			}
        	}
        	
    	} catch ( LocalizationOpFailedException lopex ) {
    		throw new VizException( lopex.getMessage() );
    	}

    	// if removing an attr set that is part of a group then check for references
    	// to it in other groups and edit and save the groups
    	//
    	if( rscDefn.applyAttrSetGroups() ) {
    		String  rscImpl = rscDefn.getRscImplementation();
// String  attrSetGroup = rscName.getRscGroup();

    		for( AttrSetGroup asg : attrSetGroupsMap.values() ) {
    			// if this is a BASE or SITE level group then it can't reference a user-defined attrSet.
    			// 
    			if( asg.getLocalizationFile().getContext().getLocalizationLevel() == LocalizationLevel.USER &&
    				asg.getResource().equals( rscDefn.getResourceDefnName() ) ) {
    				
    				if( asg.getAttrSetNames().contains( attrSetName ) ) {
    					asg.removeAttrSet( attrSetName );
    			
    					saveAttrSetGroup( asg );
    				}
    			}    				
    		}
    		
    		// loop thru all the resources for this implementation and 
    		//   check if there is a reference to this attrSet. 
    		for( String rscType : getRscTypesForRscImplementation( rscImpl ) ) {

    			ResourceDefinition rd = getResourceDefinition( rscType );

    			if( rd == null ) { // sanity check
    				continue;
    			}
    			
    			// loop thru all of the attrSetGroups for this resource
    			//
    			for( AttrSetGroup asg : getAttrSetGroupsForResource( rscType ) ) {
    				
    				if( asg != null && 
    					asg.getAttrSetNames().contains( attrSetName ) ) {
    					
    					asg.removeAttrSet( attrSetName );
    					saveAttrSetGroup( asg );		
    				}	    				
    			}
    		}
    	}
    	
    	return true;
    }
    
    
    public ArrayList<String> getRscTypesForRscImplementation( String rscImpl ) {
    	ArrayList<String> rscTypes = new ArrayList<String>();
    	
    	for( ResourceDefinition rscDefn : resourceDefnsMap.values() ) {    		 
    		if( rscDefn.getRscImplementation().equals( rscImpl ) ) {
    			rscTypes.add( rscDefn.getResourceDefnName() );
    		}
    	}
    	return rscTypes;
    }
        
    
    // put the new/edited rscDefn in the map, write it out and initialize the inventory
    //
    public boolean saveResourceDefn( ResourceDefinition rscDefn ) throws VizException {    	
    	
    	 rscDefn.validateResourceParameters( );
    	
    	boolean createRscDefn = ( getResourceDefinition( rscDefn.getResourceDefnName() ) == null );

    	LocalizationFile lFile;// ;
    	LocalizationContext userContext = NcPathManager.getInstance().getContext(
				LocalizationType.CAVE_STATIC, LocalizationLevel.USER );
    	
    	// if this is a new rsc the LocalizationFile should not be set but the name should be.
    	if( createRscDefn ) {    		
    		lFile = NcPathManager.getInstance().getLocalizationFile( userContext,
    				   rscDefn.getLocalizationName() );
    		
    	}
    	else {
    		lFile = rscDefn.getLocalizationFile();
    		
    		if( lFile.getContext().getLocalizationLevel() != LocalizationLevel.USER ) {
        		lFile = NcPathManager.getInstance().getLocalizationFile( userContext,
        				lFile.getName() );
    		}
    	}
    	
		rscDefn.setLocalizationFile( lFile );
		
		try {
			SerializationUtil.jaxbMarshalToXmlFile( rscDefn, 
					lFile.getFile().getAbsolutePath() );

			lFile.save();


			resourceDefnsMap.put( rscDefn.getResourceDefnName(), rscDefn );

			// check to see if there is an inventory for this rscDefn or if
			// we need to create one. 
			// 
			NcInventoryDefinition invDefn = rscDefn.createNcInventoryDefinition(); 
			
			// reload just in case they have changed.
			invDefnsMap = getInventoryDefinitions( true );
			
			if( invDefnsMap.containsKey( invDefn ) ) {
				rscDefn.setInventoryAlias( invDefnsMap.get( invDefn ).getInventoryName() );
			}
			else if( rscDefn.usesInventory() ) {				
				InventoryLoaderJob invLoader = new InventoryLoaderJob( invDefn, false );
				
				invLoader.schedule();
				
				// update the progress monitor
				while( invLoader.getNumberOfInventoriesLeftToLoad() > 0 ) {
					try {
						Thread.sleep(400);
					} catch (InterruptedException e) {						
					}
				}
								
				if( invLoader.getUninitializedInventoryDefns().length == 1 ) {
					rscDefn.setInventoryAlias( null ); // Initialized( false );					
					rscDefn.disableInventoryUse();
					
					throw new VizException("There was an error Initializing an Inventory : Disabling Inventory Use" );
				}

				rscDefn.setInventoryAlias( invDefn.getInventoryName() );//Initialized( true );
			
			rscDefn.enableInventoryUse();
			}
			else {
				rscDefn.disableInventoryUse();
			}
	
			
		} catch (SerializationException e) {
			throw new VizException("Error Serializing AttrSetGroup file:"+e.getMessage() );
		} catch (LocalizationOpFailedException e) {
			throw new VizException("Error Localizing file:"+e.getMessage() );
		} 

    	return true;
    }
    
    
    public ArrayList<String> getAvailPgenTypes( ) {
    	ArrayList<String> pgenTypes = new ArrayList<String>();
    	for( ResourceDefinition rscDefn : resourceDefnsMap.values() ) {
    		if( rscDefn.isPgenResource() ) {
    			pgenTypes.add( rscDefn.getResourceDefnName() );
    		}
    	}
    	return pgenTypes;
    }
    
    public Map<String, ResourceDefinition> getPgenResourceDefinitions() {
    	Map<String, ResourceDefinition> pgenRscDefnsMap = new LinkedHashMap<String, ResourceDefinition>();

    	for( ResourceDefinition rscDefn : resourceDefnsMap.values() ) {
    		if( rscDefn.getResourceCategory() == ResourceName.PGENRscCategory ) {
    			pgenRscDefnsMap.put( rscDefn.getResourceDefnName(), rscDefn );
    		}
    	}
    	
    	return pgenRscDefnsMap;
    }
    
    public ResourceDefinition getLocatorResourceDefinition() {
    	return locatorRscDefn;
    }

    // TODO : add observer for ResourceDefn, attrSet, (attributeSetGroup?)
    // files that are updated from the Localization perspective. 
    // 
	@Override
	public void fileUpdated( FileUpdatedMessage fumsg) {
		String fName = fumsg.getFileName();
		LocalizationFile lFile;
		
		// if the file had been deleted 
		if( fumsg.getChangeType() == FileChangeType.DELETED ) {
			refdParamFilesMap.remove( fName );
			// if reverted. (ie DELETED and there is a lower level file available)
			lFile = pathMngr.getStaticLocalizationFile( fumsg.getFileName() );
		}
		else {
		// get the ADDED, UPDATED file  
			lFile = pathMngr.getLocalizationFile( 
							fumsg.getContext(), fumsg.getFileName() );
		}

		// update the map with the new file 
		if( lFile != null ) {
			refdParamFilesMap.put( fName, lFile );			
		}
	}
}
