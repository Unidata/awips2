package gov.noaa.nws.ncep.viz.resources.manager;

import gov.noaa.nws.ncep.common.dataplugin.mcidas.McidasRecord;
import gov.noaa.nws.ncep.viz.gempak.grid.inv.NcInventory;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.ncdatatree.NcDataTree;
import gov.noaa.nws.ncep.edex.plugin.mosaic.common.MosaicRecord;
import gov.noaa.nws.ncep.viz.common.SelectableFrameTimeMatcher;
import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceExtPointMngr;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.uf.viz.core.catalog.CatalogQuery;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.catalog.ScriptCreator;
import com.raytheon.uf.viz.core.comm.Connector;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.raytheon.viz.alerts.IAlertObserver;
import com.raytheon.viz.alerts.observers.ProductAlertObserver;

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
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
public class ResourceDefnsMngr {
	
	// one instance per user. (Not used)
	//
    private static Map<String, ResourceDefnsMngr> instanceMap = 
    	   new HashMap<String, ResourceDefnsMngr>();

    // TODO : fold this into NcPathMngr
    private NcPathManager pathMngr;
    
	private HashMap<String,ResourceDefinition> resourceDefnsMap = null;
		
	// a map from either the rscType or the rscImpl (depending on if AttrSetGroups apply)
	// to a list of available Attribute Set Files returned from Localization.
	private Map<String,Map<String,AttributeSet>> attrSetMap;
	
	// map from the rscType+groupName to the AttrSetGroup that 
	// holds the list of attrSets
	private Map<String,AttrSetGroup> attrSetGroupsMap;
	
	private static Map<String,LocalizationFile> colorBarMap;
	
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
	
	private static List<String> availResourceCategoriesList = Arrays.asList( AvailResourceCategories );

	private static String ATTR_SET_FILE_EXT =  ".attr";

	private static ResourceDefinition locatorRscDefn = null;
	
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

        return instance;
	}
	
	private ResourceDefnsMngr( String user ) {
		pathMngr = NcPathManager.getInstance();		        
//        searchContexts = pathMngr.getLocalSearchHierarchy( LocalizationType.CAVE_STATIC );
	}
	
	public ArrayList<String> getAvailResourceCategories() {
		return (ArrayList<String>) Arrays.asList( AvailResourceCategories );
	}
	
	//
	private void readResourceDefns() throws VizException {
		if( resourceDefnsMap != null ) {
			return;
		}

		// this was used to maintain the order in the resourceDefnsTable but now that 
		// these are separate files, I don't know that this will work. Need to 
		// find another way to get these in the right order for the GUI.
		int defnIndx = 0;

        Map<String,LocalizationFile> lFiles = pathMngr.listFiles( //searchContexts,
        	     NcPathConstants.RSC_DEFNS_DIR, new String[]{  "xml" }, true, true );

		List<String> availCategories = Arrays.asList( AvailResourceCategories );
		resourceDefnsMap = new HashMap<String,ResourceDefinition>();

		for( LocalizationFile lFile : lFiles.values() ) {
			
			if( readResourceDefn( lFile, defnIndx ) ) {
				defnIndx++;
			}
		}

		// Note: Temporary solution. Would prefer to fold this into the referencing
		// attr set file when changed to xml format.
		// 
		long t0 = System.currentTimeMillis();
	    readColorbars();
	    long t1 = System.currentTimeMillis();
	    System.out.println("Time to read colorbars: " + (t1-t0) + " ms");
	    
		// read in the attrSetGroupsMap (this needs the resourceDefnsMap
	    // to be set.)
		//
	    readAttrSets( );
	    long t2 = System.currentTimeMillis();
	    System.out.println("Time to read Attr Sets: " + (t2-t1) + " ms");
	    
	    // validate that attrSetGroups in the ResourceDefinitions are actually present
	    //
//	    for( ResourceDefinition rd : resourceDefnsMap.values() ) {
//	    	if( rd.applyAttrSetGroups() ) {
//	    		for( String asgName : new ArrayList<String>( rd.getAttrSetGroupNames() ) ) {	    			
//	    			if( !attrSetGroupsMap.containsKey( 
//	    					rd.getResourceDefnName()+File.separator+ asgName ) ) {
//	    				rd.removeAttrSetGroup( asgName );
//	    				System.out.println("attrSetGroup "+ asgName+" for resourceDefn "+
//	    					rd.getResourceDefnName()+" doesn't exist???" );
//	    			}
//	    		}
//	    	}
//	    }
	    
	    // query the database to generate dynamic resource names.
	    //
	    //generateDynamicResources( );
	}

	private Boolean readResourceDefn( LocalizationFile lFile, int defnIndx ) {
		List<String> availCategories = Arrays.asList( AvailResourceCategories );

		File rscDefnFile = lFile.getFile();
		
		Object xmlObj;
		try {
			xmlObj = SerializationUtil.jaxbUnmarshalFromXmlFile( 
					rscDefnFile.getAbsolutePath() );

			if( !(xmlObj instanceof ResourceDefinition) ) {
				System.out.println("sanity check: "+rscDefnFile.getAbsolutePath()+
						" is not a ResourceDefinition xml file");
				return false;
			}

			// copy the resourceDefn to the resourceDefnsMap and then
			//  verify the implementation, and add the resource Definitions to the map.
			//
			ResourceDefinition rscDefn = (ResourceDefinition)xmlObj;
			
			
			// TODO : Change this to set the LocalizationFile or the context
		    rscDefn.setLocalizationFile( lFile );
		    
			// TODO : If the definitions are modified and written out, this will drop any invalid resourceDefns. 
	    	// Should we save these write them out anyway? Make them disabled?

	    	// Validate the category
	    	if( !availCategories.contains( rscDefn.getResourceCategory() ) ) {
	    		System.out.println( "Error configuring resource: " +
	    				rscDefn.getResourceDefnName()+" : Invalid Resource Category." ); 
	    		return false;
	    	}
	    
	    	// Validate that the resource implementation is present.
	    	//  
	    	String rscImpl = rscDefn.getRscImplementation();
	    	
	    	if( !ResourceExtPointMngr.getInstance().getAvailResources().contains( rscImpl ) ) {
	    		System.out.println( "Error configuring resource: " +
	    				rscDefn.getResourceDefnName() ); 
	    		System.out.println( "The Resource implementation: "+ rscImpl +" is not " +
	    			"specified in a NC-Resource extention point" );
	    	}
	    	else {
	    		rscDefn.setDefinitionIndex( defnIndx++ );
	    		
    			resourceDefnsMap.put( rscDefn.getResourceDefnName(), rscDefn );
    			
    			if( rscImpl.equals( "Locator" ) ) {
    				locatorRscDefn = rscDefn;
    			}
	    	}
	    	

		} catch (SerializationException e) {
			System.out.println("Error parsing "+rscDefnFile.getAbsolutePath() +" - " + e.getMessage() );
			return false;
		} 
		
		return true;
	}
	
	private void readColorbars( ) {
		colorBarMap = new HashMap<String,LocalizationFile>();

		// This will find all .xml (must be AttrSetGroup xml files) for all
		// of the Resource implementations.
		//
        Map<String,LocalizationFile> cbarLclFiles = pathMngr.listFiles(//searchContexts,
        		NcPathConstants.COLORBARS_DIR, new String[]{ ".xml" }, 
            	          true, true );

        if( cbarLclFiles.isEmpty() ) {
        	System.out.println("Error finding ColorBars Files?");
        	return;
        }
      
		// 
		for( LocalizationFile lclFile : cbarLclFiles.values() ) {

			// get the resource implementation from the path.
			String cbarName = lclFile.getName();
			cbarName = cbarName.substring( 
					NcPathConstants.COLORBARS_DIR.length()+1, cbarName.length()-".xml".length() );

			// we chould unmarsh here to validate? 
			colorBarMap.put( cbarName, lclFile );
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
        	System.out.println("Error finding AttrSetGroup Files?");
        	return;
        }
      
		// the sub-dirs under the attrSetGroups dir must match a resourceImplClass 
		// 
        // check that the naming convention is used. If not then there can be a potential problem if the  
        // group is edited since it will be given a different localization Name. 
        
		for( LocalizationFile lclFile : attrSetGrpLclFiles.values() ) {
			
			File asgFile = lclFile.getFile();
			
			if( !asgFile.exists() ) {
				System.out.println( "Can't open AttrSetGroup file: "+asgFile.getAbsolutePath() );
				continue;
			}

			Object asgObj;
			try {
				asgObj = SerializationUtil.jaxbUnmarshalFromXmlFile( 
						asgFile.getAbsolutePath( ) );

				if( !(asgObj instanceof AttrSetGroup) ) { // AttrSetGroupList
					System.out.println( asgFile.getAbsolutePath()+
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
					System.out.println("AttrSetGroup file "+
							asgFile.getName() +" has a unknown resource:"+
							asg.getResource() );
					continue;
				}

				rscImpl = asgRscDefn.getRscImplementation();

				// validate that there is a resourceImpl for this attrSetGroup
				//
				if( !ResourceExtPointMngr.getInstance().
						getAvailResources().contains( rscImpl ) ) {
					System.out.println( "Can't  find Resource Implementation (class) for " +
							"attrSetGroup : " + rscImpl );

					System.out.println( "The Resource implementation should be specified in " +
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
					System.out.println("Warning: Localization file for AttrSetGroup, "+ lclFile.getName() +
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
			System.out.println("Error finding AttrSets (for AttrSetGroups) Files?");
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
				System.out.println("Error Creating AttributeSet "+ asLclFile.getName() + 
						": "+ e.getMessage() ); 						
			}
		}

		// Next  get the attrSets for other resources which have attribute sets of their own.
		// In this case the key is the resource type name.
		//			
		attrSetLclFiles = pathMngr.listFiles( NcPathConstants.RSC_DEFNS_DIR, 
				new String[]{ ATTR_SET_FILE_EXT }, true, true );

		if( attrSetLclFiles.isEmpty() ) {
			System.out.println("Error finding AttrSet Files?");
			return;
		}

		for( LocalizationFile asLclFile : attrSetLclFiles.values() ) {

//			if( asLclFile.getContext().getLocalizationLevel() == LocalizationLevel.USER ) {
//				System.out.println(" USER");
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
				System.out.println("Error Creating AttributeSet "+ asLclFile.getName()+ 
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
					System.out.println("attrSet "+asName+" in attrSetGroup "+
							asg.getResource()+File.separator+asg.getAttrSetGroupName()+" doesn't exist.");
				}
			}
		}
	}
	
	
	// regenerate any dynamic types or subTypes.	
	//
	public void generateDynamicResources() {
		for( ResourceDefinition rscDefn : resourceDefnsMap.values() ) {
			if( !rscDefn.getSubTypeGenerator().isEmpty() ||
				!rscDefn.getRscTypeGenerator().isEmpty() ) {
				
				generateDynamicResources( rscDefn );
			}
		}
		
		// add Product Observers for each of the plugins that generate resource types or sub-types
		//
//		ProductAlertObserver.addObserver( "mcidas", 
//				new IAlertObserver() {
//					@Override
//					public void alertArrived( Collection<AlertMessage> alertMessages) {						
//					}
//		});
		
	}

	// for resources that either have a type generator (some GRIDs and Ensembles) or a subType 
	// generator (Satellite and Radars) 
 	//
	public void generateDynamicResources( ResourceDefinition rscDefn) {
	    
		// PGEN doesn't currently query the database so treat it differently
		if( !rscDefn.isPgenResource() ) {
			// create request constraints for the query
			HashMap<String, RequestConstraint> queryTerms = new HashMap<String, RequestConstraint>();
			String genParam = ( rscDefn.getSubTypeGenerator().isEmpty() ? 
					            rscDefn.getRscTypeGenerator() : rscDefn.getSubTypeGenerator() );
			
			try { // get the pluginName from the rscTypeParameters				
				HashMap<String,String> paramsMap = rscDefn.getResourceParameters();//readParamsFile( prmFile );
	
				if( !paramsMap.containsKey("pluginName") ) {
					System.out.println("Error Requesting "+genParam+" for "+
							rscDefn.getResourceDefnName()+" - The pluginName "+
							    "must be given as a resource parameter." );
				}
				else {
					rscDefn.getSubTypesMap().clear();
					
					// for types (GRIDS/Radar) create a CatalogQuery cause its faster.
					//
					if( !rscDefn.getRscTypeGenerator().isEmpty() ) {
						
						queryTerms.put( "pluginName", new RequestConstraint( paramsMap.get("pluginName" ) ) );
	
						if( rscDefn.getResourceCategory().equals( ResourceName.GridRscCategory ) || 
							rscDefn.getResourceCategory().equals( ResourceName.EnsembleRscCategory ) ) { 

							// the parameter name is 'GDFILE' but the DB table is 'modelName'
							queryTerms.put( "modelName", new RequestConstraint( paramsMap.get("GDFILE" ) ) );
						}
						else if( rscDefn.getResourceCategory().equals( ResourceName.RadarRscCategory ) ) {
						}
						
						String dynParamValues[] = CatalogQuery.performQuery( genParam, queryTerms );

						ArrayList<String> genTypesList = new ArrayList<String>();

						// 
						for( String prmVal : dynParamValues ) {							
							if( rscDefn.getResourceCategory().equals( ResourceName.GridRscCategory ) ) {
								String eventName = prmVal;
								String genType = rscDefn.getResourceDefnName()+":"+
								                     eventName;
								if( !genTypesList.contains( genType ) ) {
									genTypesList.add( genType );
								}												
							}
							// Will the ensemble members be in the GRID or Ensemble category?
							else if( rscDefn.getResourceCategory().equals( ResourceName.EnsembleRscCategory ) ) {

								// Ensemble member dynamic types are not implemented yet.
							}
							else if( rscDefn.getResourceCategory().equals( ResourceName.RadarRscCategory ) ) {
								
								// TODO : NMAP2 gives the state that the radar is in but this data is not in
								// the DB so add code to look it up.
								String genType = rscDefn.getResourceDefnName()+":"+ 
								     prmVal.toUpperCase();
								if( !genTypesList.contains( genType ) ) {
									genTypesList.add( genType );
								}												
							}
						}
						
						rscDefn.setGeneratedTypesList( genTypesList );						
					}
					else { // for subTypes (for Satellite) we may have 2 parameters so we can't use a CatalogQuery...	
						rscDefn.getGeneratedTypesList().clear();
						
						// TODO : This works because it so happens that all the parameters for resources that 
						// generate subTypes are request parameters. (ie. not something like METAR's spiFile which
						// is not part of the query.)
						// TODO : Also the names of the parameters need to match the names of the DB tbl. 
						//    It would be better to somehow specify the query constraints.						
						for( String prm : paramsMap.keySet() ) {
							queryTerms.put( prm, new RequestConstraint( paramsMap.get(prm) ) );
						}
	
						LayerProperty prop = new LayerProperty();
						prop.setDesiredProduct(ResourceType.PLAN_VIEW);
						prop.setEntryQueryParameters(queryTerms, false);
						prop.setNumberOfImages(15000); 
						String script = null;
						script = ScriptCreator.createScript(prop);
	
						if (script == null)
							return;
	
						Object[] pdoList = Connector.getInstance().connect(script, null, 60000);
	
						HashMap<String,ArrayList<String>> subTypesMap = new HashMap<String,ArrayList<String>>();
						
						for (Object pdo : pdoList) {
							String subType=null;
							String attrSetKey=null;
	
							if( rscDefn.getResourceCategory().equals( ResourceName.SatelliteRscCategory ) ) {
								if( rscDefn.getRscImplementation().equals("GiniSatellite") ) {
									SatelliteRecord satRec = (SatelliteRecord) pdo;
									subType = satRec.getSectorID();
									attrSetKey = satRec.getPhysicalElement();
								}
								else if( rscDefn.getRscImplementation().equals("McidasSatellite") ) {
									McidasRecord satRec = (McidasRecord) pdo;
									subType = satRec.getAreaName() + "_" + 
									satRec.getResolution().toString() + "km";
									attrSetKey = satRec.getImageType();
								}
							}
							else if( rscDefn.getResourceCategory().equals( ResourceName.RadarRscCategory ) ) {
								// if Mosaic there is no subType so just use 'mosaic'
								MosaicRecord rdrRec = (MosaicRecord) pdo;
								subType = "mosaic";
								attrSetKey = rdrRec.getProdName();
								// if Radar then use the name of the radar as the subType
							}
							
							if( subType != null ) {
								if( !subTypesMap.containsKey( subType ) ) {
							
									subTypesMap.put( subType, new ArrayList<String>() );
								}
							
								subTypesMap.get( subType ).add( attrSetKey );
							}
						}
	
						rscDefn.setSubTypesMap( subTypesMap );
					}
				}				
			} catch (VizException e1) {
				System.out.println("Error Generating Dynamic Resource "+ 
						rscDefn.getResourceDefnName() + ": "+e1.getMessage() );
			}		
		}
		// TODO : We need to decide what to do with PGEN Resources....
		else { // PGEN			
			rscDefn.getGeneratedTypesList().clear();
			HashMap<String,ArrayList<String>> subTypesMap = new HashMap<String,ArrayList<String>>();

	   		// PGEN generates the SubTypes from the .xml files in the products directory.
			if( !rscDefn.getResourceParameters().containsKey( "pgenDirectory" ) ) {
				System.out.println("Error generating PGEN products: "+
						"the pgenDirectory parameter is not specified.");
				return;
			}
			
			String prodDirName = rscDefn.getResourceParameters().get( "pgenDirectory" );
			
			if( !prodDirName.trim().startsWith( File.separator ) ) {
				// TODO: should call PgenUtil.getWorkingDirectory() if not 
				// for cyclical dependency. This still needs work so leaving 
				// this as current working directory for now. If user were to change the
				// pgen working directory, this would break.
				prodDirName = 
		    			NmapCommon.getPgenWorkingDirectory() + File.separator +
		    				prodDirName + File.separator;
			}
				
			File productsDir = new File( prodDirName );
	
			if( productsDir == null || !productsDir.exists() ||
										!productsDir.isDirectory() ) {
				System.out.println("Error generating PGEN products: the pgenDirectory for, "+
						rscDefn.getResourceParameters().get("pgenDirectory") + ", doesn't exist" );
				return;
			}
			
			String[] prmFileNames = productsDir.list( new FilenameFilter() {
				@Override
				public boolean accept(File dir, String name) {
					return name.endsWith( ".xml" );
				}       			
			});

			for( String prmFileName : prmFileNames ) {
				String productName = prmFileName.substring(0,prmFileName.length()-4);
					// for PGEN there aren't any 'attrSetKeys' (these are used by Satellite and Radar
					// to determine which attrSets
				if( !subTypesMap.containsKey( productName ) ) {	
					subTypesMap.put( productName, new ArrayList<String>() );
					subTypesMap.get( productName ).add( "show all attribute sets" );
				}
			}			

			rscDefn.setSubTypesMap( subTypesMap );
		}
	}
	
	//Dynamic update Data Resources
	public void dynamicUpdateDataResource ( String pluginName, String dataURI ) {
//		System.out.println ("--updateDataResource: pluginName:" + pluginName + " dataURI:" + dataURI);
		for( ResourceDefinition rscDefn : resourceDefnsMap.values() ) {
			if( (!rscDefn.getSubTypeGenerator().isEmpty() ||
				!rscDefn.getRscTypeGenerator().isEmpty()) && 
				(!rscDefn.isPgenResource()) ) {
				
				dynamicUpdateDataResource ( rscDefn, pluginName, dataURI );
			}
		}
		//update Ncgrib inventory DB
		if (pluginName.equals("ncgrib")) {
			dynamicUpdateNcGribInventoryDB ( dataURI );
		}
	}
	
	//dynamic update resources that either have a type generator (some GRIDs and Ensembles) or a subType 
	// generator (Satellite and Radars)
	private void dynamicUpdateDataResource ( ResourceDefinition rscDefn, String pluginName, String dataURI ) {
		if ( pluginName.equals("mcidas") || pluginName.equals("satellite")) {
			dynamicUpdateMcidasDataResource (rscDefn, dataURI);
		}
		else if (pluginName.equals("ncgrib")) {
			dynamicUpdateGrid2DataResource (rscDefn, dataURI);
		}
		else if (pluginName.equals("radar")) {
			dynamicUpdateRadarDataResource (rscDefn, dataURI);
		} 
		else if (pluginName.equals("mosaic")) {
			dynamicUpdateMosaicDataResource (rscDefn, dataURI );
		}
	}
	
	//dynamic update mcidas/satellite data resources
	private void dynamicUpdateMcidasDataResource (ResourceDefinition rscDefn, String dataURI) {
		String subType = null;
		String attrSetKey = null;
		if ( !rscDefn.getSubTypeGenerator().isEmpty() &&
				rscDefn.getResourceCategory().equals( ResourceName.SatelliteRscCategory ) ) {

			String [] attribs = dataURI.split("/", 8);
			HashMap<String,String> rscPrms = rscDefn.getResourceParameters();
			
			// 
			if ( rscDefn.getRscImplementation().equals("McidasSatellite") ) {
				if( rscPrms.containsKey("satelliteName") &&
					 rscPrms.get("satelliteName").equals( attribs[3] ) ) {
					
					subType=attribs[4] + "_" + attribs[5] + "km";
					attrSetKey=attribs[6];
				}
			}
			else if ( rscDefn.getRscImplementation().equals("GiniSatellite") ) {
				if ( rscPrms.containsKey("creatingEntity") &&
				     rscPrms.get("creatingEntity").equals( attribs[4] ) ) {
					subType = attribs[5];
					attrSetKey = attribs[6];
				}
			}
			if ( subType != null) {
				HashMap<String,ArrayList<String>> subTypesMap = rscDefn.getSubTypesMap();
				if ( !subTypesMap.containsKey(subType)) {
					subTypesMap.put( subType, new ArrayList<String>());
					subTypesMap.get( subType ).add( attrSetKey );
				}
				else {
					if ( ! subTypesMap.get( subType ).contains(attrSetKey) ) {
						subTypesMap.get( subType ).add( attrSetKey );
					}
				}
				rscDefn.setSubTypesMap( subTypesMap );
			}
		}
	}
	
	//dynamic update GRID2 data resources
	private void dynamicUpdateGrid2DataResource (ResourceDefinition rscDefn, String dataURI) {
		String genType = null;

		if ( !rscDefn.getRscTypeGenerator().isEmpty() &&
				rscDefn.getResourceCategory().equals( ResourceName.GridRscCategory ) ) {

			String [] attribs = dataURI.split("/", 15);
						
			HashMap<String,String> rscPrms = rscDefn.getResourceParameters();

			if ( rscPrms.containsKey("GDFILE") &&
				 rscPrms.get("GDFILE").equals( attribs[3] ) ) {
				
				String [] fileNameAttrs = attribs[12].split("\\.");
				genType = rscDefn.getResourceDefnName()+":"+ fileNameAttrs[0];
			}

			
			if ( genType != null) {
				ArrayList<String> genTypesList = rscDefn.getGeneratedTypesList();

				if( !genTypesList.contains( genType ) ) {
					genTypesList.add( genType );
					rscDefn.setGeneratedTypesList( genTypesList );
				}
			}
		}
	}
	
	//dynamic update NcGrib inventory Database
	private void dynamicUpdateNcGribInventoryDB ( String dataURI ) {
		NcInventory ncgribInventory = NcInventory.getInstance();
		if (ncgribInventory != null) {
			String [] attribs = dataURI.split("/", 16);
			String []  fileNameAttrs = attribs[12].split("\\.");
			String model = attribs[3];
			String event = fileNameAttrs[0];
			String parm = attribs[13];
			String vcord = attribs[14];
			String level1 = Integer.toString((int)Double.parseDouble(attribs[6]));
			String level2 = Integer.toString((int)Double.parseDouble(attribs[7]));
			Map<String, RequestConstraint> rcMap = new HashMap<String, RequestConstraint>();
            rcMap.put("pluginName", new RequestConstraint("ncgrib"));
            rcMap.put("modelName", new RequestConstraint(model));
            rcMap.put("eventName",new RequestConstraint(event));
            rcMap.put("parm", new RequestConstraint(parm));
            rcMap.put("vcord", new RequestConstraint(vcord));
            rcMap.put("glevel1", new RequestConstraint(level1));
            rcMap.put("glevel2", new RequestConstraint(level2));
            NcDataTree ncgridTree = ncgribInventory.getNcDataTree();
            ncgridTree.addBranch(model, event, parm, vcord, level1, level2, rcMap);
		}
	}
	//dynamic update RADAR data resources
	private void dynamicUpdateRadarDataResource (ResourceDefinition rscDefn, String dataURI) {
		String genType = null;

		if ( !rscDefn.getRscTypeGenerator().isEmpty() && 
				rscDefn.getResourceCategory().equals( ResourceName.RadarRscCategory ) ) {

			String [] attribs = dataURI.split("/", 5);
			HashMap<String,String> rscPrms = rscDefn.getResourceParameters();
			
			if ( rscPrms.containsKey("pluginName") &&
				 rscPrms.get("pluginName").equals( attribs[1] ) ) {
				genType = rscDefn.getResourceDefnName()+":"+ attribs[3].toUpperCase();
			}

			
			if ( genType != null) {
				ArrayList<String> genTypesList = rscDefn.getGeneratedTypesList();

				if( !genTypesList.contains( genType ) ) {
					genTypesList.add( genType );
					rscDefn.setGeneratedTypesList( genTypesList );
				}
			}
		}
	}
	
	//dynamic update MOSAIC data resources
	private void dynamicUpdateMosaicDataResource (ResourceDefinition rscDefn, String dataURI) {
		String subType = null;
		String attrSetKey=null;

		if ( !rscDefn.getSubTypeGenerator().isEmpty() && 
				rscDefn.getResourceCategory().equals( ResourceName.RadarRscCategory ) ) {

			String [] attribs = dataURI.split("/", 5);
			HashMap<String,String> rscPrms = rscDefn.getResourceParameters();
			
			if ( rscPrms.containsKey( "pluginName") &&
				 rscPrms.get("pluginName").equals( attribs[1] ) ) {
				subType = "mosaic";
				attrSetKey = attribs[4];
			}

			
			if ( subType != null) {
				HashMap<String,ArrayList<String>> subTypesMap = rscDefn.getSubTypesMap();
				if ( !subTypesMap.containsKey(subType)) {
					subTypesMap.put( subType, new ArrayList<String>());
					subTypesMap.get( subType ).add( attrSetKey );
				}
				else {
					if ( ! subTypesMap.get( subType ).contains(attrSetKey) ) {
						subTypesMap.get( subType ).add( attrSetKey );
					}
				}
				rscDefn.setSubTypesMap( subTypesMap );
			}
		}
	}
		
	public ResourceDefinition getResourceDefnByIndx( int defnIndx ) {
		for( ResourceDefinition rscDefn : resourceDefnsMap.values() ) {
			if( rscDefn.getDefinitionIndex() == defnIndx ) {
				return rscDefn;
			}
		}
		
		return null;
	}
				    			
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

//		System.out.println("sanity check: can't find ResourceDefinition for: "+rscType );
		return null;
	}

	public String[] getResourceCategories( ) {
		ArrayList<String> catsList = new ArrayList<String>();

    	for( ResourceDefinition rscDefn : resourceDefnsMap.values() ) {
    		if( rscDefn.getIsEnabled() ) {
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

		HashMap<String,String> paramsMap = new HashMap<String,String>( rscDefn.getResourceParameters() );

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
		
		// first get the parameters from the .prm file for this type.
		HashMap<String,String> paramsMap = new HashMap<String,String>( rscDefn.getResourceParameters() );

		// next get the attributes
		AttributeSet attrSet = getAttrSet( rscName );

		if( attrSet != null ) {
			paramsMap.putAll( attrSet.getAttributes() ); 
					//readAttrSetFile( attrSetFile.getFile() ) );
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

		// finally add 'dynamic' parameters that were queried from the database
		// 
		
		// if this is a generated type 
		if( !rscDefn.getRscTypeGenerator().isEmpty() ) {
			if( rscName.getRscCategory().equals( ResourceName.GridRscCategory ) ){
				
				// Note this will replace the 'default' eventName parameter ('%') which
				// is used for non-event GRIDS
				if( rscDefn.getRscTypeGenerator().equals( "eventName" ) ) {
					String rscType = rscName.getRscType();
					int indx = rscType.indexOf(":");
					if( indx == -1 ) {
						throw new VizException("sanity check: Can't parse eventName from Grid "+
								"Resource name :"+rscName.toString() );
					}
					
					String eventName = rscType.substring( indx+1 );  

					paramsMap.put("eventName", eventName );
				}
			}
			else if( rscName.getRscCategory().equals( ResourceName.RadarRscCategory ) ){
				if( rscDefn.getRscTypeGenerator().equals( "icao" ) ) {
					String rscType = rscName.getRscType();
					int indx = rscType.indexOf(":");
					if( indx == -1 ) {
						throw new VizException("sanity check: Can't parse icao from "+"" +
								"LocalRadar Resource name :"+rscName.toString() );
					}

					String icaoStr = rscType.substring( indx+1 ).toLowerCase();  

					paramsMap.put("icao", icaoStr );
				}
			}
		}
		
		// If there is a generated sub-type then we will need to set a parameter for this 
		// (In this case the name of the parameter in the paramsMap must be the same as the 
		// name of the variable in the BundleTemplate.) 
		// 
		if( !rscDefn.getSubTypeGenerator().isEmpty() ) {
			if( rscName.getRscGroup().isEmpty() ) {
				System.out.println("getAllResourceParameters: Sanity Check: the generated sub-type "+
						"is null for : "+ rscName.toString() );
			}
			else if( rscName.getRscCategory().equals( ResourceName.SatelliteRscCategory ) ){
				
				// TODO: This whole thing needs to be reworked. We shouldn't have to check 
				// for the implementation here.
				// GINI creates subTypes frome the 'sector' and 
				// McIdas uses a combination of the areaName and the resolution.
				if( rscDefn.getRscImplementation().equals( "McidasSatellite" ) ) {
					// 
					int indx = rscName.getRscGroup().lastIndexOf( '_' );
					String areaName = rscName.getRscGroup().substring(0, indx);
					String resStr   = rscName.getRscGroup().substring(indx+1, 
							rscName.getRscGroup().length()-2); // "km"
					paramsMap.put( "areaName",  areaName );
					paramsMap.put( "resolution", resStr );
				}
				else if( rscDefn.getRscImplementation().equals( "GiniSatellite" ) ) {
					paramsMap.put( "sectorID",  rscName.getRscGroup() );					
				}
			}
		}
		
		
		// just one more hack we can't have PGEN save a .prm file for some reason.
		if( rscDefn.isPgenResource() ) {
			if( !rscName.getRscGroup().isEmpty() ) {
				paramsMap.put("productName", rscName.getRscGroup() );
				paramsMap.put("legendString", "PGEN : "+ rscName.getRscGroup() );
			}
		}
				
		return paramsMap;		
	}
					
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

					// '@' used to be a reference to a file in the same directory but with 
					// the localization, and since this is only used for colorbars, 
					// 					
					if( !prmVal.isEmpty() && prmVal.charAt(0) == '@' ) {
						try {
							String cbarName = prmVal.substring(1, prmVal.length()-".xml".length());

							if( !colorBarMap.containsKey( cbarName ) ) {
								throw new VizException("Error reading file: "+
										asFile.getAbsolutePath() + " : Unable to find referenced "+
										" colorBar file '"+prmVal+"'." );
							}
							
							File cbarFile = colorBarMap.get( cbarName ).getFile( true );

							if( !cbarFile.exists() ) {
								throw new VizException("Error reading file: "+
									asFile.getAbsolutePath() + " colorBar file '"+prmVal+"'doesn't exist." );
							}
							
							FileReader fr = new FileReader(cbarFile);
							char[] b = new char[(int) cbarFile.length()];
							fr.read(b);
							fr.close();

							prmVal = new String(b);

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
	
    public  List<String> getResourceTypesForCategory( String rscCat, String filterStr, Boolean includeGeneratedTypes ) {
    	ArrayList<String> resourceTypes = new ArrayList<String>();
       	
       	// for other resources, get all of the resources in the given category.
       	//
    	for( ResourceDefinition rscDefn : resourceDefnsMap.values() ) {
//    		if( !rscDefn.getIsEnabled() ) {
//    			continue;
//    		}
    		
    		if( filterStr == null || filterStr.isEmpty() ||
    			rscDefn.getFilterLabels().contains( filterStr ) ) {
    			
    			if( rscDefn.getResourceCategory().equals( rscCat ) ) {
    				if( rscDefn.getRscTypeGenerator().isEmpty() ) {
        				resourceTypes.add( rscDefn.getResourceDefnName() );    					
    				}
    				
    				if( includeGeneratedTypes ) {    					
    					resourceTypes.addAll( rscDefn.getGeneratedTypesList() );
    				}
    				else if( !rscDefn.getRscTypeGenerator().isEmpty() ) {
    					resourceTypes.add( rscDefn.getResourceDefnName() );
    				}
    			}
//    			List<String> rscCatList = rscDefn.getRscCategories();
//    			if( rscCatList.contains( rscCat ) ) {
//    				resourceTypes.add(rscDefn.getResourceDefnName() );
//    			}
    		}
    	}
    	String typesArray[] = resourceTypes.toArray( new String[0] );
    	
    	// sort with the Obs types first and then the Fcst, and then alphabetically 
    	Arrays.sort( typesArray, new Comparator<String>() {
    		public int compare(String o1, String o2) {
    			ResourceDefinition rscDefn1 = getResourceDefinition( (String)o1 );
    			ResourceDefinition rscDefn2 = getResourceDefinition( (String)o2 );
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

    			
    			// otherwise return the order based on the index in the table 
				if( rscDefn1.getDefinitionIndex() == 
					rscDefn2.getDefinitionIndex() ) {
					return ((String)o1).compareToIgnoreCase( (String)o2 );    						
				}
				else {
					return ( rscDefn1.getDefinitionIndex() < 
							 rscDefn2.getDefinitionIndex() ? -1 : 1 );
				}
    		}
		}); 
		
    	return Arrays.asList( typesArray );
    }

    // if these resource type uses attributeSetGroups then return the specified 
    // attrSetGroups, if not then look for groups which are 'sub-types' which are
    // given as 
    public String[] getResourceSubTypes( String rscType ) {

		ResourceDefinition rscDefn = getResourceDefinition( rscType );

		if( rscDefn == null ) {
			return new String[0];
		}
       	
		// Grids, Satellite, Radar, etc all are dynamically updated when new alert updates 
		// are recieved (when new data is ingested). PGEN doesn't have alert updates and so
		// we need to always check for new products. 
		if( rscDefn.isPgenResource() ) {
			generateDynamicResources( rscDefn );
		}
       	// the sub-types are stored in the resourceDefnTable but they still
       	// have to refer to a sub-directory in the rscConfig Dir.
       	return rscDefn.getRscSubTypes();       	
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
//			System.out.println("Unable to find attrSet,"+asName+", for "+rscDefn.getResourceDefnName());
			return null;
		}

		// if AttrSetGroups apply for this resource do a sanity check
		// 
		if( rscDefn.applyAttrSetGroups() && !rscDefn.isPgenResource() &&
			asgName != null && !asgName.isEmpty() ) {
			
			String asgMapKey = rscDefn.getResourceDefnName()+File.separator+asgName;
			
			if( !attrSetGroupsMap.containsKey( asgMapKey ) ) {
				System.out.println("Error: cant find AttrSetGroup name, "+asgName+", for "+ 
						rscDefn.getResourceDefnName() );
				return null;
			}
			
			// Should we check that the asName is actually in the asGroup?
			//
			AttrSetGroup asg = attrSetGroupsMap.get( asgMapKey );
			if( !asg.getAttrSetNames().contains( asName ) ) {
				System.out.println("Warning: AttrSet, "+asName+", is not in group "+asgName );
				return null;
			}
		}

		return attrSetFiles.get( asName );
	
//		try {
//			asLclFile.getFile( true ); // ?? force to retrieve file?
//			return asLclFile;			
//		}
//		catch( LocalizationException le ) {
//			System.out.println("AttrSet,"+asName+", for "+rscName.toString()+" doesn't exist?" );
//			System.out.println( le.getLocalizedMessage() +"\n"+ le.getCause() );
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
   			System.out.println("No available attribute sets for "+rscImpl );
   		}
	    return attrSetList;
    }
    
    
    public String[] getAttrSetsForResource( ResourceName rscName, boolean matchGroup ) {    
       	ResourceDefinition rscDefn = getResourceDefinition( rscName.getRscType() );
       	
    	if( rscDefn == null ) {
    		return null;
    	}
       	
    	
    	if( rscDefn.applyAttrSetGroups() ) {
       		AttrSetGroup asg = getAttrSetGroupForResource( rscName );
       		if( asg != null ) {
       			return asg.getAttrSetNames().toArray( new String[0] );
       		}
       		return new String[0];
       	}

       	if( matchGroup &&
       		( rscName.getRscCategory().equals( ResourceName.SatelliteRscCategory ) ) ) { //|| 
//       		  rscName.getRscCategory().equals( ResourceName.RadarRscCategory ) ) ) {
       			
       		String rscGrp = rscName.getRscGroup();
       		// map from the subType to a list of 'keys' for available attributes.  
       		HashMap<String, ArrayList<String>> subTypesMap = rscDefn.getSubTypesMap();
       		
       		if( rscGrp == null || rscGrp.isEmpty() ) {
       			if( rscName.getRscCategory().equals( ResourceName.SatelliteRscCategory ) ) {
//       				System.out.println("Sanity Check: Satellite resource should have a group. " );
       				return new String[0]; //       				
       			}
       			else { // Radar Mosaic
       				return getAvailAttrSets( rscDefn ).toArray( new String[0] ); 
       			}
       		}
       		
       		ArrayList<String> attrSets = new ArrayList<String>();
       		
       		ArrayList<String> availAttrSetKeys = subTypesMap.get( rscGrp );
       		if( availAttrSetKeys == null ) {
   				System.out.println("Sanity Check: Satellite subType is not in the subTypeMap?? " );
   				return new String[0]; //       				
       		}
       		
       		for( String asName : getAvailAttrSets( rscDefn ) ) {
//				try {
					ResourceName fullRscName = new ResourceName( rscName );
					fullRscName.setRscAttrSetName(asName);
					
					AttributeSet aset = getAttrSet( fullRscName );
					HashMap<String, String> attrsMap = aset.getAttributes(); 
						//readAttrSetFile( getAttrSet( fullRscName ).getFile() );
           		
					if( rscName.getRscCategory().equals( ResourceName.SatelliteRscCategory ) ) {
						if( !attrsMap.containsKey("imageType") ) {
							System.out.println("Sanity Check: Satellite attrSets must have an "+
							" 'imageType' parameter." );
						}
						else {
							if( availAttrSetKeys.contains( attrsMap.get( "imageType" ) ) ) {
								attrSets.add( asName );
							}
						}
					}
					else if( rscName.getRscCategory().equals( ResourceName.RadarRscCategory ) ) {
						if( !attrsMap.containsKey("productName") ) {
							System.out.println("Sanity Check: Radar attrSets must have an "+
							" 'productName' parameter." );
						}
						else {
							if( availAttrSetKeys.contains( attrsMap.get( "productName" ) ) ) {
								attrSets.add( asName );
							}
						}
					} 
//				} catch (VizException e) {
//					System.out.println("Error parsing AttrSet "+asName+" : "+e.getMessage() );
//				}
       		}
       		return attrSets.toArray( new String[0] );
       	}
       	else {
       		return getAvailAttrSets( rscDefn ).toArray( new String[0] );
       	}		
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
    		int rscIndx = rscDefn.getDefinitionIndex();
    		
        	lFile.delete();

        	resourceDefnsMap.remove( rscDefn.getResourceDefnName() );
    		
    		// get the BASE, SITE or DESK level file to replace the deleted one.
        	lFile = NcPathManager.getInstance().getStaticLocalizationFile( lFileName );
    		
        	if( lFile != null ) {
        		// sanity check 
        		if( lFile.getContext().getLocalizationLevel() == LocalizationLevel.USER ) {
        			System.out.println("??? a User-level file still exists??");
        		}
        		
        		if( readResourceDefn( lFile, rscIndx ) ) {
        			return true;
        		}
        	}

    	} catch( LocalizationOpFailedException e ) {
    		throw new VizException( e );
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
    		throw new VizException("Can't find LocalizationFile for AttrSetGroup" );
    	}
    	
    	if( lFile.getContext().getLocalizationLevel() != LocalizationLevel.USER ) {
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
//    			rscDefn.removeAttrSetGroup( asgName );
    		}
    		else {
        		// sanity check 
        		if( lFile.getContext().getLocalizationLevel() == LocalizationLevel.USER ) {
        			System.out.println("??? a User-level file still exists??");
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
        			System.out.println("??? a User-level file still exists??");
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
    				System.out.println("Error Creating AttributeSet "+ asLclFile.getName() + 
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
//        	String  attrSetGroup = rscName.getRscGroup();

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
        
    
    // put the new/edited rscDefn in the map and write it out.
    //
    public boolean saveResourceDefn( ResourceDefinition rscDefn ) throws VizException {    	
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
		
       	//       	
		try {
			SerializationUtil.jaxbMarshalToXmlFile( rscDefn, 
					lFile.getFile().getAbsolutePath() );

			lFile.save();

			resourceDefnsMap.put( rscDefn.getResourceDefnName(), rscDefn );
			
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
    
    // method to update the pgen products
//    generateDynamicResources( pgenRscDefn );
//    
//    }
}
