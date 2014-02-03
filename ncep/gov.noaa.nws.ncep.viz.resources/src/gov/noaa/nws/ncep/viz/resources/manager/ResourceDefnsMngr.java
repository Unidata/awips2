package gov.noaa.nws.ncep.viz.resources.manager;

import static java.lang.System.out;
import gov.noaa.nws.ncep.edex.common.ncinventory.NcInventoryDefinition;
import gov.noaa.nws.ncep.edex.common.ncinventory.NcInventoryRequestMsg;
import gov.noaa.nws.ncep.viz.common.SelectableFrameTimeMatcher;
import gov.noaa.nws.ncep.viz.common.dbQuery.NcConnector;
import gov.noaa.nws.ncep.viz.common.display.NcDisplayType;
import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceExtPointMngr;
import gov.noaa.nws.ncep.viz.resources.manager.AttrSetGroup.RscAndGroupName;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceDefinitionFilters.ResourceDefinitionFilter;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import javax.xml.bind.JAXBException;

import org.eclipse.jface.dialogs.MessageDialog;
import org.geotools.referencing.operation.transform.WarpTransform2D;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.FileUpdatedMessage.FileChangeType;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.RecordFactory;
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
 * 12/18/12      #957       Greg Hull    patch the bug when deleting a localization file
 * 02/13/13      #972       Greg Hull    ResourceCategory class and NcDisplayType
 * 04/10/13      #864       Greg Hull    read/save new ResourceFilters file
 * 04/24/13      #838       B. Hebbard   Allow getAllResourceParameters to handle NTRANS (paramVal2 no longer assumed numeric/km)
 * 06/05/13      #998       Greg Hull    init subTypesList when creating new RD.
 * 08/2013       #1031      Greg Hull    retry on inventory directory request
 * 12/4/13       #1074      Greg Hull    another hack for the rscName<->parameterValues mapping; check for
 *                                       'native' in satellite subType and set to 0 resolution.    
 *
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
public class ResourceDefnsMngr {
	
	// one instance per user. (Currently only the 'base' used)
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
	
	public static final HashMap<String,String> paramInfoForRscDefnParamsMap = new HashMap<String,String>(); 
	{
		paramInfoForRscDefnParamsMap.put("frameSpan", "Integer" );
		paramInfoForRscDefnParamsMap.put("timeMatchMethod", "TimeMatchMethod" );
		paramInfoForRscDefnParamsMap.put("dfltNumFrames", "Integer");
		paramInfoForRscDefnParamsMap.put("dfltTimeRange", "Integer" );
		paramInfoForRscDefnParamsMap.put("timelineGenMethod",  "TimelineGenMethod" );
		paramInfoForRscDefnParamsMap.put("isForecast", "Boolean" );
	}

	private static List<ResourceCategory> availResourceCategoriesList = 
								Arrays.asList( ResourceCategory.values() ); 
		//Arrays.asList( AvailResourceCategories );

	private static String ATTR_SET_FILE_EXT =  ".attr";

	private static ResourceDefinition locatorRscDefn = null;
	
	private static List<VizException> badRscDefnsList = new ArrayList<VizException>();
	
	private static List<VizException> rscDefnsWarningsList = new ArrayList<VizException>();
	
	private static Map<String,TreeMap<LocalizationLevel,ResourceDefinitionFilter>> rscFiltersMap = null;
	
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

	public List<VizException> getResourceDefnWarnings() {
		return rscDefnsWarningsList;
	}
		
	public List<ResourceDefinition> getAllResourceDefinitions() {
		return new ArrayList<ResourceDefinition>( resourceDefnsMap.values() );
	}
	
	// The RDs should already have been read in.
	// fill in the rscFiltersmap from all localization files (all levels that is.)
	// if no file is present, enable everything.
	//
	private void readResourceFilters() throws VizException {

		if( rscFiltersMap == null ) {
			rscFiltersMap = new HashMap<String,TreeMap<LocalizationLevel,ResourceDefinitionFilter>>();
			
			// If a RD is not found in a Filters file then it will be enabled by default.
			// 
			Boolean dfltEnableState = true;

			// get all versions of the file and parse each one.
			Map<LocalizationLevel,LocalizationFile> filtFiles = pathMngr.getTieredLocalizationFile( NcPathConstants.RESOURCE_FILTERS ); 

			// if there is no filter files then display a warning and continue
			if( filtFiles == null ) {		
				dfltEnableState = true;
				rscDefnsWarningsList.add( 
						new VizException( "Could not find any "+
								NcPathConstants.RESOURCE_FILTERS + " files. \n"+
								"All Rsc Defns will be enabled w/o any filters" ) );
			}

			for( LocalizationLevel locLvl : filtFiles.keySet() ) {
				
				LocalizationFile rscFiltersLFile = filtFiles.get( locLvl );
				
				rscFiltersLFile.addFileUpdatedObserver( new ILocalizationFileObserver() {					
					@Override
					public void fileUpdated(FileUpdatedMessage msg) {
//						System.out.println("Localization File updated: " +
//								msg.getFileName()+" "+msg.getChangeType().toString() );

						// if deleting or updating then
						// remove all of the entries in the map at this localization lvl
						//
						if( msg.getChangeType() == FileChangeType.DELETED ||
							msg.getChangeType() == FileChangeType.UPDATED ) {
							
							for( String rdName : rscFiltersMap.keySet() ) {
								TreeMap<LocalizationLevel, ResourceDefinitionFilter> filtMap = 
									rscFiltersMap.get( rdName );
								filtMap.remove( msg.getContext().getLocalizationLevel() );						    		
							}
						}

						// if adding or updating, reread the file.
						if( msg.getChangeType() == FileChangeType.ADDED ||
							msg.getChangeType() == FileChangeType.UPDATED ) {
							
							try {
								LocalizationFile locFile = 
									NcPathManager.getInstance().getLocalizationFile( 
					    				msg.getContext(), msg.getFileName() );

								readRscFilter( locFile );
								
							} catch (SerializationException e) {
							}
						}
					}
				});
				
				
				try {
					readRscFilter( rscFiltersLFile ); 
				}
				catch (SerializationException e) {
					rscDefnsWarningsList.add( new VizException(e) );
					System.out.println("error serializing"+rscFiltersLFile.getFile().getAbsolutePath()+
							" : "+e.getMessage() );			
				}
			}

			// loop thru all the RDs and if there is no entry in the filters map, add one 
			// at the same level as the RD.
			// 
			for( ResourceDefinition rd : resourceDefnsMap.values() ) {
				String rdName = rd.getResourceDefnName();
				
				TreeMap<LocalizationLevel,ResourceDefinitionFilter> filterTreeMap =
					rscFiltersMap.get( rdName );

				if( filterTreeMap == null ) {
					filterTreeMap = new TreeMap<LocalizationLevel,ResourceDefinitionFilter>(
							LocalizationLevel.REVERSE_COMPARATOR );					
					filterTreeMap.put( rd.getLocalizationFile().getContext().getLocalizationLevel(), 
							new ResourceDefinitionFilter( rdName, dfltEnableState, null, 
									rd.getLocalizationFile().getContext().getLocalizationLevel() ) );
					rscFiltersMap.put( rdName, filterTreeMap );

					// if there a filters file then this one is just missing for some reason.
					if( filtFiles != null ) {
						rscDefnsWarningsList.add( 
							new VizException( "Could not find the Rsc Defn,"+rdName+", in any ResourceDefintionFilters files."+
									" it will be Enabled by default.") );
					}
				}
			}
		}

		return;
	}
	
	private void readRscFilter( LocalizationFile locFile ) throws SerializationException {
		String fileName = locFile.getFile().getAbsolutePath();
		LocalizationLevel lLvl = locFile.getContext().getLocalizationLevel();
		
		synchronized ( rscFiltersMap ) {
			ResourceDefinitionFilters rscDfnFilters = SerializationUtil.jaxbUnmarshalFromXmlFile(
					ResourceDefinitionFilters.class, fileName );

			for( ResourceDefinitionFilter rFilt : rscDfnFilters.getResourceDefinitionFiltersList() ) {
				String rdName = rFilt.getRscDefnName();
				rFilt.setLocLevel( lLvl );

				TreeMap<LocalizationLevel,ResourceDefinitionFilter> filterTreeMap =
					rscFiltersMap.get( rdName );

				// if there is already an entry in the map, add this one to th
				if( filterTreeMap == null ) {
					// store entries in reverse order since we only want to 
					// access the highest level. Others are there in case we need 
					// to back out.
					filterTreeMap = new TreeMap<LocalizationLevel,ResourceDefinitionFilter>(
							LocalizationLevel.REVERSE_COMPARATOR );
					rscFiltersMap.put( rdName, filterTreeMap );
				}

				filterTreeMap.put( lLvl, rFilt );
			}
		}					
	}

	//
	private void readResourceDefns() throws VizException {
		if( resourceDefnsMap != null ) {
			return;
		}
		
		Collection<String> supportedPlugins = RecordFactory.getInstance().getSupportedPlugins();
		
		// this was used to maintain the order in the resourceDefnsTable but now that 
		// these are separate files, I don't know that this will work. Need to 
		// find another way to get these in the right order for the GUI.
		long t0 = System.currentTimeMillis();

        Map<String,LocalizationFile> lFiles = pathMngr.listFiles( 
        	     NcPathConstants.RSC_DEFNS_DIR, new String[]{  "xml" }, true, true );

        if( lFiles.containsKey( NcPathConstants.RESOURCE_FILTERS ) ) {
        	lFiles.remove( NcPathConstants.RESOURCE_FILTERS );
        }

        // Create the map resource definitions
		//
		resourceDefnsMap = new HashMap<String,ResourceDefinition>();

		for( LocalizationFile lFile : lFiles.values() ) {			
			try {	
				readResourceDefn( lFile  );
				
				
    			// TODO : add localization observer to update the Map when a localization file has
    		    // changed on another cave.
    			lFile.addFileUpdatedObserver( new ILocalizationFileObserver() {						
					@Override
					public void fileUpdated(FileUpdatedMessage message) {
						
						System.out.println("Localization File for RD, "+message.getFileName()+" has been updated.\n"+
								"To get these changes you will need to restart cave.");
//		                VizApp.runAsync(new Runnable() {
//		                    @Override
//		                    public void run() {
//		                        MessageDialog warnDlg = new MessageDialog(
//		                                .getShell(), "WARNING", null, , MessageDialog.,
//		                                new String[] { "OK" }, 0);
//		                        warnDlg.open();
//		                    }
//		                });
					}
				});
			}
			catch ( VizException e ) {
				out.println("Error creating ResourceDefn from file: "+lFile.getName() );
				out.println(" --->"+e.getMessage() );
				badRscDefnsList.add( e );
			}
		}
		
		long t1 = System.currentTimeMillis();
		out.println("Time to read "+lFiles.values().size() +" Resource Definitions: " + (t1-t0) + " ms");

		// read in the rscFiltersMap, 
		try {
			readResourceFilters();
		}
		catch( VizException e ) {
			
		}
		
		// Note: Temporary solution. Would prefer to fold this into the referencing
		// attr set file if/when changed to xml format.
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

	    List<String> errRdsList = findOrCreateInventoryForRscDefns( resourceDefnsMap.values() );
	    
	    // loop thru the ResourceDefns and enable those that have been initialized and
	    // find any inventories that don't exist and create them
	    //
	    
	    for( String rmRd : errRdsList ) {
	    	resourceDefnsMap.remove( rmRd );
	    }
	}

	private void readResourceDefn( LocalizationFile lFile ) throws VizException {

		File rscDefnFile = lFile.getFile();
		
		try {
			ResourceDefinition rscDefn = SerializationUtil.jaxbUnmarshalFromXmlFile( 
					ResourceDefinition.class, rscDefnFile.getAbsolutePath()  );

			// TODO : If the definitions are modified and written out, this will drop any invalid resourceDefns. 
	    	// Should we save these write them out anyway? Make them disabled?
	    
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
	    		
	    		if( rscDefn.isRequestable() ) {
	    			if( rscDefn.getPluginName() == null ) {
	    				throw new VizException( "Failed to create Rsc Defn "+rscDefn.getResourceDefnName()+
	    				": Requestable Resource is missing required pluginName parameter");
	    			}

	    			if( !RecordFactory.getInstance().
	    					getSupportedPlugins().contains( rscDefn.getPluginName() ) ) {
	    				rscDefnsWarningsList.add( 
	    						new VizException( "Disabling "+ rscDefn.getResourceDefnName()+
	    								" because plugin, "+ rscDefn.getPluginName()+
	    						" is not activated." ) );
	    			}
	    		}
	    		
    			resourceDefnsMap.put( rscDefn.getResourceDefnName(), rscDefn );
    			
    			if( rscImpl.equals( "Locator" ) ) {
    				locatorRscDefn = rscDefn;
    			}
    			
    			// TODO : Change this to set the LocalizationFile or the context
    		    rscDefn.setLocalizationFile( lFile );
    		    
//    			if( rscDefn.usesInventory() ) {
//    				
//    				NcInventoryDefinition invDefn = 
//    							rscDefn.createNcInventoryDefinition();
//    				
//    				// throws exception on error creating a defn
//    				if( invDefnsMap.containsKey( invDefn ) ) {
//    					
//    					rscDefn.setInventoryAlias( 
//							invDefnsMap.get( invDefn ).getInventoryName() );
//    				}
//				}
	    	}
		} 
		catch (SerializationException e) {
			throw new VizException("Error parsing "+rscDefnFile.getAbsolutePath() +" - " + e.getMessage() );
		}
		catch (Exception e ) {
			throw new VizException( "Error parsing "+rscDefnFile.getAbsolutePath() +" - " + e.getMessage() );
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

				lclFile.addFileUpdatedObserver( new ILocalizationFileObserver() {					
					@Override
					public void fileUpdated(FileUpdatedMessage fumsg) {
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
				});

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
			
			// TODO : add localization observer to update  the Map when a localization file has
		    // changed on another cave.
			
			File asgFile = lclFile.getFile();
			
			if( !asgFile.exists() ) {
				out.println( "Can't open AttrSetGroup file: "+asgFile.getAbsolutePath() );
				continue;
			}
			AttrSetGroup asg;
			try {
				asg = SerializationUtil.jaxbUnmarshalFromXmlFile( AttrSetGroup.class, asgFile.getAbsolutePath( ) );
				
			} catch (SerializationException e) {
				throw new VizException("Error Parsing file "+asgFile.getAbsolutePath( ) +"\n"+e.getMessage());
			} 

			// add the ASG's in the list to the map. (PGEN is a special case since
			// 1 'default' ASG applies to all PGEN resources.)
			asg.setLocalizationFile( lclFile );

			// if not PGEN then
			// validate that there is a resourceImpl for this attrSetGroup
			//
			if( !asg.getRscAndGroupName().isPGEN() ) {

				ResourceDefinition asgRscDefn = getResourceDefinition( asg.getResource() );
				if( asgRscDefn == null ) {
					out.println("AttrSetGroup file "+
							asgFile.getName() +" has a unknown resource:"+
							asg.getResource() );
					continue;
				}
				
				String rscImpl = asgRscDefn.getRscImplementation();

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
			}

			if( attrSetGroupsMap.containsKey( asg.getRscAndGroupName().toString() ) ) {
	    		System.out.println(asg.getRscAndGroupName().toString()+" already in the map???");
	    		
	    	}

			// 
			attrSetGroupsMap.put( asg.getRscAndGroupName().toString(), asg );
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
			
			String rscType = asg.getResource();
			String attrSetMapKey="";

			if( asg.getRscAndGroupName().isPGEN() ) {
				attrSetMapKey = asg.getResource();
			}
			else {
				ResourceDefinition rscDefn = getResourceDefinition( rscType );
				if( rscDefn != null ) {
					attrSetMapKey = rscDefn.getRscImplementation();
				}
			}
			
			for( String asName : new ArrayList<String>( asg.getAttrSetNames()) ) {
				if( !attrSetMap.containsKey( attrSetMapKey ) ||
					!attrSetMap.get( attrSetMapKey  ).containsKey( asName ) ) {
					
					asg.removeAttrSet(asName);
					out.println("attrSet "+asName+" in attrSetGroup "+
							asg.getResource()+File.separator+asg.getAttrSetGroupName()+" doesn't exist.");
				}
			}
		}
	}
	
	public List<String> findOrCreateInventoryForRscDefns( Collection<ResourceDefinition> rscDefnsToSetup ) {
	    // loop thru the ResourceDefns and enable those that have been initialized and
	    // find any inventories that don't exist and create them
	    //
		// This would read the inventoryDefns from localizations (ie what edex uses)
		// to initialize but instead we will query edex to see what's there and 
		// only create inventories that don't exist.	    
//	    checkAndSaveNcInventories();
		Map<NcInventoryDefinition,NcInventoryDefinition> invDefnsMap = null;
		
		// its possible this is failing/empty on the testbed for some unknown 
		// reason. Since this will cause 'duplicate' ID to be created we should 
		// retry to make sure.
		for( int tryCount=1 ; tryCount<=5 ; tryCount++ ) {
			try {
				invDefnsMap = getInventoryDefinitions();
				
				if( invDefnsMap == null || invDefnsMap.isEmpty() ) {
					throw new VizException( "Inventory Directory is Empty?");
				}
				else {
					break;
				}
			}
			catch (VizException ve ) {
				System.out.println( "Error getting NcInventory Directory" + ve.getMessage() );
				try { Thread.sleep(1000); } catch (InterruptedException e) { }
			}
		}

		if( invDefnsMap == null ) {
			invDefnsMap = new HashMap<NcInventoryDefinition,NcInventoryDefinition>();
		}
		
	    List<NcInventoryDefinition> createInvDefns = new ArrayList<NcInventoryDefinition>();
	    List<NcInventoryDefinition> errList = new ArrayList<NcInventoryDefinition>();
	    
	    for( ResourceDefinition rd : rscDefnsToSetup ) {
	    	try {	    		
	    		HashMap<String,RequestConstraint> rc = 
	    				rd.getInventoryConstraintsFromParameters( 
	    						rd.getResourceParameters( true ) );
	    		List<String> reqParams = rd.getUnconstrainedParameters();
	    		reqParams.add( "dataTime" );
	    		
	    		for( NcInventoryDefinition edexID : invDefnsMap.keySet() ) {
	    			if( edexID.supportsQuery( rc, reqParams ) ) {
	    				if( rd.isInventoryInitialized() ) {
	    					System.out.println( "RD "+ rd.getResourceDefnName() + 
	    							" has more than one supporting ID, "+ rd.getInventoryAlias()+
	    							" and "+ edexID.getInventoryName() );
	    				}
	    				System.out.println("Inventory found for "+ rd.getResourceDefnName() + " is " + 
	    								edexID.getInventoryName() );
	    				rd.setInventoryAlias( edexID.getInventoryName() );
//	        			break;
	    			}
	    		}

	    		if( rd.usesInventory() && 
	    			rd.getInventoryEnabled() &&
	    		   !rd.isInventoryInitialized() ) {

	    			createInvDefns.add( rd.createNcInventoryDefinition() );
	    		}
	    	}
	    	catch ( VizException e ) {
				out.println("Error creating ResourceDefn from file: "+rd.getLocalizationFile().getName() );
				out.println(" --->"+e.getMessage() );
				badRscDefnsList.add( e );				
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

	    List<String> errRdsList = new ArrayList<String>();
	    
	    // for the rscDefns that just had an inventory created for them
		// enable or disable based on whether there was an error.
		//
	    for( ResourceDefinition rd : resourceDefnsMap.values() ) {
	    	
	    	if( rd.usesInventory() ) {
	    		try {
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
		    	catch ( VizException e ) {
//		    		rd.setInventoryEnabled(false);
//		    		setResourceEnable( rd.getResourceDefnName(), false );
		    		errRdsList.add( rd.getResourceDefnName() );
		    		
					out.println("Error creating ResourceDefn : "+rd.getResourceDefnName() );
					out.println(" --->"+e.getMessage() );
					badRscDefnsList.add(
							new VizException( "Error creating ResourceDefn : "+rd.getResourceDefnName()+ " : " + e.getMessage() ));				
		    	}
	    	}
	    }	 

	    return errRdsList;
	}
	
	// a list of inventories definitions available on edex.
	// used to set the inventoryInitialized flag.	
	public Map<NcInventoryDefinition,NcInventoryDefinition> getInventoryDefinitions() throws VizException {
		
		Map<NcInventoryDefinition,NcInventoryDefinition> invDefnsMap = null; 

		// query the list of inventories that exist on edex and set the
		// inventoryInitialized flag in the ResourceDefns 
		NcInventoryRequestMsg dirRequest = NcInventoryRequestMsg.makeDirectoryRequest();

		Object rslts = ThriftClient.sendRequest( dirRequest );

		if( rslts instanceof String ) {
			throw new VizException( rslts.toString() );
		}
		if( !(rslts instanceof ArrayList<?>) ) {
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
		
		return invDefnsMap;
	}
	
	public boolean isResourceNameValid( ResourceName rscName ) {
		if( rscName == null ||
			rscName.getRscCategory() == null || 
			rscName.getRscCategory() == ResourceCategory.NullCategory ||
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
	
	// defined as an array but for now only assume 1 disp type in the list. 
	public ResourceCategory[] getResourceCategories( boolean includeDisabled, NcDisplayType[] matchingDispTypes ) {
		
		if( matchingDispTypes.length != 1 ) {
			System.out.println("getResourceCategories called with more than one display type. Only matching the first one");
		}
		
 		ArrayList<ResourceCategory> catsList = new ArrayList<ResourceCategory>();

 		// loop thru all the available categories in order and if a resource defn
 		//  exists for it then 
		for( ResourceCategory rc : ResourceCategory.values() ) {
			
			for( ResourceDefinition rscDefn : resourceDefnsMap.values() ) {				
				
				if( rc == rscDefn.getResourceCategory() ) {

					if( includeDisabled || rscDefn.isEnabled() ) {

						if( rscDefn.isDisplayTypeSupported( matchingDispTypes[0] ) ) {

							if( !catsList.contains( rc ) ) {
								catsList.add( rc );
								break;
							}
						}
					}
				}
			}
		}
    	
		ResourceCategory[] catsArray = catsList.toArray( new ResourceCategory[0] );
//    	
//    	Arrays.sort( catsArray, new Comparator<String>() {
//    		public int compare(String o1, String o2) {
//    			int indx1 = availResourceCategoriesList.indexOf( (String)o1 );
//    			int indx2 = availResourceCategoriesList.indexOf( (String)o2 );
//
//    			return ((indx1 == indx2 ? 0 : (indx1 < indx2 ? -1 : 1 )));
//    		}
//		}); 
		
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
		if( rscDefn.getResourceCategory() != ResourceCategory.OverlayRscCategory ) {
			
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
				String paramVal2 = subType.endsWith( "km" ) ?
						subType.substring(indx+1, subType.length()-2) // NOTE; "km"
						: subType.substring(indx+1, subType.length());
				// TODO : get rid of these hacks and redesign the resoureName <-> parameter value mapping
				if( paramVal2.equals("native") ) { 
					paramVal2 = "0"; 
				}
				//  TODO -- Can't make following sanity-check / cleanup anymore because paramVal2
				//          for NTRANS is productName, which isn't all numeric.  Trouble...?
				//  try {
				//	  int ok = Integer.parseInt(paramVal2);
				//  } catch (NumberFormatException e ) {
				//	  paramVal2 = "0";
				//  }

				String subtypeGenParam = subTypeGenParams[0];
				paramsMap.put( subTypeGenParams[0], paramVal1 );
				paramsMap.put( subTypeGenParams[1], paramVal2 );
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
				if( rscDefn1.getResourceCategory() == ResourceCategory.SurfaceRscCategory ||
					rscDefn1.getResourceCategory() == ResourceCategory.UpperAirRscCategory ) {

					if( (!rscDefn1.isForecast() && rscDefn2.isForecast() ) ||
						 (rscDefn1.isForecast() && !rscDefn2.isForecast() ) ) {    				

						return ( rscDefn1.isForecast() ? 1 : -1);
					}
				}
				// for Radar, Mosaics before Local Radar
				else if( rscDefn1.getResourceCategory() == ResourceCategory.RadarRscCategory ) {
					if( rscDefn1.getRscImplementation().equals("RadarMosaic") ) {
						return -1;
					}
					else if( rscDefn2.getRscImplementation().equals("RadarMosaic") ) {
						return 1;
					}    				
				}

					return rscDefn1.getResourceDefnName().compareToIgnoreCase( 
						   rscDefn2.getResourceDefnName() );    						
			}
		};
	}
	
	public void setResourceEnable( String rscType, Boolean enabled ) {
		ResourceDefinitionFilter rdFilt = getResourceDefnFilter( rscType );
		if( rdFilt.getIsEnabled() != enabled ) {
			rdFilt.setIsEnabled( enabled );
			setResourceDefnFilters( rdFilt );
		}
	}
	
	public void setResourceDefnFilters( ResourceDefinitionFilter rdFilt ) {
		String rscType = rdFilt.getRscDefnName();
		
		synchronized ( rscFiltersMap ) {
			if( !rscFiltersMap.containsKey( rscType ) ) {
				rscFiltersMap.put( rscType, 
						new TreeMap<LocalizationLevel,ResourceDefinitionFilter>(
								LocalizationLevel.REVERSE_COMPARATOR ) );
			}		

		// get the highest priority filters. (stored in reverse order)
		TreeMap<LocalizationLevel, ResourceDefinitionFilter> filtMap = rscFiltersMap.get( rscType );		
		filtMap.put( LocalizationLevel.USER, rdFilt ); 

//		if( !filtMap.containsKey( LocalizationLevel.USER ) ) {
//			ResourceDefinitionFilter curRdf = getResourceDefnFilter( rscType );
//			Boolean isEnabled = curRdf.getIsEnabled();
//			
//			filtMap.put( LocalizationLevel.USER, rdFilt ); 
//		}
//		else {
//			ResourceDefinitionFilter rdFilt = filtMap.get( LocalizationLevel.USER );
//
//			rdFilt.setFilters( new ArrayList<String>( filtList ) );
//		}		
		}
	}
	
	// Don't return null. If there is no entry then create one 
	public ResourceDefinitionFilter getResourceDefnFilter( String rscType ) {

		if( !rscFiltersMap.containsKey( rscType ) ||
			 rscFiltersMap.get( rscType ).keySet().isEmpty() ) {
			// should we add and entry in the map here?
			return new ResourceDefinitionFilter( rscType, false, null, LocalizationLevel.USER );
		}

		// get the highest priority filters. (stored in reverse order)
		TreeMap<LocalizationLevel, ResourceDefinitionFilter> filtMap = rscFiltersMap.get( rscType );		
		Iterator iter = filtMap.keySet().iterator(); 
		LocalizationLevel llvl = (LocalizationLevel)iter.next();
//		if( iter.hasNext() ) {
//			System.out.println("iterator has multiple entries and first was: "+llvl.name() );
//		}
		ResourceDefinitionFilter rdFilters = filtMap.get( llvl );
		return rdFilters;
	}
	
	// loop thru all the rsc defns for this cat and return a list of all
	// filter labels.
	public List<String> getAllFilterLabelsForCategory( ResourceCategory rscCat, NcDisplayType dispType ) {
		//getResourceDefnsForCategory
		
		ArrayList<String> filterLabelsList = new ArrayList<String>();

		List<ResourceDefinition> rdList;
		try {
			rdList = getResourceDefnsForCategory(rscCat, "", dispType, false, false );

			for( ResourceDefinition rd : rdList ) {
				ResourceDefinitionFilter rdFilt = getResourceDefnFilter( rd.getResourceDefnName() );

				for( String filtStr : rdFilt.getFilters() ) {    			
					if( !filterLabelsList.contains( filtStr ) ) {
						filterLabelsList.add( filtStr );
					}
				}
			}
		} catch (VizException e) {
		}

    	return filterLabelsList;
	}
	
    public  List<ResourceDefinition> getResourceDefnsForCategory( ResourceCategory rscCat ) throws VizException {
    	return getResourceDefnsForCategory(rscCat, "", null, false, true );
    }

    public  List<ResourceDefinition> getResourceDefnsForCategory( 
    		ResourceCategory rscCat, String filterStr, 
    		NcDisplayType dispType,
    		Boolean includeGeneratedTypes,
    		Boolean includeDisabledRscDefns ) throws VizException {
    	List<ResourceDefinition> resourceDefnsList = new ArrayList<ResourceDefinition>();
       	
       	// for other resources, get all of the resources in the given category.
       	//
    	for( ResourceDefinition rscDefn : resourceDefnsMap.values() ) {

    		if( !includeDisabledRscDefns && !rscDefn.isEnabled() ) {
    			continue;
    		}
    		
    		if( filterStr == null || filterStr.isEmpty() ||
    			getResourceDefnFilter( rscDefn.getResourceDefnName() ).testFilter( filterStr ) ) {
    			
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
    	ResourceDefinition rscDefn = getResourceDefinition( rscName );
    	if( rscDefn == null ) {
    		return null;
    	}
     	return getAttrSetGroupForResource( 
    			new RscAndGroupName( rscDefn.getResourceDefnName(), rscName.getRscGroup() ) );
    }
    	
    // lookup usging the rscType and the asg name
    public AttrSetGroup getAttrSetGroupForResource(RscAndGroupName attrSetGroupName ) {        	
       	return attrSetGroupsMap.get( attrSetGroupName.toString() );       	
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
    	else {
    		for( AttrSetGroup asg : attrSetGroupsMap.values() ) {

    			if( asg.getResource().equals(  rscDefn.getResourceDefnName() ) ) {
    				attrSetGroupsList.add( asg );    			
    			}   
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
		if( rscDefn.applyAttrSetGroups() && 
			asgName != null && !asgName.isEmpty() ) {
			
			RscAndGroupName rscGrpName = new RscAndGroupName( rscDefn.getResourceDefnName(), asgName );
			
			// Should we check that the asName is actually in the asGroup?
			//
			AttrSetGroup asg = getAttrSetGroupForResource( rscGrpName ); //attrSetGroupsMap.get( asgMapKey );

			if( asg == null || !asg.getAttrSetNames().contains( asName ) ) {
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
       	
    	if( rscDefn.applyAttrSetGroups() ) {
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
		ResourceCategory rscCat = rscDefn.getResourceCategory();
		rscName.setRscCategory( rscCat );

		// TODO : need to improve the way this works. Never liked it.
		// 
		List<String> rscTypes = new ArrayList();
		
//		if( rscDefn.getRscTypeGenerator().isEmpty() ) {
//			rscTypes.add( rscDefn.getResourceDefnName() );
//		}
//		else {
//			rscTypes = rscDefn.getGeneratedTypesList();			
//		}
		
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
        	        	
        	// remove the entry in the Filters file
        	if( rscFiltersMap.containsKey( rscDefn.getResourceDefnName() ) ) { // sanity check. Should be there.
        		TreeMap<LocalizationLevel, ResourceDefinitionFilter> filtMap = rscFiltersMap.get( rscDefn.getResourceDefnName() );
        		if( filtMap.containsKey( LocalizationLevel.USER ) ) {
        			filtMap.remove( LocalizationLevel.USER );
        			saveResourceDefnFiltersFile();
        		}
        		else {
            		System.out.println("sanity check: removing RD "+ rscDefn.getResourceDefnName()+". Missing USER level in the filtersTreeMap.");        			
        		}
        	}
        	else {
        		System.out.println("sanity check: removing RD "+ rscDefn.getResourceDefnName()+". Missing entry in the filtersMap.");
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
			
	    	if( attrSetGroupsMap.containsKey( attrSetGroup.getRscAndGroupName().toString() ) ) {
	   // 		System.out.println(attrSetGroup.getRscAndGroupName().toString()+" already in the map???");
	    		
	    	}
	    	
	    	attrSetGroupsMap.put( attrSetGroup.getRscAndGroupName().toString(), attrSetGroup );
	    	
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
    	
    	AttrSetGroup asg = getAttrSetGroupForResource( new RscAndGroupName( rscType, asgName ) );
    	
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

	    	attrSetGroupsMap.remove( asg.getRscAndGroupName().toString() );
	    	
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

			try {
				asg = SerializationUtil.jaxbUnmarshalFromXmlFile( 
						AttrSetGroup.class, asgFile.getAbsolutePath( ) );

			} catch (SerializationException e) {
				throw new VizException("Error Parsing file "+asgFile.getAbsolutePath( ) +"\n"+e.getMessage());
			} 

			// add the ASG's in the list to the map. (PGEN is a special case since
			// 1 'default' ASG applies to all PGEN resources.)			
			asg.setLocalizationFile( lFile );
			
			String rscImpl="";
			ResourceDefinition asgRscDefn = getResourceDefinition( asg.getResource() );

			if( asgRscDefn == null ) {
				throw new VizException("AttrSetGroup file "+
						asgFile.getName() +" has a unknown resource:"+
						asg.getResource() );
			}

			if( !asgRscDefn.isPgenResource() ) {				
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
			}

			attrSetGroupsMap.put( asg.getRscAndGroupName().toString(), asg );
			
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
        
    // get a list of all the USER-level RD filters and save them to the Filters file.
    public void saveResourceDefnFiltersFile() throws VizException {
    	ResourceDefinitionFilters rscDfnFilters = new ResourceDefinitionFilters();

    	// loop thru the user-level filters
    	// TODO : give the RDs a meaningful ordering.
    	for( String rdName : rscFiltersMap.keySet() ) {
    		
    		// get the highest priority filters. (stored in reverse order)
    		TreeMap<LocalizationLevel, ResourceDefinitionFilter> filtMap = rscFiltersMap.get( rdName );
    		ResourceDefinitionFilter rdFilt = filtMap.get( LocalizationLevel.USER );
    		
    		if( rdFilt != null ) {
    			rscDfnFilters.getResourceDefinitionFiltersList().add( rdFilt );	
    		}
    	}
    	
//  NOTE : should we 'patch' anything that may be out of order/missing.
//		for( ResourceDefinition rd : resourceDefnsMap.values() ) {
//			if( rscDfnFilters.;
//		}

    	LocalizationContext context = pathMngr.getContext(
				LocalizationType.CAVE_STATIC, LocalizationLevel.USER );

    	LocalizationFile rscFiltersLFile = pathMngr.getLocalizationFile( context, NcPathConstants.RESOURCE_FILTERS );

		try {
			SerializationUtil.jaxbMarshalToXmlFile( 
					rscDfnFilters, rscFiltersLFile.getFile().getAbsolutePath() );

			rscFiltersLFile.save();
		} catch (LocalizationOpFailedException e) {
			throw new VizException( e );
		} catch (SerializationException e) {
			throw new VizException( e );
		}
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
			

			List<ResourceDefinition> rdList = new ArrayList<ResourceDefinition>();
			rdList.add( rscDefn );
			
			List<String> errList = findOrCreateInventoryForRscDefns( rdList );

			if( errList.isEmpty() ) {
				resourceDefnsMap.put( rscDefn.getResourceDefnName(), rscDefn );				
			}
			else {				
				throw new VizException( "Error finding or Creating Inventory.");
			}
			
			// check to see if there is an inventory for this rscDefn or if
			// we need to create one. 
			// 
//			NcInventoryDefinition invDefn = rscDefn.createNcInventoryDefinition(); 
//			
//			// reload just in case they have changed.
//			Map<NcInventoryDefinition,NcInventoryDefinition> invDefnsMap = null;
//			
//			try {
//				invDefnsMap = getInventoryDefinitions();
//			}
//			catch (VizException ve ) {
//				System.out.println(ve.getMessage());
//				invDefnsMap = new HashMap<NcInventoryDefinition,NcInventoryDefinition>();
//			}
//			
//			if( invDefnsMap.containsKey( invDefn ) ) {
//				rscDefn.setInventoryAlias( invDefnsMap.get( invDefn ).getInventoryName() );
//			}
//			
//			if( rscDefn.usesInventory() ) {				
//				InventoryLoaderJob invLoader = new InventoryLoaderJob( invDefn, false );
//				
//				invLoader.schedule();
//				
//				// update the progress monitor
//				while( invLoader.getNumberOfInventoriesLeftToLoad() > 0 ) {
//					try {
//						Thread.sleep(400);
//					} catch (InterruptedException e) {						
//					}
//				}
//								
//				if( invLoader.getUninitializedInventoryDefns().length == 1 ) {
//					rscDefn.setInventoryAlias( null ); // Initialized( false );					
//					rscDefn.disableInventoryUse();
//					
//					throw new VizException("There was an error Initializing an Inventory : Disabling Inventory Use" );
//				}
//
//				rscDefn.setInventoryAlias( invDefn.getInventoryName() );//Initialized( true );				
//			}
//			
//			if( rscDefn.getInventoryEnabled() ) {
//				rscDefn.enableInventoryUse();
//			}
//			else {
//				rscDefn.disableInventoryUse();
//			}
//			
		} catch (SerializationException e) {
			throw new VizException("Error Serializing AttrSetGroup file:"+e.getMessage() );
		} catch (LocalizationOpFailedException e) {
			throw new VizException("Error Localizing file:"+e.getMessage() );
		} 

    	return true;
    }
        
    public ResourceDefinition getLocatorResourceDefinition() {
    	return locatorRscDefn;
    }
}
