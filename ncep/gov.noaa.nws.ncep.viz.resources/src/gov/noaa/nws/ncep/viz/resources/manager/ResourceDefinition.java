package gov.noaa.nws.ncep.viz.resources.manager;

import static java.lang.System.out;
import gov.noaa.nws.ncep.common.dataplugin.mcidas.McidasRecord;
import gov.noaa.nws.ncep.common.dataplugin.ntrans.NtransRecord;
import gov.noaa.nws.ncep.common.dataplugin.pgen.PgenRecord;
import gov.noaa.nws.ncep.edex.plugin.mosaic.common.MosaicRecord;
import gov.noaa.nws.ncep.viz.common.StringListAdapter;
import gov.noaa.nws.ncep.viz.common.display.NcDisplayType;
import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.gempak.util.GempakGrid;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData.TimeMatchMethod;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData.TimelineGenMethod;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceExtPointMngr;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceExtPointMngr.ResourceParamInfo;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceExtPointMngr.ResourceParamType;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceDefinitionFilters.ResourceDefinitionFilter;

import java.io.File;
import java.io.FilenameFilter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.uf.viz.core.catalog.CatalogQuery;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.catalog.ScriptCreator;
import com.raytheon.uf.viz.core.comm.Connector;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.raytheon.uf.viz.core.rsc.URICatalog;
import com.raytheon.uf.viz.core.rsc.URICatalog.IURIRefreshCallback;
import com.raytheon.viz.alerts.IAlertObserver;
import com.raytheon.viz.alerts.observers.ProductAlertObserver;

import gov.noaa.nws.ncep.edex.common.ncinventory.NcInventoryDefinition;
import gov.noaa.nws.ncep.edex.common.ncinventory.NcInventoryRequestMsg;

/**
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 *                            Greg Hull   Created
 *  02/26/11      #408        Greg Hull   add filterable labels, rm isForecast, isEvent
 *  03/02/11      #408        Greg Hull   add definitionIndex
 *  07/18/11      #450        Greg Hull   marshal rscParameters.
 *  07/24/11      #450        Greg Hull   save LocalizationFile
 *  11/15/11                  Greg Hull   rm attrSetGroupNames
 *  01/20/12      #606        Greg Hull   query from NcInventory, rm types/sub-types lists
 *  04/23/12      #606        Greg Hull   allow NcInventory to be disabled and query/update
 *                                        for generated sub/types.
 *  05/27/12      #606        Greg Hull   createNcInventoryDefinition()
 *  05/31/12      #606        Greg Hull   save the name/alias of the inventory to query 
 *  06/05/12      #816        Greg Hull   rm definitionIndex
 *  08/29/12      #556        Greg Hull   check isRequestable() before adding as an AlertObserver (for PGEN)
 *  09/01/12      #860        Greg Hull   Add smarter caching (for all constraints) of available times.
 *  09/05/12      #860        Greg Hull   Add this to the URICatalog for storing the latest time.
 *  09/13/12      #860        Greg Hull   set default for inventoryEnabled to false.
 *  11/2012		  #885		  T. Lee	  Set unmapped satellite projection resolution to "native"
 *  01/2013                   Greg Hull   Don't create wildcard inventory constraint
 *  02/2013       #972        Greg Hull   ResourceCategory class and supported display types
 *  03/2013       #972        Greg Hull   AttrSetsOrganization
 *  04/2013       #864        Greg Hull   mv filters and isEnabled
 *  04/2013       #838        B. Hebbard  Add special handling (like satellite) for NTRANS compound subType
 *  08/2013       #1031       Greg Hull   modified inventory query 
 *  11/2013       #1074       Greg Hull   fix bug generating native satellite sub-type
 *
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
@XmlRootElement(name = "ResourceDefinition")
@XmlAccessorType(XmlAccessType.NONE)
public class ResourceDefinition implements ISerializableObject, IAlertObserver, Comparable<ResourceDefinition> {    

    @XmlElement
    private String resourceDefnName;

    @XmlElement
    @XmlJavaTypeAdapter(ResourceCategory.ResourceCategoryAdapter.class)
    private ResourceCategory resourceCategory;

    private String           localizationName; // the path
    private LocalizationFile localizationFile;

    // must match the name in an extention point which defines the java class
    // that implementes the resource.
	@XmlElement
    private String rscImplementation;  

    // the name of a column in the DB used to generated dynamic Resources.
    // (Note: this must be defined as a parameter for the resource implementation.) 
    @XmlElement
    private String subTypeGenerator;

    // in the gui this is edited in the rscType text widget but it is stored
    // here.(Note: this must be defined as a parameter for the resource implementation.) 
    @XmlElement
    private String rscTypeGenerator;

    // the resource types generated by rscTypeGenerator from either the NcInventory or, 
    // if the inventory is disabled, the DB. 
    private ArrayList<String> generatedTypesList;

    private ArrayList<String> generatedSubTypesList;

    @XmlElement
    private TimeMatchMethod timeMatchMethod;

    @XmlElement
    private int frameSpan; // if 0 then the intervals are data-driven

    @XmlElement
    private TimelineGenMethod timelineGenMethod;

    private static final int DEFAULT_FRAME_COUNT = 10;

    private static final int DEFAULT_TIME_RANGE = 24; // in hours

    @XmlElement
    private int dfltFrameCount;

    // the default number of hours of data to make available in the timeline
    @XmlElement
    private int dfltTimeRange;

    // where/how are the attribute sets located/organized
    // 
    public static enum AttrSetsOrganization {
    	BY_ATTR_SET_GROUP,
    	BY_RSC_DEFN,
    	DETERMINE_BY_RSC_CATEGORY // GRIDS, RADAR...  use ATTR_SET GROUP
    }
    
    // Most will be the default which means we will use the resource
    // category to determine whether to use attr set groups, but this will
    // let a RD override the behaviour for a category if needed.
    @XmlElement
    private  AttrSetsOrganization attrSetOrg=AttrSetsOrganization.DETERMINE_BY_RSC_CATEGORY;
    
	private List<NcDisplayType> applicableDisplayTypes;

    @XmlElement
    private String dfltGeogArea;

    // this does not include any parameters associated with subtypes or any in the
    // attributes file.
    // NOTE : Comments are kept in this map so that they can be preserved when the user
    // edits the params in the gui. This are not returned by getResourceParameters
    // but it would be nice to have a cleaner way to store the comments for a parameter.
    @XmlElement
    @XmlJavaTypeAdapter(RscParamsJaxBAdapter.class)
    private HashMap<String,String> resourceParameters;
    
    private boolean resourceParametersModified;

    private Boolean isRequestable = null; // based on class of the implementations
    
    // Default to disnabled so it must be explicitly enabled. 
    @XmlElement
    private Boolean inventoryEnabled = false;
    	
	private String  inventoryAlias = null;
	
    // a map from the resource Constraints to a cache of the availableTimes and the
    // latest time. The availableTimes may come from an inventory query or a db query and
    // the latest time may be determined from the latest time or it may be updated
    // via Raytheon's MenuUpdater via the URICatalog by processing alery notifications..
    //  
    private Map<Map<String,RequestConstraint>, DataTimesCacheEntry> availTimesCache=null;
    
    // set this to true and store the latestTimes in the URICatalog. 
    // Some RscDefns have alot of possible constraints (ie radar and some satellites...) which
    // means that the inventory queries can get hit all at once and cause a slight (1 second?) 
    // delay. In this case we will leverage Raytheon's URICatalog which listens for the URI 
    // Notifications and stores the latest times.
    //    The code can override this flag if false and still add the RD to the Catalog if
    // for example the db query is too slow (ie > 2 seconds.)
    //    This is currently only used for the latestTimes in the attr set list. The 
    // actual times are still coming from the NcInventory or the DB. 
    //    
    @XmlElement
    private Boolean addToURICatalog = false;
    
	public ResourceDefinition() {
        resourceDefnName = "";
        resourceCategory = ResourceCategory.NullCategory; // new ArrayList<String>();
        subTypeGenerator = "";
        rscTypeGenerator = "";
        resourceParameters = new HashMap<String,String>();
        resourceParametersModified = false;
        frameSpan = 0;
        dfltFrameCount = DEFAULT_FRAME_COUNT;
        dfltTimeRange = DEFAULT_TIME_RANGE;
        dfltGeogArea = "";
        timeMatchMethod = TimeMatchMethod.CLOSEST_BEFORE_OR_AFTER;
        timelineGenMethod = timelineGenMethod.USE_DATA_TIMES;     
        inventoryAlias = null;
        availTimesCache = new HashMap<Map<String,RequestConstraint>,DataTimesCacheEntry>();

        inventoryEnabled = false;
        
        generatedTypesList = new ArrayList<String>();
    	
        generatedSubTypesList = new ArrayList<String>();
    }

    // 
    // shallow copy of the attrSetGroups and subTypes lists
    public ResourceDefinition(ResourceDefinition rscDefn) {
        resourceDefnName = rscDefn.getResourceDefnName();
        resourceCategory = rscDefn.resourceCategory;

        subTypeGenerator = rscDefn.getSubTypeGenerator();
        rscTypeGenerator = rscDefn.getRscTypeGenerator();

        resourceParameters = new HashMap<String,String>( 
        		rscDefn.resourceParameters );
        resourceParametersModified = rscDefn.resourceParametersModified;

        frameSpan = rscDefn.frameSpan;
        dfltFrameCount = rscDefn.dfltFrameCount;
        dfltTimeRange = rscDefn.dfltTimeRange;

        timeMatchMethod = rscDefn.timeMatchMethod;
        timelineGenMethod = rscDefn.timelineGenMethod;

        dfltGeogArea = rscDefn.dfltGeogArea;
        rscImplementation = rscDefn.rscImplementation;
        
        setLocalizationFile( rscDefn.getLocalizationFile() );
 
        inventoryAlias = null;
        
        availTimesCache = new HashMap<Map<String,RequestConstraint>,DataTimesCacheEntry>();
        
        inventoryEnabled = rscDefn.inventoryEnabled;
        
        generatedTypesList = new ArrayList<String>();
        
        generatedSubTypesList = new ArrayList<String>();
    }

    public String getLocalizationName() {
		return localizationName;
	}

	public void setLocalizationName(String name) {
		this.localizationName = name;
	}

    public LocalizationFile getLocalizationFile() {
		return localizationFile;
	}

	public void setLocalizationFile(LocalizationFile lFile) {
		this.localizationFile = lFile;
		if( lFile != null ) {
			setLocalizationName( lFile.getName() );
		}
		else {
			localizationName = "";
		}
	}

    public String getSubTypeGenerator() {
        return subTypeGenerator;
    }

    public void setSubTypeGenerator(String subTypeGenerator) {
        this.subTypeGenerator = subTypeGenerator;
    }

    public String[] getSubTypeGenParamsList() {
    	
    	if( subTypeGenerator == null || subTypeGenerator.trim().isEmpty() ) {
    		return new String[0];
    	}
    	String prmsList[] = subTypeGenerator.split(",");
    	for( int i=0; i<prmsList.length ; i++ ) {
    		prmsList[i] = prmsList[i].trim();
    	}
    	return prmsList;
    }
    
	// TODO : Need to change this to return the constraint field instead of the 
	// generator parameter. Til then all parameters that generate
	// a type or sub type must be the same name as the request constraint. 
    public String getRscTypeGenerator() {
        return (rscTypeGenerator == null ? "" : rscTypeGenerator );
    }

    public void setRscTypeGenerator(String rscTypeGenerator) {
        this.rscTypeGenerator = rscTypeGenerator;
    }

    public String getResourceDefnName() {
        return resourceDefnName;
    }

    public Boolean getAddToURICatalog() {
		return addToURICatalog;
	}

	public void setAddToURICatalog(Boolean addToURICatalog) {
		this.addToURICatalog = addToURICatalog;
	}

    // resourceParameters includes the comments but don't return them.
    //
    public HashMap<String,String> getResourceParameters( boolean includeDefaults ) {
    	HashMap<String,String> prmsWithoutComments = new HashMap<String,String>();

    	for( String prmName : resourceParameters.keySet() ) {
			if( !prmName.trim().startsWith("!") ) {
				prmsWithoutComments.put( prmName, resourceParameters.get(prmName) );		
			}
		}
		if( includeDefaults ) {
			// the default values specified in the extention point
			HashMap<String,String> dfltParamValues = getDefaultParameterValues();
			
			for( String dfltPrm : dfltParamValues.keySet() ) {
				if( !prmsWithoutComments.containsKey( dfltPrm ) ) {
					prmsWithoutComments.put( dfltPrm, dfltParamValues.get( dfltPrm ) );
				}
			}
		}
		// 
    	return prmsWithoutComments;
    }
    
    // TODO : we might want to insert a 'blank' place holder 
    // for IMEPLEMENTATION or REQUEST_CONSTRAINT parameters that are defined by the
    // implementation but not specified here. This may happen to existing user
    // RDs after a new parameter is defined. (except we'd need to figure out what to 
    // do in the case of parameters like imageType's that are normally specified in the
    // attribute sets.
    //
    public String getResourceParametersAsString() {
		if( resourceParameters.isEmpty() ) {
			return "";
		}

		StringBuffer strBuf = new StringBuffer();

		for( String prmName : resourceParameters.keySet() ) {
			if( prmName.startsWith("!") ) {
				strBuf.append( prmName +"\n");
			}
			else {
				strBuf.append( prmName+"="+ resourceParameters.get( prmName ) + "\n" );		
			}
		}
		return strBuf.toString();			
    	
    }
    
    public void setResourceParameters(HashMap<String,String> typeParams) {
        if (resourceParameters.isEmpty()
                || !resourceParameters.equals(typeParams)) {
        	resourceParametersModified = true;
        }

        resourceParameters = new HashMap<String,String>( typeParams );
    }
    
    public void setResourceParametersFromString( String prmsStr ) {
    	
    	// sanity check that the keys are all the same.
    	//
		String[] prmStrs = prmsStr.split("\n");
		
		for( String prmStr : prmStrs ) {
			if( prmStr.startsWith( "!" ) ) {
				resourceParameters.put( prmStr, prmStr );
			}
			else {
				int equalsIndx = prmStr.indexOf("=");
				String prmName = prmStr.substring(0, equalsIndx );					
				String prmVal = prmStr.substring( equalsIndx+1, prmStr.length() );
				resourceParameters.put( prmName.trim(), prmVal.trim() );
			}
		}
    }
    
    public boolean getResourceParamsModified() {
        return resourceParametersModified;
    }

    public void setResourceParamsModified(boolean rscTypeParamsModified) {
        this.resourceParametersModified = rscTypeParamsModified;
    }

    // isEnabled used to be stored here so instead of changing the code that called this method, 
    // just implement it here. 
    public boolean isEnabled() {
    	ResourceDefinitionFilter rdf;
		try {
			rdf = ResourceDefnsMngr.getInstance().getResourceDefnFilter( resourceDefnName );
		} catch (VizException e) {
			return false;
		}
    	// TODO : somewhere there should be logic to determine the default 
    	// enabled state of a RD that doesn't have an entry in the filters file. Here? or in mngr?
    	return ( rdf == null ? false : rdf.getIsEnabled() ); 
    }

    public Boolean isDisplayTypeSupported( NcDisplayType dispType ) {
    	for( NcDisplayType dt : getSupportedDisplayTypes() ) {
    		if( dt == dispType ) {
    			return true;
    		}
    	}
    	return false;
    }
    
    public NcDisplayType[] getSupportedDisplayTypes() {
    	if( applicableDisplayTypes == null ) {
    		applicableDisplayTypes = new ArrayList<NcDisplayType>();
    	}
    	
    	Class<?> implClass = ResourceExtPointMngr.getInstance().getResourceDataClass( rscImplementation );
    	
    	try {
			Object rscData = implClass.newInstance();
			if( rscData instanceof AbstractNatlCntrsRequestableResourceData ) {
				return ((AbstractNatlCntrsRequestableResourceData)rscData).getSupportedDisplayTypes();
			}
			else if( rscData instanceof AbstractNatlCntrsResourceData ) {
				return ((AbstractNatlCntrsResourceData)rscData).getSupportedDisplayTypes();
			}
			else {
				System.out.println("??? Rsc Impl "+rscImplementation+"has non-NC resource class");	
			}
		} catch (InstantiationException e) {
			System.out.println("Error instantiating class ("+implClass.getName()+") for resource: "+ rscImplementation );
		} catch (IllegalAccessException e) {
			System.out.println("Error instantiating class ("+implClass.getName()+") for resource: "+ rscImplementation );
		}
    	
    	return new NcDisplayType[0];
    }
    
    public boolean isForecast() {
    	return (timelineGenMethod == TimelineGenMethod.USE_CYCLE_TIME_FCST_HOURS ||
    			timelineGenMethod == TimelineGenMethod.USE_FCST_FRAME_INTERVAL_FROM_REF_TIME );
        //return filterLabels.contains("Forecast");
    }

    public ResourceCategory getResourceCategory() {
        return resourceCategory;
    }

    public void setResourceCategory(ResourceCategory rCat) {
        this.resourceCategory = rCat;
    }

    public void setResourceCategory(String rCatStr) {
        this.resourceCategory = ResourceCategory.getCategory( rCatStr );
    }

    public String getRscImplementation() {
        return rscImplementation;
    }

    public void setRscImplementation(String rscImplementation) {
        this.rscImplementation = rscImplementation;
    }

    public TimeMatchMethod getTimeMatchMethod() {
        return timeMatchMethod;
    }

    public void setTimeMatchMethod(TimeMatchMethod timeMatchMethod) {
        this.timeMatchMethod = timeMatchMethod;
    }

    public TimelineGenMethod getTimelineGenMethod() {
        return timelineGenMethod;
    }

    public void setTimelineGenMethod(TimelineGenMethod timelineGenMethod) {
        this.timelineGenMethod = timelineGenMethod;
    }

    public int getFrameSpan() {
        return frameSpan;
    }

    public void setFrameSpan(int frameSpan) {
        this.frameSpan = frameSpan;
    }

    public int getDfltFrameCount() {
        return dfltFrameCount;
    }

    public void setDfltFrameCount(int dfltFrameCount) {
        this.dfltFrameCount = dfltFrameCount;
    }

    public int getDfltTimeRange() {
        return dfltTimeRange;
    }

    public void setDfltTimeRange(int dfltTimeRange) {
        this.dfltTimeRange = dfltTimeRange;
    }

    public String getDfltGeogArea() {
        return dfltGeogArea;
    }

    public void setDfltGeogArea(String dfltGeogArea) {
        this.dfltGeogArea = dfltGeogArea;
    }

    public void setResourceDefnName(String rName) {
        this.resourceDefnName = rName;
    }

    public boolean isPgenResource() {
        return resourceCategory == ResourceCategory.PGENRscCategory;
    }

    public AttrSetsOrganization getAttrSetOrg() {
		return attrSetOrg;
	}

	public void setAttrSetOrg(AttrSetsOrganization attrSetOrg) {
		this.attrSetOrg = attrSetOrg;
	}

    public boolean applyAttrSetGroups() {
    	if( attrSetOrg == AttrSetsOrganization.DETERMINE_BY_RSC_CATEGORY ) {
    		return  resourceCategory == ResourceCategory.GridRscCategory ||
	        		resourceCategory == ResourceCategory.RadarRscCategory ||
//	        		resourceCategory == ResourceCategory.PGENRscCategory ||
	                resourceCategory == ResourceCategory.EnsembleRscCategory ||
	                resourceCategory == ResourceCategory.SpaceRscCategory;
    	}
    	else if( attrSetOrg == AttrSetsOrganization.BY_ATTR_SET_GROUP ) {
    		return true;
    	}
    	else {
    		return false;
    	}
    }

    // the plugin must be given as a resource parameter
    //
    public String getPluginName() {
    	return getResourceParameters(false).get("pluginName");    	
    }
    
    public List<String> getUnconstrainedParameters() {
    	HashMap<String,RequestConstraint> constrMap = 
    			getInventoryConstraintsFromParameters( getResourceParameters( true ),true );
    	List<String> unconstPrms = new ArrayList<String>();
    	
    	for( String prm : constrMap.keySet() ) {
    		if( constrMap.get( prm ) == RequestConstraint.WILDCARD ) {
    			unconstPrms.add( prm );
    		}
    	}
    	//  TODO : add dataTime here?
    	return unconstPrms;
    }
    
    public HashMap<String,RequestConstraint>  getInventoryConstraintsFromParameters( 
    		HashMap<String,String> paramValues ) {
    	return getInventoryConstraintsFromParameters(paramValues, false );
    }

    public HashMap<String,RequestConstraint>  getInventoryConstraintsFromParameters( 
    		HashMap<String,String> paramValues, Boolean includeWildcard ) {

    	HashMap<String,RequestConstraint> inventoryConstraints = 
    		new HashMap<String,RequestConstraint>( );

    	inventoryConstraints.put( "pluginName", new RequestConstraint( getPluginName() ) );
    	
    	HashMap<String, ResourceParamInfo> rscImplParams = 
    				ResourceExtPointMngr.getInstance().getResourceParameters( rscImplementation );

    	// override the constraints in the inventory with those from the params 
    	// if there is no param value for the constraint then change to a wildcard.
    	//
    	// (most of the time the paramName and constraint name are the same except for
    	// GDFILE where the constraint is 'modelName') So for this case we need to 
    	//    	
    	for( ResourceParamInfo prmInfo : rscImplParams.values() ) {    		

			// if this parameter is defined as a request constraint and 
    		// if it has a non-wildcard value, then create a inventory constraint for it 
    		// 
    		if( prmInfo.getParamType() == ResourceParamType.REQUEST_CONSTRAINT ) {
    			
    			String prmName = prmInfo.getParamName();
    			String cnstrName = prmInfo.getConstraintName();

    			if( paramValues.containsKey( prmName ) ) {    				    				
    				// if the constraint value is a wildcard then don't add this
    				// to the list since it will fail in the case where the
    				// db value is a null.
    				RequestConstraint reqConstr = getConstraintFromParamValue( 
    													paramValues.get( prmName ) );
    				
    				if( includeWildcard || 
    					reqConstr != RequestConstraint.WILDCARD ) {

    					inventoryConstraints.put( cnstrName, reqConstr );
    				}
    			}
//    			else {
    			// 'LIKE' doesn't work for integers. Just leave this out.
//    				if( prmInfo.getParamClass() == Integer.class ) {
//    					inventoryConstraints.put( cnstrName, RequestConstraint.WILDCARD );
//    				}
//    				inventoryConstraints.put( cnstrName, RequestConstraint.WILDCARD );
//    			}
    		}
    	}    	
    	return inventoryConstraints;
    }

    public String getInventoryAlias() {
    	return inventoryAlias;
    }
    
    public RequestConstraint getConstraintFromParamValue( String paramVal ) {
    	if( paramVal.equals( "%" ) ) {
    		return RequestConstraint.WILDCARD;
    	}
    	else if( paramVal.indexOf( "%" ) != -1 ) {
        	return new RequestConstraint( paramVal, ConstraintType.LIKE );
    	}
    	else if( paramVal.indexOf( "," ) == -1 ) {
        	return new RequestConstraint( paramVal, ConstraintType.EQUALS );
    	}
    	else { 
    		return new RequestConstraint( paramVal, ConstraintType.IN );
    	}
    	// TODO : do we need a syntax for 'BETWEEN'
    }

    public boolean isInventoryInitialized() {
    	return (inventoryAlias != null && !inventoryAlias.isEmpty() ); 
    			//inventoryInitialized;
    }

    // This alias is either read from the existing inventory, or
    // it is set when the inventory is created for this ResourceDefn.
    // Either way this is what is used to query the inventory and is 
    // also used as a flag to indicate that an inventory exists for
    // the RscDefn.
    //
    public void setInventoryAlias( String invAlias ) {
    	inventoryAlias = invAlias;
    }

    public Boolean getInventoryEnabled() {
		return inventoryEnabled;
	}

	public void setInventoryEnabled(Boolean enableInventory) {
		this.inventoryEnabled = enableInventory;		
	}

	// The inventory should have been initialized before enabling the inventory 
	public void enableInventoryUse() {

    	if( !usesInventory() || !isInventoryInitialized() ) {
        	inventoryEnabled = false;
    		return;
    	}
    	inventoryEnabled = true;

		ProductAlertObserver.removeObserver( getPluginName(), this );		
	}
	
	// disabling the inventory means we will need to query the types/sub-type
	// and maintain the list with the data URI notifications.
	//
	public void disableInventoryUse() throws VizException {

		setInventoryEnabled( false );
		
		// if we need to initialize and maintain any generated types or subTypes
		//
		if( isRequestable() &&
			( getSubTypeGenParamsList().length > 0 ||  
		     !getRscTypeGenerator().isEmpty() ) ) {

			queryGeneratedTypes();

			ProductAlertObserver.addObserver( getPluginName(), this );			
		}
	}

	// based of the base class of the resource implementation
    //
    public boolean isRequestable() {
    	if( isRequestable == null && rscImplementation != null ) {
        	isRequestable = false;
        	
    		Class<?> implClass = ResourceExtPointMngr.getInstance().getResourceDataClass( rscImplementation );
        	
        	try {
    			Object rscData = implClass.newInstance();
    			isRequestable = (rscData instanceof AbstractNatlCntrsRequestableResourceData);
    				
    		} catch (InstantiationException e) {
    			System.out.println("Error instantiating class ("+implClass.getName()+") for resource: "+ rscImplementation );
    		} catch (IllegalAccessException e) {
    			System.out.println("Error instantiating class ("+implClass.getName()+") for resource: "+ rscImplementation );
    		}    		
    	}
    	return (isRequestable == null ? false : isRequestable);
    	//!(getResourceCategory() == ResourceCategory.OverlayRscCategory 
    			///*|| isPgenResource()*/ );  
    }
    
    public boolean usesInventory() {
    	// TODO : other 'gempak' plugins??? 
    	return isRequestable() &&
    		   !isPgenResource() && 
    		   !getPluginName().equals( GempakGrid.gempakPluginName );
    }
        
    public HashMap<String,String> getDefaultParameterValues() {
    	HashMap<String,String> dfltParamValues = new HashMap<String,String>();
		HashMap<String, ResourceParamInfo> rscImplParams = 
			ResourceExtPointMngr.getInstance().getResourceParameters( getRscImplementation() );

    	for( ResourceParamInfo prmInfo : rscImplParams.values() ) {
    		String dfltVal = prmInfo.getDefaultValue();
    		
    		if( dfltVal != null && !dfltVal.isEmpty() ) {
    			dfltParamValues.put( prmInfo.getParamName(), dfltVal );
    		}
    	}
    	
    	return dfltParamValues;
    }

	public void validateResourceParameters( ) throws VizException {
		HashMap<String, ResourceParamInfo> rscImplParams = 
			ResourceExtPointMngr.getInstance().getResourceParameters( getRscImplementation() );

		// the parameters defined by the resource definition and use the defaults 
		// specified by the implementation is not given.
		//
		HashMap<String,String> paramValues = getResourceParameters( true );

		// sanity check on timelineGen
		if( rscImplementation == "ModelFcstGridContours" &&
			timelineGenMethod != TimelineGenMethod.USE_CYCLE_TIME_FCST_HOURS ) {
			System.out.println("Sanity Check: GRID rsc has a non-forecast timelineGenMethod???");
		}
		
		// check that all of the paramValues are specified for the rsc implementation
		// 
		for( String prm : paramValues.keySet() ) {
			if( !prm.equals("pluginName") &&
				!rscImplParams.containsKey( prm ) ) {

				throw new VizException( getResourceDefnName()+" has a parameter, "+prm+
						", that is not recognized by its implementation: "+ getRscImplementation() );
			}
		}

		// a list of all the generated parameters
		List<String> genParamsList = new ArrayList<String>( Arrays.asList( getSubTypeGenParamsList() ) ); 
		if( !getRscTypeGenerator().isEmpty() ) {
			genParamsList.add( getRscTypeGenerator() );
		}
		
		// check that all the parameters defined for the implmentation either have a 
		// value given in the rsc params, will be generated, or have a default value 
		// 
		for( ResourceParamInfo implPrmInfo : rscImplParams.values() ) {
			String implPrm = implPrmInfo.getParamName();
			String constraintName = implPrmInfo.getConstraintName();
			
			if( implPrmInfo.getParamType() == ResourceParamType.EDITABLE_ATTRIBUTE ||
				implPrmInfo.getParamType() == ResourceParamType.NON_EDITABLE_ATTRIBUTE ) {
				// if checking for attributes...
				continue;
			}
			else if( implPrmInfo.getParamType() == ResourceParamType.REQUEST_CONSTRAINT ) {
			
				// if this param will be generated.
				// 
				if( genParamsList.contains( constraintName ) ) {
					continue;
				}
				else {
					// if the needed param is not set in the resource defn or is set to empty 
					String paramValue = paramValues.get( implPrm );

					if( paramValue == null || paramValue.isEmpty() ) {

//						paramValue = dfltParamValues.get( implPrm );
//
//						// if there is no default value specified by the implementation
//						//
//						if( paramValue == null || paramValue.isEmpty() ) {
							throw new VizException(getResourceDefnName()+ " is missing a value for the parameter "+implPrm+"." );
//						}
					}
				}
			}
		}		
	}

	// if an alias 
	public NcInventoryDefinition createNcInventoryDefinition() throws VizException {

		// Each resource implementation defines parameters which are used to
		// constraint the data read from the database. (ex. the modelName for Grids)
		// These parameters ultimately must be given values in order to instantiate 
		// a Resource. The parameters may come from :
		//     1) parameters defined here by the Resource Definition, 
		// 	   2) generating resource type or sub-types.
		//     3) parameters defined in attribute sets. 
		// 
		// At this point, we only have values for 1) while values for 2) and 3) are
		// defined later when the user selects the full ResourceName (ie. sub-type & attrSet)
		// So parameters/constraints for 2) and 3) must be kept in the inventory.
		//
		//   The inventory has 'baseConstraints' which all data in the inventory must pass. And
		// it has 'inventoryConstraints' which will define the constraints that each node at a
		// given level must pass. 
		// 
		HashMap<String, ResourceParamInfo> rscImplParams = 
			ResourceExtPointMngr.getInstance().getResourceParameters( rscImplementation );

		HashMap<String,String> paramValues = getResourceParameters(false);

		// the parameters used to define the data that gets stored in the inventory.
		// this will be all the request constraint params that don't have a specified value
		// for this resource (such as any type or sub-type generating params and any 
		// parameters that are specified in an attribute set. (ex. radar product codes and 
		// satellite imageTypes.)
		//
		HashMap<String, RequestConstraint> baseConstraints = 
			new HashMap<String,RequestConstraint>();

		//    		HashMap<String, RequestConstraint> inventoryConstraints = 
		//    								new HashMap<String,RequestConstraint>();
 
		baseConstraints.put( "pluginName", new RequestConstraint( getPluginName() ) );
		//        	inventoryConstraints.put( "pluginName", new RequestConstraint( getPluginName() ) );
		ArrayList<String>  inventoryParamNames = new ArrayList<String>();
		inventoryParamNames.add( "pluginName" );
		
//		if( rscImplementation == "ModelFcstGridContours" ) {
//			inventoryParamNames.add("info.ensembleId");
//		}

		// loop thru all of the request constraint parameters defined for the implementation and
		// add it to either the base constraints or the inventory constraints. 
		//
		for( ResourceParamInfo prmInfo : rscImplParams.values() ) {    		
			if( prmInfo.getParamType() == ResourceParamType.REQUEST_CONSTRAINT ) {

				// if the parameter has a value then 
				// 
				if( paramValues.containsKey( prmInfo.getParamName() ) ) {

					String prmValue = paramValues.get( prmInfo.getParamName() ).trim();

					// if the paramber value is a wildcard or a non-constant
					// value (ie. a list of reportTypes), then we will
					// need to put this parameter in the inventory
					//
					if( prmValue.indexOf(',') != -1 ) {
						inventoryParamNames.add( prmInfo.getConstraintName() );
						baseConstraints.put( prmInfo.getConstraintName(),
								new RequestConstraint( prmValue, ConstraintType.IN ) );
					}
					else if( prmValue.equals("%") ) {
						inventoryParamNames.add( prmInfo.getConstraintName() );
						baseConstraints.put( prmInfo.getConstraintName(),
								new RequestConstraint( prmValue, ConstraintType.LIKE ) );
					}
					else {
						baseConstraints.put( prmInfo.getConstraintName(),
								new RequestConstraint( prmValue, ConstraintType.EQUALS ) );
					}
				}
				else { // if there is no parameter value given add to the inventoryConstrains
					inventoryParamNames.add( prmInfo.getConstraintName() );
					//        				inventoryConstraints.put( prmInfo.getConstraintName(),    			
					//        						RequestConstraint.WILDCARD ); 
				}
			}    		
			// sanity check : if the parameter is not given in the attribute set then we  
			//   won't be able to instantiate the resourceData. 
			// These should be NON_EDITABLE_ATTRIBUTES
			else if( prmInfo.getParamType() == ResourceParamType.IMPLEMENTATION_PARAM &&
					 (prmInfo.getDefaultValue() == null ||
					  prmInfo.getDefaultValue().isEmpty() ) ) {
				
				if( !paramValues.containsKey( prmInfo.getParamName() ) ) {
					throw new VizException( "Error initializing rscDefn "+
							getResourceDefnName()+ ": a value is not specified for parameter "+
							prmInfo.getParamName() );
				}
			}
		}

		// make sure that the inventory tree is ordered with the pluginName first followed by
		// the generating params and any defined in the attrSets and with the dataTimes as end nodes.
		//
		//    		inventoryParamNames = new ArrayList<String>( inventoryConstraints.keySet() );

		Collections.sort( inventoryParamNames, new Comparator<String>() {
			@Override
			public int compare(String s1, String s2) {

				if( s1.equals( s2 ) ) {
					return 0;
				}
				int p1=999, p2=999;

				if( s1.equals( "pluginName" ) ) {
					p1 = 0;
				}
				if( s2.equals( "pluginName" ) ) {
					p2 = 0;
				}

				if( s1.equals( getRscTypeGenerator() ) ) {
					p1 = 1;
				}
				if( s2.equals( getRscTypeGenerator() ) ) {
					p2 = 1;
				}

				if( getSubTypeGenParamsList().length > 0 ) {
					if( s1.equals( getSubTypeGenParamsList()[0] ) ) {
						p1 = 2;
					}
					if( s2.equals( getSubTypeGenParamsList()[0] ) ) {
						p2 = 2;
					}	
				}
				if( getSubTypeGenParamsList().length > 1 ) {
					if( s1.equals( getSubTypeGenParamsList()[1] ) ) {
						p1 = 3;
					}
					if( s2.equals( getSubTypeGenParamsList()[1] ) ) {
						p2 = 3;
					}	
				}

				// radar has elevation and productCode parameters in the attrSets 
				// Do we need to specify an order in this case?
				return p1-p2;
			}
		});

		inventoryParamNames.add( "dataTime" );

		String inventoryName;
		if( inventoryAlias == null || inventoryAlias.isEmpty() ) {
			inventoryName = LocalizationManager.getInstance().getCurrentUser() + ":" +
												getResourceDefnName();
		}
		else {
			inventoryName = inventoryAlias;
		}

		return new NcInventoryDefinition( inventoryName, baseConstraints, inventoryParamNames );
	}

//	private void initInventory( Boolean reload ) throws VizException {
//    	    	
//    	if( !usesInventory() ) {
//    		return;
//    	}
//    	else if( getPluginName() == null ) {
//    		throw new VizException( "Error creating ResourceDefn ("+resourceDefnName+
//    				") The pluginName must be given as a resource parameter." );
//    	}
//		else if( getPluginName().equals( GempakGrid.gempakPluginName ) ) {
//			return;
//		}
//		else if( isInventoryInitialized() && !reload ) {
//    		return;
//    	}
//		else if( !isInventoryInitialized() && reload ) {
//			// can't reload if not created yet
//			//reload = 
//		}
//		return;
//    }

    //
    public ArrayList<String> getGeneratedTypesList() throws VizException {
    	
    	if( !inventoryEnabled ) {
    		return generatedTypesList;
    	}

    	generatedTypesList.clear();

    	if( !usesInventory() ) {
    		return generatedTypesList;
    	}
    	else if( getRscTypeGenerator().isEmpty() ) {
    		return generatedTypesList;
    	}
    	else if( !isInventoryInitialized() ) {
//    		out.println("getGeneratedTypesList: inventory not loaded");
    		throw new VizException("Inventory Not Initialized.");
//    		if( inventoryLoadStrategy == InventoryLoadStrategy.NO_INVENTORY ) {
//    			out.println("RscDefn "+ getResourceDefnName() + " has a generating type "+
//    				" which requires an inventory" );
//    			setInventoryLoadStrategy( InventoryLoadStrategy.ON_DEMAND );
//    		}
    	}

		// get a list of the generated Resource Types from the inventory.
		//
		Map<String, RequestConstraint> searchConstraints = 
					getInventoryConstraintsFromParameters( getResourceParameters(true) );

		// Create an NcInventoryRequest script to get query the ncInventory for the types.
		// 
//		String inventoryName = LocalizationManager.getInstance().getCurrentUser() + ":" +
//										getResourceDefnName();

		NcInventoryRequestMsg reqMsg = NcInventoryRequestMsg.makeQueryRequest();
		reqMsg.setInventoryName( inventoryAlias );
		reqMsg.setReqConstraintsMap( searchConstraints );
		reqMsg.setUniqueValues( true );

		// TODO : Need to change this to return the constraint field instead of the 
		// generator parameter. Til then all parameters that generate
		// a type or sub type must be the same name as the request constraint. 
		String genType = getRscTypeGenerator();
		
		String cnstrName = null;
		
		if ( genType != null ) {
			HashMap<String, ResourceParamInfo> rscImplParams = 
				ResourceExtPointMngr.getInstance().getResourceParameters( rscImplementation );
			for( ResourceParamInfo prmInfo : rscImplParams.values() ) {    		
				if( prmInfo.getParamType() == ResourceParamType.REQUEST_CONSTRAINT ) {
					String prmName = prmInfo.getParamName();
					if ( prmName.equals(genType)) {
						cnstrName = prmInfo.getConstraintName();
						break;
					}
				}
			}
    	}
		reqMsg.setRequestedParams( new String[]{cnstrName} );

		Object rslts;

		long t01 = System.currentTimeMillis();

		rslts = ThriftClient.sendRequest( reqMsg );

		if( !(rslts instanceof String[]) ) {
			out.println("Inventory Request Error: expecting String[] return." );
			disableInventoryUse();
			throw new VizException("Inventory Request Error:"+rslts.toString() +
					"\nDisabling Inventory use.");
		}

		long t02 = System.currentTimeMillis();

		String[] rsltsArray = (String[])rslts;

//		out.println("Inventory Query for RscTypes for "+ resourceDefnName + 	
//				" took "+ (t02-t01)+ "msecs for "+ rsltsArray.length + " results." );
//		out.println("    RscTypes = "+ (rsltsArray.toString() ) );

		// just 1 requested param so the result string will be the 1 value
		for( String rsltStr : rsltsArray ) {
//			String genTypesArr[] = rsltStr.split("/");
			generatedTypesList.add( getResourceDefnName() + ":" + rsltStr );//genTypesArr[ genTypesArr.length-1] );
		}

    	return generatedTypesList;
    }

    //
    public ArrayList<String> generatedSubTypesList() throws VizException {
		// Grids, Satellite, Radar, etc all are dynamically updated when new alert updates 
		// are received (when new data is ingested). PGEN doesn't have alert updates and so
		// we need to always check for new products.   
    	// 
    	if( isPgenResource() ) {
    		queryGeneratedTypes();
    		return generatedSubTypesList;
    	}

    	// note that if the inventory is at first enabled and then disabled, generatedSubTypesList
    	// will have been populated by the inventory and not queryGeneratedTypes. Also,
    	// any new sub types ingested will not be found since the productAlertObserver was not 
    	// added on startup.
    	//
    	if( !inventoryEnabled ) {
    		return generatedSubTypesList;
    	}

    	generatedSubTypesList.clear();

    	if( !usesInventory() ) {
    		return generatedSubTypesList;
    	}
    	else if( getSubTypeGenParamsList().length == 0 ) {
    		return generatedSubTypesList;
    	}
    	else if( !isInventoryInitialized() ) {
    		throw new VizException("Inventory Not Initialized.");
    	}

		// the parameters used to define the data that gets stored in the inventory.
		// this will be all the request constraint params that don't have a specified value
		// for this resource. This will be any type or sub-type generating params and any 
		// parameters that are specified in an attribute set. (ex. radar product codes and 
		// satellite imageTypes.)
		//		
    	Map<String, RequestConstraint> searchConstraints = 
   		     getInventoryConstraintsFromParameters( getResourceParameters(true) );

    	// get the names of the db fields which may be different than the rsc's parameter names.
    	int numSubTypeGenParams = getSubTypeGenParamsList().length;
    	String[] subTypeParamNames=getSubTypeGenParamsList();
    	String[] queryParams = new String[numSubTypeGenParams];

    	HashMap<String, ResourceParamInfo> rscImplParams = 
				ResourceExtPointMngr.getInstance().getResourceParameters( rscImplementation );
    	
    	for(  int i=0 ; i<subTypeParamNames.length ; i++ ) {
    		String subTypeParam = subTypeParamNames[i];
    		queryParams[i] = subTypeParam; //init
    		if( rscImplParams.containsKey( subTypeParam ) ) {
    			queryParams[i] = rscImplParams.get( subTypeParam).getConstraintName();
    		}
    	}
    	
		// Create an NcInventoryRequest script to get query the ncInventory for the types.
		// 		
		NcInventoryRequestMsg reqMsg = NcInventoryRequestMsg.makeQueryRequest();
		reqMsg.setInventoryName( inventoryAlias );
		reqMsg.setReqConstraintsMap( searchConstraints );
		reqMsg.setRequestedParams( queryParams );
		reqMsg.setUniqueValues( true );

		Object rslts;

		long t01 = System.currentTimeMillis();

		rslts = ThriftClient.sendRequest( reqMsg );

		if( !(rslts instanceof String[]) ) {
			out.println("Inventory Request Error: " +rslts.toString() );
			throw new VizException("Inventory Request Error: String[] response expecting instead of "+
					rslts.getClass().getName()+" : "+rslts.toString() );
		}

		long t02 = System.currentTimeMillis();

		String[] rsltsArray = (String[])rslts;

//    	out.println("Inventory Query for Rsc Sub-Types for "+ resourceDefnName+ 
//	    		" took "+ (t02-t01)+ "msecs for "+ rsltsArray.length + " results." );
//	    out.println("    RscSubTypes = "+ (rsltsArray.toString() ) );

	    for( Object queryRslt : rsltsArray ) {
	    	String rsltStr = (String)queryRslt;
	    	if( rsltStr == null ) {
	    		continue;
	    	}
	    	
	    	// the results from the NcInventory are have had spaces replaced with
	    	// underscores and are delimited with "/"'s.
	    	//
	    	String[] queryResults = rsltStr.replaceAll("_", " ").split("/");

	    	if( queryResults.length != queryParams.length ) {
	    		System.out.println("sanity check: number of requested params doesn't match returned values");
	    		continue;
	    	}
	    	
	    	if( numSubTypeGenParams == 1 ) {
	    		generatedSubTypesList.add( queryResults[0] );
	    	}
	    	// if there are 2 parameters used to generate the subType query for the second 
	    	// parameter and create a subType for each unique combination.
	    	//
	    	else if( numSubTypeGenParams == 2 ) {
	    		String subType = queryResults[0] + "_" + queryResults[1];

    			if( getPluginName().equals("mcidas") ) {
    				// if we are going to hard-code the resolution then we may as well hard-code its position in the 
    				// list as the seconde param.
    				if( subTypeParamNames[1].equals("resolution")) {  // check resolution
    					// note that the 'km' here will make the code 
	    				// to parse the subType non-generic so we might want to change it. 
	    				subType = queryResults[0] + "_"+
	    						 (queryResults[1].equals( "0" ) ? "native" : queryResults[1] + "km");   	    				    				
	    			}
	    		}
	    		
	    		
	    		if( !generatedSubTypesList.contains( subType ) ) {
	    			generatedSubTypesList.add( subType );					
	    		}
	    		else out.println("subType already in the list?"); // shouldn't happen
	    	}
	    }

        return generatedSubTypesList;
    }

    // 
    public List<String> queryInventoryParameter( ResourceName rscName, String invPrm ) throws VizException {
    	if( !getInventoryEnabled() ) {
    		throw new VizException("Inventory Not Enabled.");
    	}
    	if( !isInventoryInitialized() ) {
    		throw new VizException("Inventory Not Initialized.");
    	}

    	// Create an NcInventoryRequest script to get query the ncInventory for the types.
    	// 
//		String inventoryName = LocalizationManager.getInstance().getCurrentUser() + ":" +
//								getResourceDefnName();
    	// this will create constraints for all of the REQUEST_CONSTRAINT parameters
    	// specified in the RD, AS, and generated type/subType in the name.
    	//
    	HashMap<String, RequestConstraint> searchConstraints = 
    		getInventoryConstraintsFromParameters( 
    				ResourceDefnsMngr.getInstance().getAllResourceParameters( rscName ) );

    	NcInventoryRequestMsg reqMsg = NcInventoryRequestMsg.makeQueryRequest();
		reqMsg.setInventoryName( inventoryAlias );
		reqMsg.setRequestedParams( new String[]{invPrm} );
		reqMsg.setReqConstraintsMap( searchConstraints );
		reqMsg.setUniqueValues( true );

		Object rslts;

		long t01 = System.currentTimeMillis();

		rslts = ThriftClient.sendRequest( reqMsg );
		
		if( !(rslts instanceof String[]) ) {
			out.println("Inventory Request Error: expecting String[] return." );
			throw new VizException("Inventory Request Error: String[] response expecting instead of "+
					rslts.getClass().getName()+" : "+rslts.toString() );
		}

		long t02 = System.currentTimeMillis();

		List<String> rsltsArray = Arrays.asList( (String[])rslts );

//		out.println("Inventory Query for "+ resourceDefnName+" for "+ invPrm+ 
//    				" took "+ (t02-t01)+ "msecs for "+ rsltsArray.size() + " results." );
    		    		    		
		return rsltsArray;    	
    }

    
    public List<DataTime> getNormalizedDataTimes( ResourceName rscName, int intervalMins ) throws VizException {
    	List<DataTime> dataTimes = getDataTimes( rscName );
    	ArrayList<DataTime> normalizedDataTimes = new ArrayList<DataTime>();
    	
    	for( DataTime dt : dataTimes ) {
            long intervalMillis = intervalMins * 1000 * 60; // minutes to millisecons
            long millis = dt.getValidTime().getTimeInMillis() + (intervalMillis / 2);
            millis = ((millis / intervalMillis) * intervalMillis);
            DataTime normalizedTime = new DataTime( new Date(millis) ); 

            if( !normalizedDataTimes.contains( normalizedTime ) ) {
            	normalizedDataTimes.add( normalizedTime );
            }
    	}
    	
    	return normalizedDataTimes;
    }
    
    private void addTimesCacheEntry( ResourceName rscName ) throws VizException {
    		
    		List<ResourceName> rscNamesList = new ArrayList<ResourceName>();
    		rscNamesList.add( rscName );
//        	 if the cache is empty go ahead and initialize entries for all
//        	 possible resources.
//     		rscNamesList = ResourceDefnsMngr.getInstance().getAllSelectableResourceNamesForResourcDefn( this );

//    		for( ResourceName rName : rscNamesList ) {
		    ResourceName rName = rscName;
		    
			Map<String, RequestConstraint> resourceConstraints = 
				getInventoryConstraintsFromParameters( 
						ResourceDefnsMngr.getInstance().getAllResourceParameters( rName ) );

			// many resourceNames will have the same set of constraints so only 
			// store the unique ones.
			//
			if( !availTimesCache.containsKey( resourceConstraints ) ) {
				availTimesCache.put( resourceConstraints, 
						new DataTimesCacheEntry( resourceConstraints ) );
			}
//    		}
    		
    		// There can be more than 1 entry in the cache for each RD. Generated types/sub-types
    		// for example, or for cases where the attribute sets contain request constraint parameters 
    		// (ie synop reportTypes or mcidas imageTypes.) 
    			
    		// I pulled 4 out of my arse. Not sure what the best number is.
    		// Even though the inventory is very quick, getLatestTime() can get called 
    		// many times by the LabelProvider when showing the attrSet List and there can
    		// be a slight delay for example with all (40) radar products.
    		// 
    		// 
    		if( availTimesCache.size() == 4 ) {
    			for( DataTimesCacheEntry uriRefreshCallback : availTimesCache.values() ) {    				
    				uriRefreshCallback.addToUriCatalog( );    				
    			}
    		}
    		else if( availTimesCache.size() > 4 ) {
				availTimesCache.get( resourceConstraints ).addToUriCatalog();
    		}
    }
    
    // Return the latest time or if there either is NoData or if the time hasn't been 
    // set yet, return a Null DataTime.
    //
    public DataTime getLatestDataTime( ResourceName rscName ) throws VizException {
    	
    	if( !isRequestable() ) {
			return null;
    	}
    	
    	Map<String, RequestConstraint> resourceConstraints = 
    		getInventoryConstraintsFromParameters( 
    				ResourceDefnsMngr.getInstance().getAllResourceParameters( rscName ) );

    	// if times are cached for these constraints, and if the times haven't expired,
    	//   then just return the cached times.
    	//
    	DataTime latestTime = null;
    	
    	if( availTimesCache.containsKey( resourceConstraints ) ) {    		    		
    		latestTime = availTimesCache.get( resourceConstraints ).getLatestTime();
    		
//    		if( latestTime != null ) {
//    			if( latestTime.isNull() ) {
//    				out.println("Returning NO_DATA for "+rscName.toString() );
//    			}
//    			else {
//    				out.println("returning latestTime "+latestTime.toString() +" for "+rscName.toString()+" from cache.");
//    			}
//    		}
    	}

    	// if not found then force a query to be made to get the times/latestTime.  
    	// ( ??? Do we still want to do this if the inventory is not enabled?)
    	// 
    	if( latestTime == null  ) {
    		long t0 = System.currentTimeMillis();
    		
    		// if not querying an inventory, add to the URI catalog so 
    		// at least we can get the latest times from there.
        	if( !usesInventory() ||
        		(inventoryEnabled && !isInventoryInitialized()) ) {
        		
        	}
        	
    		getDataTimes( rscName ); // this will add the times to the cache.
    	
    		if( availTimesCache.containsKey( resourceConstraints ) ) {
    			latestTime = availTimesCache.get( resourceConstraints ).getLatestTime();
    			
//    			if( latestTime == null ) {
//    				out.println("latestTime still null even after times query??? :  "+rscName.toString() );
//    			}
//    			else {
//    				out.println("querying latestTime "+latestTime.toString() +" for "+rscName.toString()+" from cache.");
//    			}
    			
    			// if the query took a long time, then go ahead and add this to the URI Catalog.
    			//
        		if( System.currentTimeMillis()-t0 > 2000 ) {
        			availTimesCache.get( resourceConstraints ).addToUriCatalog();
        		}
    		}
    	}
    	
    	// if 'not set' still return a 'Null' dataTime.
    	return (latestTime == null ? new DataTime(new Date(0)) : latestTime);
    }
    
    // update this to optionally either return all times or only matching cycle times
    // 
    public List<DataTime> getDataTimes( ResourceName rscName ) throws VizException {

    	if( !usesInventory() ) {
			return new ArrayList<DataTime>();
    	}
    	else if( inventoryEnabled && !isInventoryInitialized() ) {
    		throw new VizException("Inventory Not Initialized.");
    	}
    	
    	Map<String, RequestConstraint> resourceConstraints = 
    		getInventoryConstraintsFromParameters( 
    				ResourceDefnsMngr.getInstance().getAllResourceParameters( rscName ) );

    	if( !availTimesCache.containsKey( resourceConstraints ) ) {
    		// if there is no entry in the cache for this resourceName, add an entry
    		// and possibly add it to the URICatalog
    		addTimesCacheEntry( rscName );
    	}
    	
    	// if times are cached for these constraints, and if the times haven't expired,
    	//   then just return the cached times.
    	//    	

    	DataTimesCacheEntry cachedTimesEntry = availTimesCache.get( resourceConstraints );

    	List<DataTime> availTimes = cachedTimesEntry.getAvailableTimes();

    	if( availTimes != null ) {
    		return availTimes;
    	}    		
    	
    	// (Do not remove the entry in the cache since this may be being refreshed 
    	// with the latestTimes from the URICatalog. 
    	
    	
    	try {
    		DataTime dataTimeArr[] = null;
    		
    		// if the inventory is enabled
    		//
    		if( inventoryEnabled ) {
    			
    	    	NcInventoryRequestMsg reqMsg = NcInventoryRequestMsg.makeQueryRequest();    			
    			reqMsg.setInventoryName( inventoryAlias );
    			reqMsg.setRequestedParams( new String[]{"dataTime"} );
    			reqMsg.setReqConstraintsMap( 
    					(HashMap<String, RequestConstraint>)resourceConstraints );
    			reqMsg.setUniqueValues( true );

    			Object rslts;

    			long t01 = System.currentTimeMillis();

    			rslts = ThriftClient.sendRequest( reqMsg );
    			
    			if( !(rslts instanceof String[]) ) {
    				out.println("Inventory Request Failed: "+rslts.toString() );
    				
    				disableInventoryUse();
    				
    				throw new VizException("Inventory Request Failed: "+rslts.toString() +
    						" Disabling inventory use." );
    			}

    			long t02 = System.currentTimeMillis();

    			String[] rsltsList = (String[]) rslts;
    			dataTimeArr = new DataTime[ rsltsList.length ];

    			for( int i=0 ; i<rsltsList.length ; i++ ) {
    				dataTimeArr[i] = ( rsltsList[i] == null ? 
    							  new DataTime(new Date(0)) : new DataTime( rsltsList[i] ) );
    			}
    		}
    		else  { // if the inventory is not enabled
    				// 
                LayerProperty property = new LayerProperty();

                property.setDesiredProduct( ResourceType.PLAN_VIEW );
                property.setEntryQueryParameters(
                		resourceConstraints, true, null);
                dataTimeArr = property.getEntryTimes();                
        	}

    		Arrays.sort( dataTimeArr );
    		
    		List<DataTime>  availTimesList = Arrays.asList( dataTimeArr );
    		
    		availTimesCache.get( resourceConstraints ).setAvailableTimes( availTimesList ); 
            
    		return availTimesList;

    	} catch (VizException e) {
//    		out.println("Inventory failed query for "+ resourceDefnName+
//    				" .... "+ e.getMessage() );
			throw new VizException( "Inventory DataTime Query Failed\n" + e.getMessage() );
    	}
    }
    
    // return a summary of the dump...
    public String dumpInventory() throws VizException {
    	if( !usesInventory() || !inventoryEnabled ) {
    		throw new VizException("Inventory not enabled.");
    	}
    	else if( !isInventoryInitialized() ) {
    		throw new VizException("Inventory Not Initialized.");
    	}
    
    	// Create an NcInventoryRequest script to get query the ncInventory for the types.
    	// 
    	NcInventoryRequestMsg reqMsg = NcInventoryRequestMsg.makeDumpRequest();
//		String inventoryName = LocalizationManager.getInstance().getCurrentUser() + ":" +
//								getResourceDefnName();
		reqMsg.setInventoryName( inventoryAlias );
		reqMsg.setRequestedParams( new String[]{"dataTime"} );
//		reqMsg.setDumpToFile( true );
//		reqMsg.setReqConstraintsMap(  );
		
		Object rslts;

		long t01 = System.currentTimeMillis();

		rslts = ThriftClient.sendRequest( reqMsg );

//		out.println("inv request returned "+rslts.getClass().getCanonicalName() );
		
		if( !(rslts instanceof String) ) {
//			out.println("Inventory Dump Error: expecting String return." );
			throw new VizException("Inventory Dump Request Error: String response expecting instead of "+
					rslts.getClass().getName() );
		}

		long t02 = System.currentTimeMillis();

		out.println("Inventory Dump for "+ resourceDefnName+ 
				" took "+ (t02-t01)+ "msecs " );
		return (String)rslts;
    }
    
    public void dispose( ) {
    	// 
    	generatedSubTypesList.clear();
    	generatedTypesList.clear();
    	
		ProductAlertObserver.removeObserver( getPluginName(), this );
		
		// clean up the availTimesCache
		// (note that if the cache is being updated with latest times from
		// the URI catalog, the DataTimesCacheEntry objects may stick around and
		// still get updated. But we can't remove them and it won't hurt anything 
		// 
		for( Map<String,RequestConstraint> rCon : availTimesCache.keySet() ) {
			availTimesCache.get(rCon).getAvailableTimes( true ).clear();
		}
		availTimesCache.clear();
		
		// TODO: do we need to remove the inventory alias? Shouldn't need to.
    }
        
    public void queryGeneratedTypes() throws VizException {
    	if( !isRequestable() ) {
    		return;
    	}
    	    	
    	generatedSubTypesList.clear();
    	generatedTypesList.clear();
    	
    	try {
    		// the parameters used to define the data that gets stored in the inventory.
    		// this will be all the request constraint params that don't have a specified value
    		// for this resource. This will be any type or sub-type generating params and any 
    		// parameters that are specified in an attribute set. (ex. radar product codes and 
    		// satellite imageTypes.)
    		//		
//    		int numSubTypeGenParams = getSubTypeGenParamsList().length;
//    		String queryParam = getSubTypeGenParamsList()[ numSubTypeGenParams-1 ];

    		HashMap<String, RequestConstraint> requestConstraints = 
    			getInventoryConstraintsFromParameters( getResourceParameters(false) );

//    		requestConstraints.put( "pluginName", new RequestConstraint( getPluginName() ) );

    		if( !getRscTypeGenerator().isEmpty() ) {

    			String genTypesRslts[] = CatalogQuery.performQuery( getRscTypeGenerator(), requestConstraints );

    			// 
    			for( String genType : genTypesRslts ) {							
    				genType = getResourceDefnName()+":"+genType;

    				if( !generatedTypesList.contains( genType ) ) {
    					generatedTypesList.add( genType );
    				}												
    			}    		
    		}
    		else if( getSubTypeGenParamsList().length > 0 ) {    		

    			LayerProperty prop = new LayerProperty();
    			prop.setDesiredProduct(ResourceType.PLAN_VIEW);
    			prop.setEntryQueryParameters(requestConstraints, false);
    			prop.setNumberOfImages(15000); 
    			
    			String script = ScriptCreator.createScript(prop);
    			if( script == null ) {
    				throw new VizException("Error creating query script???");
    			}
    			
    			Object[] pdoList;
    			pdoList = Connector.getInstance().connect( script, null, 60000 );

    			for (Object pdo : pdoList) {
    				String subType=null;

    				if( //getResourceCategory() == ResourceCategory.NtransRscCategory 
    					  getResourceCategory().getCategoryName().equals("NTRANS")) {
    					if( getRscImplementation().equals("NTRANS") ) {
    						NtransRecord ntransRec = (NtransRecord) pdo;
    						subType = ntransRec.getMetafileName() + "_" + 
    						ntransRec.getProductName();
    					}
    				}
    				else if( getResourceCategory() == ResourceCategory.SatelliteRscCategory ) {
    					if( getRscImplementation().equals("GiniSatellite") ) {
    						SatelliteRecord satRec = (SatelliteRecord) pdo;
    						subType = satRec.getSectorID();
    					}
    					else if( getRscImplementation().equals("McidasSatellite") ) {
    						McidasRecord satRec = (McidasRecord) pdo;
    						if ( satRec.getResolution() == 0 && getSubTypeGenParamsList()[1].equals("resolution") ) {
       							subType = satRec.getAreaName() + "_native";
    						} else {
    							subType = satRec.getAreaName() + "_" + 
    							satRec.getResolution().toString() + "km";
    						}
    					}
    				}
    				// ??? OBE, this shouldn't get called?
    				else if( getResourceCategory() == ResourceCategory.RadarRscCategory ) {
    					// if Mosaic there is no subType so just use 'mosaic'
    					MosaicRecord rdrRec = (MosaicRecord) pdo;
    					subType = "mosaic";
//    					attrSetKey = rdrRec.getProdName();
    					// if Radar then use the name of the radar as the subType
    				}
    				else if( getResourceCategory() == ResourceCategory.PGENRscCategory ) {
    					PgenRecord pgenRec = (PgenRecord) pdo;
    					if( subTypeGenerator.indexOf("dataTime") > -1 ) {
        					subType = pgenRec.getDataTime().toString();
    					}
    					else if( subTypeGenerator.indexOf("activitySubtype") > -1 ) {
    						subType = pgenRec.getActivitySubtype();
    					}
    					else if( subTypeGenerator.indexOf("activityLabel") > -1 ) {
    						subType = pgenRec.getActivityLabel();
    					}
    					else if( subTypeGenerator.indexOf("activityName") > -1 ) {
    						subType = pgenRec.getActivityName();
    					}
    					else if( subTypeGenerator.indexOf("activityType") > -1 ) {
    						subType = pgenRec.getActivityType();
    					}
    					else if( subTypeGenerator.indexOf("site") > -1 ) {
    						subType = pgenRec.getSite();
    					}
    					else if( subTypeGenerator.indexOf("desk") > -1 ) {
    						subType = pgenRec.getDesk();
    					}
    					else if( subTypeGenerator.indexOf("forecaster") > -1 ) {
    						subType = pgenRec.getForecaster();
    					}
    					else {
    						System.out.println("Unrecognized PGEN rsc generating subType"+
    								subTypeGenerator );
    					}
    				}

// doing this will cause the dataTime query to fail because the subType is taken as a constraint
// when doing the time querys. 
//	subType = subType.replaceAll(" ", "_");
    				if( subType != null && !subType.trim().isEmpty() ) {
    					if( !generatedSubTypesList.contains( subType ) ) {
    						generatedSubTypesList.add( subType );
    					}
//    					if( !subTypesMap.containsKey( subType ) ) {
//
//    						subTypesMap.put( subType, new ArrayList<String>() );
//    					}
//
//    					subTypesMap.get( subType ).add( attrSetKey );
    				}
    			}
    		}
		} catch (VizException e) {
			// if this is a stack trace we don't need to display the whole stack
			if( e.getMessage().length() > 200 ) {
				System.out.println( e.getMessage() );
				e = new VizException( e.getClass().getName() + " exception in queryGeneratedTypes " );
			}
			
			throw e;
		}
		catch (Exception e ) {
			throw new VizException( e );				
		}
    }
    
    // if the inventory is disabled then this will be called to update the generating types or subTypes
    // from the ingested URIs    
	@Override
	public void alertArrived( Collection<AlertMessage> alertMessages ) {
		
		HashMap<String, RequestConstraint> baseConstraints = 
			getInventoryConstraintsFromParameters( getResourceParameters(false) );

		for( AlertMessage alrtMsg : alertMessages ) {
			Map<String, Object> uriAttrValues = new HashMap<String,Object>( alrtMsg.decodedAlert );

			for( String attrName : uriAttrValues.keySet() ) {
				Object attribsObj = uriAttrValues.get( attrName );
				if( attribsObj instanceof String && 
						!attrName.equals("dataTime") ) {

					String attrStr = ((String)attribsObj); 
//					if( attrStr.indexOf(' ') != -1 ) {           		    	
//						uriAttrValues.put(attrName, attrStr.replace(' ', '_') );
//						out.println( "WARNING: URI update message for "+ alrtMsg.dataURI +
//								"\n     Attribute "+ attrName +" has a space : '"+ (String)attribsObj +"'" );
//					}
				}
			}

			// check to see if this URI matches the 'base' constraints (ie. satelliteName, modelName) for this RD.
			boolean useUri = true;

			for( String prmName : baseConstraints.keySet() ) {

				RequestConstraint rc = baseConstraints.get( prmName );

				if( !uriAttrValues.containsKey( prmName ) ) {
					out.println("??? dataUriNotification, "+alrtMsg.dataURI+
							", doesn't contain value for baseConstraint,"+ prmName );
					useUri = false;
					break;
				}
				else if( !rc.evaluate( uriAttrValues.get( prmName ) ) ) {
					useUri = false;
					break;            		
				}
			}

			if( !useUri ) {
				continue; // next URI
			}

			if( !getRscTypeGenerator().isEmpty() ) {
				if( !uriAttrValues.containsKey( getRscTypeGenerator() ) ) {
					out.println("??? dataUriNotification, "+alrtMsg.dataURI+
							", doesn't contain TypeGenerator,"+getRscTypeGenerator() );
					useUri = false;
				}
				else {
					String genType = getResourceDefnName()+":"+uriAttrValues.get( getRscTypeGenerator() );

					if( !generatedTypesList.contains( genType ) ) {
						generatedTypesList.add( genType );
					}
				}
			}

			for( String genPrm : getSubTypeGenParamsList() ) {

				if( !uriAttrValues.containsKey( genPrm ) ) {
					out.println("??? dataUriNotification, "+alrtMsg.dataURI+", doesn't contain SubTypeGenerator,"+
							genPrm );
					useUri = false;
				}
			}

			if( !useUri ) {
				continue;
			}

			String subType = null;
			
			if( getSubTypeGenParamsList().length == 1 ) {  // ie Gini Satellite
				// the gini decoder puts spaces in the sectorIds and physicalElements 
				// so replace the '_'s from the URI
				String genPrm = getSubTypeGenParamsList()[0];
				subType = uriAttrValues.get( genPrm ).toString().replaceAll("_", " ");
			}
			else if( getSubTypeGenParamsList().length == 2 ) {  // ie Mcidas Satellite
				String genPrm1 = getSubTypeGenParamsList()[0];
				String genPrm2 = getSubTypeGenParamsList()[1];
				subType = uriAttrValues.get( genPrm1 ).toString();
				if ( uriAttrValues.get( genPrm2 ).toString().equals( "0" ) && 
						getSubTypeGenParamsList()[1].equals("resolution") ) {
					subType = subType+"_native";
				}
				else {
					subType = subType+"_"+uriAttrValues.get( genPrm2 ).toString() + "km";
				}
			}

			if( subType != null &&
			   !generatedSubTypesList.contains( subType ) ) {
				
				generatedSubTypesList.add( subType );
			}
		}

	}

	@Override
	public int compareTo(ResourceDefinition o) {
		return this.getResourceDefnName().compareToIgnoreCase( o.getResourceDefnName() );
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime
				* result
				+ ((resourceDefnName == null) ? 0 : resourceDefnName.hashCode());
		result = prime
				* result
				+ ((resourceParameters == null) ? 0 : resourceParameters
						.hashCode());
		result = prime
				* result
				+ ((rscImplementation == null) ? 0 : rscImplementation
						.hashCode());
		result = prime
				* result
				+ ((rscTypeGenerator == null) ? 0 : rscTypeGenerator.hashCode());
		result = prime
				* result
				+ ((subTypeGenerator == null) ? 0 : subTypeGenerator.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		ResourceDefinition other = (ResourceDefinition) obj;
		if (applicableDisplayTypes == null) {
			if (other.applicableDisplayTypes != null)
				return false;
		} else if (!applicableDisplayTypes.equals(other.applicableDisplayTypes))
			return false;
		if (attrSetOrg != other.attrSetOrg)
			return false;
		if (dfltFrameCount != other.dfltFrameCount)
			return false;
		if (dfltTimeRange != other.dfltTimeRange)
			return false;
		if (frameSpan != other.frameSpan)
			return false;
		if (inventoryEnabled == null) {
			if (other.inventoryEnabled != null)
				return false;
		} else if (!inventoryEnabled.equals(other.inventoryEnabled))
			return false;
		if (resourceCategory.getCategoryName() == null) {
			if (other.resourceCategory.getCategoryName() != null)
				return false;
		} else if (!resourceCategory.getCategoryName().equals(other.resourceCategory.getCategoryName()))
			return false;
		if (resourceDefnName == null) {
			if (other.resourceDefnName != null)
				return false;
		} else if (!resourceDefnName.equals(other.resourceDefnName))
			return false;
		if (resourceParameters == null) {
			if (other.resourceParameters != null)
				return false;
		} else if (!resourceParameters.equals(other.resourceParameters))
			return false;
		if (rscImplementation == null) {
			if (other.rscImplementation != null)
				return false;
		} else if (!rscImplementation.equals(other.rscImplementation))
			return false;
		if (rscTypeGenerator == null) {
			if (other.rscTypeGenerator != null)
				return false;
		} else if (!rscTypeGenerator.equals(other.rscTypeGenerator))
			return false;
		if (subTypeGenerator == null) {
			if (other.subTypeGenerator != null)
				return false;
		} else if (!subTypeGenerator.equals(other.subTypeGenerator))
			return false;
		if (timeMatchMethod != other.timeMatchMethod)
			return false;
		if (timelineGenMethod != other.timelineGenMethod)
			return false;
		return true;
	}
}