package gov.noaa.nws.ncep.viz.resources.manager;

import gov.noaa.nws.ncep.viz.common.StringListAdapter;
import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData.TimeMatchMethod;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData.TimelineGenMethod;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.serialization.ISerializableObject;

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
 * 
 * </pre>
 * 
 * @author
 * @version 1
 */
@XmlRootElement(name = "ResourceDefinition")
@XmlAccessorType(XmlAccessType.NONE)
public class ResourceDefinition implements ISerializableObject {

    // the index of this resourceDefinition in the resourceDefinitions.xml
    // file/table
    private int definitionIndex;

    // Currently not implemented but this would allow a resource to be defined
    // but not visible on the resource Selection Dialog
    @XmlElement
    private boolean isEnabled;

    @XmlElement
    private String resourceDefnName;

    @XmlElement
    private String resourceCategory;

    // private List<ResourceCategory> rscCategories;

    //
    @XmlElement
    @XmlJavaTypeAdapter(StringListAdapter.class)
    private ArrayList<String> filterLabels;
    
    private String           localizationName; // the path
    private LocalizationFile localizationFile;

	@XmlElement
    private String rscImplementation;

    // the name of a column in the DB used to generated dynamic Resources.
    //
    @XmlElement
    private String subTypeGenerator;

    // in the gui this is edited in the rscType text widget but it is stored
    // here
    @XmlElement
    private String rscTypeGenerator;

    private ArrayList<String> generatedTypesList; // set from DB if
                                                  // null
//    @XmlElement
//    @XmlJavaTypeAdapter(StringListAdapter.class)
//    private ArrayList<String> attrSetGroupNames;

    private HashMap<String, ArrayList<String>> subTypesMap;

    @XmlElement
    private TimeMatchMethod timeMatchMethod;

    // This can be
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

    @XmlElement
    private String dfltGeogArea;

    // TODO : remove this since it should no longer be needed.
//    @XmlElement
//    private BinOffset binOffset;

    // this does not include any parameters associated with subtypes or any in the
    // attributes file.
    // NOTE : Comments are kept in this map so that they can be preserved when the user
    // edits the params in the gui. This are not returned by getResourceParameters
    // but it would be nice to have a cleaner way to store the comments for a parameter.
    @XmlElement
    @XmlJavaTypeAdapter(RscParamsJaxBAdapter.class)
    private HashMap<String,String> resourceParameters;
    
    //private ArrayList<RscParameter> rscTypeParameters;

    private boolean resourceParametersModified;

    // TODO : since the prms HashMap will not be ordered, we may want to implement
    // these another way so the user can see the comments/help/description text
    // when editing them.    
//    static public class RscParameter {
//    	public String name;
//    	public String value;
//    	public String description;    	
//    }

    
    public ResourceDefinition() {
        isEnabled = true;
        resourceDefnName = "";
        resourceCategory = ""; // new ArrayList<String>();
        filterLabels = new ArrayList<String>();
        subTypesMap = new HashMap<String, ArrayList<String>>();
        subTypeGenerator = "";
        rscTypeGenerator = "";
        generatedTypesList = new ArrayList<String>();
        resourceParameters = new HashMap<String,String>();
        resourceParametersModified = false;
        frameSpan = 0;
        dfltFrameCount = DEFAULT_FRAME_COUNT;
        dfltTimeRange = DEFAULT_TIME_RANGE;
        dfltGeogArea = NmapCommon.getDefaultMap();
        timeMatchMethod = TimeMatchMethod.CLOSEST_BEFORE_OR_AFTER;
        timelineGenMethod = timelineGenMethod.USE_DATA_TIMES;
    }

    // shallow copy of the attrSetGroups and subTypes lists
    public ResourceDefinition(ResourceDefinition rscDefn) {
        resourceDefnName = rscDefn.getResourceDefnName();
        resourceCategory = rscDefn.resourceCategory;

        isEnabled = rscDefn.isEnabled;
        // rscSubTypes = new ArrayList<String>( rscDefn.getRscSubTypes() );
        filterLabels = new ArrayList<String>(rscDefn.getFilterLabels());

        // deep copy the subTypesMap.
        // (This will probably be removed after creation since the new defition
        // will usually have its own subtypes.)
        subTypesMap = new HashMap<String, ArrayList<String>>();
        for (String subType : rscDefn.subTypesMap.keySet()) {
            subTypesMap.put(subType, new ArrayList<String>());

            for (String asName : rscDefn.subTypesMap.get(subType)) {
                subTypesMap.get(subType).add(asName);
            }
        }

        subTypeGenerator = rscDefn.getSubTypeGenerator();
        rscTypeGenerator = rscDefn.getRscTypeGenerator();

        generatedTypesList = new ArrayList<String>(
                rscDefn.getGeneratedTypesList());

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

    public String[] getRscSubTypes() {
        return subTypesMap.keySet().toArray(new String[0]);
    }

    public String getSubTypeGenerator() {
        return subTypeGenerator;
    }

    public void setSubTypeGenerator(String subTypeGenerator) {
        this.subTypeGenerator = subTypeGenerator;
    }

    public String getRscTypeGenerator() {
        return rscTypeGenerator;
    }

    public void setRscTypeGenerator(String rscTypeGenerator) {
        this.rscTypeGenerator = rscTypeGenerator;
    }

    public ArrayList<String> getGeneratedTypesList() {
        return generatedTypesList;
    }

    public void setGeneratedTypesList(ArrayList<String> genTypes) {
        this.generatedTypesList = genTypes;
    }

    public HashMap<String, ArrayList<String>> getSubTypesMap() {
        return subTypesMap;
    }

    public void setSubTypesMap(HashMap<String, ArrayList<String>> stm) {
        this.subTypesMap = stm;
    }

    public String getResourceDefnName() {
        return resourceDefnName;
    }

    public int getDefinitionIndex() {
        return definitionIndex;
    }

    public void setDefinitionIndex(int definitionIndex) {
        this.definitionIndex = definitionIndex;
    }

    public void addResourceCategory(String cat) {
        resourceCategory = cat;
        // if( !rscCategories.contains(cat) ) {
        // rscCategories.add( cat );
        // }
    }

    // resourceParameters includes the comments but don't return them.
    //
    public HashMap<String,String> getResourceParameters() {
    	HashMap<String,String> prmsWithoutComments = new HashMap<String,String>();
		for( String prmName : resourceParameters.keySet() ) {
			if( !prmName.trim().startsWith("!") ) {
				prmsWithoutComments.put( prmName, resourceParameters.get(prmName) );		
			}
		}
    	return prmsWithoutComments;
    }
    
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

    public void setResourceImpl(String ri) { // Class<?> rscCls ) {
        rscImplementation = ri;
    }

    public boolean getIsEnabled() {
        return isEnabled;
    }

    public void setEnabled(boolean isEnabled) {
        this.isEnabled = isEnabled;
    }

    public boolean isForecast() {
        return filterLabels.contains("Forecast");
    }

    public void setForecast(boolean forecast) {
        if (forecast && !isForecast()) {
            filterLabels.add("Forecast");
        } else if (!forecast && isForecast()) {
            filterLabels.remove("Forecast");
        }
    }

    public boolean isEventResource() {
        return filterLabels.contains("Event");
    }

    public void setEventResource(boolean e) {
        if (e && !isEventResource()) {
            filterLabels.add("Event");
        } else if (!e && isEventResource()) {
            filterLabels.remove("Event");
        }
    }

    // public List<String> getRscCategories() {
    public String getResourceCategory() {
        return resourceCategory;
    }

    public void setResourceCategory(String rCat) {
        this.resourceCategory = rCat;
    }

    public ArrayList<String> getFilterLabels() {
        return filterLabels;
    }

    public void setFilterLabels(ArrayList<String> filterLabels) {
        this.filterLabels = filterLabels;
    }

    public void addFilterLabel(String filterLabel) {
        if (!filterLabels.contains(filterLabel)) {
            filterLabels.add(filterLabel);
        }
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
        return resourceCategory.equals(ResourceName.PGENRscCategory);
    }

    public boolean applyAttrSetGroups() {
        return resourceCategory.equals(ResourceName.GridRscCategory)
                || resourceCategory.equals(ResourceName.RadarRscCategory)
                || resourceCategory.equals(ResourceName.PGENRscCategory)
                || resourceCategory.equals(ResourceName.EnsembleRscCategory);
    }
}