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

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.time.BinOffset;

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

    // @XmlElement
    // private boolean isForecast;

    // @XmlElement
    // private boolean isEventResource;

    @XmlElement
    private String resourceDefnName;

    @XmlElement
    private String resourceCategory;

    // private List<ResourceCategory> rscCategories;

    //
    @XmlElement
    @XmlJavaTypeAdapter(StringListAdapter.class)
    private ArrayList<String> filterLabels;

    // location of the .prm file and attribute set files
    @XmlElement
    private String configDir;

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
                                                  // rscTypeGenerator is not
                                                  // null

    @XmlElement
    @XmlJavaTypeAdapter(StringListAdapter.class)
    private ArrayList<String> attrSetGroupNames;

    private HashMap<String, File> availAttrSets;

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

    // if null then don't bin the data.
    @XmlElement
    private BinOffset binOffset;

    // this is read from the .prm file in the config dir and it
    // does not include any parameters associated with subtypes or any in the
    // attributes file.
    private String rscTypeParameters;

    private boolean rscTypeParamsModified;

    public ResourceDefinition() {
        isEnabled = true;
        resourceDefnName = "";
        resourceCategory = ""; // new ArrayList<String>();
        filterLabels = new ArrayList<String>();
        attrSetGroupNames = new ArrayList<String>();
        // attrSetGroups = new HashMap<String,AttrSetGroup>();
        availAttrSets = new HashMap<String, File>();
        // rscSubTypes = new ArrayList<String>();
        subTypesMap = new HashMap<String, ArrayList<String>>();
        subTypeGenerator = "";
        rscTypeGenerator = "";
        generatedTypesList = new ArrayList<String>();
        rscTypeParameters = "";
        rscTypeParamsModified = false;
        frameSpan = 0;
        dfltFrameCount = DEFAULT_FRAME_COUNT;
        dfltTimeRange = DEFAULT_TIME_RANGE;
        dfltGeogArea = NmapCommon.getDefaultMap();
        binOffset = null;
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

        availAttrSets = new HashMap<String, File>(rscDefn.availAttrSets);

        subTypeGenerator = rscDefn.getSubTypeGenerator();
        rscTypeGenerator = rscDefn.getRscTypeGenerator();

        generatedTypesList = new ArrayList<String>(
                rscDefn.getGeneratedTypesList());

        attrSetGroupNames = new ArrayList<String>(
                rscDefn.getAttrSetGroupNames());
        rscTypeParameters = new String(rscDefn.rscTypeParameters);
        rscTypeParamsModified = rscDefn.rscTypeParamsModified;

        frameSpan = rscDefn.frameSpan;
        dfltFrameCount = rscDefn.dfltFrameCount;
        dfltTimeRange = rscDefn.dfltTimeRange;

        binOffset = rscDefn.binOffset;
        timeMatchMethod = rscDefn.timeMatchMethod;
        timelineGenMethod = rscDefn.timelineGenMethod;

        dfltGeogArea = rscDefn.dfltGeogArea;
        rscImplementation = rscDefn.rscImplementation;

        configDir = rscDefn.configDir;
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

    public void addAttrSetFile(String asName, File asFile) {
        availAttrSets.put(asName, asFile);
    }

    public void removeAllAttrSets() {
        availAttrSets.clear();
    }

    public Set<String> getAvailAttrSets() {
        return availAttrSets.keySet();
    }

    public File getAttrSetFile(String asName) {
        if (availAttrSets.containsKey(asName)) {
            return availAttrSets.get(asName);
        } else {
            return null;
        }
    }

    public HashMap<String, ArrayList<String>> getSubTypesMap() {
        return subTypesMap;
    }

    public void setSubTypesMap(HashMap<String, ArrayList<String>> stm) {
        this.subTypesMap = stm;
    }

    public ArrayList<String> getAttrSetGroupNames() {
        return attrSetGroupNames;
    }

    public void setAttrSetGroupNames(ArrayList<String> attrSetGroupNames) {
        this.attrSetGroupNames = attrSetGroupNames;
    }

    public void addAttrSetGroupName(String asgName) {
        if (!attrSetGroupNames.contains(asgName)) {
            attrSetGroupNames.add(asgName);
        }
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

    public String getRscTypeParameters() {
        return rscTypeParameters;
    }

    public void setRscTypeParameters(String typeParams) {
        if (rscTypeParameters.isEmpty()
                || !rscTypeParameters.equals(typeParams)) {
            rscTypeParamsModified = true;
        }

        rscTypeParameters = typeParams;
    }

    public boolean getRscTypeParamsModified() {
        return rscTypeParamsModified;
    }

    public void setRscTypeParamsModified(boolean rscTypeParamsModified) {
        this.rscTypeParamsModified = rscTypeParamsModified;
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

    //
    public boolean removeAttrSetGroup(String asgName) {

        if (attrSetGroupNames.contains(asgName)) {
            attrSetGroupNames.remove(asgName);
            // attrSetGroups.remove( asgName );
            return true;
        } else { // not in the map?
            return false;
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

    public BinOffset getBinOffset() {
        return binOffset;
    }

    public void setBinOffset(BinOffset binOffset) {
        this.binOffset = binOffset;
    }

    public void setResourceDefnName(String rName) {
        this.resourceDefnName = rName;
    }

    public String getConfigDir() {
        return configDir;
    }

    public void setConfigDir(String dDir) {
        configDir = dDir;
    }

    public boolean isPgenResource() {
        return resourceCategory.equals(ResourceName.PGENRscCategory);
    }

    public boolean applyAttrSetGroups() {
        return resourceCategory.equals(ResourceName.GridRscCategory)
                || resourceCategory.equals(ResourceName.RadarRscCategory)
                || resourceCategory.equals(ResourceName.PGENRscCategory)
                || resourceCategory.equals(ResourceName.EnsembleRscCategory);
        // applyAttrSetGroups;
        // return rscCategories.contains( ResourceName.GridRscCategory) ||
        // rscCategories.contains( ResourceName.SatelliteRscCategory);//
        // applyAttrSetGroups;
    }
}