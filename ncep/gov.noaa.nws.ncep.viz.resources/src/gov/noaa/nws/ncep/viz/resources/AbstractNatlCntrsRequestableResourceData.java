package gov.noaa.nws.ncep.viz.resources;

import gov.noaa.nws.ncep.viz.common.RGBColorAdapter;
import gov.noaa.nws.ncep.viz.common.display.NcDisplayType;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceAttrSet;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceAttrSet.RscAttrValue;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceExtPointMngr;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceExtPointMngr.ResourceParamInfo;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceExtPointMngr.ResourceParamType;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceCategory;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceDefinition;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceDefnsMngr;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceName;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceName.ResourceNameAdapter;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

//import gov.noaa.nws.ncep.viz.resources.manager.ResourceDefinition.InventoryLoadStrategy;

/**
 * This is the abstract class for all Natl Cntrs requestable resources. It is
 * very similar to the AbstractNatlCntrsResourceData class with the only real
 * difference being that it extends AbstractRequestableResourceData instead of
 * AbstractResourceData. The main purpose of this class is to manage the
 * resource's attributes which are stored in a named ResourceAttrSet (.attr
 * files) The values in the attrSet can be loaded to and from the ResourceData.
 * The edit Attributes dialog uses this to get the attribute values from the
 * resource. When an RBD is being created the attribute values are stored in the
 * .prm file and in a ResourceData and both are written out to the RBD file
 * along with a flag indicating whether the attribute values were edited from
 * the original named attribute set. When the RBD is loaded the resource uses
 * this flag to use either the values in the .prm file at load time (which may
 * have changed from when the RBD was created) or from the unmarshalled
 * resource.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * July 20, 2009           mgao        Initial creation
 * Aug 03, 2009            ghull       rm 'Attr' getter methods
 * Aug 06, 2009            ghull       construct() -> constructResource()
 * Aug 18, 2009    147     ghull       add getNormalizedTime()
 * Oct 20, 2009    145     ghull       don't set frameTimes from dominant rsc in construct.
 * Mar 10, 2010    257     ghull       add BEFORE_OR_EQUAL timeMatchMethod for lightning
 * Apr 5, 2010     259     ghull       add legendColor
 * Aug 18, 2010    273     ghull       add dfltNumFrames
 * Sep 01, 2010    307     ghull       add isAutoUpdatable
 * Sep 16, 2010    307     ghull       generate forecast timelines 
 * Mar 03, 2011    408     ghull       frameInterval -> frameSpan
 * Nov 15, 2011            ghull       add resolveLatestCycleTime, 
 * Nov 29, 2011    518     ghull       add dfltFrameTimes
 * Feb 06, 2012    606     ghull       getDataTimes from Inventory if it exist
 * Feb 07, 2013    #972    ghull       ResourceCategory class. Supported NcDisplayType
 * Apr 15, 2013    #864    ghull       add USE_CYCLE_TIME_FCST_HOURS and Event timeMatchMthd
 * 04/10/2013      #958    qzhou       Added autoupdate for solar in isAutoUpdateable
 * 07/15/2013      #1011   ghull       add MATCH_ALL_DATA timeMatchMethod for PgenResource
 * 06/16/2014      TTR1026 jwu         sort data time for local radar in getAvailableDataTimes()
 * 
 * </pre>
 * 
 * *
 * 
 * @author ghull
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
public abstract class AbstractNatlCntrsRequestableResourceData extends
        AbstractRequestableResourceData implements INatlCntrsResourceData,
        ISerializableObject {

    @XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB legendColor;

    // if true then the attribute values are stored in the member variables, if
    // false then
    // the attribute values are stored in the rscAttrSet.
    @XmlAttribute
    protected boolean isEdited = false;

    protected ResourceExtPointMngr rscExtPointMngr = null;

    // the full name/path of the resource with its category, sub-category...
    @XmlAttribute
    @XmlJavaTypeAdapter(ResourceNameAdapter.class)
    protected ResourceName resourceName = null;

    @XmlElement
    protected String resourceVersion = null;

    // This is used to set the start/end times for each frame in the
    // frameDataMap. All
    // this resources data will be within this interval. The value is
    // initialized with
    // the value in the ResourceDefinition. A value of 0 is not valid except in
    // the case
    // of 'Event' type resources where the data have valid ranges. (ex. Airmets,
    // Warn...)
    // This value may be used to generate a timeline if the timelineGenMethod is
    // set to
    // USE_FRAME_INTERVAL.
    //
    @XmlElement
    protected int frameSpan; // in minutes

    @XmlElement
    protected TimelineGenMethod timelineGenMethod;

    @XmlElement
    protected TimeMatchMethod timeMatchMethod;

    // the following are set from the ResourceDefn and are used to initialize
    // the timeline when this resource is the dominant
    @XmlElement
    protected int dfltNumFrames;

    @XmlElement
    protected int dfltTimeRange; // in hours

    @XmlElement
    protected String dfltFrameTimes; // GEMPAKs GDATTIME

    protected AbstractVizResource<?, ?> ncRsc;

    //
    public static enum TimeMatchMethod {
        EXACT, BEFORE_OR_EQUAL, CLOSEST_BEFORE_OR_EQUAL, CLOSEST_AFTER_OR_EQUAL, CLOSEST_BEFORE_OR_AFTER,
        // Used by PGEN resource but it actually chooses the latest data
        // available
        MATCH_ALL_DATA,
        // This was created when the "Event" filter was removed. This now
        // is an indication of 'Event'-based resources and requires that the
        // TimelineGenMethod be set to MANUAL. Currently the behaviour of
        // an EVENT TimeMatchMethod is the same as EXACT in that it sets a
        // frames start/end time to the frame time. (ie a frame span of 0.)
        //
        EVENT
    }

    public static enum TimelineGenMethod {
        USE_FRAME_INTERVAL, // use a frame interval (dflt to frame span) to
                            // generate frame times
        USE_DATA_TIMES, // use the data times from the dominant source
        USE_CYCLE_TIME_FCST_HOURS, //
        USE_FCST_FRAME_INTERVAL_FROM_REF_TIME, // For Taf.
        // USE_FCST_FRAME_INTERVAL_FROM_CYCLE_TIME, // currently no
        // implementations of this
        USE_MANUAL_TIMELINE
        // DETERMINE_FROM_RSC_IMPLEMENTATION,
    }

    public AbstractNatlCntrsRequestableResourceData() {
        super();
        isEdited = false;
        resourceVersion = "";
        ncRsc = null;
        timeMatchMethod = TimeMatchMethod.CLOSEST_BEFORE_OR_AFTER;
        timelineGenMethod = TimelineGenMethod.USE_DATA_TIMES;
        frameSpan = 60;
        legendColor = new RGB(255, 255, 255);
        rscExtPointMngr = ResourceExtPointMngr.getInstance();
    }

    // Version can be used to test whether an RBD was created with an older
    // version of the resource
    // Currently this is not enforced or implemented by any of the resources.
    public String getResourceVersion() {
        return resourceVersion;
    }

    public void setResourceVersion(String resourceVersion) {
        this.resourceVersion = resourceVersion;
    }

    // TODO : A better way to do this would be to get the ResourceDefinition and
    // check if the resource Implementation is a satellite or radar.
    //
    public boolean isAutoUpdateable() {
        ResourceCategory rscCat = getResourceName().getRscCategory();
        if (rscCat != null && rscCat == ResourceCategory.SatelliteRscCategory
                || rscCat == ResourceCategory.RadarRscCategory
                || rscCat == ResourceCategory.SpaceRscCategory) {
            return true;
        }
        return false;
    }

    // Event-type resources like sigmets, WARN, ffa.... should override this
    // method
    // and return true.
    public boolean isEventResource() {
        return false;
    }

    // could base this off the EventResource or the frameSpan
    public boolean isAvailAsDominantResource() {
        return (frameSpan != 0);
    }

    // get/set for isEdited
    public void setIsEdited(boolean e) {
        isEdited = e;
    }

    public boolean getIsEdited() {
        return isEdited;
    }

    public final ResourceName getResourceName() {
        return resourceName;
    }

    public void setResourceName(ResourceName rscName) {
        resourceName = new ResourceName(rscName);
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

    public int getDfltNumFrames() {
        return dfltNumFrames;
    }

    public void setDfltNumFrames(int dfltNumFrames) {
        this.dfltNumFrames = dfltNumFrames;
    }

    public int getDfltTimeRange() {
        return dfltTimeRange;
    }

    public void setDfltTimeRange(int dfltTimeRange) {
        this.dfltTimeRange = dfltTimeRange;
    }

    public String getDfltFrameTimes() {
        return dfltFrameTimes;
    }

    public void setDfltFrameTimes(String dfltFrameTimes) {
        this.dfltFrameTimes = dfltFrameTimes;
    }

    // some rsc param values may change from EQUALS to a wildcard (ie %) so
    // make sure that the correct constraint type is set.
    @Override
    public HashMap<String, RequestConstraint> getMetadataMap() {
        HashMap<String, RequestConstraint> mm = new HashMap<String, RequestConstraint>(
                metadataMap);

        for (String rcName : metadataMap.keySet()) {
            RequestConstraint rc = metadataMap.get(rcName);
            if (rc.getConstraintValue().trim().equals("%")
                    && rc.getConstraintType() != ConstraintType.LIKE) {
                if (rcName.equals("dataTime")) {
                    mm.remove(rcName);
                } else {
                    mm.put(rcName, RequestConstraint.WILDCARD);
                }
            }
        }

        return mm;
    }

    public String getPluginName() {
        if (getMetadataMap().containsKey("pluginName")) {
            return getMetadataMap().get("pluginName").getConstraintValue();
        } else {
            return "";
        }
    }

    // if there is a cycle time then this is a forecast resource.
    //
    // TODO : should we allow for a resource to be a forecast resource w/o
    // having a cycle time?
    //
    public boolean isForecastResource() {
        return resourceName.isForecastResource();
    }

    public void setPluginName(String pluginName) {
        if (!getMetadataMap().containsKey("pluginName")) {
            getMetadataMap().put("pluginName", new RequestConstraint());
        }

        getMetadataMap().get("pluginName").setConstraintValue(pluginName);
    }

    // implement here as a convienience since almost all of our
    // resources are on map based displays. Other resources
    // that are written to draw to differendt display types will
    // need to override this.
    public NcDisplayType[] getSupportedDisplayTypes() {
        return new NcDisplayType[] { NcDisplayType.NMAP_DISPLAY };
    }

    // in D2D the construct method will call setTimeList which will set the
    // TimeMatchingMap in the descriptor.
    // but for us the non-dominant resources will not know there frameTimes
    // until the dominant source is constructed since this
    // is where the frameTimes are defined for all resources. So... we either
    // need to ensure that the dominant resource is constructed
    // first and then update the TimeMatchingMap here or (preferrably) we will
    // need to postpone setting the TimeMatchingMap until all
    // the resources are constructed and then set the TimeMatchingMap. We will
    // do this in the init method.
    //
    @Override
    public AbstractVizResource<?, ?> construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        // Bypass the AbstractRequestableResourceData construct() since it does
        // stuff we don't need/want. Like setting the frameTimes from the
        // resource data times.
        // AbstractVizResource<?, ?> rsc = super.construct( loadProperties,
        // descriptor );
        AbstractVizResource<?, ?> rsc = constructResource(loadProperties, null);
        // store off the resource. Currently this is done only to be able to
        // update the color
        // capability when the color attribute is changed.
        if (rsc instanceof INatlCntrsResource) {
            // The current design assumes that each ResourceData will only
            // create one Resource. If this
            // needs to change then we will either need to store a list of
            // ncRscs or create a new
            // AbstractNatlCntrsResource class and put the color update code in
            // it.
            if (ncRsc != null) {
                System.out
                        .println("Sanity Check: ncRsc != null. A ResourceData is attempting to construct ");
                System.out.println(" a resource that already exists. ");
            }
            ncRsc = rsc;
        } else {
            System.out
                    .println("A NatlCntrsResourceData is constructing a non-NatlCntrs Resource???");
        }

        // if this resource was not edited (ie attribute values changed from the
        // original values in
        // the rscAttrSet) then get the values from the rscAttrSet and set the
        // member variables with them.
        // In other words, if the attribute values in the rscAttrSet were
        // changed from the time when the
        // RBD was created to now we will use the current values.) (This
        // behaviour could be change to
        // only apply to the default attrSet if we wanted to.)
        //
        // if( rscAttrSet == null && rscAttrSetName != null ) {
        // rscAttrSet = new ResourceAttrSet( rscAttrSetName );
        // }
        //
        // if( rscAttrSet != null ) {
        // if( isEdited ) {
        // // if the attributes were edited then the values in rscAttrSet are
        // different so update them
        // // here. (Note: currently this is not required. We could instead just
        // not create an attrSet.)
        // // Or we might want to change the name to prevent inadvertant writing
        // out of edited attributes to
        // // the file.)
        // getResourceAttrValues( rscAttrSet );
        // }
        //
        // // call setRscAttrSet instead of setResourceAttrValues since
        // // setRscAttrSet may be overridden
        // setRscAttrSet( rscAttrSet );
        // }

        return rsc;
    }

    // There are better/faster ways of doing this I'm sure, but for now
    // just call getAvailableDataTimes to do this.
    //
    public void resolveLatestCycleTime() {
        if (getResourceName().getCycleTime() == null
                && getResourceName().isLatestCycleTime()) {
            getAvailableDataTimes();
        }
    }

    // return a list of all of the data times from the database. If
    // this is a forecast resource with a cycle time, this will only
    // return a list of unique cycle times.
    public List<DataTime> getAvailableDataTimes() {
        List<DataTime> availTimesList = null;

        try {
            ResourceDefinition rscDefn = ResourceDefnsMngr.getInstance()
                    .getResourceDefinition(getResourceName());

            // first attempt to get the times from the inventory and
            // if there is no
            // if( !rscDefn.hasInventory() &&
            // rscDefn.getInventoryLoadStrategy() !=
            // InventoryLoadStrategy.NO_INVENTORY ) {
            //
            // rscDefn.loadInventory( false );
            // }

            if (rscDefn.getInventoryEnabled()
                    && rscDefn.isInventoryInitialized()) {
                availTimesList = rscDefn.getDataTimes(getResourceName());
            } else {
                try {
                    DataTime[] availTimes = null;
                    availTimes = getAvailableTimes();
                    if (availTimes == null) {
                        return new ArrayList<DataTime>();
                    }
                    availTimesList = Arrays.asList(availTimes);

                } catch (VizException e) {
                    System.out.println("Error getting Available Times: "
                            + e.getMessage());
                    return null;
                }
            }
        } catch (VizException e1) {
            return availTimesList;
        }

        // if there is a cycle time, filter out other times and sort by the
        // forecast hours
        // TODO: don't get the cycle time from the resourceName....
        //
        if (getResourceName().getCycleTime() != null) {
            // DataTime cycleTime = getResourceName().getCycleTime();
            long cycleTimeMs = 0;

            // if latest then get the latest time
            // and set the resolved time in the ResourceName
            //
            if (getResourceName().isLatestCycleTime()) {
                for (DataTime dt : availTimesList) {
                    if (dt.getRefTime().getTime() > cycleTimeMs) {

                        cycleTimeMs = dt.getRefTime().getTime();

                        getResourceName().setCycleTime(dt);
                    }
                }
            } else {
                cycleTimeMs = getResourceName().getCycleTime().getRefTime()
                        .getTime();
            }
            ArrayList<DataTime> tmpTimesList = new ArrayList<DataTime>();

            // add all the forecast times for the given cycleTime.
            // (TODO: confirm that duplicate valid times (with different periods
            // are not getting added here.)
            for (DataTime dt : availTimesList) {
                if (dt.getRefTime().getTime() == cycleTimeMs) {
                    // create a DataTime without a period which may lead to
                    // duplicate times.
                    DataTime dataTime = new DataTime(dt.getRefTime(),
                            dt.getFcstTime());
                    if (!tmpTimesList.contains(dataTime)) {
                        tmpTimesList.add(dataTime);
                    }
                }
            }
            availTimesList = tmpTimesList;
        }

        /*
         * TTR 1026: Local radar data times are not sorted so we need to sort
         * them in ascending
         */
        // order.
        String rscName = getResourceName().toString();
        if (rscName != null && rscName.contains("RADAR/LocalRadar")) {
            Collections.sort(availTimesList);
        }

        return availTimesList;
    }

    // get a list of the defined attributes for this resource and
    //
    public ResourceAttrSet getRscAttrSet() {

        // if( rscAttrSet == null && rscAttrSetName != null ) {

        HashMap<String, ResourceParamInfo> rscImplParamInfo = rscExtPointMngr
                .getParameterInfoForRscImplementation(getResourceName());

        if (rscImplParamInfo == null) {
            return null;
        }

        ResourceAttrSet rscAttrSet = new ResourceAttrSet(
                resourceName.getRscAttrSetName()); // rscAttrSetName );

        for (ResourceParamInfo prmInfo : rscImplParamInfo.values()) {
            if (prmInfo.getParamType() != ResourceParamType.EDITABLE_ATTRIBUTE) {
                continue;
            }

            Method[] mthds = this.getClass().getDeclaredMethods();
            String paramName = prmInfo.getAttributeName();

            String getMthdName = "get"
                    + paramName.substring(0, 1).toUpperCase()
                    + paramName.substring(1);

            for (Method m : mthds) {
                if (m.getName().equals(getMthdName)) {
                    Class<?>[] params = m.getParameterTypes();
                    Class<?> rtype = m.getReturnType();

                    // This would be a nice sanity check but I would have to go
                    // back and change all ints and booleans
                    // in the getters and setters for old resources even though
                    // they are compatible with the defined classes
                    // if( rtype != attrInfo.getAttrClass() ) {
                    // System.out.println("Warning: Attribute "+attrName
                    // +" is not defined\n"+
                    // "as correct type:" +rtype.getName() + " != " +
                    // attrInfo.getAttrClass().getName() );
                    // }

                    if (params.length == 0) {
                        Object attrVal = null;
                        try {
                            attrVal = m.invoke(this);

                            Constructor<?> cc = rtype.getConstructor(rtype);
                            if (cc != null) {
                                attrVal = cc.newInstance(attrVal);
                            }

                            rscAttrSet.setAttrValue(paramName, attrVal);

                        } catch (NoSuchMethodException e) {
                            // if there is no copy constructor go ahead and set
                            // the attribute value
                            rscAttrSet.setAttrValue(paramName, attrVal);

                        } catch (IllegalAccessException iae) {
                            System.out.println(iae.getMessage());
                        } catch (IllegalArgumentException iae) {
                            System.out.println(iae.getMessage());
                        } catch (InvocationTargetException ite) {
                            System.out.println(ite.getMessage());
                        } catch (ClassCastException cce) {
                            System.out.println(cce.getMessage());
                        } catch (SecurityException e) {
                            System.out.println(e.getMessage());
                        } catch (InstantiationException e) {
                            System.out.println(e.getMessage());
                        }
                    }
                }
            }
        }
        // }

        return rscAttrSet;
    }

    // the rscAttrSet should only contain attributes defined for this resource.
    //
    public boolean setRscAttrSet(ResourceAttrSet newRscAttrSet) {
        if (newRscAttrSet == null) {
            return false;
        }
        // else if( rscAttrSet == null ) {
        // rscAttrSet = new ResourceAttrSet( rscAttrSet.getRscAttrSetName() );
        // }

        // if( rscAttrSetName == null ||
        // !rscAttrSetName.equals( rscAttrSet.getRscAttrSetName() ) ) {
        // rscAttrSetName = new String( rscAttrSet.getRscAttrSetName() );
        // }

        // Set<String> attrNames = rscAttrSet.getAttrNames();
        HashMap<String, ResourceParamInfo> rscImplParamInfo = rscExtPointMngr
                .getParameterInfoForRscImplementation(getResourceName());

        if (rscImplParamInfo == null) {
            System.out.println("Couldn't find rsc impl parameter info for "
                    + getResourceName());
            return false;
        }

        // loop thru the attributes and use Java Bean utils to set the
        // attributes on the resource
        for (ResourceParamInfo prmInfo : rscImplParamInfo.values()) {

            if (prmInfo.getParamType() != ResourceParamType.EDITABLE_ATTRIBUTE) {
                continue;
            }

            String attrName = prmInfo.getAttributeName();

            // make sure that this attrSet has this attributeName
            if (!newRscAttrSet.hasAttrName(attrName)) {
                continue;
            }

            RscAttrValue rscAttr = newRscAttrSet.getRscAttr(attrName);
            Object attrValue = rscAttr.getAttrValue();
            Class<?> attrClass = rscAttr.getAttrClass();

            if (attrClass != prmInfo.getParamClass()) {
                System.out.println("Unable to set Attribute " + attrName
                        + " because it is defined as " + " the wrong type: "
                        + attrClass.getName() + " != "
                        + prmInfo.getParamClass().getName());
                continue;
            } else if (attrValue == null) {
                continue;
            }

            String setMthdName = "set" + attrName.substring(0, 1).toUpperCase()
                    + attrName.substring(1);

            Method[] mthds = this.getClass().getDeclaredMethods();

            for (Method m : mthds) {
                if (m.getName().equals(setMthdName)) {
                    Class<?>[] params = m.getParameterTypes();
                    Class<?> rtype = m.getReturnType();

                    // This would be a nice sanity check but I would have to go
                    // back and change all ints and booleans
                    // in the getters and setters for old resources even though
                    // they are compatible with the defined classes
                    // if( params[0] != attrInfo.getAttrClass() ||
                    // params.length != 1) {
                    // System.out.println("Error setting rsc attr "+attrName+" : setter class "
                    // +
                    // "has incompatible argument.");
                    // System.out.println("Warning: Attribute "+attrName
                    // +" is not defined\n"+
                    // "as correct type:" +params[0].getName() + " != " +
                    // attrInfo.getAttrClass().getName() );
                    // continue;
                    // }

                    try {
                        m.invoke(this, attrValue);
                    } catch (IllegalAccessException iae) {
                        System.out.println(iae.getMessage());
                    } catch (IllegalArgumentException iae) {
                        System.out.println(iae.getMessage());
                    } catch (InvocationTargetException ite) {
                        System.out.println(ite.getMessage());
                    } catch (ClassCastException cce) {
                        System.out.println(cce.getMessage());
                    }

                }
            }
        }

        if (ncRsc != null) {
            ((INatlCntrsResource) ncRsc).resourceAttrsModified();
        }

        return true;
    }

    public void autoUpdate(Object updateData) {
        this.fireChangeListeners(ChangeType.DATA_UPDATE, updateData);
    }

    // This is called by the autoUpdater when new data arrives and is also
    // called by the NCMapDescriptor to indicate that the timeline has changed.
    //
    @Override
    public void update(Object updateData) {
        // Do not process auto update alerts from raytheon's AutoUpdater since
        // we will
        // get alerts from NcAutoUpdater.
        // System.out.println("raytheon's update called");
        // super.update( updateData );
    }

    // using same logic as other resources. ie use frameIntervalval to set a
    // range and offset
    // and use these to create the normalized time.
    // protected DataTime getNormalizedTime( DataTime time) {
    // if( frameIntervalval == 0 ) {
    // return time;
    // }
    // int intervalRange = frameIntervalval * 1000;
    // int intervalOffset = intervalRange / 2;
    //
    // long millis = time.getValidTime().getTimeInMillis();
    // millis -= intervalOffset;
    // millis = ((millis / intervalRange) * intervalRange) + intervalRange;
    // return new DataTime(new Date(millis));
    // }
    //

    public void setLegendColor(RGB legClr) {
        legendColor = legClr;
    }

    public RGB getLegendColor() {
        if (legendColor == null) {
            legendColor = new RGB(255, 255, 255);
        }
        return legendColor;
    }

    @Override
    public boolean equals(Object obj) {

        if (obj == null || !super.equals(obj)) {
            return false;
        }

        if (getClass() != obj.getClass()) {
            return false;
        }

        AbstractNatlCntrsRequestableResourceData other = (AbstractNatlCntrsRequestableResourceData) obj;

        if ((legendColor == null && other.legendColor != null)
                || (legendColor != null && other.legendColor == null)) {
            return false;
        }
        if (!legendColor.toString().equals(other.legendColor.toString())) {
            return false;
        }
        if (isEdited != other.isEdited) {
            return false;
        }
        if ((resourceName == null && other.resourceName != null)
                || (resourceName != null && other.resourceName == null)) {
            return false;
        }
        if (!resourceName.toString().equals(other.resourceName.toString())) {
            return false;
        }
        if (!resourceVersion.equals(other.resourceVersion)) {
            return false;
        }
        if (frameSpan != other.frameSpan) {
            return false;
        }
        if (timeMatchMethod != other.timeMatchMethod) {
            return false;
        }
        if (dfltNumFrames != other.dfltNumFrames) {
            return false;
        }

        // Compare the attributes here to avoid having to write code in all of
        // the resource classes.
        // (NOTE: This currently isn't comparing PlotModel and ColorBar
        // attributes.
        ResourceAttrSet thisAttrSet = this.getRscAttrSet();
        ResourceAttrSet otherAttrSet = other.getRscAttrSet();

        if (thisAttrSet == null && otherAttrSet != null) {
            return false;
        } else if (thisAttrSet != null && otherAttrSet == null) {
            return false;
        } else if (thisAttrSet != null) {
            return thisAttrSet.equals(otherAttrSet);
        }

        return true;
    }

}
