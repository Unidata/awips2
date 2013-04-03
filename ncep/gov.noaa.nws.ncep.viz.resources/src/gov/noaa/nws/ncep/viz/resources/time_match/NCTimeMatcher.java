package gov.noaa.nws.ncep.viz.resources.time_match;

import gov.noaa.nws.ncep.viz.common.SelectableFrameTimeMatcher;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData.TimelineGenMethod;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceName;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceName.ResourceNameAdapter;
import gov.noaa.nws.ncep.viz.ui.display.NCMapDescriptor;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.apache.commons.lang.Validate;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.AbstractTimeMatcher;
import com.raytheon.uf.viz.core.drawables.AbstractDescriptor;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FrameChangeMode;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FrameChangeOperation;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

/**
 * Time matching for Natl Cntrs is based on the dominant source. The data times
 * defined by it are the times for all of the resources. Other resources will
 * need to time match their data to this list of data times.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 08/11/09       145      Greg Hull    Initial creation
 * 10/20/09       145      Greg Hull    setFrameTimes
 * 09/06/10       307      Greg Hull    add timeRange, frameInterval
 * 01/29/11       365      Greg Hull    add latestRefTimeSentinel 
 * 02/01/11       365      Greg Hull    change generateTimes to set frameTimes 
 *                                      to allow for initialization without timelineControl
 * 03/07/11     migration  Greg Hull    rm notifyResourceAdd and change defn of initialLoad
 * 11/29/11       518      Greg Hull    add dfltFrameTimes
 * 06/17/12       713      Greg Hull    typo in copy constr for skipValue
 * 08/27/12       851      Greg Hull    ignore dataTime level when creating new frame Times
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class NCTimeMatcher extends AbstractTimeMatcher implements
        ISerializableObject {

    protected transient AbstractNatlCntrsRequestableResourceData dominantRscData;

    @XmlAttribute
    @XmlJavaTypeAdapter(ResourceNameAdapter.class)
    protected ResourceName dominantResourceName;

    @XmlAttribute
    protected int numFrames;

    @XmlAttribute
    protected int skipValue;

    @XmlAttribute
    protected int timeRange; // in hours

    @XmlAttribute
    protected int frameInterval; // if -1 then use data times
    
    private final static DataTime latestRefTimeSentinel = new DataTime(
            new Date(0));

    @XmlElement
    protected DataTime refTime; // if null, use current

    // all the times in the db based on the dominant resource
    private List<DataTime> allAvailDataTimes;

    // all the times in the db based on the dominant resource
    private List<DataTime> selectableDataTimes;

    // the frame times that will be used for the RBD
    protected List<DataTime> frameTimes;

    // ie GDATTIME used to set the initial frame times
    @XmlAttribute 
    protected String dfltFrameTimesStr;

    private final int dfltNumFrames = 0;

    private final int dfltSkipFrames = 0;

    private boolean timesLoaded = false;

    private boolean isForecast = false;

    private ArrayList<NCMapDescriptor> descriptorList;

    /**
     * Default Constructor.
     */
    public NCTimeMatcher() {
        super();
        descriptorList = new ArrayList<NCMapDescriptor>();
        dominantRscData = null;
        dominantResourceName = null;
        allAvailDataTimes = new ArrayList<DataTime>();
        selectableDataTimes = new ArrayList<DataTime>();
        frameTimes = new ArrayList<DataTime>();
        numFrames = dfltNumFrames;
        skipValue = dfltSkipFrames;
        timeRange = 0; // set from dominant resource
        frameInterval = -1;
        timeRange = 0;
        refTime = null;
        timesLoaded = false;
        isForecast = false;
        setCurrentRefTime();
    }

    public NCTimeMatcher(NCTimeMatcher tm) {
        super();
        descriptorList = new ArrayList<NCMapDescriptor>();
        allAvailDataTimes = new ArrayList<DataTime>(tm.allAvailDataTimes);
        selectableDataTimes = new ArrayList<DataTime>(tm.selectableDataTimes);
        frameTimes = new ArrayList<DataTime>(tm.frameTimes);
        timesLoaded = tm.timesLoaded;
        numFrames = tm.numFrames;
        skipValue = tm.skipValue;
        timeRange = tm.timeRange;
        refTime = (tm.refTime == null ? null : new DataTime(
                tm.refTime.getRefTime(), tm.refTime.getFcstTime()));
        frameInterval = tm.frameInterval;
        // TODO : This needs to clone the dominant resource
        // try {
        // dominantRscData = (AbstractNatlCntrsRequestableResourceData)
        // ObjectCloner.deepCopy( tm.getDominantResource() );
        // } catch (Exception e) {
        // e.printStackTrace();
        // }
        dominantRscData = tm.dominantRscData;
        dominantResourceName = new ResourceName(tm.getDominantResourceName());
        isForecast = tm.isForecast;
    }

    public void addDescriptor(NCMapDescriptor desc) {
        if (!descriptorList.contains(desc)) {
            descriptorList.add(desc);
        }
    }

    public void removeDescriptor(NCMapDescriptor desc) {
        if (descriptorList.contains(desc)) {
            descriptorList.remove(desc);
        }
    }

    public ResourceName getDominantResourceName() {
        return dominantResourceName;
    }

    public boolean isForecast() {
        return isForecast;
    }

    public void setForecast(boolean isForecast) {
        this.isForecast = isForecast;
    }

    public int getNumFrames() {
        return numFrames;
    }

    public void setNumFrames(int numFrames) {
        this.numFrames = numFrames;
    }

    public int getSkipValue() {
        return skipValue;
    }

    public void setSkipValue(int skip) {
        this.skipValue = skip;
    }

    public int getTimeRange() {
        return timeRange;
    }

    public void setTimeRange(int timeRange) {
        this.timeRange = timeRange;
    }

    public int getFrameInterval() {
        return frameInterval;
    }

    public void setFrameInterval(int frameInterval) {
        this.frameInterval = frameInterval;
    }

    public List<DataTime> getFrameTimes() {
        return frameTimes;
    }

    public void setFrameTimes(ArrayList<DataTime> ft) {
        frameTimes = ft;
        // do we always want to set numFrames here?
        numFrames = frameTimes.size();
    }

    public List<DataTime> getSelectableDataTimes() {
        return selectableDataTimes;
    }

    public void setSelectableDataTimes(ArrayList<DataTime> selDataTimes) {
        this.selectableDataTimes = selDataTimes;
    }

    public DataTime getRefTime() {
        return refTime;
    }

    public void setRefTime(DataTime refTime) {
        this.refTime = refTime;
    }

    public void setLatestRefTime() {
        refTime = latestRefTimeSentinel;
    }

    public void setCurrentRefTime() {
        refTime = null;// new DataTime( Calendar.getInstance().getTime() );
    }

    public boolean isLatestRefTime() {
        return (refTime == latestRefTimeSentinel);
    }

    public boolean isCurrentRefTime() {
        return (refTime == null);
    }

    public boolean isDataAvailable() {
        return !allAvailDataTimes.isEmpty();
    }

    public AbstractNatlCntrsRequestableResourceData getDominantResource() {
        return dominantRscData;
    }

    public boolean isAutoUpdateable() {
        return (dominantRscData == null ? false : 
        	dominantRscData.isAutoUpdateable());
    }

    // set the dominant resource
    public void setDominantResourceData(
            AbstractNatlCntrsRequestableResourceData domRscData) {
        dominantRscData = domRscData;
        if (dominantRscData == null) {
            dominantResourceName = null;
            isForecast = false;
        } else {
            dominantResourceName = dominantRscData.getResourceName();
            isForecast = dominantRscData.isForecastResource();
        }
    }

    // set the dominant resource and update the frameTimes
    public void updateFromDominantResource() {

        if (dominantRscData == null) {
            frameTimes.clear();
            selectableDataTimes.clear();
            numFrames = 0;
            frameInterval = -1;
            timeRange = 0;
            isForecast = false;
            refTime = null;
            return;
        }

        numFrames = dominantRscData.getDfltNumFrames();

        timeRange = dominantRscData.getDfltTimeRange();

        skipValue = 0; // no default but reset to 0

        isForecast = dominantRscData.isForecastResource();
         
        dfltFrameTimesStr = dominantRscData.getDfltFrameTimes();
        
//      if (isForecast) {
        if( dominantRscData.getResourceName().getCycleTime() != null ) {
        	refTime = null;
        } else {
            setCurrentRefTime();
        }

        // the frameInterval here is only used to generate the timeline.
        if (dominantRscData.getTimelineGenMethod() == TimelineGenMethod.USE_DATA_TIMES) {
            frameInterval = -1;
        } else if (dominantRscData.getTimelineGenMethod() == TimelineGenMethod.USE_MANUAL_TIMELINE) {
            if (frameInterval <= 0) {
                frameInterval = 60; // what to use here as a default
            }
        } else if (dominantRscData.getTimelineGenMethod() == TimelineGenMethod.USE_FRAME_INTERVAL) {
            frameInterval = dominantRscData.getFrameSpan();
        } else { // ???
            return;
        }

        loadTimes(true);
    }

    // if refTime is null, then either use the most recent data as the refTime
    // or the cycle
    // time for forecast data.
    public boolean generateTimeline() {
        frameTimes.clear();
        selectableDataTimes.clear();

        if (dominantRscData == null) {
            return false;
        } else if (frameInterval == 0) {
            return false;
        }

        long frameIntervalMillisecs = ((long) frameInterval) * 60 * 1000;
        long timeRangeMillisecs = ((long) timeRange) * 60 * 60 * 1000;

        // determine the reference time to use.
        // refTime is marshalled out to the bundle file and may be null
        // or 'latest' or a time set by the user.
        long refTimeMillisecs = 0;

        // 	if (isForecast) {        
        // check cycleTime instead of isForecast since some resources may
        // be forecast w/o a cycletime
        if( dominantResourceName.getCycleTime() != null ) {
            if (!dominantResourceName.isLatestCycleTime()) {
                DataTime cyclTime = dominantResourceName.getCycleTime();
                refTimeMillisecs = (cyclTime == null ? 0 : cyclTime
                        .getRefTime().getTime());
            }
//            else {
//                System.out.println("Can't get a ref time from 'latest' cycle time");
//                return false;
//            }
        } else if( isCurrentRefTime() ) {
            refTimeMillisecs = Calendar.getInstance().getTimeInMillis();
        } else if( isLatestRefTime() ) {
            refTimeMillisecs = 0;
        } else {
            refTimeMillisecs = refTime.getRefTime().getTime();
        }

        // if refTime is Latest or 
        // if using the data to generate the timeline, or
        // if we need to get the cycle time for a forecast resource, then
        //  we will need to query the times of the dominant resource.
        if (refTimeMillisecs == 0 || frameInterval == -1) {
            allAvailDataTimes = dominantRscData.getAvailableDataTimes();

            if (allAvailDataTimes == null) { // no data
                allAvailDataTimes = new ArrayList<DataTime>(); // 
                return false;
            }

            // if refTime is not given (ie Latest) then get it from the data
            if (refTimeMillisecs == 0) {
                if (!allAvailDataTimes.isEmpty()) {

                    // TODO : setting the refTime here will cause this time
                    // to be saved to the rbd. Is this what we want or should
                    // this
                    // save the sentinel value and get latest when the rbd is
                    // loaded?
                    //
                    refTime = allAvailDataTimes.get((isForecast ? 0
                            : allAvailDataTimes.size() - 1));
                    refTimeMillisecs = refTime.getRefTime().getTime();
                } else {
                    return false;
                }
            }

            // if generating times from the data then get only those times in
            // the selected range.
            if (frameInterval == -1) {

                long oldestTimeMs = (isForecast ? refTimeMillisecs
                        : refTimeMillisecs - timeRangeMillisecs - 1);

                long latestTimeMs = (isForecast ? refTimeMillisecs
                        + timeRangeMillisecs + 1 : refTimeMillisecs);

                for (DataTime time : allAvailDataTimes) {
                    long timeMS = time.getValidTime().getTimeInMillis();

                    if (timeMS >= oldestTimeMs && timeMS <= latestTimeMs) {
                    	
//                    	if( frmTimeMatcher == null ||
//                    		frmTimeMatcher.matchTime( time ) ) {                  		
                    		selectableDataTimes.add(time);
//                    	}
                    }
                }
            }
        }
        
        // if a frameInterval is set then use it to create a list of times
        // within the defined range.
        // 
        if (frameInterval > 0) {

            long frameTimeMillisecs = refTimeMillisecs;

            if (isForecast) {
                while (frameTimeMillisecs <= refTimeMillisecs + timeRangeMillisecs) {
                	
                	DataTime time = new DataTime( new Date( frameTimeMillisecs));
                	
//                	if( frmTimeMatcher == null ||
//                    	frmTimeMatcher.matchTime( time ) ) {                    		
                		selectableDataTimes.add(time);
//                	}

                    frameTimeMillisecs += frameIntervalMillisecs;
                }
            } else {

                // frameTimeMillisecs = normRefTime.getRefTime().getTime();;

                while (frameTimeMillisecs >= refTimeMillisecs - timeRangeMillisecs) {

                    DataTime normRefTime = getNormalizedTime( new DataTime(
                            new Date(frameTimeMillisecs)));

//                	if( frmTimeMatcher == null ||
//                		frmTimeMatcher.matchTime( normRefTime ) ) {
                       selectableDataTimes.add(0, normRefTime); // new DataTime(
                        // new Date(frameTimeMillisecs)  ) );
//                    }	

                    frameTimeMillisecs -= frameIntervalMillisecs;
                }
            }
        }

        SelectableFrameTimeMatcher frmTimeMatcher = null;
        
        if( dfltFrameTimesStr != null ) {
        	try {
				frmTimeMatcher = new SelectableFrameTimeMatcher( dfltFrameTimesStr );
			} catch (VizException e) {
				// sanity check since this should already be validated
				System.out.println("bad GDATTIM string:"+dfltFrameTimesStr);
				frmTimeMatcher = null;
			}
        }

        int skipCount = 0;
        // ArrayList<DataTime> tempFrameTimes = new ArrayList<DataTime>();

        // set the initial frameTimes from the skip value and numFrames
        //
        for (skipCount = 0; skipCount < selectableDataTimes.size(); skipCount++) {
        	if (skipCount % (skipValue + 1) == 0) {
        		
        		DataTime selectableTime = ( isForecast ?
        				selectableDataTimes.get( skipCount ) :
        					selectableDataTimes.get(
        							selectableDataTimes.size()-skipCount-1) );

        		if( frmTimeMatcher == null || 
        			frmTimeMatcher.matchTime( selectableTime ) ) {
        			
        			if (isForecast) {
        				frameTimes.add( selectableTime );
        			} else {
        				frameTimes.add( 0, selectableTime );
        			}
        		}
        		if (frameTimes.size() == numFrames) {
        			break;
        		}
        	}
        }

        // if the list is empty don't set numFrames.
        if (frameTimes.size() > 0) { // numFrames > frameTimes.size() ) {
            numFrames = frameTimes.size();
        }

        return true;
    }

    // This is called by the dominant resource when updated data arrives. It
    // will determine
    // if one or more frames times are needed for the new data.
    // note that these times are not actually added to the timeline until the
    // auto-update
    // occurs.
    public ArrayList<DataTime> determineNewFrameTimes(DataTime newDataTime) {
    	// in somecases the dataTime from the data can have a level set which
    	// can mess up the equals() method.
    	DataTime newFrameTime = new DataTime( newDataTime.getValidTime() );
    	
        ArrayList<DataTime> newFrameTimes = new ArrayList<DataTime>(1);

        // if the timeline was created using DATA_TIMES then the new time is the
        // new frame time unless
        // it is already in the frameTimes.
        if (frameInterval == -1) {
            if (!frameTimes.contains(newFrameTime)) {
                newFrameTimes.add(newFrameTime);
            }
        } else if (frameInterval != 0) { // if MANUAL or FRAME_TIMES
            // TODO : we could add forecast updates but right now it doesn't
            // make sense since
            // we only update on images and the data for new cycles isn't
            // 'updated'
            if (!isForecast) {
                long lastFrameTimeMs = frameTimes.get(frameTimes.size() - 1)
                        .getValidTime().getTime().getTime();
                long nextFrameTimeMs = lastFrameTimeMs;

                // current time isn't appropriate if we are using archive data
                // long currTimeMs = Calendar.getInstance().getTime().getTime();

                // This will create frames up to the current time which usually
                // won't occur but is
                // possible if no updates are received for a complete frame.
                // (TODO : is this what we want or should we only update to the
                // given data?)
                while (nextFrameTimeMs + frameInterval * 1000 * 60 < newFrameTime
                        .getValidTime().getTime().getTime()) {
                    nextFrameTimeMs += frameInterval * 1000 * 60;
                    newFrameTimes.add(new DataTime(new Date(nextFrameTimeMs)));
                }
            }
        }

        return newFrameTimes;
    }

    // on autoUpdate this will modify the generated timeline to add the new
    // times
    // and drop the oldest.
    // The descriptors are also updated with the new frameTimes.
    //
    // newTimes will be sorted and will not be empty
    //
    // TODO : If the current frame is the last frame then this will remove the
    // current frame.
    // Do we want to change this?
    public void updateTimeline(ArrayList<DataTime> newTimes) { // DataTime[]
                                                               // newTimes ) {

        int frmCnt = frameTimes.size();
        int numFramesToStep = 0;

        // for each new frame time.
        for (DataTime newTime : newTimes) {
            // insert each time into the list in order and then remove the
            // oldest time.
            if (!frameTimes.contains(newTime)) {
                // if newer than the last, add it to the end of the list.
                if (newTime.greaterThan(frameTimes.get(frmCnt - 1))) {
                    frameTimes.add(newTime);
                    frameTimes.remove(0);
                    numFramesToStep++;
                } else {
                    for (int f = 0; f < frmCnt; f++) {
                        if (frameTimes.get(f).greaterThan(newTime)) {
                            frameTimes.add(f, newTime);
                            frameTimes.remove(0);
                            numFramesToStep++;
                            break;
                        }
                    }
                }
            }
        }

        // update the times in the descriptors which will in turn
        // update the timeline for the resources in the descriptor.
        //
        for (NCMapDescriptor ncDescr : descriptorList) {
            ncDescr.updateDataTimes(frameTimes.toArray(new DataTime[0]));

            // Since we want to keep the resources displaying the same frame
            // data and we
            // have just changed the timeline, we need to change to the previous
            // frame.
            // (Note: this will also trigger the frameChanged listeners so that
            // the
            // frameTime on the status bar is updated.)
            while (numFramesToStep-- > 0) {
                ncDescr.changeFrame(FrameChangeOperation.PREVIOUS,
                        FrameChangeMode.TIME_ONLY);
            }
        }
    }

    //
    protected DataTime getNormalizedTime(DataTime time) {
        if (frameInterval <= 0) {
            // just returning time here was causing problems when toString is
            // called
            return new DataTime(time.getRefTime(), time.getFcstTime());
        }

        long frameIntervalMillis = frameInterval * 1000 * 60; // minutes to
                                                              // millisecons
        long millis = time.getValidTime().getTimeInMillis()
                + (frameIntervalMillis / 2);
        millis = ((millis / frameIntervalMillis) * frameIntervalMillis);
        return new DataTime(new Date(millis));
    }

    public boolean loadTimes(boolean reloadTimes) {

        if (!timesLoaded || reloadTimes) {

            if (dominantRscData == null) {
                if (dominantResourceName != null) {

                }
            }

            generateTimeline();

            timesLoaded = true;
        }

        return timesLoaded;
    }

    // This is called by raytheon's NcAutoUpdater

    // Assume that the frameTimes have been changed. We will need to update the
    // descriptor's
    // timeMatchingMap with the new frameTimes for all of the existing
    // resources.
    // TODO : we will need to call this when a new frame is added and this will
    // need to ensure that
    // all the non-dominant resource have new frames for new/updated frameTimes.
    // Also issues with
    // multi-pane displays.
    @Override
    public void redoTimeMatching(IDescriptor descriptor) throws VizException {
        Validate.isTrue(descriptor instanceof AbstractDescriptor, ""
                + this.getClass().getName() + " depends on AbstractDescriptor");
        // Map<AbstractVizResource<?, ?>, DataTime[]> timeMatchingMap =
        // ((MapDescriptor)descriptor).getTimeMatchingMap();
        //
        // DataTime[] dataTimes = frameTimes.toArray( new DataTime[0] );
        //
        // for( AbstractVizResource<?, ?> rsc : timeMatchingMap.keySet() ) {
        // timeMatchingMap.put( rsc, dataTimes );
        // }
    }

	@Override
	public void redoTimeMatching(AbstractVizResource<?, ?> resource) {
		
	}

    // TODO : What should the behaviour be if the user removes/unloads the
    // dominant resource?
    //
    @Override
    public void handleRemove(AbstractVizResource<?, ?> resource,
            IDescriptor descriptor) {

        if (resource instanceof AbstractNatlCntrsResource<?, ?>) {

            if (dominantRscData != null
                    && dominantRscData.getResourceName().equals(
                            ((INatlCntrsResourceData) resource
                                    .getResourceData()).getResourceName())) {
                // TODO : User should not be removing the Dominant Resource
//                System.out
//                        .println("Warning : removing the Dominant Resource for this RBD");
            }

            dominantRscData = null;
        }
    }

    // @Override
    // public void notifyResourceAdd(AbstractVizResource<?, ?> resource,
    // IDescriptor descriptor) {
    // // NC will be setting the tmb directly
    // if (!(resource.getResourceData() instanceof
    // AbstractRequestableResourceData)) {
    // return;
    // }
    // // TODO: Will it be ok to add a non-requestable resource? probably not.
    // if( !(resource instanceof INatlCntrsResource ) ||
    // !(resource.getResourceData() instanceof INatlCntrsResourceData ) ) {
    // System.out.println("Sanity Check: Adding non-NatlCntrs Resource to ncTimeMatcher");
    // }
    // }

    @Override
    public DataTime[] initialLoad(LoadProperties loadProps,
            DataTime[] availableTimes, IDescriptor descriptor)
            throws VizException {
        return frameTimes.toArray(new DataTime[0]);
    }

}