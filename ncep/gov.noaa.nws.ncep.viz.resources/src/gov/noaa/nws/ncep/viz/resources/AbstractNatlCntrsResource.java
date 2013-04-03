/**
 * 
 */
package gov.noaa.nws.ncep.viz.resources;

import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData.TimeMatchMethod;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData.TimelineGenMethod;
import gov.noaa.nws.ncep.viz.resources.time_match.NCTimeMatcher;
import gov.noaa.nws.ncep.viz.ui.display.NCMapDescriptor;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Set;
import java.util.TreeMap;
import java.util.concurrent.ConcurrentLinkedQueue;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.catalog.ScriptCreator;
import com.raytheon.uf.viz.core.comm.Connector;
import com.raytheon.uf.viz.core.drawables.AbstractDescriptor;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FrameChangeMode;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FrameChangeOperation;
import com.raytheon.uf.viz.core.drawables.IFrameCoordinator;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceType;

/**
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 29 May 2009     #115      Greg Hull    Initial Creation.
 * 11 Mar 2010     #257      Greg Hull    add overridable preProcessFrameUpdate()
 * 20 Apr 2010               Greg Hull    implement disposeInternal to dispose of the FrameData
 * 30 Apr 2010     #276      Greg Hull    Abstract the dataObject instead of assuming PluginDataObject.
 * 05 Dec 2011               Shova Gurung Added progDiscDone to check if progressive disclosure is run 
 * 										  for frames that are already populated
 * 16 Feb 2012     #555      Shova Gurung Remove progDiscDone. Add call to setAllFramesAsPopulated() in queryRecords().
 * 										  Remove frameData.setPopulated(true) from processNewRscDataList().
 * 20 Mar 2012     #700      B. Hebbard   In processNewRscDataList(), when new frame(s) are created by auto update,
 *                                        set frame to LAST and issueRefresh() to force paint (per legacy; TTR 520).
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
public abstract class AbstractNatlCntrsResource<T extends AbstractNatlCntrsRequestableResourceData, D extends IMapDescriptor>
		extends AbstractVizResource<AbstractNatlCntrsRequestableResourceData, IMapDescriptor>
		implements INatlCntrsResource {	
	
	// 
	public static interface IRscDataObject {
		abstract DataTime getDataTime();
	}
	
	// a wrapper for the common case when a PluginDataObject is the resource Data Object.
	// 
	public static class DfltRecordRscDataObj implements IRscDataObject {
		private PluginDataObject pdo;

		public DfltRecordRscDataObj( PluginDataObject o ) {
			pdo = o;
		}
		
		@Override
		public DataTime getDataTime() {
			return pdo.getDataTime();
		}
		
		public PluginDataObject getPDO() {
			return pdo;
		}
	}
	
	// the expected class for the resourceDataObjects 
	//protected Class rscDataObjectClass; 
	
	//protected static DataTime lastFrameEndTime;
	
	public abstract class AbstractFrameData {
		
		protected DataTime frameTime;

		// for resources that need to populated the frames only when the frame is 
		// first displayed.
		protected boolean  populated;

		protected DataTime startTime; // valid times without a forecast hour
		protected DataTime endTime;
		
		protected long startTimeMillis;
		protected long endTimeMillis;
	
	 	// set the frame start and end time based on: 
		//   - frame time (from dominant resource), 
		//   - the frameInterval for this resource (may be different than the 
		//        frame interval used to generate the timeline) and
		//   - the timeMatchMethod. 
		protected AbstractFrameData( DataTime ftime, int frameInterval ) {

			// if there is a validPeriod or levels, ignore them and just use the valid time.
			this.frameTime = new DataTime( ftime.getRefTime(), ftime.getFcstTime() );
			this.populated = false;
	        long frameMillis = frameTime.getValidTime().getTimeInMillis();

			switch( resourceData.getTimeMatchMethod() ) {

			case EXACT : {
		        startTime = new DataTime( frameTime.getValidTime() );
		        endTime   = new DataTime( frameTime.getValidTime() );
			}
			case CLOSEST_BEFORE_OR_AFTER : {
		        startTime = new DataTime( new Date(frameMillis - frameInterval*1000*60/2 ) );
		        endTime   = new DataTime( new Date(frameMillis + frameInterval*1000*60/2-1000 ) );
				break;
			}
			case CLOSEST_BEFORE_OR_EQUAL : { 					
		        startTime = new DataTime( new Date(frameMillis - frameInterval*1000*60 ) );
		        endTime   = new DataTime( frameTime.getValidTime() );
				break;
			}
			case CLOSEST_AFTER_OR_EQUAL : {
		        startTime = new DataTime( frameTime.getValidTime() );
		        endTime   = new DataTime( new Date(frameMillis + frameInterval*1000*60-1000 ) );
		        break;
			}
			case BEFORE_OR_EQUAL : {
				startTime = new DataTime( new Date(0) );
				endTime   = new DataTime( frameTime.getValidTime() );
				break;
			}
			}
			
			startTimeMillis = startTime.getValidTime().getTimeInMillis();
			endTimeMillis   = endTime.getValidTime().getTimeInMillis();
		}
		
		final public boolean isRscDataObjInFrame( IRscDataObject rscDataObj ) {	
			DataTime dataTime = rscDataObj.getDataTime();
			return ( dataTime == null ? false : timeMatch( dataTime ) >= 0 );
		}
		
		// return -1 if the data doesn't match. if the return value is 0 or positive
		// then this is the number of seconds from the perfect match.
		public long timeMatch( DataTime dataTime ) {
			
			long dataTimeMillis = dataTime.getValidTime().getTimeInMillis();
			TimeRange dataTimeRange = dataTime.getValidPeriod();
			
			switch( resourceData.getTimeMatchMethod() ) {

			case EXACT : {
				long frameTimeMillis = frameTime.getValidTime().getTimeInMillis();
				
				if( dataTimeRange.isValid() ) {
					if( dataTimeRange.getStart().getTime() <= frameTimeMillis &&
						frameTimeMillis <= dataTimeRange.getEnd().getTime() ) {
						return 0;
					}
					else {
						return -1;
					}
				}
				else {
					return (frameTimeMillis == dataTimeMillis ? 0 : -1);
				}
			}
			// mainly (only?) for lightning. Might be able to remove this timeMatchMethod 
			// if lighting resource is modified?
			case BEFORE_OR_EQUAL : {
				return ( dataTimeMillis > endTimeMillis ? -1 :
					                (endTimeMillis-dataTimeMillis)/1000);
			}
			case CLOSEST_BEFORE_OR_AFTER :
			case CLOSEST_BEFORE_OR_EQUAL :
			case CLOSEST_AFTER_OR_EQUAL : {	
				// This should be an invalid case. if this is an event type resource then
				// it should be an EXACT time match. Still, for now leave this logic in here.
				if( dataTimeRange.isValid() ) {
					System.out.println("Timematching a dataTime with a valid interval with a non-EXACT\n "+
								       "TimeMatchMethod.");
					return -1;
//					long dataStartTimeMillis = dataTimeRange.getStart().getTime();
//					long dataEndTimeMillis   = dataTimeRange.getEnd().getTime();
//					
//					if( dataStartTimeMillis > endTimeMillis || 
//						dataEndTimeMillis  <= startTimeMillis ) {
//						return false;
//					}
//					else {
//						return true;
//					}
				}

				// return -1 if this is not a match.
				// (since the start/end times are based on the timeMatchMethod,
				//  we can just check that the datatime is not within the start/end)
				//
				if( startTimeMillis >= dataTimeMillis || 
					dataTimeMillis > endTimeMillis ) {
						return -1;
				}
				// 
				else if( resourceData.getTimeMatchMethod() == 
						                 TimeMatchMethod.CLOSEST_BEFORE_OR_EQUAL ) {
					return (endTimeMillis-dataTimeMillis) / 1000;
				}
				else if( resourceData.getTimeMatchMethod() == 
				                         TimeMatchMethod.CLOSEST_AFTER_OR_EQUAL ) {
					return (dataTimeMillis-startTimeMillis) / 1000;
				}
				else if( resourceData.getTimeMatchMethod() == 
								         TimeMatchMethod.CLOSEST_BEFORE_OR_AFTER ) {
					return Math.abs( frameTime.getValidTime().getTime().getTime() -
							               dataTimeMillis) / 1000 ;
				}
				}
			}
			return -1;
		}
		
		// only return true if the data was added to the frame. It is possible for 
		// some resources for the data to time match but not be added because there 
		// is already data in the frame that is a better match.
		//
		public abstract boolean updateFrameData(IRscDataObject rscDataObj);

		public void dispose() {
			// override this if need to dispose of anything in the Frame.
		}
		
		public DataTime getFrameTime() {
			return frameTime;
		}
		
		public DataTime getFrameStartTime() {
			return startTime;
		}
		
		public DataTime getFrameEndTime() {
			return endTime;
		}

		public boolean isPopulated() {
			return populated;
		}
		public void setPopulated(boolean p) {
			populated = p;
		}
		
	}

	protected boolean initialized = false;

	// Map from a frame time string to a structure containing
	// displayable data elements for that frame...
	protected TreeMap<Long, AbstractFrameData> frameDataMap;

	// This list is populated by the initial query and also by the auto update
	// when new data is ingested for this resource. These objects time matched
	// to one or more frames.
	protected ConcurrentLinkedQueue<IRscDataObject> newRscDataObjsQueue;
	
	// This list caches objects that are ingested and are newer than the latest frame.
	// When a frame is later created due to auto updating, these objects are moved to
	// the newRscDataObjsQueue.
	protected ArrayList<IRscDataObject> autoUpdateCache;
	
	// the new frame times that will be created during the next auto update
	protected ArrayList<DataTime> newFrameTimesList;
	
	protected DataTime currFrameTime;

	private boolean autoUpdateReady=false;
	
	// if the frameGenMthd is USE_FRAME_INTERVAL then this is the time
	// when the next frame will be created if autoupdate is on.
	// Note: that more than one frame may be created if the user turns
	// autoupdate on long after the next frame time has past.
	private DataTime nextFrameTime=null; 
	
	
	protected AbstractNatlCntrsResource(T resourceData, LoadProperties props) {
		super(resourceData, props);
		frameDataMap = new TreeMap<Long, AbstractFrameData>();
		newRscDataObjsQueue = new ConcurrentLinkedQueue<IRscDataObject>();
		
		autoUpdateCache = new ArrayList<IRscDataObject>();
		newFrameTimesList = new ArrayList<DataTime>();
		currFrameTime = null;

		// if requestable add a resourceChanged listener that is called by Raytheon's 
		// AbstractRequestableResource when update() is called.
		if( resourceData instanceof AbstractNatlCntrsRequestableResourceData ) {
			resourceData.addChangeListener(new IResourceDataChanged() {
				@Override
				public void resourceChanged(ChangeType type, Object object) {
					
					if( object == null ) {
						System.out.println( "resourceChanged called with null object for "+
								getResourceData().getResourceName().toString() );
						return;
					}
					
					// TODO : need to make sure that these are the same types of 
					// objects that are returned by the queryRecords method (or the method
					// that the resource uses to populate the frameData).
					//
					if (type == ChangeType.DATA_UPDATE) {
						if( object instanceof Object[] ) {
							for( Object obj : (Object[])object ) {
								for( IRscDataObject dataObj : processRecord( obj ) ) {
									newRscDataObjsQueue.add(dataObj);
								}
							}
						}
						else {
							for( IRscDataObject dataObj : processRecord( object ) ) {
								newRscDataObjsQueue.add(dataObj);
							}
						}
						
						// can't call processNewRscDataList here since we are not in the UI thread when 
						// the AutoUpdater calls us and this will end up updating the status line's
						// frame time is a new frame is created.
						//
						autoUpdateReady = true;
					}
				}
			});
		}
	}
	
	// the timeline (times in the timeMatcher) have changed so we must update the
	// frames in the frameDataMap and process any data saved in the autoUpdateCache.
	//
	public Boolean updateTimeline( ) {
		NCTimeMatcher timeMatcher = (NCTimeMatcher) descriptor.getTimeMatcher();
		List<DataTime> newFrameTimes = timeMatcher.getFrameTimes();

		// loop thru all of the new frame times and if the frameDataMap doesn't have an entry for
		// this time create a new entry. 
		for( DataTime frameTime : newFrameTimes ) {
			if( !frameDataMap.containsKey( frameTime.getValidTime().getTime().getTime() ) ) {
				AbstractFrameData newFrame = this.createNewFrame( frameTime, 
																  resourceData.frameSpan );
				frameDataMap.put( frameTime.getValidTime().getTime().getTime(), newFrame );
			}
		}

		// loop thru all of the times in the frameDataMap and if the time is not in the
		// new frameTimes then remove the frame from the map.
		ArrayList<Long> frameTimesInMap = new ArrayList<Long>( frameDataMap.keySet() );
		
		for( long frameTimeMs : frameTimesInMap ) {
			if( !newFrameTimes.contains( new DataTime( new Date( frameTimeMs ) ) ) ) {
				frameDataMap.get(frameTimeMs).dispose();
				frameDataMap.remove( frameTimeMs );
			}
		}
		
		// copy all of the cached objects for auto update and process them.
		// 
		newRscDataObjsQueue.addAll( autoUpdateCache );
		autoUpdateCache.clear();
				
		return true;
	}

	
	// This is the default implementation for the common case where the Resource
	// simply wants to process the PluginDataObject itself but some resources may
	// want to override this method if a record contains more than one 'data object' and
	// these objects are to be time matched. (ex. some sigmet resources have multiple sigmets
	// per record and each has a separate valid time.)
	// 
	protected IRscDataObject[] processRecord( Object pdo ) {
		if( !(pdo instanceof PluginDataObject) ) {
			System.out.println( "Resource Impl "+getClass().getName() + " must override " +
					"the processRecord method to process data objects of class: "+
					pdo.getClass().getName() );
			return null;
		}
		
		DfltRecordRscDataObj rscDataObj = new DfltRecordRscDataObj( (PluginDataObject)pdo );		
		return new DfltRecordRscDataObj[]{ rscDataObj };
	}
	
	// This assumes that the given time is in the map
	//
	public AbstractFrameData getFrame( DataTime dataTime ) {
		AbstractFrameData frameData = null;
		
		if( dataTime != null ) {
			frameData = frameDataMap.get( dataTime.getValidTime().getTime().getTime() );
		}
		return frameData;
	}
	
	public AbstractFrameData getCurrentFrame( ) {
		if( currFrameTime == null ) {
			return null;
		}
		return frameDataMap.get( currFrameTime.getValidTime().getTime().getTime() );
	}

	public DataTime getCurrentFrameTime() {
		return currFrameTime;
	}
	
	public ArrayList<DataTime> getFrameTimes() {
		ArrayList<DataTime> frmTimes = new ArrayList<DataTime>();
		for( long t : frameDataMap.keySet() ) {
			frmTimes.add( new DataTime( new Date(t) ) );
		}
		return frmTimes;
	}
	
	public NCMapDescriptor getNCMapDescriptor() {
		if( getDescriptor() instanceof NCMapDescriptor ) {
			return (NCMapDescriptor)getDescriptor();			
		}
		return null;
	}
	
	// 
	public final void initInternal(IGraphicsTarget grphTarget) throws VizException {
    	if( !initialized ) {
    		// create the frameDataMap based on the timeFrames from the timeMatcher.
    		//ArrayList<DataTime> frameTimes
    		NCTimeMatcher timeMatcher = (NCTimeMatcher)descriptor.getTimeMatcher();
    		List<DataTime> frameTimes = timeMatcher.getFrameTimes();
    		
    		for( DataTime frameTime : frameTimes ) {
    			if( frameTime != null ) {
    				AbstractFrameData newFrame = this.createNewFrame( frameTime, 
    						                             resourceData.frameSpan );
    				frameDataMap.put( frameTime.getValidTime().getTime().getTime(), newFrame );
    			}
    		}
    		
    		// if using a frameInterval to generate the timeline, predict the 
    		// time for the next frame which will be created if auto updating 
    		if( !frameTimes.isEmpty() &&
    			getResourceData().getTimelineGenMethod() != TimelineGenMethod.USE_DATA_TIMES ) {
    			nextFrameTime = new DataTime( new Date(frameDataMap.lastKey()), 
    					                      timeMatcher.getFrameInterval() );
    		}
    		
    		// This is now done in the NCMapDescriptor when the timeMatcher is set
    		//
//            ((AbstractDescriptor) descriptor).getTimeMatchingMap().put(
//            		 this, frameTimes.toArray( new DataTime[0] ) );
            ((NCMapDescriptor) descriptor).setFrameTimesForResource( 
            		this, frameTimes.toArray( new DataTime[0] ) );
            // each resource may decide when and how to populate the frameData.
    		initResource( grphTarget );
    		initialized = true;
    	}
    }

	abstract public void initResource(IGraphicsTarget grphTarget) throws VizException;

	
	// don't let derived classes override paintInternal. Override paintFrame()
	public final void paintInternal(IGraphicsTarget target, PaintProperties paintProps)
			throws VizException {
		
		if( !newRscDataObjsQueue.isEmpty() ||
			(!newFrameTimesList.isEmpty() &&
			  getNCMapDescriptor().isAutoUpdate()) ) {
			processNewRscDataList();
		}
		
		if( paintProps == null || paintProps.getDataTime() == null) {
			// should we still call the resource's paintFrame in case it needs to do something 
			// even if there is no time?
			return;
		}

		currFrameTime = paintProps.getDataTime();
		AbstractFrameData currFrame = frameDataMap.get( 
				currFrameTime.getValidTime().getTime().getTime() );

		if( currFrame == null ) {
			System.out.println("paint(): Unable to find Frame Data for current Time "
					+ currFrameTime );
			return;
		}

		// let the derived resources do this
//		if( !currFrame.isPopulated() ) {
//			populateFrame( currFrame );
//		}
		
		paintFrame(currFrame, target, paintProps);
	}

	protected abstract void paintFrame(AbstractFrameData frameData,
			   IGraphicsTarget target, PaintProperties paintProps ) throws VizException;

	
	// loop thru newDataObjectsList and update frameDataMap. If a frame for a given
	// record time doesn't exist then create a new Frame
	//
	protected synchronized boolean processNewRscDataList() { // boolean isUpdate ) {
		
		// allow resources to pre process the data before it is added to the frames
		preProcessFrameUpdate();
		
		NCTimeMatcher timeMatcher = (NCTimeMatcher) descriptor.getTimeMatcher();

		while( !newRscDataObjsQueue.isEmpty() ) {
			IRscDataObject rscDataObj = newRscDataObjsQueue.poll();
			
			boolean foundFrame = false;
			boolean lastFrame  = false;
			
			// loop through the frames and add this record to all that it time matches to
			//
			for( AbstractFrameData frameData : frameDataMap.values() ) { 
				if( frameData != null ) {					
					if( frameData.isRscDataObjInFrame( rscDataObj ) ) {
						if( addRscDataToFrame( frameData, rscDataObj ) ) {
							//frameData.setPopulated(true);
							foundFrame = true;
						}
						
						if( frameData == frameDataMap.lastEntry().getValue() ) {
							lastFrame = true;
						}
					}
				}
			}
			
			// if not in any frames (or if in the last frame) and if updating and if this the data is 
			//    newer than the latest frame then cache the data for auto update.
			//    NOTE: this will 'drop' (not cache) data from the initial query that doesn't match the  
			//    selected timeline which means that if the user later enables auto-update, this data will  
			//    not be loaded as part of the auto-update. This is by design but could be changed if the 
			//    user wants a different behaviour.)
			//    NOTE: if the data is in the last frame then it may still potentially match the next frame 
			//    when an update occurs. (WAPITA) (Note2: the auto update code is written to update a frame even
			//    if it is not the last frame. (ie. data is received out of order for some reason.) but this 
			//    will cause any non-dominant data to not be displayed on these frames because we are only 
			//    checking for the last frame here.) 
			long dataTimeMs = getDataTimeMs(rscDataObj);//rscDataObj.getDataTime().getValidTime().getTime().getTime();

			if( timeMatcher.isAutoUpdateable()) {

				if( (!foundFrame && autoUpdateReady && 
						dataTimeMs > frameDataMap.firstKey()) 
						|| lastFrame ) {
					// if there is the possibility of auto updating (if the dominant resource
					// is a satellite or radar image) then store off the data in the autoUpdateCache
					// and then update the timeline if auto update is enabled..
					//
					autoUpdateCache.add( rscDataObj );

					// if this is the dominantResource, and this data is from a data update alert then 
					//    determine if a new frame is needed. 
					// 
					if( isDominantResource() && autoUpdateReady ) {
						newFrameTimesList.addAll(
								timeMatcher.determineNewFrameTimes( rscDataObj.getDataTime() ) );
					}
				}
			}
		}

		// allow resources to post-process the data after it is added to the frames
		postProcessFrameUpdate();

		autoUpdateReady = false;
		
		// if there is data in the auto update cache and if auto update is now enabled
		// and if this is the dominant resource for the timeline then we need to 
		// update the timeline and process the data in the cache.
		//
		if( !newFrameTimesList.isEmpty() &&
			getNCMapDescriptor().isAutoUpdate() ) {
			
			// update the list of times in the timeMatcher.
			// this will then trigger the descriptor to update its datatimes and 
			// notify its resources (including this one) to create and update the frames
			timeMatcher.updateTimeline( newFrameTimesList );
			newFrameTimesList.clear();
			// advance to the new last frame (per legacy; could change)...
			descriptor.getFrameCoordinator().changeFrame(
					IFrameCoordinator.FrameChangeOperation.LAST,
					IFrameCoordinator.FrameChangeMode.TIME_ONLY);
			// ...and make sure it gets painted
			issueRefresh();
		}
		
		return true;
	}

	// break out from updateFrames to allow subclasses to override if needed.
	protected boolean addRscDataToFrame( AbstractFrameData frameData, 
									   IRscDataObject rscDataObj ) {
		return frameData.updateFrameData(rscDataObj);
	}

	// allow this to be overridden if derived class needs to
	protected boolean preProcessFrameUpdate() {
		return true;
	}

	// allow this to be overridden if derived class needs to
	protected boolean postProcessFrameUpdate() {
		return true;
	}

	protected abstract AbstractFrameData createNewFrame( DataTime frameTime, 
			                                             int frameInterval );

	// This method can be used as a convenience if the MetadataMap constraints are
	// all that is needed to query the data.
	public void queryRecords() throws VizException {

		HashMap<String, RequestConstraint> queryList = new HashMap<String, RequestConstraint>(
				resourceData.getMetadataMap());

		LayerProperty prop = new LayerProperty();
		prop.setDesiredProduct(ResourceType.PLAN_VIEW);
		prop.setEntryQueryParameters(queryList, false);
		prop.setNumberOfImages(15000); // TODO: max # records ?? should we cap
										// this ?
		String script = null;
		script = ScriptCreator.createScript(prop);

		if (script == null)
			return;

		Object[] pdoList = Connector.getInstance().connect(script, null, 60000);

		for (Object pdo : pdoList) {
			for( IRscDataObject dataObject : processRecord( pdo ) )	{	
				newRscDataObjsQueue.add(dataObject);
			}
		}

		// 
		setAllFramesAsPopulated();
	}

	public boolean isDominantResource( ) {
		NCTimeMatcher timeMatcher = (NCTimeMatcher) descriptor.getTimeMatcher();

		if( timeMatcher != null && 
			timeMatcher.getDominantResourceName() != null ) {
		
			String domRscName = timeMatcher.getDominantResourceName().toString();
			
			if( domRscName.equals( getResourceData().getResourceName().toString() ) ) {
				return true;
			}
		}
		
		return false;	
	}
	protected void removeAllNewDataObjects() {
		newRscDataObjsQueue.clear();
	}
	
	// Don't think this should be called for NatlCntrs resources...?
	// if so then need to lookup the frame and remove it
	@Override
	public final void remove( DataTime dataTime ) {
//		super.remove( dataTime );
		// remove the given frame and call dispose
		// will the system call this for each resource removed? Need all resources to be in sync.
	}

	// let the resource refresh and do anything else it needs to do after
	// modifying its attributes.
	// override if the resource needs this
	public void resourceAttrsModified() {
		// Do nothing
	}
	
	// override this if needed
	@Override
    protected void disposeInternal() {
		for( AbstractFrameData frameData : frameDataMap.values() ) {
			frameData.dispose();
		}
    }
	
	protected void clearFrames() {
		for( AbstractFrameData frameData : frameDataMap.values() ) {
			frameData.dispose();
		}
	}
	
    @Override
    public String toString() {
        return this.resourceData.toString();
    }
    
    // added since WarnResource gets null pointers in processNewRscDataList()
	protected long getDataTimeMs(IRscDataObject rscDataObj) {
		return rscDataObj.getDataTime().getValidTime().getTime().getTime();
 
	}
	
	// set the populated flag in all frames
	// this is done when the data for all frames has been queried at one time
	// as opposed to populating as each frame is displayed.
	//	
	protected void setAllFramesAsPopulated() {
		for( AbstractFrameData frameData : frameDataMap.values() ) {
			frameData.setPopulated(true);
		}
	}
}
