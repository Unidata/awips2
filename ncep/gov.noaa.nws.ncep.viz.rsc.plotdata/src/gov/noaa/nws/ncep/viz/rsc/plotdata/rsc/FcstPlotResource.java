package gov.noaa.nws.ncep.viz.rsc.plotdata.rsc;

import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource.AbstractFrameData;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource.IRscDataObject;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceName;
import gov.noaa.nws.ncep.viz.rsc.plotdata.rsc.PlotResource2.FrameData;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.raytheon.viz.pointdata.PlotInfo;


/**
 * Derive from PlotResource2
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *  10/04/2010     #307    ghull       Created 
 *  02/16/2012     #555    sgurung     Changed frameData.setPopulated() to frameData.setPopulated(true) in populateFrame().
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1.0
 */
public class FcstPlotResource extends PlotResource2 {

    ArrayList<DataTime> dataTimes = new ArrayList<DataTime>();

	public FcstPlotResource(PlotResourceData data, LoadProperties props) {
		super(data, props);
		
		//  If this is the only thing different about the Model Grid Plots then
		// we won't need a separate ModelGridPlotResource class 
		if( data.getPluginName().equals("ncgrib" ) )  {
	        data.setPlotInfoRetriever( new ModelGridPlotInfoRetriever() );
		}
		else {
			// use the plotInfoRetriever set by PlotResource2
		}
	}
	
	// init PlotResource2 and then query all the cycle times and forecast times
	// so that we can query the correct data
	@Override
    public void initResource( IGraphicsTarget aTarget ) throws VizException {
		super.initResource(aTarget);
		
		// query all of the times in the db and filter out only the given cycle time.
    	ResourceName rscName = getResourceData().getResourceName();
        DataTime   cycleTime = rscName.getCycleTime();

        // latest should already be resolved here.
        if( cycleTime == null || rscName.isLatestCycleTime() ) { 
        	return;
        }

        // Note that since this is not constraining the grid parameter, level... there is 
        // still a potential problem if the db is missing times for the displayed param 
        // while times for other params exist.
		HashMap<String, RequestConstraint> queryList = new HashMap<String, RequestConstraint>(
				resourceData.getMetadataMap());

        LayerProperty property = new LayerProperty();
        property.setDesiredProduct( ResourceType.PLAN_VIEW );
        DataTime[] availableTimes;
        
        try {
			property.setEntryQueryParameters( queryList );			
	        availableTimes = property.getEntryTimes();
        }
        catch( VizException e) {
        	throw e;
        }

        // loop thru all the available times in the db and  
        //   if the ref time matches the cycle time for this resource and if
        //   it hasn't already been added, add it to the list of available times
        for( DataTime dt : availableTimes ) {
        	// create a dataTime without a possible validPeriod.
        	DataTime availTime = new DataTime( dt.getRefTime(), dt.getFcstTime() );
        	DataTime refTime = new DataTime( dt.getRefTime() );

        	if( cycleTime.equals( refTime ) ) {	        		
        		if( !dataTimes.contains( availTime ) ) {	        			
        			dataTimes.add( availTime );
        		}
        	}        	
        }
	}
	
	// Override the PlotResource2 version which will query the db based on the start
	// and end frame times. This won't work here because the query constraints for the
	// dataTime doesn't work with a forecast hour. So instead we will check all the
	// available times and for those that timeMatch, we will query the database for
	// matching dataTimes.
	@Override
    protected void populateFrame( FrameData frameData ) throws VizException {
		if( frameData.isPopulated() ) {
			return;
		}
		
		for( DataTime dt : dataTimes ) {

			// TODO : replace this with call to timeMatch when delivered
//			if( frameData.timeMatch(dt) >= 0 ) {
			long dtms = dt.getValidTime().getTimeInMillis();
			long stms = frameData.getFrameStartTime().getValidTime().getTimeInMillis();
			long etms = frameData.getFrameEndTime().getValidTime().getTimeInMillis();
			
			if( stms < dtms && dtms <= etms ) {
				HashMap<String, RequestConstraint> metadataMap = new HashMap<String, RequestConstraint>(
						plotRscData.getMetadataMap() );
				RequestConstraint timeConstraint = new RequestConstraint( dt.toString() );

				metadataMap.put("dataTime", timeConstraint );
				long t0 = System.currentTimeMillis();

				List<PlotInfo> plotInfoObjs = plotRscData.getPlotInfoRetriever().getStations(metadataMap);

				long t1 = System.currentTimeMillis();
				System.out.println("InitFrame Took: " + (t1 - t0));

				for( PlotInfo pltInfo : plotInfoObjs ) {	

					// have to add this since the dataTime is not getting set by the 
					// plotInfoRetriever?
					if( pltInfo.dataTime == null ) {
						pltInfo.dataTime = dt;
					}

					for( IRscDataObject rscDataObj : processRecord( pltInfo )) {
						// sanity check: this should always be true since we constrained the query
						if( frameData.isRscDataObjInFrame( rscDataObj ) ) {
							frameData.updateFrameData( rscDataObj );
						}
						else {
							// if we don't change the plotInfoRetriever to set the dataTime, then this
							// really is a serious sanity check.....
							System.out.println("plotInfo obj doesn't time match to the frame which queried it???");
						}
					}
				}
			}
		}

		frameData.calculateProgDisc();
		
		frameData.setPopulated(true);
    }
	
//	@Override
//	protected AbstractFrameData createNewFrame( DataTime frameTime, int timeInt ) {
//		FrameData newFrame = new FrameData( frameTime, timeInt );		
//		return newFrame;
//	}

}
