package gov.noaa.nws.ncep.viz.resources.manager;

import static java.lang.System.out;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.rsc.URICatalog;
import com.raytheon.uf.viz.core.rsc.URICatalog.IURIRefreshCallback;

// an entry in the availTimesCache which also may listen for
// updates from Raytheon's MenuUpdater
//
/**
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 *  09/05/12     #860       Greg Hull   Created for the ResourceDefinitions time cache
 *  
 * </pre>
 * 
 * @author ghull
 * @version 1
 */

public class DataTimesCacheEntry implements IURIRefreshCallback {
   
	// TODO : picked 30 seconds out of my ?#*@!.  We can use whatever makes the most sense.
    private static final long cacheHoldTime = 30*1000; // 30 seconds
    
    // set to true when we are added to the URICatalog
    private Boolean inUriCatalog = false; 
    
    private Map<String, RequestConstraint> resourceConstraints;
    
	private     DataTime  latestTime; // may be set separately from dataTimes 

	private List<DataTime> dataTimes;
	private long cacheTime; // the time the dataTimes were queried or 0 if not set yet
	
	public DataTimesCacheEntry( Map<String, RequestConstraint> reqCon ) {
		resourceConstraints = reqCon;
		cacheTime = 0;
    	dataTimes = new ArrayList<DataTime>();
    	latestTime = null;
	}

	// if this is added to the URI Catalog then Raytheon's MenuUpdater will
	// call updateTime() to update the latest time.
	// 
	public void addToUriCatalog() {
		if( !inUriCatalog ) {
			URICatalog.getInstance().catalogAndQueryDataURI( resourceConstraints, this );
			inUriCatalog = true;
		}
	}
	
	// return the latest time, or a Null dataTime if no data, or a null value
	// if either the cache has expired or a time has not been set yet.
	//
	// if in the catalog this will be up to date and no query is needed.
	// if not and the cache is still valid, then use the stored time,
	// if out of date then query all the times. 
	// TODO (do we need to check if the inventory is being utilized and if not
	// possibly not do the query since it may(?) take too long?)
	//
	public DataTime getLatestTime() {
		if( inUriCatalog ) {
			return latestTime;
		}    		   
		else if( System.currentTimeMillis()-cacheTime < cacheHoldTime ) {
    		return latestTime;    			
		}
		else {
			return null;
		}
//		// sanity check
//		if( latestTime != null && 
//			!latestTime.isNull() ) {
//			
//			if( !dataTimes.isEmpty() &&
//					latestTime.getValidTime().getTimeInMillis() != 
//						dataTimes.get( dataTimes.size()-1 ).getValidTime().getTimeInMillis() ) {
//				out.println("??latestTime and lastTime in availTimes list don't match");
//			}
//		}
	}

	// return null to indicate not set
	
	public List<DataTime> getAvailableTimes( ) {
		if( cacheTime == 0 ) {
			return null;
		}    		
		else if( System.currentTimeMillis()-cacheTime < cacheHoldTime ) {
//			out.println("returning cached times from "+ 
//					(System.currentTimeMillis()-cacheTime) + " msecs ago");    			
    		return dataTimes;
		}
		return null;
	}

	public List<DataTime> getAvailableTimes( Boolean ignoreCacheTime ) {
		if( ignoreCacheTime ) {
			return dataTimes;
		}
		return getAvailableTimes();
	}

	// Set the cache time to the current time assuming that the times were 
	// just queried.
	public void setAvailableTimes( List<DataTime> dtlist ) {
		cacheTime = System.currentTimeMillis();
    	dataTimes = new ArrayList<DataTime>( dtlist );
    	
    	// if the latestTime has not been set yet, set it.
    	if( latestTime == null ) {
    		if( dataTimes.isEmpty() ) {
    			synchronized ( this ) {
    				latestTime = new DataTime(new Date(0));
    			}
    		}
    		else {
    			updateTime( dataTimes.get( dataTimes.size()-1 ) );
    		}
    	}
	}
	
	// this is called by Raytheon's MenuUpdater class which processes DataURI 
	// notifications and updates the URICatalog to which this has been added.
	//
	@Override
	public void updateTime(DataTime newTime) {
		synchronized ( this ) {
			if( newTime == null ) {					
				if( latestTime == null ) {
					latestTime = new DataTime(new Date(0));
				}
				else if( !latestTime.isNull() ) {
					out.println("Sanity check: updateTIme for "+ resourceConstraints.toString() +" with null time when the latestTime is already set" );
					/// ??? what does this mean? Shouldn't  happen.
				}
			}
			else if( latestTime == null ) {
				latestTime = newTime;
			}
			// NOTE : I think this will rarely, if ever, matter, but in the case where the valid time is newer but the reftime is older,
			// and choose the time with the most recent refTime.
			//							  
			else if( newTime.getRefTime().getTime() >= 
				latestTime.getRefTime().getTime() && 
					newTime.getValidTime().getTimeInMillis() > 
						latestTime.getValidTime().getTimeInMillis() ) { 

//				out.println("updateTime() for RscName "+ resourceConstraints.toString()+
//						"from :"+ latestTime.toString() + ") -> newTime ("+newTime.toString() );

				latestTime = newTime;
			}
		}
	}
	
}
