package gov.noaa.nws.ncep.viz.common;

import java.util.ArrayList;
import java.util.Date;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;


/**
 *
 *  Model grid attribute sets must allow for a GDATTIM parameter that allows users to control 
 *  the default timeline tic mark pre-selection in terms of forecast hours.
 *  Typical use cases would be for precipitation (omit the first forecast hour F00), or when 
 *  the desk only performs analyses at F06, F12, F18 and F24 but the model data is of a much 
 *  higher frequency. These are just two examples.
 *  
 *  Valid formats are numerous but currently only the following are implemented:
 *  
 *     LIST: GDATTIM=F1;F2;F3; ... ;Fn (forecast hours separated by semicolons,
 *     RANGE: GDATTIM=F1-F2[-INCR] (forecast hour range with an increment)
 *     ALL: GDATTIM=FALL (all forecast hours)
 *     
 *     Examples:
 *        GDATTIM=F06;F12;F18;F24
 *        GDATTIM=F06-F24-6 (same as above)
 *        GDATTIM=F12-F48-12
 *         
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	    Description
 * ------------	----------	---------------	--------------------------
 * 11/28/11       #518      Greg Hull       created
 * 04/24/13		  689	    Xiaochuan		Add the frame should include end of time.
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */

public class SelectableFrameTimeMatcher {

	private boolean matchAll = false;
	
	private final Integer MAX_FCST_HR = 384;
	
	private ArrayList<DataTime> selectableTimes = new ArrayList<DataTime>();
	

	
	public SelectableFrameTimeMatcher( String timeMatchStr ) throws VizException {
		
		// parse the string and create a list of times 
		//
		if( timeMatchStr == null || timeMatchStr.isEmpty() ) {
			throw new VizException("SelectableFrameTimeMatcher: null/empty string");
		}
		
		timeMatchStr = timeMatchStr.toLowerCase().trim();
		
		if( timeMatchStr.equals( "fall") ) {
			matchAll = true;
			return;
		}
		
		String[] subStrs = timeMatchStr.split(";");
		
		// each semi-colon delimited subString may be either
		// a range or single time.
		//
		for( String subStr : subStrs ) {
			
			// if matching a range of times
			//
			if( subStr.indexOf('-') > 0 ) {
				String startEndIncrStrs[] = subStr.split("-");
				if( startEndIncrStrs.length < 2 ||
					startEndIncrStrs.length > 3 ) {
//					System.out.println("Error Parsing GDATTIME string "+timeMatchStr );
					throw new VizException("SelectableFrameTimeMatcher: Error Parsing GDATTIME string: "
							+ timeMatchStr);
				}
				
				DataTime startTime = parseTime( startEndIncrStrs[0].trim() );
				DataTime endTime = parseTime( startEndIncrStrs[1].trim() );
				
				if( startTime == null || endTime == null ) {
					continue;
				}
				
				Integer incr = 60*60;

				if( startEndIncrStrs.length > 2 ) {	
					incr = Integer.parseInt( startEndIncrStrs[2].trim() ) * 60 * 60;
				}
				
				// Note : this logic will need to change when we add parsing of
				// full GDATTIM syntax. 
				for( Integer f = startTime.getFcstTime() ; f <= endTime.getFcstTime() ; f += incr ) {
					selectableTimes.add( 
							new DataTime( startTime.getRefTime(), f ));		

				}
				
			}
			else { // else if matching a single time
				DataTime dt = parseTime( subStr.trim() );
				
				if( dt != null ) {
					selectableTimes.add( dt );
				}
			}			
		}
	}
	
	// Currently only parsing forecast times. 
	//
	private DataTime parseTime( String timeStr ) throws VizException {
		//DataTime parsedTime = new DataTime();
		
		if( timeStr.startsWith("f") ) {
			Integer fcstHr = Integer.parseInt( timeStr.substring(1) );
			
			// 'null' refTime and fcst time in seconds
			return new DataTime( new Date(0), fcstHr*60*60 ); //			
		}
		else if( timeStr.equals("last") ) {
			// 'null' refTime and fcst time in seconds
			return new DataTime( new Date(0), MAX_FCST_HR*60*60 ); //						
		}
		else {
//			System.out.println("SelectableFrameTimeMatcher can't parse GDATTIM token"+timeStr);
			throw new VizException("SelectableFrameTimeMatcher: Error Parsing GDATTIME token: "
					+ timeStr);
		}
	}
	
	public boolean matchTime( DataTime dataTime ) {
		if( matchAll ) { 
			return true;
		}
		
		for( DataTime dt : selectableTimes ) {
			
			// TODO : add check for ref time later. For now just test the forecast times.
			//
//			if( dt.getRefTime() != null ) {
//				
//			}
			
			if( dt.getFcstTime() == dataTime.getFcstTime() ) {
				return true;
			}			
		}
		
		return false;		
	}
}

