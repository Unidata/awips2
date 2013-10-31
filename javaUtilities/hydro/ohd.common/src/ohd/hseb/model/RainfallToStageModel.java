/*
 * Created on Jul 18, 2003
 *
 * 
 */
package ohd.hseb.model;

import ohd.hseb.measurement.*;

/**
 * @author Chip Gobs
 *
 * This class combines the RainfallRunoffModel
 * with a UnitHydrograph and a Rating Curve to output a TimeSeries object.
 */
public interface RainfallToStageModel
{ 
  
  
//  -------------------------------------------------------------	

	
	public RegularTimeSeries calculateStageTimeSeries(long startTime,
											   long endTime,
										       RegularTimeSeries precipTimeSeries,
                                               RegularTimeSeriesHolder runOffTimeSeriesHolder);
									          
		
  
} //end interface RainfallToStageModel