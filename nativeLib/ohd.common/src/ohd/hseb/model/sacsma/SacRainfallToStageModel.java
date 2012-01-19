/*
 * Created on Jul 18, 2003
 *
 * 
 */
package ohd.hseb.model.sacsma;

import ohd.hseb.measurement.*;
import ohd.hseb.model.*;
//import ohd.hseb.util.CodeTimer;

/**
 * @author Chip Gobs
 *
 * This class combines the RainfallRunoffModel
 * with a UnitHydrograph and a Rating Curve to output a TimeSeries object.
 */
public class SacRainfallToStageModel implements RainfallToStageModel
{ 
    private RainfallRunoffModel _rainfallRunoffModel = null;
    private ForecastAdjuster _forecastAdjuster = null;
 
	private RatingCurve _ratingCurve = null;
	private UnitHydrograph _unitgraph = null;
    
    private RegularTimeSeriesHolder _priorRunoffTimeSeriesHolder = null;    
    private IrregularTimeSeriesHolder _observedStageTimeSeriesHolder = null;
    private RegularTimeSeriesHolder _simulatedDischargeTimeSeriesHolder = null;
    
    private final static long _millisPerHour = 60 * 60 * 1000;
  
//  -------------------------------------------------------------	

	public SacRainfallToStageModel(RainfallRunoffModel rainfallRunoffModel,
						        UnitHydrograph unitgraph,
                                RatingCurve ratingCurve,
                                RegularTimeSeriesHolder priorRunoffTimeSeriesHolder,
                                IrregularTimeSeriesHolder observedStageTimeSeriesHolder,
                                ForecastAdjuster forecastAdjuster,
                                RegularTimeSeriesHolder simulatedDischargeTimeSeriesHolder)
	{
		
        _rainfallRunoffModel = rainfallRunoffModel;
        
		_ratingCurve = ratingCurve;
		_unitgraph = unitgraph;
        
        _priorRunoffTimeSeriesHolder = priorRunoffTimeSeriesHolder;
        
        
        // forecast adjustment variables
        _observedStageTimeSeriesHolder = observedStageTimeSeriesHolder;
        _forecastAdjuster = forecastAdjuster;
        _simulatedDischargeTimeSeriesHolder = simulatedDischargeTimeSeriesHolder;
		
		
	} //end RainfallToStageModel()
	
    //	-------------------------------------------------------------

	public RegularTimeSeries calculateStageTimeSeries(long startTime,
											   long endTime,
										       RegularTimeSeries precipTimeSeries,
                                               RegularTimeSeriesHolder runoffTimeSeriesHolder
                                               )
									          
	{
		
        String header = "SacRainfallToStageModel.calculateStageTimeSeries():";
        
         
        RegularTimeSeries priorRunoffTimeSeries = _priorRunoffTimeSeriesHolder.getTimeSeries();
        
		int intervalInHours = 1;
        long intervalInMillis = _millisPerHour * intervalInHours;
		double evaporationAmount = 0.0;
		double precipAmount = 0.0;
        
        long tsStartTime = startTime + intervalInMillis;
    

     

        RegularTimeSeries runoffTs =
               _rainfallRunoffModel.calculateRunoff(startTime,
                                                    endTime,
                                                    precipTimeSeries);
                                 
        runoffTimeSeriesHolder.setTimeSeries(runoffTs);                            
    
    
        RegularTimeSeries totalFlowTimeSeries =
                                new RegularTimeSeries(tsStartTime, endTime,
                                        intervalInHours, MeasuringUnit.cfs);
      
      
        //note: the previous totalFlowTimeSeries will be ADDED(math) to the flow output
        // from the unit hydrograph
      
        //use prior runoff
        
        long priorRunoffStartTime = startTime - 
                                   (_unitgraph.getOrdinateCount() * _millisPerHour);
                                   
        long priorRunoffEndTime = startTime; 
        totalFlowTimeSeries =
                         _unitgraph.calculateTotalDischargeTs("prior ",
                                                    totalFlowTimeSeries, 
                                                    priorRunoffTimeSeries,
                                                    priorRunoffStartTime,
                                                    priorRunoffEndTime,
                                                    intervalInMillis);        
      
      
    //    System.out.println(header + "after _unitgraph.calculateTotalDischargeTs(), prior total flow time series\n " + totalFlowTimeSeries);
      
        //use newly calculated runoff
        totalFlowTimeSeries =
                          _unitgraph.calculateTotalDischargeTs("new ",   
                                            totalFlowTimeSeries, 
                                            runoffTs,
                                            tsStartTime,
                                            endTime,
                                            intervalInMillis);        
        
        
        
                                
    //    System.out.println(header + "after _unitgraph.calculateTotalDischargeTs(), new total flow time series\n " + totalFlowTimeSeries);
        
        //lop off the hours at the beginning that are before the model run start time
        totalFlowTimeSeries.trimTimeSeriesAtStart(tsStartTime);
      
     
        //store the simulation results
        _simulatedDischargeTimeSeriesHolder.setTimeSeries(totalFlowTimeSeries);
       
        // forecast adjustment
        if (_forecastAdjuster.getParams().shouldDoAdjustment())
        {
           // CodeTimer timer = new CodeTimer();
            //timer.start();
            IrregularTimeSeries observedStageTimeSeries = _observedStageTimeSeriesHolder.getTimeSeries();
            IrregularTimeSeries observedDischargeTimeSeries = _ratingCurve.getDischargeIrregularTimeSeries(observedStageTimeSeries);
            totalFlowTimeSeries = _forecastAdjuster.getAdjustedTimeSeries(observedDischargeTimeSeries, totalFlowTimeSeries);   
           // timer.stop(" the forecast adjustment took ");
        }
        
        
    //    System.out.println(header + "after totalFlowTimeSeries.trimTimeSeriesAtStart(), new total flow time series\n " + totalFlowTimeSeries);
 
        
        // put through the rating curve to get the stages
        RegularTimeSeries stageTimeSeries = 
            _ratingCurve.getStageRegularTimeSeries(totalFlowTimeSeries);

		return stageTimeSeries;
		
	} //end getStageTimeSeries
	
//	---------------------------------------------------------------------
    
   
   

}
