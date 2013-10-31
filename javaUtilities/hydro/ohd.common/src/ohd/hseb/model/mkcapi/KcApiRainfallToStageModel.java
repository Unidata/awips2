/*
 * Created on Jul 18, 2003
 *
 * 
 */
package ohd.hseb.model.mkcapi;

import ohd.hseb.measurement.*;
import ohd.hseb.model.*;
/**
 * @author Chip Gobs
 *
 * This class combines the RainfallRunoffModel
 * with a UnitHydrograph and a Rating Curve to output a TimeSeries object.
 */
public class KcApiRainfallToStageModel implements RainfallToStageModel
{ 
    private RainfallRunoffModel _rainfallRunoffModel = null;
    private ForecastAdjuster _forecastAdjuster = null;
    
	private RatingCurve _ratingCurve = null;
	private UnitHydrograph _unitgraph = null;
    
    private final static long _millisPerHour = 60 * 60 * 1000;
    
    private MeasurementHolder _baseStageMeasurementHolder = null;
    private IrregularTimeSeriesHolder _observedStageTimeSeriesHolder = null;
    private RegularTimeSeriesHolder _simulatedDischargeTimeSeriesHolder = null;
    
    
    private final static MeasuringUnit _stageUnit = MeasuringUnit.feet;
    private final static MeasuringUnit _dischargeUnit = MeasuringUnit.cfs;
   
//  -------------------------------------------------------------	

	public KcApiRainfallToStageModel(RainfallRunoffModel rainfallRunoffModel,
						        UnitHydrograph unitgraph,
                                RatingCurve ratingCurve,
                                MeasurementHolder baseStageMeasurementHolder,
                                IrregularTimeSeriesHolder observedStageTimeSeriesHolder,
                                ForecastAdjuster forecastAdjuster,
                                RegularTimeSeriesHolder simulatedDischargeTimeSeriesHolder)
	{
		
        _rainfallRunoffModel = rainfallRunoffModel;
        
		_ratingCurve = ratingCurve;
		_unitgraph = unitgraph;
        
        _baseStageMeasurementHolder = baseStageMeasurementHolder;
        
        _observedStageTimeSeriesHolder = observedStageTimeSeriesHolder;
        
		_forecastAdjuster = forecastAdjuster;
        _simulatedDischargeTimeSeriesHolder = simulatedDischargeTimeSeriesHolder;
		
	} //end RainfallToStageModel()
	
    //	-------------------------------------------------------------

	public RegularTimeSeries calculateStageTimeSeries(long startTime,
											   long endTime,
										       RegularTimeSeries precipTimeSeries,
                                               RegularTimeSeriesHolder runoffTimeSeriesHolder)
									        
                                             
	{
		
        
        Measurement baseflowMeasurement = null;
        
        //get the baseflow measurement
        if (_ratingCurve != null)
        {
            double stageValue = _baseStageMeasurementHolder.getMeasurement().getValue(_stageUnit);
            double dischargeValue = _ratingCurve.getDischargeFromStage(stageValue);
            baseflowMeasurement = new Measurement(dischargeValue, _dischargeUnit); 
        }
        String header = "KcApiRainfallToStageModel.calculateStageTimeSeries():";
        
		int intervalInHours = 1;
        long intervalInMillis = _millisPerHour * intervalInHours;
		double evaporationAmount = 0.0;
		double precipAmount = 0.0;
        
        long tsStartTime = startTime + intervalInMillis;

	      
      /*     
        System.out.println(header + "startTime = " + 
                           DbTimeHelper.getDateTimeStringFromLongTime(startTime) +
                           " endTime = " +
                           DbTimeHelper.getDateTimeStringFromLongTime(endTime));                
      */

     //  System.out.println(header + "precipTimeSeries = " + precipTimeSeries);
      //  System.out.println(header + "evap TimeSeries = " + potentialEvapTimeSeries);
        

     

        RegularTimeSeries runoffTs =
               _rainfallRunoffModel.calculateRunoff(startTime,
                                                    endTime,
                                                    precipTimeSeries);
                                 
        runoffTimeSeriesHolder.setTimeSeries(runoffTs);                            
                                                    
        //System.out.println(header + "--------------------------------");                                           
       // System.out.println(header + "runoffTs = " + runoffTs);                                             
  	            
      
        RegularTimeSeries totalFlowTimeSeries =
                                new RegularTimeSeries(tsStartTime, endTime,
                                        intervalInHours, MeasuringUnit.cfs);
      
      
        
       // System.out.println(header + "after unithydrograph, prior total flow time series " + totalFlowTimeSeries);
      
        //use newly calculated runoff
        totalFlowTimeSeries =
                         _unitgraph.calculateTotalDischargeTs("new ",   
                                            totalFlowTimeSeries, 
                                            runoffTs,
                                            tsStartTime,
                                            endTime,
                                            intervalInMillis);        
                            
       
       
        //System.out.println(header + "after unithydrograph, new total flow time series " + totalFlowTimeSeries);
       
        // create a test baseFlow time series
        RegularTimeSeries baseFlowTimeSeries = 
                      new RegularTimeSeries(tsStartTime, endTime, 
                                              intervalInHours,
                                              MeasuringUnit.cfs,
                                              baseflowMeasurement.getValue()
                                              );
         
       // System.out.println(header + "base flow time series " + baseFlowTimeSeries);
        
            
        // add the baseflow to the current total flow
        totalFlowTimeSeries = RegularTimeSeries.add(totalFlowTimeSeries, baseFlowTimeSeries);
        
         
    
       // System.out.println(header + "after addition of baseflow, new total flow time series " + totalFlowTimeSeries);
        
        
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
    
        
        
        RegularTimeSeries stageTimeSeries = 
            _ratingCurve.getStageRegularTimeSeries(totalFlowTimeSeries);

	   return stageTimeSeries;
		
	} //end getStageTimeSeries
	
//	---------------------------------------------------------------------
    
    

}
