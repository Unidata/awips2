//
// filename: KcApiRainfallRunoffModel.java
// author  : Chip Gobs

// description: This class is used to encapsulate the MKC-API
//

package ohd.hseb.model.mkcapi;

//import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.measurement.*;
import ohd.hseb.model.*;

public class KcApiRainfallRunoffModel implements RainfallRunoffModel
{
    
    //debugging variable from 5/27/04
   // private boolean _oldWay = false;

//*****************************************************************************
//  Private data
//*****************************************************************************
    private boolean _success = true;
    
    private double _thresholdRunoff;
    private double _ffhValue;
   
    private	double _soilMoistureIndex;

    private long _startTime = 0; //starting time of the model states
    private long _tsStartTime = 0; // starting time of the output = startTime + 1hour
    private long _endTime = 0;
    
    private RegularTimeSeries _hourlyMapTs = null;
    private RegularTimeSeries _accumMapTs = null;
  
    private	RegularTimeSeries _accumRunoffTs = null;
    private	RegularTimeSeries _hourlyRunoffTs = null;
    
    // the output dischard time series
    private RegularTimeSeries _hourlyDischargeTs = null;
    
    private static final MeasuringUnit _precipUnit = MeasuringUnit.inches;
    private static final MeasuringUnit _runoffUnit = MeasuringUnit.inches;
    private static final int _millisPerHour = 60 * 60 * 1000;
    
    private double _missingValue = -999.0;

//*****************************************************************************
//  KcApiModel() - constructor has the Threshold Runoff, 1-hour FFH, hourly MAPs,
//	and unit-hydrograph passed in and then.....
//
//*****************************************************************************
   public KcApiRainfallRunoffModel(double tro, double ffh)
   {
       _thresholdRunoff = tro;
       _ffhValue = ffh;
	   
    
   } // end of constructor


    public void setFfhValue(double ffhValue)
    {
        _ffhValue = ffhValue;
    }
    

    public double getFfhValue()
    {
        return _ffhValue;
    }
// *****************************************************************************
//   calcSMI() - method which calculates the Soil Moisture Index
// *****************************************************************************

    public RegularTimeSeries calculateRunoff( long startTime,
                                              long endTime,      
                                              RegularTimeSeries hourlyMapTimeSeries)
    {   



        _startTime = startTime;
        _endTime = endTime;
        
        int intervalInMillis = 1 * _millisPerHour;
        _tsStartTime = _startTime + intervalInMillis;
        
        // the time series start time is one hour after the startTime,
        // which represents the last saved model state time
     


        String header = "KcApiModel.calculateRunoff():";
       /* System.out.println(header + "running the KcApiModel with startTime = " 
                            + DbTimeHelper.getDateTimeStringFromLongTime(startTime) +
                            " endTime = " +
                            DbTimeHelper.getDateTimeStringFromLongTime(endTime));
        */
        
        //note: not doing anything with the evaporationTimeSeries
        // find a better way to package the specific input requirements for
        // the various models
         
        _success = true;
        
        _hourlyMapTs = hourlyMapTimeSeries;
      
        _hourlyMapTs.convert(_precipUnit);
        
       
        // System.out.println(header + " before stretch _hourlyMapTs = " + _hourlyMapTs);
        
           _hourlyMapTs.stretchTimeSeries(_tsStartTime, _endTime, 0.0);

        //System.out.println(header + " after stretch _hourlyMapTs = " + _hourlyMapTs);

     
     //   System.out.println(header + " _thresholdRunoff = " + _thresholdRunoff);

     //   System.out.println(header + " _ffhValue = " + _ffhValue);
     
        

        if (_thresholdRunoff < 0.0)    
            _success = false;
       // else if (_latestFFH < 0.0)
       //     _success = false;
        else if (_hourlyMapTs.getMeasurementCount() < 1)
            _success = false;
        
        if (_success)
             _success = calcSMI();
        
        if (_success)
            _success = calcAccumMap();
        
        if (_success)
            _success = calcAccumRO();
        
        if (_success)
            _success = calcHourlyRO();

    
        return _hourlyRunoffTs;
    } // end of calculateRunoff()
	

//  *****************************************************************************
//    computed() - method which checks if a Discharge TS was computed or not
//  *****************************************************************************
     public boolean computedSuccessfully()
     {
      return _success;

     } // end of computed method


//  *****************************************************************************
//    getSMI() - method which returns the Soil Moisture Index
//  *****************************************************************************
      public double getSMI()
      {
          return _soilMoistureIndex;
      } // end of getSMI method


//  *****************************************************************************
//    getDischargeTS() - method which returns an array which contains the hourly
//    discharge values computed using the MBRFC SMI method
//  *****************************************************************************
      public RegularTimeSeries getDischargeTS()
      {
          if (computedSuccessfully())
              return _hourlyDischargeTs;
          else
              return null;

      }

      public void setThresholdRunoff(double thresholdRunoff)
      {
          _thresholdRunoff = thresholdRunoff;
      }

      public double getThresholdRunoff()
      {
          return _thresholdRunoff;
      }

    
//*****************************************************************************
//  calcSMI() - method which calculates the Soil Moisture Index
//*****************************************************************************
    private boolean calcSMI()
    {
        String header = "KcApiRainfallRunoffModel.calcSMI(): ";    
        double tryThisSMI = 0.18;
	    double runoff = 0.0;
     
       // System.out.println(header + "_ffhValue = " +  _ffhValue );
       // System.out.println(header + "_thresholdRunoff = " +  _thresholdRunoff );
       
     
        runoff = _thresholdRunoff;                
        while (runoff >= _thresholdRunoff)
        {
            tryThisSMI = tryThisSMI + 0.01;
            runoff = runMBRFC_API(tryThisSMI, _ffhValue);
        } 
          
	    _soilMoistureIndex = tryThisSMI;
 
 
 
     // System.out.println(header + "_soilMoistureIndex = " +  _soilMoistureIndex );
        
 
 
	return true;

   } // end of calcSMI method

//*****************************************************************************
//  runMBRFC_API() - method which returns the Runoff given the SMI and precip
//*****************************************************************************
    private double runMBRFC_API(double s, double p)
    {
    	double smi = s;
    	double precip = p;
    
    	double en = 0.0;
    	double d = 0.0;
    	double runoff = 0.0;
    
    	en = 0.89 + (0.63 * smi);
    	d = 0.306 *  Math.pow(smi, 1.865);
    	runoff = Math.pow( ( Math.pow(precip, en) + Math.pow(d, en) ) , (1.0/en) ) - d;
    
    	return runoff;

    } // end of calcSMI method

//*****************************************************************************
//  calcAccumMap() - method which takes the individual hourly MAPs and
//	calculates the accumulated MAPs for each hour
//*****************************************************************************
   private boolean calcAccumMap()
   {
       String header = "KcApiModel.calcAccumMap()";
      
      // long startTime = _tsStartTime;
      // long endTime = _endTime;

       // old way fixed 5/27/04 
       
       long startTime = _tsStartTime;
       long endTime = _endTime; 
       

       MeasuringUnit hourlyMapTsUnit = _hourlyMapTs.getMeasuringUnit();

       _accumMapTs = new RegularTimeSeries(startTime, endTime, 1, hourlyMapTsUnit);
        
       Measurement accumMeasurement = null;
       
       
       //initialize the first accumulated value to the first hourly value
        
       Measurement firstHourlyMeasurement = _hourlyMapTs.getMeasurementByTime(startTime);
       Measurement firstAccumMeasurement = new Measurement(firstHourlyMeasurement);
        
       _accumMapTs.setMeasurementByTime(firstAccumMeasurement, startTime);
       
       double hourlyValue = 0.0;
       double previousAccumValue = 0.0;
       
       final long millisPerHour = 1000 * 60 * 60;
       
       // load the time series with accumulated precip values
       for (long time=startTime+millisPerHour ; time <= endTime; time+= millisPerHour)
       {
          
           
           hourlyValue = _hourlyMapTs.getMeasurementValueByTime(time, _precipUnit);
           previousAccumValue = _accumMapTs.getMeasurementValueByTime(time-millisPerHour, _precipUnit);
           
           double newValue = 0.0;
           
           if (hourlyValue > 0.0)
           {
               newValue = previousAccumValue + hourlyValue;   
           }
           else
           {
               newValue = previousAccumValue;
           }
                  
        
         // This was just a coding error.  It did not cause a problem since the
         // values were the same anyway - INCHES
         // REPLACED 5/27/04 Measurement newMeasurement = new Measurement(newValue, _hourlyMapTsUnit);    
           Measurement newMeasurement = new Measurement(newValue, _precipUnit);
           
           _accumMapTs.setMeasurementByTime(newMeasurement, time);
             
       } //end accum for loop
	
       return true; 
   } // end of calcAccumMap method


//*****************************************************************************
//  calcAccumRO() - method which 
//*****************************************************************************
    private boolean calcAccumRO()
    {  
        String header = "KcApiRainfallRunoffModel.calcAccumRO(): ";
        
        
//      this block commented out 5/27/04 from KcApiRainfallRunoffModel.calcAccumRO()
        //long startTime = _accumMapTs.getStartTime();
       // long endTime = _accumMapTs.getEndTime();
        
        // this block added 5/27/04 to KcApiRainfallRunoffModel.calcAccumRO()     
        long  startTime = _tsStartTime;
        long  endTime = _endTime; 
        
       
 
        
        double accumRunoffValue = 0.0;     
        long intervalInMillis = _millisPerHour;
          
        _accumRunoffTs = new RegularTimeSeries(startTime, endTime, 1, _runoffUnit);
	           
        for (long time = startTime; time  <= endTime; time += intervalInMillis)
 	    {
            //double accumMapValue = getMeasurementValueByIndex(_accumMapTs, i, _precipUnit);
		    double accumMapValue = _accumMapTs.getMeasurementValueByTime(time, _precipUnit);
            
            if (accumMapValue < 0.0)
            {
                accumMapValue = 0.0;
            }
           
            accumRunoffValue = runMBRFC_API(_soilMoistureIndex, accumMapValue);
            Measurement accumRunoffMeasurement = new Measurement(accumRunoffValue, _runoffUnit); 
            _accumRunoffTs.setMeasurementByTime(accumRunoffMeasurement, time);
	    }
        
       
    
        return true;

    } // end of calcAccumRO method


//*****************************************************************************
//  calcHourlyRO() - method which 
//*****************************************************************************
   private boolean calcHourlyRO()
   {
       String header = "KcApiModel.calcHourlyRO(): ";

        long startTime = _accumRunoffTs.getStartTime();
        long endTime = _accumRunoffTs.getEndTime();
        
       _hourlyRunoffTs = new RegularTimeSeries(startTime, endTime, 1, _runoffUnit);
       
    
	
       double accumValue = _accumRunoffTs.getMeasurementValueByTime( startTime, _runoffUnit);
       Measurement accumMeasurement = new Measurement(accumValue, _accumRunoffTs.getMeasuringUnit());
	   _hourlyRunoffTs.setMeasurementByTime(accumMeasurement, startTime);
       

	   for (int i=1; i<_accumRunoffTs.getMeasurementCount(); i++)
	   {
           double currentAccumValue = getMeasurementValueByIndex(_accumRunoffTs, i, _runoffUnit);
           double previousAccumValue = getMeasurementValueByIndex(_accumRunoffTs, i-1, _runoffUnit);
           
           double runoffValue = currentAccumValue - previousAccumValue;
           
      //     System.out.println(header + " _accumRunoffTs.getMeasuringUnit() = " +  _accumRunoffTs.getMeasuringUnit());
 
           
           Measurement runoffMeasurement = new Measurement(runoffValue, 
                                                        _accumRunoffTs.getMeasuringUnit());
            
           long time = _hourlyRunoffTs.getMeasurementTimeByIndex(i);
           _hourlyRunoffTs.setMeasurementByTime(runoffMeasurement, time);
           //String timeString = DbTimeHelper.getDateTimeStringFromLongTime(time);
           
           /*
           if (runoffValue != 0.0)
           {
                System.out.println(header + " runoffMeasurement = " + 
                              runoffMeasurement + " at " + timeString);
           }
           */
	   }
       
       //System.out.println(header + "bug version = " + _oldWay);
      // System.out.println(header + "_accumMapTs      = " + _accumMapTs);
      // System.out.println(header + "_accumRunoffTs   = " + _accumRunoffTs);
      // System.out.println(header + "_hourlyRunoffTs  = " +  _hourlyRunoffTs);
      // System.out.println(header + " _accumRunoffTs = " +  _accumRunoffTs);
 
        
       return true;

   } // end of calcHourlyRO method

   // ---------------------------------------------------------------------------
   private double getMeasurementValueByIndex(RegularTimeSeries ts, int index, MeasuringUnit unit)
   {
       double value = _missingValue;
            
       Measurement m = ts.getMeasurementByIndex(index);
        
       if (m != null)
       {
           if (! m.isMissing() )
           {
               m = m.getCopy(unit);
               value = m.getValue();    
           }
           else
           {
               value = _missingValue;  //set this class's version of a missing value   
           }       
       }
       
       return value; 
   }

   
   public RainfallRunoffModelType getModelType()
   {
       return RainfallRunoffModelType.API_MKC;
   }
   

} // end of KcApi class
