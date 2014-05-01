/*
 * Created on Jun 23, 2003
 *
 * 
 */
package ohd.hseb.model.sacsma;
import java.util.*;
import java.io.*;
import java.text.*;

import ohd.hseb.model.*;
import ohd.hseb.measurement.*;
import ohd.hseb.util.*;

/**
 * @author Chip Gobs
 *
 * 
 */
public class SacSmaTest 
{
    
    private static boolean _newTest = true;
        

//----------------------------------------------
    public static void main(String[] args) throws Exception
    {
    	Logger logger = new FileLogger("D:/Data/code/SiteSpecific/output/SacSmaTiming.out");
    	
    	logger.log("log this");
    	
    	CodeTimer timer = new CodeTimer(logger);
	    timer.start();
	    
	    testModel();
	    
	    timer.stop("Done with 1 iteration of testModel");	
     
       // testAll();
    	
	} //end main
	
	//---------------------------------------------------------------------------
	
    //---------------------------------------------------------------------------
    

	//-----------------------------------------------------------------------
    /*
     * 
     * 
     * 
     
        PXADJ =   1.000
        PEADJ =   1.000
        UZTWM =  60.000
        UZFWM =  25.000
        UZK   =   0.350
        PCTIM =   0.000
        ADIMP =   0.000
        RIVA  =   0.003
        ZPERC =  210.000
        REXP  =   1.40
        LZTWM =  120.000
        LZFSM =  25.000
        LZFPM = 150.000
        LZSK  =   0.080
        LZPK  =   0.004
        PFREE =   0.250
        RSERV =   0.300
        SIDE  =   0.000
        IOPTET=0  //we don't have this one, so ignore it
        EFC   =   0.50
        
        
        use these as the starting states (put them in the database)
        
         UZTWC  UZFWC  LZTWC  LZFSC  LZFPC  ADIMC
         31.02  0.009  31.94  0.060  74.74  62.04
       */
       
      //  -----------------------------------------------------------------------
  
      public static void testModel()
      {
         
              SacSmaParameters params = null;
              SacSmaState state = null;
              SacSmaRainfallRunoffModel model = null;
          
              RegularTimeSeriesHolder evapTimeSeriesHolder = new RegularTimeSeriesHolder();
        
              int hoursPerInterval = 1;
              double evaporationAmount = 0.0;
              double precipAmount = 0.0;
        
        
              double[] precipArray = null;
              double[] evaporationArray = null;
        
              String dataDir = null;
     
              try
              {
               
                  if (_newTest) //test against data provided by Mike Smith
                  {
                      
                      dataDir = "D:/Data/code/SiteSpecific/input/States_params/";
                              
                      params = getTestParams();
                      state =  getTestState();
      
                       
                      precipArray = readPrecipArray(dataDir + "precip.in.txt");
                      evaporationArray = readEvapArray(dataDir + "et_demand.txt");
                      //evaporationArray = readEvapArray(dataDir + "actual_et.txt");
                  }
                  else //test against the data provided by LMRFC
                  {
                      dataDir = "D:/Data/code/exampleCode/sacsma/";
                      double[] parameterArray = readParameterArray(dataDir + "parmsandco");
                      double[] stateArray = readStateArray(dataDir + "parmsandco");
              
                      params = new SacSmaParameters(parameterArray);
                      state = new SacSmaState(stateArray);
  
                      precipArray = readPrecipArray(dataDir + "rainfile");
                      evaporationArray = readEvapArray(dataDir + "evapfile");
                  }
                  
                  
                  RegularTimeSeries evapTimeSeries = getEvapTimeSeries(evaporationArray);
                  evapTimeSeriesHolder.setTimeSeries(evapTimeSeries);
              
                  boolean useMapeTimeSeries = true;
              
                  double[] values = {1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 };
                  MonthlyValues mapeAdjustmentValues = new MonthlyValues(values);
              
                  double peAdjust = 1.0;
                  double pxAdjust = 1.0;
              
                  model = new SacSmaRainfallRunoffModel(state,
                                          params, 
                                          peAdjust,
                                          pxAdjust,
                                          useMapeTimeSeries,
                                          mapeAdjustmentValues,
                                          evapTimeSeriesHolder);
              }
              catch (java.io.IOException e)
              {
                  e.printStackTrace();
                  System.exit(1);
              }
       
              int length = Math.min(precipArray.length, evaporationArray.length);
        
              String formatString = "#####.000";
              NumberFormat formatter = new DecimalFormat(formatString);
        
              System.out.println("Java version of SacSma:");
        
              PrintWriter runoffWriter = null;
              PrintWriter internalsWriter = null;
        
              try
              {
                runoffWriter =
                      new PrintWriter(new FileOutputStream("D:/Data/code/SiteSpecific/output/java_runoff.out"));
    
                internalsWriter = 
                      new PrintWriter(new FileOutputStream("D:/Data/code/SiteSpecific/output/java_internals.out"));
              }
              catch(java.io.IOException e)
              {
                  System.out.println("Unable to open the output files.");
                  System.exit(1);
              }
        
              String runoffMessage = null;
              String internalsMessage = null;

              internalsMessage = "at startup: " + 
                                 getStateString(model.getState()) +
                                 model.getParameters().toString();
                                         
              System.out.println(internalsMessage);
              internalsWriter.println(internalsMessage);
              
              internalsWriter.println("DAY  HR  UZTWC  UZFWC  LZTWC  LZFSC  LZFPC  ADIMC  RAIN+MELT ACT-ET TOT-RO ");


              int day = 1;
              int hour = 1;

              //for each piece of input data
              for (int i = 0 ; i < length; i++)
              {
 
                   precipAmount = precipArray[i];
                   evaporationAmount = evaporationArray[i];
             
                   double totalChannelInflow = 
                          model.calculate(hoursPerInterval/ 24.0,
                                        precipAmount,
                                        evaporationAmount);
  
  
                   if (_newTest )
                   {
  
                       runoffMessage = "day = " + day +
                                   " hour = " + hour +
                                   " px = " + precipAmount +
                                   " evap = " + evaporationAmount +
                                   " runoff = " + formatter.format(totalChannelInflow);
                   }
                   else //old test with LMRFC's data
                   {
                       runoffMessage = "i = " + i +
                                " px = " + precipAmount +
                                " evap = " + evaporationAmount +
                                " runoff = " + formatter.format(totalChannelInflow);                 
                   }                
                          
                  
            
                   System.out.println(runoffMessage);
                   runoffWriter.println(runoffMessage);
                   //System.out.println(model);
             
        

                   internalsMessage =  day + "     " + hour + 
                                     "   " + getStateString(model.getState()) + "   " +
                                    precipAmount + "   " +
                                    evaporationAmount + "   " +
                                    formatter.format(totalChannelInflow);
                                         
                  // System.out.println(internalsMessage);
                   internalsWriter.println(internalsMessage);
             
                  hour++;
                  if (hour > 24)
                  {
                      hour = 1;
                      day++;
                  } 
             
        
              } //end for
        
              runoffWriter.close();
              internalsWriter.close();
        
          } //end testModel   
           
      
	//-----------------------------------------------------------------------  
    private static SacSmaParameters getTestParams()
    {
        //uses sept93 data for ELDO2)
            /*
        PXADJ =   1.000
              PEADJ =   1.000
              UZTWM =  60.000
              UZFWM =  25.000
              UZK   =   0.350
              PCTIM =   0.000
              ADIMP =   0.000
              RIVA  =   0.003
              ZPERC =  210.000
              REXP  =   1.40
              LZTWM =  120.000
              LZFSM =  25.000
              LZFPM = 150.000
              LZSK  =   0.080
              LZPK  =   0.004
              PFREE =   0.250
              RSERV =   0.300
              SIDE  =   0.000
              IOPTET=0  //we don't have this one, so ignore it
              EFC   =   0.50
            */
            
        SacSmaParameters params = new SacSmaParameters();   
        
        params.setPxadj(1.0);
        params.setPeadj(1.0);
        params.setUztwm(60.0);
        params.setUzfwm(25.0);
        params.setUzk(0.35);
        params.setPctim(0.000);
        params.setAdimp(0.000);
        params.setRiva(0.003);
        params.setZperc(210.000);
        params.setRexp(1.40);
        params.setLztwm(120.000);
        params.setLzfsm(25.0);
        params.setLzfpm(150.000);
        params.setLzsk(0.080);
        params.setLzpk(0.004);
        params.setPfree(0.250);
        params.setRserv(0.300);
        params.setSide(0.000);
        params.setEfc(.50);
        
        return params;
    }
    
 //  -----------------------------------------------------------------------  
    private static SacSmaState getTestState()
    {
         /*
        use these as the starting states (put them in the database)
        
              UZTWC  UZFWC  LZTWC  LZFSC  LZFPC  ADIMC
              31.02  0.009  31.94  0.060  74.74  62.04
            */
        /*
          
          Since the sac93_states test data does not explicitly give initial conditions,
          I have reconstructed the initial conditions by using the 0+1 states and
          using numeric analysis approximated the previous state (0)          
         
          The xxxxxDiff variables show the approximate change in the state variables per iteration
          for a particular
          calibration when there is no precip (which is the case for the first 10 or so intervals).
           
         */    
         
        double uztwcDiff = -0.103;    
        double uzfwcDiff = 0;
        double lztwcDiff = -0.02;
        double lzfscDiff = -0.0002;
        double lzfpcDiff = -0.0133;
        double adimcDiff = -0.12;
            
        SacSmaState state = new SacSmaState();
        
        // started with state 1, go back to the approximate state 0
        state.setUztwc(31.02 - uztwcDiff );
        state.setUzfwc(0.009 - uzfwcDiff );
        state.setLztwc(31.94 - lztwcDiff);
        state.setLzfsc(0.060 - lzfscDiff);
        state.setLzfpc(74.74 - lzfpcDiff );
        state.setAdimc(62.04 - adimcDiff);

        return state;
        
    }
    //  -----------------------------------------------------------------------  
	public static void testAll() throws Exception
	{
		 
		  double[] stateArray = null;
		  double[] parameterArray = null;
		  
		  SacSmaParameters params = null;
		  SacSmaState state = null;
		  SacSmaRainfallRunoffModel model = null;

          RegularTimeSeriesHolder evapTimeSeriesHolder =
                                 new RegularTimeSeriesHolder();
    
		
		  int hoursPerInterval = 1;
		  double evaporationAmount = 0.0;
		  double precipAmount = 0.0;
	
	
		  double[] precipArray = null;
		  double[] evaporationArray = null;
		  
		  UnitHydrograph ugh = UnitHydrograph.getTestUnitHydrograph();
		  IrregularTimeSeries currentTimeSeries = null;
		  IrregularTimeSeries totalTimeSeries = 
		                      new IrregularTimeSeries(MeasuringUnit.cfs);
    
 
		  try
		  {
			  parameterArray = readParameterArray("D:/Data/exampleCode/sacsma/parmsandco");
			  stateArray = readStateArray("D:/Data/exampleCode/sacsma/parmsandco");
			  
			  params = new SacSmaParameters(parameterArray);
			  state = new SacSmaState(stateArray);
  
          
              precipArray = readPrecipArray("D:/Data/exampleCode/sacsma/rainfile");
              evaporationArray = readEvapArray("D:/Data/exampleCode/sacsma/evapfile");

              RegularTimeSeries evapTimeSeries = getEvapTimeSeries(evaporationArray);
              evapTimeSeriesHolder.setTimeSeries(evapTimeSeries);
              
              
              boolean useMapeTimeSeries = true;
              
              double[] values = {1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 };
              MonthlyValues mapeAdjustmentValues = new MonthlyValues(values);
              
              double peAdjust = 1.0;
              double pxAdjust = 1.0;
              
              
              model = new SacSmaRainfallRunoffModel(state,
                                      params, 
                                      peAdjust,
                                      pxAdjust,
                                      useMapeTimeSeries,
                                      mapeAdjustmentValues,
                                      evapTimeSeriesHolder);
	
		  }
		  catch (java.io.IOException e)
		  {
			  e.printStackTrace();
			  System.exit(1);
		  }
   
		  int length = Math.min(precipArray.length, evaporationArray.length);
	
		  String formatString = "#####.000";
		  NumberFormat formatter = new DecimalFormat(formatString);
	
		  System.out.println("Java version of SacSma:");
	
		  PrintWriter runoffWriter = null;
		  PrintWriter internalsWriter = null;
	
		  try
		  {
			runoffWriter =
				  new PrintWriter(new FileOutputStream("D:/Data/code/SiteSpecific/output/java_runoff.out"));

			internalsWriter = 
				  new PrintWriter(new FileOutputStream("D:/Data/code/SiteSpecific/output/java_internals.out"));
		  }
		  catch(java.io.IOException e)
		  {
			  System.out.println("Unable to open the output files.");
			  System.exit(1);
		  }
	
		  String runoffMessage = null;
		  String internalsMessage = null;

		  internalsMessage = "at startup: " + 
							 getStateString(model.getState()) +
							 model.getParameters().toString();
                                     
		  System.out.println(internalsMessage);
		  internalsWriter.println(internalsMessage);

		  long time = System.currentTimeMillis();
			
		  //for each piece of input data
		  for (int i = 0 ; i < length; i++)
		  {
 
		      precipAmount = precipArray[i];
			  evaporationAmount = evaporationArray[i];
    	     
			  double totalChannelInflow = 
						  model.calculate(hoursPerInterval/ 24.0,
										precipAmount,
										evaporationAmount);
  
  				 
			  AbsTimeMeasurement runoff = new AbsTimeMeasurement(totalChannelInflow, time, MeasuringUnit.mm);
			  time += 60 *60 * 1000; //add an hour
			  currentTimeSeries = ugh.getFlowTimeSeries(runoff);
  
			  totalTimeSeries = IrregularTimeSeries.add(totalTimeSeries, currentTimeSeries);
  
			  runoffMessage = "i = " + i + " px = " + precipAmount +
									  " evap = " + evaporationAmount +
									  "  runoff = " +
									  formatter.format(totalChannelInflow);
                           
                      
                           
			  System.out.println(runoffMessage);
			  runoffWriter.println(runoffMessage);
			  //System.out.println(model);
		 
			  internalsMessage = getStateString(model.getState());
                                     
			  System.out.println(internalsMessage);
			  internalsWriter.println(internalsMessage);
		 
		  } //end for
	
		  runoffWriter.close();
		  internalsWriter.close();
	
	  } //end testAll
    
    
 // -------------------------------------------------------------
	
//	-------------------------------------------------------------
/*       
	private static String getStateStringVerbose(SacSmaState state)
	{
		   String formatString = "#####.000";
		   NumberFormat format = new DecimalFormat(formatString);
    	
		   String string = " uztwc = " + format.format(state.getUztwc())  + 
						   " uzfwc = " + format.format(state.getUzfwc())  +
		   " lztwc = " + format.format(state.getLztwc())  + "\n" +
		   " lzfsc = " + format.format(state.getLzfsc())  + 
		   " lzfpc = " + format.format(state.getLzfpc())  +
		   " adimc = " + format.format(state.getAdimc())  + "\n";
           
           
		   //" rsum =  " + state.getRsumString()  +  "\n";
		

		   return string;
	   }
*/
//	----------------------------------------------
private static String getStateString(SacSmaState state)
{

/* UZTWC   UZFWC   LZTWC   LZFSC   LZFPC   ADIMC */

   String formatString = "###.000";
   NumberFormat format = new DecimalFormat(formatString);

   String string = format.format(state.getUztwc())  + 
                   " " + format.format(state.getUzfwc())  +
           " " + format.format(state.getLztwc())  +
           " " + format.format(state.getLzfsc())  + 
           " " + format.format(state.getLzfpc())  +
           " " + format.format(state.getAdimc());
   
   return string;
}
 
//---------------------------------------------- 
/*       
private static String getStateLabelString()
{
   return "UZTWC   UZFWC   LZTWC   LZFSC   LZFPC   ADIMC ";  
}
*/
//  ----------------------------------------------

    private static boolean isWhiteSpace(char c)
    {
    	boolean result = false;
    	 
		if ( (c == ' ') || (c== '\t') )
		{
		    result = true;	
			
		}	 
    	return result;
    }
    
//	----------------------------------------------
	private static boolean isPartOfANumber(char c)
	{
		boolean result = false;
    	 
		if ( ((c >= '0') && (c <= '9')) || 
		  	  (c =='-') || (c == '.')
		   )
		    {
				result = true;
			}
			
		return result;
	} //end isPartOfANumber
    
//	   ----------------------------------------------
    private static double[] readDoubleArray(String fileName, boolean skipFirstLine) throws java.io.IOException
    {
       // System.out.println("----------opening file = " + fileName);
        List doubleList = new ArrayList();
        
        FileReader fileReader = new FileReader(fileName);
        BufferedReader bufferedReader = new BufferedReader(fileReader);
        //FileReader reader = new FileReader(fileName);
          
        String line = null;
        int lineCount = 0;
        
       
        line = "";
        
        while( line != null)
        {
			line = bufferedReader.readLine();
			if (line == null)
			{
			    break;
			}
        	lineCount++;
		//	System.out.println(fileName + ": line " + lineCount + " = " + line);
        	
        	if  ((skipFirstLine) && (lineCount == 1) )
        	{
        		continue; // skip this line entirely
        	}
        	
        	int lastPos = 0;
        	StringBuffer numberBuffer = new StringBuffer();
        	boolean success = false;
        	boolean foundNumberPart = false;
        	String numberString = null;
        	
        	// assemble the number string from the line
        	for (int i = 0; i < line.length() ; i++)
        	{
        		char c = line.charAt(i);
        	    if  (  isPartOfANumber(c) ||
        	          ( isWhiteSpace(c) && (foundNumberPart == false ) )
        	        ) 
        	    {
        	        if (isWhiteSpace(c))
        	        {
        	        	//System.out.println("I found whitespace at character " + i);
        	        }
        	        else
        	        {
        	        	foundNumberPart = true;
        	    	    numberBuffer.append(c);
					   // System.out.println("numberBuffer = " + numberBuffer + " i = " + i);
        	    	    success = true;
        	        }
        	    }
        	    else //we are done with looking for the number
        	    {
        	    	numberString = numberBuffer.toString();
        	    	//System.out.println("numberString = " + numberString);
        	        break;
        	    }
        	} //end for
        	
        	if ((success) && (numberString == null) )
        	{
				numberString = numberBuffer.toString();	
        	}
        
        	if (success)
        	{
        		double number = -9999.9999;
        	    number = Double.parseDouble(numberString);
				doubleList.add(new Double(number));		
        	}
        } //end while
   
        bufferedReader.close();
        fileReader.close();
        
        //copy the list contents to the array of doubles       
		double[] doubleArray = new double[doubleList.size()];
        for (int i = 0; i < doubleList.size(); i++)
        {
        	
            Double doubleVar = (Double) doubleList.get(i);
			doubleArray[i] = doubleVar.doubleValue();	
        }
        
      //  System.out.println("SacSmaTest.readDoubleArray size = " +
     //                      doubleList.size());
        return doubleArray;	
    }
  
//     ----------------------------------------------
  
    private static double[] readStateArray(String fileName) throws java.io.IOException
    {
    	double[] wholeArray = readDoubleArray(fileName, true);
    	double[] sizedArray = new double[6];
    	
    	int skipAmount = 16;
    	
    	//skip the first 16 parameters in the file
    	for (int i = 0; i < sizedArray.length; i++)
    	{
    		sizedArray[i] = wholeArray[i + skipAmount];
    	}
    
        return sizedArray;	
    }

//     ----------------------------------------------

    
	private static double[] readParameterArray(String fileName) throws java.io.IOException
	{
		   double[] wholeArray = readDoubleArray(fileName, true);
		   double[] sizedArray = new double[16];
		   
//		skip the last 6 state/carryover variables in the file
		   for (int i = 0; i < sizedArray.length; i++)
		   {
			  sizedArray[i] = wholeArray[i];
		   }
		   
		   return sizedArray;
    	
	}

//     ----------------------------------------------
    
	private static double[] readPrecipArray(String fileName) throws java.io.IOException
	{
			
	    return readDoubleArray(fileName, true);
	}
    
//     ----------------------------------------------
    
	private static double[] readEvapArray(String fileName) throws java.io.IOException
	{
      
		return readDoubleArray(fileName, false);
	}


//  ----------------------------------------------
    private static RegularTimeSeries getEvapTimeSeries(double[] evapArray)
    {
        
        long endTime = System.currentTimeMillis();
        endTime = TimeHelper.truncateTimeInMillisToNearestHour(endTime, 1);
        
        final long millisPerHour = 60 * 60 * 1000;
        long startTime = endTime - (millisPerHour *  (evapArray.length -1));
        
        int intervalInHours = 1;        

        RegularTimeSeries ts = new RegularTimeSeries(startTime, endTime,
                                                     intervalInHours,
                                                     MeasuringUnit.mm);
      
        return ts;  
    }

//  ----------------------------------------------
    public static void oldTestModel()
      {
         
          double[] stateArray = null;
          double[] parameterArray = null;
          
          SacSmaParameters params = null;
          SacSmaState state = null;
          SacSmaRainfallRunoffModel model = null;
          
          RegularTimeSeriesHolder evapTimeSeriesHolder = new RegularTimeSeriesHolder();
        
          int hoursPerInterval = 1;
          double evaporationAmount = 0.0;
          double precipAmount = 0.0;
        
        
          double[] precipArray = null;
          double[] evaporationArray = null;
        
     
          try
          {
              parameterArray = readParameterArray("D:/Data/exampleCode/sacsma/parmsandco");
              
              
              
              stateArray = readStateArray("D:/Data/exampleCode/sacsma/parmsandco");
              
              params = new SacSmaParameters(parameterArray);
              state = new SacSmaState(stateArray);
  
            
              
              precipArray = readPrecipArray("D:/Data/exampleCode/sacsma/rainfile");
              evaporationArray = readEvapArray("D:/Data/exampleCode/sacsma/evapfile");
            
              RegularTimeSeries evapTimeSeries = getEvapTimeSeries(evaporationArray);
              evapTimeSeriesHolder.setTimeSeries(evapTimeSeries);
              
              boolean useMapeTimeSeries = true;
              
              double[] values = {1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 };
              MonthlyValues mapeAdjustmentValues = new MonthlyValues(values);
              
              double peAdjust = 1.0;
              double pxAdjust = 1.0;
              
              model = new SacSmaRainfallRunoffModel(state,
                                      params, 
                                      peAdjust,
                                      pxAdjust,
                                      useMapeTimeSeries,
                                      mapeAdjustmentValues,
                                      evapTimeSeriesHolder);
          }
          catch (java.io.IOException e)
          {
              e.printStackTrace();
              System.exit(1);
          }
       
          int length = Math.min(precipArray.length, evaporationArray.length);
        
          String formatString = "#####.000";
          NumberFormat formatter = new DecimalFormat(formatString);
        
          System.out.println("Java version of SacSma:");
        
          PrintWriter runoffWriter = null;
          PrintWriter internalsWriter = null;
        
          try
          {
            runoffWriter =
                  new PrintWriter(new FileOutputStream("D:/Data/code/SiteSpecific/output/java_runoff.out"));
    
            internalsWriter = 
                  new PrintWriter(new FileOutputStream("D:/Data/code/SiteSpecific/output/java_internals.out"));
          }
          catch(java.io.IOException e)
          {
              System.out.println("Unable to open the output files.");
              System.exit(1);
          }
        
          String runoffMessage = null;
          String internalsMessage = null;

          internalsMessage = "at startup: " + 
                             getStateString(model.getState()) +
                             model.getParameters().toString();
                                         
          System.out.println(internalsMessage);
          internalsWriter.println(internalsMessage);


          //for each piece of input data
          for (int i = 0 ; i < length; i++)
          {
 
               precipAmount = precipArray[i];
               evaporationAmount = evaporationArray[i];
             
               double totalChannelInflow = 
                      model.calculate(hoursPerInterval/ 24.0,
                                    precipAmount,
                                    evaporationAmount);
  
               runoffMessage = "i = " + i + " px = " + precipAmount +
                                      " evap = " + evaporationAmount +
                                      "  runoff = " +
                                      formatter.format(totalChannelInflow);
                               
                          
                               
               System.out.println(runoffMessage);
               runoffWriter.println(runoffMessage);
               //System.out.println(model);
             
               internalsMessage = getStateString(model.getState());
                                         
               System.out.println(internalsMessage);
               internalsWriter.println(internalsMessage);
             
             
        
          } //end for
        
          runoffWriter.close();
          internalsWriter.close();
        
      } //end oldTestModel
    
}


