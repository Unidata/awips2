/*
 * Created on Oct 2, 2003
 *
 * 
 */
package ohd.hseb.util;

import java.util.*;
import java.io.*;

import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.measurement.Measurement;
import ohd.hseb.measurement.MeasuringUnit;
import ohd.hseb.measurement.RegularTimeSeries;

/**
 * author: Chip Gobs
 */
public class TimeSeriesFileManager
{
	
	public static double[] readPrecipArray(String fileName) throws java.io.IOException
	{
	
		return readDoubleArray(fileName, true);
	}

//		-------------------------------------------------------	

	public static double[] readEvapArray(String fileName) throws java.io.IOException
	{
  		return readDoubleArray(fileName, false);
	}
//		-------------------------------------------------------	

	public static double[] readDoubleArray(String fileName, boolean skipFirstLine) throws java.io.IOException
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
//		-------------------------------------------------------	
   
	public static double[] readStateArray(String fileName) throws java.io.IOException
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
//		-------------------------------------------------------	
  
	public static double[] readParameterArray(String fileName) throws java.io.IOException
	{
		double[] wholeArray = readDoubleArray(fileName, true);
		double[] sizedArray = new double[16];
   
		//	skip the last 6 state/carryover variables in the file
		for (int i = 0; i < sizedArray.length; i++)
		{
		   sizedArray[i] = wholeArray[i];
		}
   
		return sizedArray;
	}
    
//		----------------------------------------------
	private static boolean isWhiteSpace(char c)
	{
		boolean result = false;
	 
		if ( (c == ' ') || (c== '\t') )
		{
			result = true;	
		
		}	 
		return result;
	}

//		----------------------------------------------
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

//	----------------------------------------------------------------------------------------------
   
    public void saveRegularTimeSeries(RegularTimeSeries rts, String filePath)
    {
        saveRegularTimeSeries(rts, filePath, null);
    }
    
//  ----------------------------------------------------------------------------------------------
    
	public void saveRegularTimeSeries(RegularTimeSeries rts, String filePath, MeasuringUnit unit)
	{
        
        if (unit == null)
        {
            unit = rts.getMeasuringUnit();
        }
        
	    PrintWriter outputStream = null;
	    try
	    {
	        outputStream = new PrintWriter(new FileOutputStream(filePath));
	    }
	    catch(FileNotFoundException e)
	    {
	        System.out.println("Error opening the file " + filePath);
	    }
	    
	    outputStream.println(DbTimeHelper.getDateTimeStringFromLongTime(rts.getStartTime()));  //1st line in the file
	    outputStream.println(DbTimeHelper.getDateTimeStringFromLongTime(rts.getEndTime()));  //2nd line in the file
	    outputStream.println(rts.getIntervalInHours());  //3rd line in the file
	    
	    //outputs the values of RTS to the file, starting from index = 0
	    for (int i = 0; i < rts.getMeasurementCount(); i++)  
	    {
	        outputStream.println(rts.getMeasurementValueByIndex(i, unit));
	    }
	    
	    outputStream.close();
	    
	}  //end of saveRegularTimeSeries()
	
    
    /****************************************************************************
     * reading from a file, fileNameStr, returns an object of RegularTimeSeries 
     * or null. User needs to check the returned is null or not.
     ****************************************************************************/
	public RegularTimeSeries readRegularTimeSeries(String filePath, MeasuringUnit unit)
	{
	    RegularTimeSeries retrievedRTS = null;

	    //This is where a real application would open the file.
	    //System.out.println("Opening: " + filePath + ".");

	    BufferedReader inputStream = null;
	    String startTimeStr = null;
	    String endTimeStr   = null;
	    int intervalInHours = 0;

	    /*-------open the file and get start time & end time & intervalInHours --------*/
	    try
	    {
	        inputStream = new BufferedReader(new FileReader(filePath));
	        startTimeStr = inputStream.readLine().trim();  //get 1st line
	        endTimeStr = inputStream.readLine().trim();  //get 2nd line
            intervalInHours = Integer.parseInt(inputStream.readLine().trim());  //get 3rd line
            
     /*       retrievedRTS = new RegularTimeSeries(DbTimeHelper.getLongTimeFromDateTimeString(startTimeStr),
                    DbTimeHelper.getLongTimeFromDateTimeString(endTimeStr),
                    intervalInHours,
                    unit);
       */     
            /*-------------read the file from 4th line to end to retrieve all the values-------*/ 
            int count = 0;
            String valueLine = null;

            while ((valueLine = inputStream.readLine()) != null) //start reading from 4th line
            {
                if(retrievedRTS == null)
                {
                    retrievedRTS = new RegularTimeSeries(DbTimeHelper.getLongTimeFromDateTimeString(startTimeStr),
                        DbTimeHelper.getLongTimeFromDateTimeString(endTimeStr),
                        intervalInHours,
                        unit);
                }
                Measurement measurement = new Measurement(Double.parseDouble(valueLine.trim()), unit);
                long time = (retrievedRTS.getStartTime() + count * intervalInHours * TimeHelper.MILLIS_PER_HOUR );
                retrievedRTS.setMeasurementByTime(measurement, time);
                //  System.out.println("count = " + count + ": " + Double.parseDouble(valueLine.trim()));

                count++;
            }
            inputStream.close();
            
	    }
	    catch (FileNotFoundException e)
	    {
	        System.out.println("File " + filePath + " was not found");
	    }
	    catch (Exception e)
	    {
	        System.out.println("Error reading from file " + filePath);
	    }

        return retrievedRTS;  //maybe an object, maybe null. user needs to check the returned.
	}

    
}
