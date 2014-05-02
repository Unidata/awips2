package com.raytheon.viz.mpe.ui.dialogs.postanalysis;

import java.awt.Rectangle;
import java.io.IOException;
import java.util.Date;

import com.raytheon.uf.common.mpe.util.XmrgFile;
import com.raytheon.viz.mpe.ui.DisplayFieldData;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;

//----------------------------------------------------------------------------
/**
 * Class to handle precip totalling algorithms in MPE's PostAnalysis
 * <p>
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * December 2013 DCS 167    C. Gobs   Initial Creation
 * 
 * </pre>
 * 
 * 
 * @author Chip Gobs
 * 
 */
public class PrecipTotaller
{
	public final static long MILLIS_PER_HOUR = 60 * 60 * 1000;
	public final static long MILLIS_PER_DAY = 24 * MILLIS_PER_HOUR;

	//----------------------------------------------------------------------------
	
	public double[][] get24HourBestEstimateGrid(int maxRows, int maxCols,
												 Date endTime, double missingValue, double scaleFactor)
	{
		
		Date startTime =  new Date(endTime.getTime() - MILLIS_PER_DAY);
		
		double[][] totalPrecipGrid = getTotalPrecipGrid(maxRows, maxCols,
														DisplayFieldData.Xmrg,
														startTime, endTime, missingValue,
														scaleFactor);
		
		
		return totalPrecipGrid;
	}
	
	//----------------------------------------------------------------------------

	public double[][] getTotalPrecipGrid(int maxRows, int maxCols,
										 DisplayFieldData displayFieldData,
										 Date startDateTime, Date endDateTime,
										 double missingValue,
										 double scaleFactor)
	{
	
		//reads the appropriate MPE grid type and totals up
		//hourly precip over the specified length of time
		//startTime is exclusive
		//endTime is inclusive, so if you want a total from 12Z to 12Z,
		// the routine does not read the first 12Z file, but starts at 13z and then
		// reads all up to and including the final 12Z file
		// returns an HRAP grid with precip values in INCHES
		
		String header = "PrecipTotaller.getTotalPrecipGrid(): ";
		
		long startTime = startDateTime.getTime();
		long endTime = endDateTime.getTime();
	
		double[][] totalPrecipGrid = new double[maxRows][maxCols];
		
		//init the grid
		for (int row = 0; row < maxRows; row++)
		{
			for (int col = 0; col < maxCols; col++)
			{
				totalPrecipGrid[row][col] = 0.0;
			}
		}

		// total up each hour
		for (long time = startTime + MILLIS_PER_HOUR;  time <= endTime; time += MILLIS_PER_HOUR)
		{
			XmrgFile xmrgFile = MPEDisplayManager.getXmrgFile(displayFieldData, new Date(time));

			System.out.println(header + " Reading file " + xmrgFile.getFile().getName());
			
			double[][] hourlyGrid = getHourlyGrid(xmrgFile);
			
			if (hourlyGrid != null)
			{
				addGridToTotalGrid(maxRows, maxCols, totalPrecipGrid, hourlyGrid, scaleFactor);
			}
		}
	
		
		return totalPrecipGrid;
	}

	//----------------------------------------------------------------------------

	private void addGridToTotalGrid(int maxRows, int maxCols, double[][] totalPrecipGrid,
			double[][] hourlyGrid, double scaleFactor)
	{
		
		double hourlyValue = 0.0;
		
		for (int row = 0; row < maxRows; row++)
		{
			for (int col = 0; col < maxCols; col++)
			{
				hourlyValue = hourlyGrid[row][col];

				if (hourlyValue >= 0.0)
				{
					totalPrecipGrid[row][col] += hourlyValue * scaleFactor;
				}
			}
		}
	} //end addGridToTotalGrid()

	//----------------------------------------------------------------------------

	private double[][] getHourlyGrid(XmrgFile xmrgFile )
	{
		double[][] grid = null;
		
		try
		{			
		    if ( xmrgFile.getFile().length() > 0)
		    {
		        xmrgFile.load();

		        Rectangle hrapExtent = xmrgFile.getHrapExtent();

		        short[][]  hourlyShortData = xmrgFile.getData(hrapExtent);


		        int maxCols = hrapExtent.width;
		        int maxRows = hrapExtent.height;

		        grid = new double[maxRows][maxCols];
		        double value = 0.0;

		        for (int row = 0; row < maxRows; row++)
		        {
		            for (int col = 0; col < maxCols; col++)
		            {

		                short shortValue = hourlyShortData[row][col];

		                if (shortValue >= 0)
		                {
		                    //convert from hundredths of MM to inches
		                    value = ( ((double) shortValue) / ( 25.4 * 100.0) );
		                }
		                else //keep special MISSING value
		                {
		                    value = shortValue;
		                }

		                grid[row][col] = value;
		            } //end for col
		        } //end for row

		    }//end if length > 0
		} //end try
		catch (IOException e)
		{
		    e.printStackTrace();
		}


		return grid;
	}

	
	//----------------------------------------------------------------------------

} //end class PrecipTotaller
