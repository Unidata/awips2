package com.raytheon.viz.mpe.ui.dialogs.postanalysis;

/**
 * Handles calculation of max, min, and average values by keeping a running calculation.  
 * Can be used in loops. Little setup is required.  
 * <p>
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * September 2013 DCS 167    C. Gobs   Initial Creation
 * 
 * </pre>
 * 
 * 
 * @author Chip Gobs
 * 
 */

public class MaxMinAverage 
{
	private double min = Double.MAX_VALUE;
	private double max = Double.MIN_VALUE;
	private double average = 0.0;
	
	private long total = 0;
	
	public MaxMinAverage()
	{
		clear();
	}
	
	public void clear()
	{
		min = Double.MAX_VALUE;
		max = Double.MIN_VALUE;
		average = 0.0;
	}
	
	public void submit(double value)
	{
		if (value < min)
		{
			min = value;
		}
		
		if (value > max)
		{
			max = value;
		}
		
		total++;
		
		double newPortion = 1/total;
		double oldPortion = 1 - newPortion;
		
		average = (average*oldPortion) + value * newPortion;
	}
	
	public String toString()
	{
		return " max = " + max + " min = " + min + " average = " + average;
	}
	
}
