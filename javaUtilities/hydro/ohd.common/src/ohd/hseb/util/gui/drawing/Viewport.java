/*
 * Created on Jul 16, 2003
 *
 * Viewport provides the tools for translating between
 * data coordinates and screen coordinates.
 */
package ohd.hseb.util.gui.drawing;

import ohd.hseb.util.*;
import java.awt.*;

/**
 * @author Chip Gobs
 *
 * This class provides a mapping between screen coordinates and
 * data coordinates.
 */
public class Viewport
{
	private Point _minScreenPoint = null;
	private Point _maxScreenPoint = null;
	
	private DataPoint _minDataPoint = null;
	private DataPoint _maxDataPoint = null;
	
	private boolean _flipYOrientation = false;
	private boolean _flipXOrientation = false;
	
//	----------------------------------------------------------------------------
	public Viewport(Point minScreenPoint,
						  Point maxScreenPoint,
						  DataPoint minDataPoint, 
						  DataPoint maxDataPoint)
	{
		_minScreenPoint = minScreenPoint;
		_maxScreenPoint = maxScreenPoint;
	
		_minDataPoint = minDataPoint;
		_maxDataPoint = maxDataPoint;
	
		return;
	}
	
   //	----------------------------------------------------------------------------
	
	
	public Viewport(Viewport viewport)
	{
		_minScreenPoint = viewport.getMinScreenPoint();
		_maxScreenPoint = viewport.getMaxScreenPoint();
		
		_minDataPoint = viewport.getMinDataPoint();
		_maxDataPoint = viewport.getMaxDataPoint();
		
		_flipXOrientation = viewport._flipXOrientation;
		_flipYOrientation = viewport._flipYOrientation;
		
		return;
	
	}
	
	//	----------------------------------------------------------------------------
	
	public boolean isViewable(DataPoint dataPoint)
	{
		boolean result = false;
		
		if (
		       (dataPoint.getX() >= _minDataPoint.getX()) && 
		       (dataPoint.getX() <= _maxDataPoint.getX()) &&
		       (dataPoint.getY() >= _minDataPoint.getY()) && 
		       (dataPoint.getY() <= _maxDataPoint.getY())
		   )
		{
			result = true;	
		}
		
		return result;
				   
	}
	
    //	----------------------------------------------------------------------------
	
	public void setFlipYOrientation(boolean flip)
	{
		_flipYOrientation = flip;	
	}
	
	//	----------------------------------------------------------------------------
		
	public void setFlipXOrientation(boolean flip)
	{
			_flipXOrientation = flip;	
	}
	
	//	----------------------------------------------------------------------------
		
	public DataPoint getDataPoint(Point screenPoint)
	{
		DataPoint dataPoint = new DataPoint();
		
		
		double x = calcDataCoord(screenPoint.x,
								_minScreenPoint.x,
								_maxScreenPoint.x,
								_minDataPoint.getX(),
								_maxDataPoint.getX(),
								_flipXOrientation);
										
		double y = calcDataCoord(screenPoint.y, 
								_minScreenPoint.y,
								_maxScreenPoint.y,
								_minDataPoint.getY(),
								_maxDataPoint.getY(),
								_flipYOrientation);
		
		dataPoint.setX(x);
		dataPoint.setY(y);
		
		return dataPoint;
	}
	
    //	----------------------------------------------------------------------------
	
	public Point getScreenPoint(DataPoint dataPoint)
	{
		Point screenPoint = new Point();
		
		int x = calcScreenCoord(dataPoint.getX(),
								_minScreenPoint.x,
								_maxScreenPoint.x,
						  		_minDataPoint.getX(),
								_maxDataPoint.getX(),
								_flipXOrientation);
							    
		int y = calcScreenCoord(dataPoint.getY(),
								_minScreenPoint.y,
								_maxScreenPoint.y,
								_minDataPoint.getY(),
								_maxDataPoint.getY(),
								_flipYOrientation);					    
		
		screenPoint.x = x;
		screenPoint.y = y;
		
		return screenPoint;
	}
	
	//----------------------------------------------------------------------------
	
	private double calcDataCoord(int screenCoord,
								 int minScreenCoord,
								 int maxScreenCoord,
								 double minDataCoord,
								 double maxDataCoord,
								 boolean flipOrientation)
	{
	    double dataCoord = 0.0;
	    int lengthDrawn = 0;
	    
	    int useableScreenLength = Math.abs(maxScreenCoord - minScreenCoord);
		double maxDataDiff = maxDataCoord - minDataCoord;
		
		double fractionalDistance = 0.0;
	 
	    /*
	     * avoid possible divide by 0
	     */
		if (useableScreenLength == 0)
		{
		    return 0.0;	
		}
	      
	    // determine how far from the appropriate edge the coordinate is
	    if (flipOrientation)
	    {
			//lengthDrawn = Math.abs(screenCoord - maxScreenCoord);
	        lengthDrawn = maxScreenCoord - screenCoord;
	    }
	    else
	    {
			lengthDrawn = screenCoord - minScreenCoord;	
			//lengthDrawn = Math.abs(screenCoord - minScreenCoord);	
	    }
	    
	    //figure out the proportion of the screen distance the coordinate is
		fractionalDistance = (double) lengthDrawn/
								(double) useableScreenLength;
						
		// get the dataCoord from the fractionalDistance							
	    dataCoord = (fractionalDistance * maxDataDiff) + minDataCoord;
	    
	    return dataCoord;	
	}
	//	----------------------------------------------------------------------------
		
	private int calcScreenCoord(double dataCoord,
									 int minScreenCoord,
									 int maxScreenCoord,
									 double minDataCoord,
									 double maxDataCoord,
								     boolean flipOrientation)
	{
	    int screenCoord = 0;
	    
		int useableScreenLength = Math.abs(maxScreenCoord  - minScreenCoord);
		double maxDataDiff = maxDataCoord - minDataCoord;
		
		double curDataDiff = dataCoord - minDataCoord;
		
		
		// avoid possible divided by 0
		if (maxDataDiff == 0)
		{
		    return 0;	
		}
		
		
		//  Rounding
		double fractionDrawn = ((double)curDataDiff /(double) maxDataDiff);
		int lengthDrawn = (int) Math.round(fractionDrawn * useableScreenLength);
		                           
		if (flipOrientation)
		{
		    screenCoord = maxScreenCoord - lengthDrawn;	
		}
		else
		{
		    screenCoord = minScreenCoord + lengthDrawn;
		}
	
		return screenCoord;
		
	} //end calcScreenCoord
	
	//	----------------------------------------------------------------------------
	

	public void setMinScreenPoint(Point minScreenPoint)
	{
		_minScreenPoint = minScreenPoint;
	}
	
	//	----------------------------------------------------------------------------
	
	public Point getMinScreenPoint()
	{
		return _minScreenPoint;
	}

	//	----------------------------------------------------------------------------
	

	public void setMaxScreenPoint(Point maxScreenPoint)
	{
		_maxScreenPoint = maxScreenPoint;
	}

	//	----------------------------------------------------------------------------
	

	public Point getMaxScreenPoint()
	{
		return _maxScreenPoint;
	}

	//	----------------------------------------------------------------------------
	

	public void setMinDataPoint(DataPoint minDataPoint)
	{
		_minDataPoint = minDataPoint;
	}
	//	----------------------------------------------------------------------------
	
	public DataPoint getMinDataPoint()
	{
		return _minDataPoint;
	}
	//	----------------------------------------------------------------------------
	
	public void setMaxDataPoint(DataPoint maxDataPoint)
	{
		_maxDataPoint = maxDataPoint;
	}
	//	----------------------------------------------------------------------------
	
	public DataPoint getMaxDataPoint()
	{
		return _maxDataPoint;
	}
	
//	----------------------------------------------------------------------------

//	----------------------------------------------------------------------------
		
	public static void main(String[] args)
	{
		 Point minScreenPoint = new Point(0,0);
		 Point maxScreenPoint = new Point(500, 500);
		 
		 DataPoint minDataPoint = new DataPoint(0.0, 0.0);
		 DataPoint maxDataPoint = new DataPoint(100, 100);
		 
		 Viewport vp = new Viewport(minScreenPoint,
									maxScreenPoint,
									minDataPoint,
									maxDataPoint);
	  
		 for (int i = 0; i <= 500; i+=100)
		 {
			for (int j = 0; j <= 500; j+=100)
			{	
				Viewport.test(vp, i, j);
			}
		 }
	   	 		
	   	 											
	} //end main
	
	//	----------------------------------------------------------------------------
	
	public static void test(Viewport viewport, int x, int y)
	{
		Point screenPoint = new Point(x, y);   	 
		DataPoint currentDataPoint = null;
		Point newPoint = null;
	   	  
		currentDataPoint = viewport.getDataPoint(screenPoint);							
	   	 
		newPoint = viewport.getScreenPoint(currentDataPoint);
	  
		System.out.println("orig screenPoint = " +  screenPoint);
		System.out.println("data Point = " +  currentDataPoint);
		System.out.println("converted point " + newPoint);
		
		return;
	} //end test

} //end class ViewPort
