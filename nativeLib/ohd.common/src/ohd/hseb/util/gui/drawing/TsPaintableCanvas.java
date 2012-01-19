/*
 * Created on Oct 7, 2003
 *
 * 
 */
package ohd.hseb.util.gui.drawing;

import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.util.List;

import ohd.hseb.measurement.*;
import ohd.hseb.util.*;
//import ohd.hseb.util.gui.*;

/**
 * @author Chip Gobs
 *
 * 
 */
public class TsPaintableCanvas extends PaintableCanvas
{
	
	private MeasuringUnit _displayedMeasuringUnit = null;
	private Viewport _viewport = null;


	private List _tsCanvasPainterList = new ArrayList();
	private List _eventListeners = new ArrayList();

	private int _intervalInHours = 1;
		
	private int _leftMargin = 50;
	private int _rightMargin = 50;
	private int _topMargin = 40;
	private int _bottomMargin = 50;

	//time constant
	private static final int MILLIS_PER_HOUR = 1000 * 60 * 60;	

	
	// the default viewing window for the timeSeries
	//private long _defaultTimeWindowInHours = 72;
	private long _defaultTimeWindowInHours = 72;
	
	private long _startTime = (System.currentTimeMillis()/MILLIS_PER_HOUR)*MILLIS_PER_HOUR;
	private long _endTime = _startTime + 
							(_defaultTimeWindowInHours * MILLIS_PER_HOUR);



	private String _title = null;	
  
    //testing only
    private int _showCount = 0;

//---constructor------------------------------------------------

	public TsPaintableCanvas(MeasuringUnit displayedMeasuringUnit, 
					int x, int y, 
					int width, int height)
	{
		super(x, y, width, height);
		
		_displayedMeasuringUnit = displayedMeasuringUnit;
			
		addListeners();
	
		setVisible(true);

		
		repaint();

	} //TsPaintableCanvas
	
//	--------------------------------------------------------
    public void addTsCanvasPainterAfter(TsCanvasPainter painterToAdd, TsCanvasPainter painterToAddAfter)
    {
        int index = -1;
        
        index = _tsCanvasPainterList.indexOf(painterToAddAfter);

        if (index != -1)
        {
            addTsCanvasPainter(painterToAdd, index + 1);
        }
        else
        {
            throw (new Error("Error:  Attempted to add after a painter that does not exist"));
        }

        return;
    }
//  --------------------------------------------------------
   
    public void addTsCanvasPainter(TsCanvasPainter painter, int index)
    {
       //  String header = "TsPaintableCanvas.addTsCanvasPainter(2 arg): ";
        
          _tsCanvasPainterList.add(index, painter);
          addCanvasPainter(index, painter);
          
      //    System.out.println(header + " _tsCanvasPainterList has " + _tsCanvasPainterList.size() + " elements.");
          
          return;
    }

	public void addTsCanvasPainter(TsCanvasPainter painter)
	{
       //   String header = "TsPaintableCanvas.addTsCanvasPainter(1 arg): ";
		  _tsCanvasPainterList.add(painter);
		  addCanvasPainter(painter);
          
       //   System.out.println(header + " _tsCanvasPainterList has " + _tsCanvasPainterList.size() + " elements.");
          
		  return;
	}
    
//  --------------------------------------------------------
    public void removeTsCanvasPaintersBetweenExclusive(TsCanvasPainter firstPainter, TsCanvasPainter lastPainter)
    {
       // String header = "TsPaintableCanvas.removeTsCanvasPaintersBetweenExclusive() ";
        boolean haveFoundFirst = false;
        boolean done = false;
        
        List <TsCanvasPainter> paintersToRemoveList = new ArrayList<TsCanvasPainter>();
        
        //determine which painters to remove
        for (int i = 0; i < _tsCanvasPainterList.size(); i++)
        {
            TsCanvasPainter painter = (TsCanvasPainter) _tsCanvasPainterList.get(i);
            
           // System.out.println(header + "at top of loop, painter = " + painter);
            
            
            if (haveFoundFirst)
            {
                if (painter != lastPainter)
                {
                    //System.out.println(header + "adding painter to the removal list.");
                    paintersToRemoveList.add(painter);
                }
                else //we are done
                {
                   // System.out.println(header + "found last painter.");
                    done = true;
                    break;
                }
            }  
            else if (painter == firstPainter) //keep looking for the first painter
            {
                haveFoundFirst = true;
               // System.out.println(header + "found first painter.");
            }    
        } //end for
        
      //  System.out.println(header + "removing " + paintersToRemoveList.size() + " painters.");
        //actually remove the painters
        for (TsCanvasPainter painter : paintersToRemoveList)
        {
            removeTsCanvasPainter(painter);
        }
    }
    
//  --------------------------------------------------------
    public void removeTsCanvasPaintersBetweenInclusive(TsCanvasPainter firstPainter, TsCanvasPainter lastPainter)
    {
        String header = "TsPaintableCanvas.removeTsCanvasPaintersBetweenInclusive() ";
        boolean haveFoundFirst = false;
        boolean done = false;
        
        List <TsCanvasPainter> paintersToRemoveList = new ArrayList<TsCanvasPainter>();
        
        //determine which painters to remove
        for (int i = 0; i < _tsCanvasPainterList.size(); i++)
        {
            TsCanvasPainter painter = (TsCanvasPainter) _tsCanvasPainterList.get(i);
            
           // System.out.println(header + "at top of loop, painter = " + painter);
            
            
            if (haveFoundFirst)
            {
                if (painter != lastPainter)
                {
                    //System.out.println(header + "adding painter to the removal list.");
                    paintersToRemoveList.add(painter);
                }
                else //we are done
                {
                    paintersToRemoveList.add(painter);
                   // System.out.println(header + "found last painter.");
                    done = true;
                    break;
                }
            }  
            else if (painter == firstPainter) //keep looking for the first painter
            {
                haveFoundFirst = true;
                paintersToRemoveList.add(painter);
               // System.out.println(header + "found first painter.");
            }    
        } //end for
        
        System.out.println(header + "removing " + paintersToRemoveList.size() + " painters.");
        //actually remove the painters
        for (TsCanvasPainter painter : paintersToRemoveList)
        {
            removeTsCanvasPainter(painter);
        }
    }
    
//  --------------------------------------------------------
    
    
    
    public void removeTsCanvasPainter(TsCanvasPainter painter)
    {
          _tsCanvasPainterList.remove(painter);
          removeCanvasPainter(painter);
          return;
    }
//	--------------------------------------------------------
	public void setTitle(String graphTitleText)
	{
		_title = graphTitleText;
		return;
	}

//	--------------------------------------------------------
	
    public long getStartTime()
    {
        return _startTime;    
    }
    
//  --------------------------------------------------------
    public long getEndTime()
    {
        return _endTime;    
    }
    
//  --------------------------------------------------------
   
    public int getLeftMargin()
    {
        return _leftMargin;
    }
    
//  --------------------------------------------------------
    
    public void setLeftMargin(int leftMargin)
    {
        _leftMargin = leftMargin;
        adjustViewport();
    }
    
//  --------------------------------------------------------
    public int getRightMargin()
    {
        return _rightMargin;
    }
//  --------------------------------------------------------
    public void setRightMargin(int rightMargin)
    {
        _rightMargin = rightMargin;
        adjustViewport();
    }
//  --------------------------------------------------------
    public int getTopMargin()
    {
        return _topMargin;
    }   
//  --------------------------------------------------------
    public void setTopMargin(int topMargin)
    {
        _topMargin = topMargin;
        adjustViewport();
    }
//  --------------------------------------------------------
     
    public String getTitle()
	{
		return _title;
	}

//	--------------------------------------------------------
	public void setTimeWindow(long startTime, long endTime)
	{
		_startTime = startTime;
		_endTime = endTime;

		adjustViewport();

	}

//	--------------------------------------------------------

	
	public void slideTimeWindow(int hoursToShift)
	{
		
		long millisToShift =  hoursToShift * MILLIS_PER_HOUR; 
		
		setTimeWindow(_startTime + millisToShift,
		              _endTime + millisToShift);
	}

//	--------------------------------------------------------
	
	public void draw(Graphics g)
	{
		//System.out.println("TsPaintableCanvas.draw()");
	
		//drawBackground(g);	
	
	    //draw the stuff from all the custom painters
		super.draw(g);
	
	
	}

//	--------------------------------------------------------



//	--------------------------------------------------------

    public void setDisplayedMeasuringUnit(MeasuringUnit measuringUnit)
    {
        String header = "TsPaintableCanvas.setDisplayedMeasuringUnit()";
        
        //System.out.println("displayed Measuring Unit is changing from " + 
        //          _displayedMeasuringUnit + " to " + measuringUnit);
                  
        _displayedMeasuringUnit = measuringUnit;
        
        
    } 
    
//	--------------------------------------------------------
   
    public MeasuringUnit getDisplayedMeasuringUnit()
    {
        return _displayedMeasuringUnit;
    }

//	-------------------------------------------------------

	public Viewport getViewport()
	{
		return _viewport;
	}

//	------------------------------------------------------------------------
 
//	------------------------------------------------------------------------
   
	private void addListeners()
	{
	
		//the resize listener
		addComponentListener
		(
			new ComponentAdapter()
			{ 
				public void componentResized(ComponentEvent event)
				{
					resize();		  
				}
			}
		);


	} //end addListeners()

//  ------------------------------------------------------------------------------

    public AbsTimeMeasurement getAbsTimeMeasurementByTruncation(Point point)
    {
        
        DataPoint dataPoint = _viewport.getDataPoint(point);
        MeasuringUnit unit = getDisplayedMeasuringUnit();
                        
        //System.out.println("TsPaintableCanvas.mouseListener() unit = " + unit);
                        
        double clickedValue = dataPoint.getY();
        double value = MathHelper.roundToNDecimalPlaces(clickedValue, 2);
                        
                       
                        
        long clickedTime = (long)dataPoint.getX();
        long time = TimeHelper.truncateTimeInMillisToNearestHour(clickedTime, 
                                                                 _intervalInHours);
                    
        AbsTimeMeasurement measurement = new AbsTimeMeasurement(value, time, unit);     
           
        return measurement; 
    }
    
//  ------------------------------------------------------------------------------

    public AbsTimeMeasurement getAbsTimeMeasurementByRounding(Point point)
    {
        
        DataPoint dataPoint = _viewport.getDataPoint(point);
        MeasuringUnit unit = getDisplayedMeasuringUnit();
                        
        //System.out.println("TsPaintableCanvas.mouseListener() unit = " + unit);
                        
        double clickedValue = dataPoint.getY();
        double value = MathHelper.roundToNDecimalPlaces(clickedValue, 2);
                        
                         
        long clickedTime = (long)dataPoint.getX();
        long time = TimeHelper.roundTimeInMillisToNearestHour(clickedTime);
                                                              
                    
        AbsTimeMeasurement measurement = new AbsTimeMeasurement(value, time, unit);     
           
        return measurement; 
    }
//	------------------------------------------------------------------------------
    public void refresh()
    {
       // String header = "TsPaintableCanvas.refresh(): ";
       
       // System.out.println(header);
        adjustViewport();    
    }
//  ------------------------------------------------------------------------------

	private void adjustViewport()
	{
		//String header = "TsPaintableCanvas.adjustViewport(): ";
        //System.out.println(header);
         	
		int minUseableX = 0 + _leftMargin;
		int minUseableY = 0 + _topMargin;
 
		int maxUseableX = getWidth() - _rightMargin;
		int maxUseableY = getHeight() - _bottomMargin; 
    
		Point minScreenPoint = new Point(minUseableX, minUseableY);
		Point maxScreenPoint = new Point(maxUseableX, maxUseableY);

		long minTime = _startTime;
		long maxTime = _endTime;
	    
		Measurement minMeasurement = getMinMeasurementForPainters();
		Measurement maxMeasurement = getMaxMeasurementForPainters();

        double minValue = 0.0;
        double maxValue = 1.0;

        if (minMeasurement != null)
        {
            minValue = minMeasurement.getValue();
        }
        else
        {
            minValue =  0.0;
        }


        if (maxMeasurement != null)
        {
            maxValue = maxMeasurement.getValue();
        }
   
        if (maxValue <= 0.0)
        {
           maxValue = 1.0;
        }

        ScalingHelper scalingHelper = new ScalingHelper(minValue, maxValue);
        
        minValue = scalingHelper.getMinScaleValue();
        maxValue = scalingHelper.getMaxScaleValue();
	    
        //System.out.println(header + "minValue = " + minValue);
       // System.out.println(header + "maxValue = " + maxValue);
        
		DataPoint minDataPoint = new DataPoint(minTime, minValue);
		DataPoint maxDataPoint = new DataPoint(maxTime, maxValue);
 
		if (_viewport == null)
		{
			_viewport = new Viewport(minScreenPoint,
										 maxScreenPoint,
										 minDataPoint,
										 maxDataPoint);
			_viewport.setFlipYOrientation(true);
		}
		
		else //_viewport != null
		{
		    _viewport.setMinScreenPoint(minScreenPoint);
		    _viewport.setMaxScreenPoint(maxScreenPoint);
		    _viewport.setMinDataPoint(minDataPoint);
		    _viewport.setMaxDataPoint(maxDataPoint);
		    
		    _viewport.setFlipYOrientation(true);
		}
		
		
		repaint();
	
	} //end adjustViewport
	
//-----------------------------------------------------------------
   private Measurement getMinMeasurementForPainters()
   {
   		Measurement minMeasurement = null;
   	
        
   		// check each TsCanvasPainter for its max and min
		for (int i = 0 ; _tsCanvasPainterList != null && i <_tsCanvasPainterList.size() ; i++)
		{
		  
			TsCanvasPainter painter = (TsCanvasPainter) _tsCanvasPainterList.get(i);
				
            MeasuringUnit unit = getDisplayedMeasuringUnit();
			Measurement tempMin = Measurement.getConvertedCopy(painter.getMinMeasurement(), unit) ;
				
			if (tempMin != null)
			{
		
			    if ( (minMeasurement == null) ||
			         (tempMin.getValue() < minMeasurement.getValue())
			       )
			    {
				    minMeasurement = tempMin;	
			    }
			    
				//System.out.println("TsPaintableCanvas.getMinMeasurement(): tempMin is NOT null");
			}
			
		} //end for
   	
       //System.out.println("TsPaintableCanvas.getMinMeasurement(): minMeasurement = " + minMeasurement);   
   
    
   		return minMeasurement;
   }
// -----------------------------------------------------------------
	private Measurement getMaxMeasurementForPainters()
	{
	    Measurement maxMeasurement = null;
   	
		// check each TsCanvasPainter for its max and min
		for (int i = 0 ; _tsCanvasPainterList != null && i < _tsCanvasPainterList.size() ; i++)
		{
			TsCanvasPainter painter = (TsCanvasPainter) _tsCanvasPainterList.get(i);	
			
            MeasuringUnit unit = getDisplayedMeasuringUnit();
			Measurement tempMax = Measurement.getConvertedCopy(painter.getMaxMeasurement(), unit);
			
			if (tempMax != null)
			{
		
				if ( (maxMeasurement == null) ||
				   (tempMax.getValue() > maxMeasurement.getValue())
				    )
				{
				    maxMeasurement = tempMax;	
				}
				
				//System.out.println("TsPaintableCanvas.getMaxMeasurement(): tempMax is NOT null");	
			}
			
	    } //end for
	    
        //System.out.println("TsPaintableCanvas.getMaxMeasurement(): maxMeasurement = " + maxMeasurement);   
        
	    return maxMeasurement;
	} // end  getMaxMeasurementForPainters()
//-----------------------------------------------------------------
	
	private void resize()
	{
		adjustViewport();
	} //end resize()
//--------------------------------------------------------------------------	

} //end TsPaintableCanvas


