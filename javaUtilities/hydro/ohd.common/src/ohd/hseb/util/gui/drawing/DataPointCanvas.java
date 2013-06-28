/*
 * Created on Jul 12, 2004
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
import ohd.hseb.util.gui.drawing.PaintableCanvas;


/**
 * @author GobsC
 *
 *  This class is at the same hierarchical level as TsPaintableCanvas,
 *  but is more flexible with what kind of data is on the X axis.
 *
 *  Unlike the TsPaintableCanvas, it assumes that all the X axis will be
 *   painted.
 */
public class DataPointCanvas extends PaintableCanvas
{
    
    private String _className = "DataPointCanvas";
    
    private MeasuringUnit _displayedYMeasuringUnit = null;
    private MeasuringUnit _displayedXMeasuringUnit = null;

    private Viewport _viewport = null;
    private ScalingHelper _horizontalScalingHelper = null;
    private ScalingHelper _verticalScalingHelper = null;
   
   
    private List _dataPointCanvasPainterList = new ArrayList();
       
    private int _leftMargin = 50;
    private int _rightMargin = 50;
    private int _topMargin = 40;
    private int _bottomMargin = 50;

  
    private String _title = null;   
  
  
//    ---constructor------------------------------------------------

    public DataPointCanvas(MeasuringUnit displayedXMeasuringUnit,
                           MeasuringUnit displayedYMeasuringUnit, 
                           int x, int y, 
                           int width, int height,
                           ScalingHelper horizontalScalingHelper,
                           ScalingHelper verticalScalingHelper )
    {
        super(x, y, width, height);
    
        _displayedXMeasuringUnit = displayedXMeasuringUnit;
        _displayedYMeasuringUnit = displayedYMeasuringUnit;
        
        _horizontalScalingHelper = horizontalScalingHelper;
        _verticalScalingHelper = verticalScalingHelper;
         
        addListeners();

        setVisible(true);

    
        repaint();

    } //DataPointCanvas
    
//      --------------------------------------------------------
    

    public void addDataPointCanvasPainter(DataPointCanvasPainter painter)
    {
        _dataPointCanvasPainterList.add(painter);
        
        addCanvasPainter(painter);
        
        return;
    }
    
    public void removeAllDataPointCanvasPainters()
    {
        _dataPointCanvasPainterList.clear();
        return;
    }
    
//      --------------------------------------------------------

    public void setTitle(String graphTitleText)
    {
        _title = graphTitleText;
        return;
    }

//      --------------------------------------------------------
   
    public int getLeftMargin()
    {
        return _leftMargin;
    }

//      --------------------------------------------------------
    public int getRightMargin()
    {
        return _rightMargin;
    }
//      --------------------------------------------------------
    public int getTopMargin()
    {
        return _topMargin;
    }
//      --------------------------------------------------------

    public int getBottomMargin()
    {
        return _bottomMargin;
    }
//  -----------------------------------------------------------------

    public String getTitle()
    {
        return _title;
    }

//      --------------------------------------------------------
   
    public void draw(Graphics g)
    {
        //System.out.println("TsPaintableCanvas.draw()");

        //drawBackground(g);    

        //draw the stuff from all the custom painters
        super.draw(g);
    }

//      --------------------------------------------------------

    public void setDisplayedMeasuringUnit(MeasuringUnit measuringUnit)
    {
        String header = "TsPaintableCanvas.setDisplayedMeasuringUnit()";
    
        //System.out.println("displayed Measuring Unit is changing from " + 
        //          _displayedMeasuringUnit + " to " + measuringUnit);
              
        _displayedYMeasuringUnit = measuringUnit;
        
    } 
//      --------------------------------------------------------
   
    public MeasuringUnit getDisplayedYMeasuringUnit()
    {
        return _displayedYMeasuringUnit;
    }

//      -------------------------------------------------------
    public MeasuringUnit getDisplayedXMeasuringUnit()
    {
        return _displayedXMeasuringUnit;
    }

//      -------------------------------------------------------

    public Viewport getViewport()
    {
        return _viewport;
    }

  
//      ------------------------------------------------------------------------
   
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

//      ------------------------------------------------------------------------------

 
    public void refresh()
    {
        adjustViewport();    
    }
//      ------------------------------------------------------------------------------

    private MaxMinData getMaxAndMinCoordinateValues(boolean isXOrdinate)
    {
        String header = "DataPointCanvas.getMaxAndMinCoordinateValues(): ";

        Measurement minMeasurement = null;
        Measurement maxMeasurement = null;
            
           
        minMeasurement = getMinMeasurementForPainters(isXOrdinate);
        maxMeasurement = getMaxMeasurementForPainters(isXOrdinate);
           

        double minValue = 0.0;
        double maxValue = 1.0;
            
        ScalingHelper scalingHelper = null;
            
        if (isXOrdinate)
        {
            scalingHelper = _horizontalScalingHelper;    
        }
        else
        {
            scalingHelper = _verticalScalingHelper;
        }
            

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

         
        scalingHelper.setMinDataValue(minValue);
        scalingHelper.setMaxDataValue(maxValue);
        
 //       System.out.println(header + "orig minValue = " + minValue + " orig maxValue = " + maxValue);
            
        minValue = scalingHelper.getMinScaleValue();
        maxValue = scalingHelper.getMaxScaleValue();
        
//        System.out.println(header + "new minValue = " + minValue + " new maxValue = " + maxValue);
     
        MaxMinData maxMinData = new MaxMinData(maxValue, minValue);

        return maxMinData;
    }

//      ------------------------------------------------------------------------------

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
  
            // determine the max and min data values for the Y-axis
        MaxMinData maxMinYValue = getMaxAndMinCoordinateValues(false);
        
        double minYValue = maxMinYValue.getMinValue();
        double maxYValue = maxMinYValue.getMaxValue();


        // determine the max and min data values for the X-axis 
        MaxMinData maxMinXValue = getMaxAndMinCoordinateValues(true);

        double minXValue = maxMinXValue.getMinValue();
        double maxXValue = maxMinXValue.getMaxValue();


        DataPoint minDataPoint = new DataPoint(minXValue, minYValue);
        DataPoint maxDataPoint = new DataPoint(maxXValue, maxYValue);

        if (_viewport == null)
        {
            _viewport = new Viewport(minScreenPoint, maxScreenPoint,
                    minDataPoint, maxDataPoint);
            _viewport.setFlipYOrientation(true);
        }

        else  //_viewport != null
        {
            _viewport.setMinScreenPoint(minScreenPoint);
            _viewport.setMaxScreenPoint(maxScreenPoint);
            _viewport.setMinDataPoint(minDataPoint);
            _viewport.setMaxDataPoint(maxDataPoint);

            _viewport.setFlipYOrientation(true);

        }

        repaint();

    } //end adjustViewport

// end  getMaxMeasurementForPainters()
//    -----------------------------------------------------------------
   private Measurement getMinMeasurementForPainters(boolean isXOrdinate)
   {
        Measurement minMeasurement = null;
        Measurement  painterMinMeasurement = null;
        MeasuringUnit unit = null;
    

        if (isXOrdinate)
        {
            unit = getDisplayedXMeasuringUnit();
        }
        else
        {
            unit = getDisplayedYMeasuringUnit();
        }
    
       // check each DataPointCanvasPainter for its min
        for (int i = 0 ; _dataPointCanvasPainterList != null && i <_dataPointCanvasPainterList.size() ; i++)
        {
      
            DataPointCanvasPainter painter = (DataPointCanvasPainter) _dataPointCanvasPainterList.get(i);
            
            
            if (isXOrdinate)
            {
                painterMinMeasurement = painter.getMinXMeasurement();  
            }
            else
            {
                painterMinMeasurement = painter.getMinYMeasurement();  
            }
            
            Measurement tempMin = Measurement.getConvertedCopy(painterMinMeasurement, unit) ;
            
            if (tempMin != null)
            {
    
                if ( (minMeasurement == null) ||
                     (tempMin.getValue() < minMeasurement.getValue())
                   )
                {
                    minMeasurement = tempMin;   
                }
            
            }
        
        } //end for
 
        return minMeasurement;
    }
//     -----------------------------------------------------------------
    private Measurement getMaxMeasurementForPainters( boolean isXOrdinate)
    {
        Measurement maxMeasurement = null;
        Measurement painterMaxMeasurement = null;
        MeasuringUnit unit = null;
        
        if (isXOrdinate)
        {
            unit = getDisplayedXMeasuringUnit();
        }
        else
        {
            unit = getDisplayedYMeasuringUnit();
        }
            
        // check each DataPointCanvasPainter for its max
        for (int i = 0 ; _dataPointCanvasPainterList != null && i < _dataPointCanvasPainterList.size() ; i++)
        {
            DataPointCanvasPainter painter = (DataPointCanvasPainter) _dataPointCanvasPainterList.get(i);    
        
            if (isXOrdinate)
            {
                painterMaxMeasurement = painter.getMaxXMeasurement();  
            }
            else
            {
                painterMaxMeasurement = painter.getMaxYMeasurement();  
            }
            
        
            Measurement tempMax = Measurement.getConvertedCopy( painterMaxMeasurement, unit);
        
            if (tempMax != null)
            {
    
                if ( (maxMeasurement == null) ||
                     (tempMax.getValue() > maxMeasurement.getValue())
                    )
                {
                    maxMeasurement = tempMax;   
                }
            
            }
        
        } //end for
             
       return maxMeasurement;
    
    } // end  getMaxMeasurementForPainters()
//    -----------------------------------------------------------------
    
    private void resize()
    {
        adjustViewport();
    } //end resize()



//    --------------------------------------------------------------------------

    

}
