/*
 * Created on Jul 19, 2004
 *
 * 
 */
package ohd.hseb.sshp.window;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionAdapter;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.Comparator;
import java.util.List;

import ohd.hseb.measurement.Measurement;
import ohd.hseb.measurement.MeasurementPoint;
import ohd.hseb.measurement.MeasurementPointComparator;
import ohd.hseb.measurement.MeasurementPointSeries;
import ohd.hseb.measurement.MeasuringUnit;
import ohd.hseb.model.RatingPoint;
import ohd.hseb.util.DataPoint;
import ohd.hseb.util.gui.drawing.DataPointCanvasBackgroundPainter;
import ohd.hseb.util.gui.drawing.DataCurvePainter;
import ohd.hseb.util.gui.drawing.DataPointCanvas;
import ohd.hseb.util.gui.drawing.ScalingHelper;

/**
 * @author GobsC
 *
 * 
 */
public class RatingCurveDataPointCanvasAdapter
{
    private DataPointCanvas _canvas = null;
    private DataPointCanvasBackgroundPainter _dataBackgroundPainter = null;
    private DataCurvePainter _dataPointCurvePainter = null;
    private MeasurementPointSeries _measurementPointSeries = null;
      
    MeasuringUnit _xUnit = MeasuringUnit.cfs;
	MeasuringUnit _yUnit = MeasuringUnit.feet;
  
//-------------------------------------------------------------------------------------   
       
    
    private class CanvasMouseMotionListener extends MouseMotionAdapter
    {
        private DataPointCanvas _canvas2 = null;
          
        public CanvasMouseMotionListener(DataPointCanvas canvas)
        {
            _canvas2 = canvas;  
        }
        
        public void mouseMoved(MouseEvent event)
        {

             super.mouseMoved(event);
             Point screenPoint = event.getPoint();

             DataPoint dataPoint = 
                  _canvas2.getViewport().getDataPoint(screenPoint);
                       
            
             if (_canvas2.getViewport().isViewable(dataPoint))
             {
                 updateCanvasDataPointText(dataPoint, _canvas);
             }
         }  
        
    } 
 
//  -------------------------------------------------------------------------------------   
 

    public void updateCanvasDataPointText(DataPoint dataPoint,
            DataPointCanvas canvas)
    {
        //System.out.println("in TsMainWindow.update(DataPoint dp)");

        String xUnitString = canvas.getDisplayedXMeasuringUnit().toString();
        String yUnitString = canvas.getDisplayedYMeasuringUnit().toString();

        String message = getCanvasDataPointString(dataPoint, xUnitString,
                yUnitString);

        canvas.setToolTipText(message);

        return;
    } 

// -------------------------------------------------------------------------------------
        
    private String getCanvasDataPointString(DataPoint dataPoint,
                                            String xUnitString,
                                            String yUnitString)
    {
        NumberFormat nfX = new DecimalFormat("###");

        long hour = (long) Math.round(dataPoint.getX());
        String xValueString = nfX.format(hour);

        NumberFormat nfY = new DecimalFormat("###.##");
        String yValueString = nfY.format(dataPoint.getY());

        String dataPointString = xValueString + " " + xUnitString + " , "
                + yValueString + " " + yUnitString;

        return dataPointString;
    }
//  -------------------------------------------------------------------------------------   
    public DataPointCanvas initCanvas()
    {      
        
        
        // init the drawing variables
        boolean compareXValues = true;
        boolean ascendingOrder = true;
        Comparator comparator = new MeasurementPointComparator(compareXValues, ascendingOrder); // compare X values
        
        _measurementPointSeries = new MeasurementPointSeries(_xUnit, _yUnit, comparator);
               
        int x = 0;
        int y = 0;
        int width = 1000;
        int height = 700;
        
    	
		// scaling helpers end up affecting the overall viewport window and
		// what numbers are displayed for major and minor tick marks by painter  
		ScalingHelper horizontalScalingHelper = new ScalingHelper(0.0, 0.0);
		ScalingHelper verticalScalingHelper = new ScalingHelper(0.0, 0.0);
	
		/*
		double[] niceMinorIncrementArray = {1,  2,  4,  8,  12, 24  };
		horizontalScalingHelper.setNiceMinorIncrementArray(niceMinorIncrementArray);
		
		double[] niceMajorIncrementArray = {6, 12, 24, 48, 96, 120 };
		horizontalScalingHelper.setNiceMajorIncrementArray(niceMajorIncrementArray);
		*/
		
		_canvas = new DataPointCanvas(_xUnit, _yUnit,
		                            x, y,
		                            width, height,
		                            horizontalScalingHelper,
		                            verticalScalingHelper);
		                      
		_canvas.setPreferredSize(new Dimension(width, height));
		_canvas.setMinimumSize(new Dimension(width, height));
		
		Color outlineColor = Color.WHITE;
		boolean showMinorHorizontalTicks = true;
		boolean showMinorVerticalTicks = true;
		boolean showMinorVerticalLabels = false;
		
		// background Painter   
		_dataBackgroundPainter =
		              new DataPointCanvasBackgroundPainter(outlineColor, _canvas, 
		                                         "cfs", "feet",
		                                        horizontalScalingHelper, verticalScalingHelper);
		
		_dataBackgroundPainter.setShowMinorHorizontalTicks(showMinorHorizontalTicks);
		_dataBackgroundPainter.setShowMinorVerticalTicks(showMinorVerticalTicks);
		_dataBackgroundPainter.setShowMinorVerticalLabels(showMinorVerticalLabels);
		                                               
		// this painter is not to be used for determining scaling, 
		//so, this is just considered a plain-old CanvasPainter,
		// instead of a DataPointCanvasPainter
		_canvas.addCanvasPainter(_dataBackgroundPainter);
		
		
		// data point curve painter
		Color curveColor = Color.green;
		_dataPointCurvePainter = new DataCurvePainter(_measurementPointSeries, _canvas, curveColor);
		                                                     
		_canvas.addDataPointCanvasPainter(_dataPointCurvePainter);
		
		_canvas.setTitle("Rating Curve");
      
        _canvas.addMouseMotionListener(new CanvasMouseMotionListener(_canvas));

        return _canvas;
		
        
  
    } //end initCanvas

    // ------------------------------------------------------------------------------------- 
  
    public void refreshCanvas(Object dataObject)
    { 
        List ratingPointList = (List) dataObject;
        loadCanvasWithData(ratingPointList);
        
        _canvas.refresh();
    }
    
//	------------------------------------------------------------------------------------
	
	
	private void loadCanvasWithData(List ratingPointList)
    {
	    _measurementPointSeries.clear();
   
        for ( int i = 0; i < ratingPointList.size(); i++ )
        {
            RatingPoint ratingPoint = (RatingPoint) ratingPointList.get( i );
                
            // add Measurement Points to the points for the unitHydrograph
             Measurement dischargeMeasurement = new Measurement(ratingPoint.getDischarge(), _xUnit);                
             Measurement stageMeasurement = new Measurement(ratingPoint.getShiftedStage(), _yUnit);
             
             MeasurementPoint point = new MeasurementPoint(dischargeMeasurement, stageMeasurement);
            
             _measurementPointSeries.addPoint(point);

        }

     //   System.out.println("loadCanvasWithData says it has " + 
     //           _measurementPointSeries.getPointCount() + " points.");
    }

//	------------------------------------------------------------------------------------

}
