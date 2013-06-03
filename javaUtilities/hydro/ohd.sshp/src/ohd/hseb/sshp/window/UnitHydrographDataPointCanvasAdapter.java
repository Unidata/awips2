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
import ohd.hseb.model.DurationCode;
import ohd.hseb.model.UnitHydrographDescriptor;
import ohd.hseb.model.UnitHydrographEntry;
import ohd.hseb.util.DataPoint;
import ohd.hseb.util.gui.drawing.DataPointCanvasBackgroundPainter;
import ohd.hseb.util.gui.drawing.DataCurvePainter;
import ohd.hseb.util.gui.drawing.DataPointCanvas;
import ohd.hseb.util.gui.drawing.DataPointCanvasAdapter;
import ohd.hseb.util.gui.drawing.ScalingHelper;

/**
 * @author GobsC
 * This class helps the user to use the DataPointCanvas class from within The UnitHydrographEditor
 * dialog.  It serves to keep all of the drawing-related code together.
 * 
 */
public class UnitHydrographDataPointCanvasAdapter implements DataPointCanvasAdapter
{
    private List _unitHydrographDescriptorList = null;
    
    private DataPointCanvas _canvas = null;
    private DataPointCanvasBackgroundPainter _dataBackgroundPainter = null;
    
    private DataCurvePainter[] _curvePainterArray = null;
    
    private MeasurementPointSeries[] _measurementPointSeriesArray = null;
    
    private MeasuringUnit _xUnit = MeasuringUnit.hours;
    private MeasuringUnit _yUnit = MeasuringUnit.cfs;
    
    private static final Color[] _colorCurveArray = {Color.YELLOW, 
            							Color.RED,
            							Color.BLUE,
            							Color.CYAN,
            							Color.MAGENTA, 
            							Color.GREEN,
            							Color.ORANGE,
            							Color.GRAY,
            							Color.PINK};
    
  
    //-------------------------------------------------------------------------------------   

    public UnitHydrographDataPointCanvasAdapter()
    {
       
        
    }
//-------------------------------------------------------------------------------------   
    public DataPointCanvas initCanvas(Object initializationObject)
    {
        _unitHydrographDescriptorList = (List) initializationObject;
         
          
         
        int x = 0;
        int y = 0;
        int width = 700;
        int height = 700;
            
        MeasuringUnit dischargeUnit = MeasuringUnit.cfs;
        MeasuringUnit timeUnit = MeasuringUnit.hours;
          
        // scaling helpers end up affecting the overall viewport window and
        // what numbers are displayed for major and minor tick marks by painter  
        ScalingHelper horizontalScalingHelper = new ScalingHelper(0.0, 0.0);
        ScalingHelper verticalScalingHelper = new ScalingHelper(0.0, 0.0);
        
        double[] niceMinorIncrementArray = {1,  2,  4,  8,  12, 24  };
        horizontalScalingHelper.setNiceMinorIncrementArray(niceMinorIncrementArray);
    
        double[] niceMajorIncrementArray = { 6, 12, 24, 48, 96, 120 };
        horizontalScalingHelper.setNiceMajorIncrementArray(niceMajorIncrementArray);
        
        horizontalScalingHelper.setBaseFactorStartingPoint(1.0);
    
    
        _canvas = new DataPointCanvas(timeUnit, dischargeUnit,
                                          x, y,
                                          width, height,
                                          horizontalScalingHelper,
                                          verticalScalingHelper);
                                    
        _canvas.setPreferredSize(new Dimension(width, height));
        _canvas.setMinimumSize(new Dimension(width, height));
        
        
        Color outlineColor = Color.WHITE;
    
        
         // background Painter   
        _dataBackgroundPainter =
                            new DataPointCanvasBackgroundPainter(outlineColor, _canvas, 
                                                      "hours", "cfs", 
                                                      horizontalScalingHelper, 
                                                      verticalScalingHelper);
        
        _dataBackgroundPainter.setShowMinorHorizontalTicks(true);
        _dataBackgroundPainter.setShowMinorVerticalTicks(true);
        
        // this painter is not to be used for determining scaling, 
        //so, this is just considered a plain-old CanvasPainter,
        // instead of a DataPointCanvasPainter
        _canvas.addCanvasPainter(_dataBackgroundPainter);
    
        
        initMeasurementPointSeriesAndPainters(_unitHydrographDescriptorList);
        
         
        //overall canvas settings
        _canvas.setTitle("Unit Hydrograph");     
        _canvas.addMouseMotionListener(new CanvasMouseMotionListener(_canvas));

        return _canvas;
        
    } //end initCanvas
    // ------------------------------------------------------------------------------------- 
    private void addBackgroundPainter()
    {
        
        
    }
    
    // ------------------------------------------------------------------------------------- 
    
    public void redrawCanvas()
    {
        _canvas.refresh();
    }
    
    public void refreshCanvas(Object dataObject)
    { 
        List unitHydrographEntryList = (List) dataObject;
        loadCanvasWithData(unitHydrographEntryList);
        _canvas.refresh();
    }
    // ------------------------------------------------------------------------------------- 
    public void setPainterShouldPaint(UnitHydrographDescriptor descriptor, boolean shouldPaint)
    {
        int index = findIndexOfDescriptor(descriptor);
        
        if (index > -1)
        {
            DataCurvePainter painter = _curvePainterArray[index];
        
            painter.setShouldPaint(shouldPaint);
        }
    }
    // -------------------------------------------------------------------------------------     
   
    
    private void initMeasurementPointSeriesAndPainters(List unitHydrographDescriptorList)
    {
        // init the drawing variables
        boolean compareXValues = true;
        boolean ascendingOrder = true;
        Comparator comparator = new MeasurementPointComparator(compareXValues, ascendingOrder); // compare X values

           
  
        int listSize = unitHydrographDescriptorList.size();
        _curvePainterArray = new DataCurvePainter[listSize];
        _measurementPointSeriesArray = new MeasurementPointSeries[listSize];

        for (int i = 0; i < _measurementPointSeriesArray.length; i++)
        {
            MeasurementPointSeries series = new MeasurementPointSeries(_xUnit,
                    _yUnit, comparator);
            _measurementPointSeriesArray[i] = series;
        }

        
        //remove all the paints and re-add the background painter
        _canvas.removeAllDataPointCanvasPainters();
 
        if (_dataBackgroundPainter != null)
        {
            _canvas.addCanvasPainter(_dataBackgroundPainter);
        }
        
        
        // painter creation Loop
        // there is 1 MeasurementPointSeries per painter
        // There is 1 MeasurementPointSeries per UnitHydrographDescriptor
        for (int i = 0; i < _measurementPointSeriesArray.length; i++)
        {
            MeasurementPointSeries series = _measurementPointSeriesArray[i];
            Color curveColor = getCurveColor(i);

            DataCurvePainter painter = new DataCurvePainter(series, _canvas,
                    curveColor);

            _curvePainterArray[i] = painter;
            _canvas.addDataPointCanvasPainter(painter);
            painter.setShouldPaint(false);

        }

    } //end initMeasurementPointSeries
    
    // -------------------------------------------------------------------------------------
    
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
 
    private Color getCurveColor(int index)
    {       
        if (index >= _colorCurveArray.length)
        {
            index = index % _colorCurveArray.length;
        }
        
        return _colorCurveArray[index];
    }
   
    // ------------------------------------------------------------------------------------- 
    private int findIndexOfDescriptor(UnitHydrographDescriptor desc)
    {
        int index = _unitHydrographDescriptorList.indexOf(desc);
        
        return index;
    }
    // ------------------------------------------------------------------------------------- 
    private void loadCanvasWithData(List uhgList)
    {
      
         
        int measurementPointSeriesIndex = 0;

        _unitHydrographDescriptorList = 
            	UnitHydrographDescriptor.createUnitHydrographDescriptorList(uhgList);
        
        initMeasurementPointSeriesAndPainters(_unitHydrographDescriptorList);
        
        
        boolean loadApiModel = true;

        Measurement ordinalMeasurement = null;
        Measurement dischargeMeasurement = null;
        MeasurementPoint point = null;


        // load the various MeasurementPointSeries with their data
        for (int i = 0; i < uhgList.size(); i++)
        {
            UnitHydrographEntry uhgEntry = (UnitHydrographEntry) uhgList.get(i);

            // add Measurement Points to the points for the unitHydrograph
            int hourInterval = DurationCode.getHoursFromCode(uhgEntry.getDur());
            
            ordinalMeasurement = new Measurement(uhgEntry.getOrdinal()*hourInterval, _xUnit);
            dischargeMeasurement = new Measurement(uhgEntry.getDischarge(),_yUnit);

            point = new MeasurementPoint(ordinalMeasurement,
                    dischargeMeasurement);
            
            //find the right measurement point series and add the point to it
            measurementPointSeriesIndex = findMeasurementPointSeriesIndexByEntry(uhgEntry);
            MeasurementPointSeries series = _measurementPointSeriesArray[measurementPointSeriesIndex];
            if (series != null)
            {
                series.addPoint(point);
            }
         }


        // make sure that the MeasurementPointSeries end in a 0 discharge
        // the last Y measurement needs to be 0 for graphing the unitgraph, so
        // that
        // the scaling does not start, at say, 10, instead of 0

        for (int i = 0; i < _measurementPointSeriesArray.length; i++)
        {
            ensureMeasurementSeriesStartsAndEndsInZero(_measurementPointSeriesArray[i]);
        }

    }
    
    // -------------------------------------------------------------------------------------
    private int findMeasurementPointSeriesIndexByEntry(UnitHydrographEntry uhgEntry)
    {
        int index = -1;
         
        for (int i = 0; i < _unitHydrographDescriptorList.size(); i++)
        {
            UnitHydrographDescriptor descriptor = (UnitHydrographDescriptor)
            								_unitHydrographDescriptorList.get(i);
            if (descriptor.isMatchingEntry(uhgEntry))
            {
                index = i;
                break;
            }
        }
        
        return index;
    }
    // -------------------------------------------------------------------------------------
      
    private void ensureMeasurementSeriesStartsAndEndsInZero(MeasurementPointSeries series)
    {
        MeasurementPoint lastPoint = null;
        int lastIndex = series.size() - 1;

         
        if (lastIndex >= 0)
        {
            lastPoint = series.getPoint(lastIndex);
            
            Measurement lastXMeasurement = lastPoint.getXMeasurement();
            Measurement lastYMeasurement = lastPoint.getYMeasurement();

            MeasuringUnit xUnit = lastXMeasurement.getUnit();
            MeasuringUnit yUnit = lastYMeasurement.getUnit();
            
            Measurement firstXMeasurement = new Measurement(0.0, xUnit);
            Measurement firstYMeasurement = new Measurement(0.0, yUnit);
            
            //add a (0.0, 0.0) first point to the series
            MeasurementPoint firstPoint = new MeasurementPoint(firstXMeasurement, firstYMeasurement);
            series.addPoint(firstPoint);
            
            lastPoint = series.getPoint(lastIndex);

           
            if (lastYMeasurement.getValue() != 0)
            {
                Measurement newYMeasurement = new Measurement(0.0,
                        yUnit);

                
                double newXValue = lastXMeasurement.getValue() + 1.0;
                Measurement newXMeasurement = new Measurement(newXValue,
                        xUnit);

                MeasurementPoint newLastPoint = new MeasurementPoint(
                        newXMeasurement, newYMeasurement);
                series.addPoint(newLastPoint);
            }
        }
        
        


        return;

    }
    // -------------------------------------------------------------------------------------
}
