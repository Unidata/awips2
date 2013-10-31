package ohd.hseb.timeserieslite.gui.drawing;

import java.awt.Point;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionAdapter;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

import ohd.hseb.measurement.Measurement;
import ohd.hseb.measurement.MeasuringUnit;
import ohd.hseb.util.DataPoint;
import ohd.hseb.util.ValueMapper;
import ohd.hseb.util.gui.drawing.TsPaintableCanvas;

public class TsCanvasToolTipListener extends MouseMotionAdapter
{
        private TsPaintableCanvas _canvas = null;
        private ValueMapper _valueMapper = null;
       
        // ---------------------------------------------------------------------------------
        
        
        public TsCanvasToolTipListener(TsPaintableCanvas canvas, ValueMapper valueMapper)
        {
            _canvas = canvas;  
            _valueMapper = valueMapper; 
        }
        // ---------------------------------------------------------------------------------
             
        public void mouseMoved(MouseEvent event)
        {
             super.mouseMoved(event);
             Point screenPoint = event.getPoint();

             DataPoint dataPoint = 
                  _canvas.getViewport().getDataPoint(screenPoint);
                       
            
             if (_canvas.getViewport().isViewable(dataPoint))
             {
                 updateDataPointText(dataPoint, _valueMapper, _canvas);
             }
         }  
        
   
    
    // ---------------------------------------------------------------------------------
    
        public void updateDataPointText(DataPoint dataPoint,
                                        ValueMapper valueMapper,
                                        TsPaintableCanvas canvas)
    {
        //System.out.println("in TsMainWindow.update(DataPoint dp)");
    
        String leftUnitString = canvas.getDisplayedMeasuringUnit().toString();
        String rightUnitString = valueMapper.getOutputMeasuringUnit().toString();
    
        String message = getDataPointString(dataPoint, valueMapper, leftUnitString, rightUnitString);
    
        //textField.setText(message);
        
        canvas.setToolTipText(message);
         
        return;
    }       
  
    // ---------------------------------------------------------------------------------
    
    private String getDataPointString(DataPoint dataPoint,
                                      ValueMapper valueMapper, 
                                      String leftUnitString,
                                      String rightUnitString)
    {
        NumberFormat nf = new DecimalFormat("####.##");
        String  valueString = nf.format(dataPoint.getY());
    
        String mappedValueString = "";
        
        if (valueMapper != null)
        {
            MeasuringUnit inputUnit = valueMapper.getInputMeasuringUnit();
            Measurement inputMeasurement = new Measurement(dataPoint.getY(), inputUnit);
  
            Measurement mappedMeasurement = valueMapper.getResultMeasurement(inputMeasurement);
            
            double mappedValue = mappedMeasurement.getValue();
            
            mappedValueString = nf.format(mappedValue);
        }

        long time = (long) Math.floor( dataPoint.getX());

        String timeString = getDateTimeStringToMinutes(time);

        String dataPointString = timeString  + ", " +
                                     valueString + " " + leftUnitString + ", "  +
                                     mappedValueString + " " + rightUnitString;

        return dataPointString;
    }
   
    //---------------------------------------------------------------------- 
       
    private String getDateTimeStringToMinutes(long time)
    {

        SimpleDateFormat sdf = new SimpleDateFormat("yyyy/MM/dd HH:mm");
        sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
        String timeString = sdf.format(new Date(time)) + " Z";

        return timeString;
    }

   //---------------------------------------------------------------------- 

}
