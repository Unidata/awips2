package ohd.hseb.timeserieslite.pdc;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.measurement.AbsTimeMeasurement;
import ohd.hseb.measurement.IrregularTimeSeries;
import ohd.hseb.measurement.IrregularTimeSeriesHolder;
import ohd.hseb.measurement.Measurement;
import ohd.hseb.measurement.MeasuringUnit;
import ohd.hseb.measurement.RegularTimeSeries;
import ohd.hseb.measurement.RegularTimeSeriesHolder;
import ohd.hseb.model.FlowToStageValueMapper;
import ohd.hseb.model.PrecipPPAndPCValueMapper;
import ohd.hseb.model.RatingCurve;
import ohd.hseb.model.SigRiverLevels;
import ohd.hseb.model.StageToFlowValueMapper;
import ohd.hseb.timeserieslite.Location;
import ohd.hseb.timeserieslite.PDCDataType;
import ohd.hseb.timeserieslite.ParamCode;
import ohd.hseb.timeserieslite.TSLDrawingMgr;
import ohd.hseb.timeserieslite.TSLFrame;
import ohd.hseb.timeserieslite.gui.drawing.SignificantLinePainter;
import ohd.hseb.timeserieslite.gui.drawing.TsBarPainter;
import ohd.hseb.timeserieslite.gui.drawing.TsCanvasToolTipListener;
import ohd.hseb.timeserieslite.gui.drawing.TsDataPointPainter;
import ohd.hseb.timeserieslite.gui.drawing.TslCanvasPainter;
import ohd.hseb.timeserieslite.gui.drawing.TslTimeLinePainter;
import ohd.hseb.timeserieslite.gui.drawing.TslBackgroundPainter;
import ohd.hseb.util.CodeTimer;
import ohd.hseb.util.MathHelper;
import ohd.hseb.util.TimeHelper;
import ohd.hseb.util.TimeHolder;
//import ohd.hseb.util.TimerHelper;
import ohd.hseb.util.UnitValueMapper;
import ohd.hseb.util.ValueMapper;
import ohd.hseb.util.gui.drawing.TsPaintableCanvas;


public class PDCDrawingMgr implements TSLDrawingMgr
{
    private static final long MILLIS_PER_HOUR = 60*60*1000;
    private static final int TOP_MARGIN = 100;
    
    private static Map _physicalElementMap = new HashMap();
    static 
    {
        _physicalElementMap.put("HG", PDCDataType.HEIGHT);
        _physicalElementMap.put("HP", PDCDataType.HEIGHT);
        _physicalElementMap.put("HT", PDCDataType.HEIGHT);
        _physicalElementMap.put("QR", PDCDataType.DISCHARGE);
        _physicalElementMap.put("QT", PDCDataType.DISCHARGE);
        _physicalElementMap.put("LS", PDCDataType.LAKE_STORAGE);
        _physicalElementMap.put("PP", PDCDataType.PRECIP_TOTAL);
        _physicalElementMap.put("PC", PDCDataType.PRECIP_PC);
        _physicalElementMap.put("SW", PDCDataType.SNOW_WATER_EQUIVALENT);
        _physicalElementMap.put("SD", PDCDataType.SNOW_DEPTH);
        
        _physicalElementMap.put("TA", PDCDataType.TEMPERATURE);
        _physicalElementMap.put("TD", PDCDataType.DEWPOINT);
       
        _physicalElementMap.put("WS", PDCDataType.WIND_SPEED);
        _physicalElementMap.put("WD", PDCDataType.WIND_DIRECTION);
        
        _physicalElementMap.put("XR", PDCDataType.RELATIVE_HUMIDITY);
                
    }
    
    private int _hoursBackInTime = 120;
    private int _hoursForwardInTime = 168;
    
    private PdcTslDataManager _dataMgr = null;
    private String _locationId = null;
    private List _paramCodeList = new ArrayList();
    private String _jdbcConnectionString = null;
    
  //  private PDCDataType _dataType = null;
    private List _dataTypeList = new ArrayList();
    
    private long _startTime = 0;
    private long _endTime = 0;
 
    private int HOURS_TO_ROUND = 6;
    
    
//  timer-related code
    private int _timerDelayInMillis = 60 * 1 * 1000 ; 
    
    private String[] _rememberedArgStringArray = null;
    private boolean _exitOnClose = false;
    
    private TSLFrame _frame = new TSLFrame();
   
    // --------------------------------------------------------------------------------------------------

    //  --------------------------------------------------------------------------------------------------
     
    public PDCDrawingMgr()
    {
       // ActionListener refreshTimerListener = new RefreshTimerListener();
        
       // TimerHelper refreshTimerHelper = new TimerHelper(refreshTimerListener, _timerDelayInMillis); 
        
    }
    
    // --------------------------------------------------------------------------------------------------
   
  
    public void display(String[] argStringArray, boolean exitOnClose)
    {
        //save these for later, when refresh is called
        _rememberedArgStringArray = argStringArray;
        _exitOnClose = exitOnClose;
       
        handleCommandLineArgs(argStringArray);
        
        String header = "PDCDrawingMgr.display()";
        
        if (_frame == null)
        {
            _frame = new TSLFrame();

            _frame.setExitOnClose(exitOnClose);
        }
        
       // System.out.println(header + "after TSLFrame()");
        
        TsPaintableCanvas canvas = _frame.getCanvas();
        
        
        PDCDataType dataType = null;
            
        if ( _dataTypeList.size() > 0 )
        {
            dataType = (PDCDataType) _dataTypeList.get(0);
            
        }
        
     //   System.out.println(header + " before initializeCanvas() ");
        initializeCanvas(dataType, _paramCodeList, canvas);
     //   System.out.println(header + " after initializeCanvas() ");
               
        _frame.setVisible(true);   
        
    }
    
    // --------------------------------------------------------------------------------------------------
    
    private void initializeCanvas(PDCDataType dataType,
                                  List paramCodeList,
                                  TsPaintableCanvas canvas)
    {         
        
        String header = "initializeCanvas(): ";
        initializeTimeWindow(canvas);
        
        //make room for the large area required by the text header in the graph
        canvas.setTopMargin(TOP_MARGIN);
     
         
        if (dataType == PDCDataType.HEIGHT)
        {
            initForHeight(_locationId, paramCodeList, canvas);
        }
        
        else if (dataType == PDCDataType.DISCHARGE)
        {
              initForDischarge(_locationId, paramCodeList, canvas);
        }
        
        else if (dataType == PDCDataType.LAKE_STORAGE)
        {
     //       initForLakeStorage(_locationId, canvas);
        }
        
        
        else if (dataType == PDCDataType.PRECIP_TOTAL)
        {
            System.out.println(header + "before initForPrecipDualMode() ");
            initForPrecipDualMode(_locationId, paramCodeList, canvas);
            System.out.println(header + "after initForPrecipDualMode() ");
        }
        
        else if (dataType == PDCDataType.PRECIP_PC)
        {
            System.out.println(header + "before initForPrecipPC() ");
            initForPrecipPC(_locationId, paramCodeList, canvas);
            System.out.println(header + "after initForPrecipPC() ");
        }
        
        else if (dataType == PDCDataType.SNOW_WATER_EQUIVALENT)
        {
            initForSnow(_locationId, paramCodeList, canvas);
        }
        
        else if (dataType == PDCDataType.SNOW_DEPTH)
        {
            initForSnow(_locationId, paramCodeList, canvas);
        }
        
        else if (dataType == PDCDataType.TEMPERATURE)
        {
            initForTemperature(_locationId, paramCodeList, canvas);
        }
      
        else if (dataType == PDCDataType.DEWPOINT)
        {
            initForDewpoint(_locationId, paramCodeList, canvas);
        }
          
        else if (dataType == PDCDataType.WIND_DIRECTION)
        {
            initForWindDirection(_locationId, paramCodeList, canvas);
        }
        
        else if (dataType == PDCDataType.WIND_SPEED)
        {
            initForWindSpeed(_locationId, paramCodeList, canvas);
        }
        
        else if (dataType == PDCDataType.RELATIVE_HUMIDITY)
        {
            initForHumidity(_locationId, paramCodeList, canvas);
        }
       
        
        
         //call this again to make it redraw everything correctly
        // it wasn't working without this
        canvas.setTimeWindow(getStartTime(), getEndTime());
            
        return;     
    }
    
    // --------------------------------------------------------------------------------------------------
    private void initializeTimeWindow(TsPaintableCanvas canvas)
    {
        //set up time window 
        long currentTime = System.currentTimeMillis();
        long roundedCurrentTime = 
            TimeHelper.truncateTimeInMillisToNearestHour(currentTime, HOURS_TO_ROUND);
          
        setStartTime(roundedCurrentTime - (_hoursBackInTime * MILLIS_PER_HOUR));
        setEndTime(getStartTime() + (_hoursForwardInTime * MILLIS_PER_HOUR));
        
        canvas.setTimeWindow(getStartTime(), getEndTime());
   
        return; 
    }
    
    // --------------------------------------------------------------------------------------------------
    
    private boolean isValueAvailable(Measurement measurement, MeasuringUnit unit)
    {
        boolean result = false;
        
        if (measurement != null)
        {
            if (measurement.getValue(unit) > 0.0)
            {
                result = true;
            }
        }
        return result;
    }
    
    // --------------------------------------------------------------------------------------------------
    
    private void addCanvasListeners(TsPaintableCanvas canvas, ValueMapper valueMapper)
    {   
        //Add listener for the tooltip
        TsCanvasToolTipListener toolTipMgr = new TsCanvasToolTipListener(canvas, valueMapper);
        canvas.addMouseMotionListener(toolTipMgr);
        
        return;
    }
    
    // --------------------------------------------------------------------------------------------------
     
    private void handleCommandLineArgs(String[] argStringArray)
    {
        String header = "PdcDrawingManager.handleCommandLineArgs(): ";
          
        //includes the optional jdbcConnectionString
        if (argStringArray.length > 1)
        {
           _jdbcConnectionString = argStringArray[1];
           _dataMgr = new PdcTslDataManager(_jdbcConnectionString);
        }
        
        //location id
        if (argStringArray.length > 2)
        {
           _locationId = argStringArray[2];
        }
        
   
//      create param code list and dataType list
        for (int i = 3; i < argStringArray.length; i++)
        {
            String paramCodeString = argStringArray[i]; 
            System.out.println(header + "paramCodeString = " + paramCodeString);
            
            ParamCode paramCode = new ParamCode(paramCodeString);
            
            paramCode = expandParamCode(_locationId, paramCode);
            
            if (paramCode != null)
            {
                _paramCodeList.add(paramCode);
                
                // String physicalElement = paramCode.substring(0,2);
                PDCDataType dataType = getDataTypeFromPhysicalElement(paramCode.getPe());
                _dataTypeList.add(dataType);
            }        
        }
        
        return; 
    }
    //  -------------------------------------------------------------------------------------
    private ParamCode expandParamCode(String locationId, ParamCode paramCode)
    {
        
        String header = "PDCDrawingMgr.expandParamCode(): ";
        ParamCode expandedParamCode = new ParamCode(paramCode);
        String expandedParamCodeString = null;
        
        String typeSource = paramCode.getTypeSource();
        String peString = paramCode.getPe();
        
        if (typeSource.indexOf('-') == 1)
        {
            if (typeSource.charAt(0) == 'F')
            {
                typeSource = _dataMgr.getPreferredFcstTypeSource(locationId, peString);
                
                if (typeSource != null)
                {    
                    expandedParamCode.setTypeSource(typeSource);
                    expandedParamCodeString = expandedParamCode.toString();
                }
                else
                {
                    expandedParamCode = null;
                    expandedParamCodeString = null;
                }
            }
            
        }
        else
        {
            expandedParamCodeString = expandedParamCode.toString();
        }
  
        System.out.println(header + "original paramCode =  " + paramCode.toString() );
        System.out.println(header + "expandedParamCode =  " + expandedParamCodeString );
        
        return expandedParamCode;
        
    }
    
    //  -------------------------------------------------------------------------------------

    private ParamCode getParamCode(List paramCodeList)
    {
        ParamCode paramCode = null;
        
        
        if  ( (paramCodeList != null) && (paramCodeList.size() > 0) )
        {
             paramCode = (ParamCode) paramCodeList.get(0);    
        }
        
        return paramCode;
    }
    
    
    //  -------------------------------------------------------------------------------------
    private PDCDataType getDataTypeFromPhysicalElement(String physicalElement)
    {
          
        PDCDataType dataType = (PDCDataType) _physicalElementMap.get(physicalElement);
        if (dataType == null)
        {
            if (stringStartsWith(physicalElement, 'H'))
            {
                dataType = PDCDataType.HEIGHT;
            }
            if (stringStartsWith(physicalElement, 'Q'))
            {
                dataType = PDCDataType.DISCHARGE;
            }
        }
         
        return dataType;
             
    }
    //  -------------------------------------------------------------------------------------
    private boolean stringStartsWith(String physicalElement, char firstCharacter)
    {
        boolean result = false;
        
        if ( (physicalElement!= null) && 
             (physicalElement.length() > 0) &&
             (physicalElement.charAt(0) == firstCharacter)
           )
        {
            result = true;
        }
        
        return result;
    }
    //  -------------------------------------------------------------------------------------
 
    public void initForHeight(String locationId, List paramCodeList,  TsPaintableCanvas canvas)
    {
        
        String header = "PDCDrawingMgr.initForHeight(): ";
        
        MeasuringUnit unit = MeasuringUnit.feet;
        
        canvas.setDisplayedMeasuringUnit(unit);
        
        
        Color outlineColor = Color.WHITE;
        Color timeLineColor = Color.YELLOW;
        
        String leftAxisLabelString = "feet";
        String rightAxisLabelString = null; //actually depends on rating curve existence
        long currentTime = System.currentTimeMillis();
        TimeHolder currentTimeHolder = new TimeHolder();
        currentTimeHolder.setTime(currentTime);        
  
        String physicalElement = "HG";
           
        // set title and measuring units
        
        
        String titleString = "River Stages for " + locationId;
         //read in the observed time series
        
        
        
        IrregularTimeSeries obsTs = null;
        IrregularTimeSeries fcstTs = null;
        
        ParamCode obsParamCode = null;
        ParamCode fcstParamCode = null;
        
        String obsParamCodeString = null;
        String fcstParamCodeString = null;
       
        
        for (int i = 0; i < paramCodeList.size(); i++)
        {
            ParamCode paramCode = (ParamCode) paramCodeList.get(i);
             
            if (paramCode.getTypeSource().charAt(0) == 'F')
            {
                
                CodeTimer fcstStageTimer = new CodeTimer();
                fcstStageTimer.start();
                //read in the forecast time series
                fcstTs = 
                    _dataMgr.loadFcstStageTimeSeries(locationId, paramCode);
                
                fcstStageTimer.stop(header + "loadFcstStageTimeSeries took");
                
                fcstParamCode = paramCode;
                fcstParamCodeString = fcstParamCode.toString();
            }
            else //observed
            {
                CodeTimer stageTimer = new CodeTimer();
                stageTimer.start();
                
                obsTs = 
                    _dataMgr.loadObservedStageTimeSeries(locationId, paramCode, getStartTime()); 
                stageTimer.stop(header + "loadObservedStageTimeSeries took ");
                
                
                obsParamCode = paramCode;
                obsParamCodeString = obsParamCode.toString();
            }
            
            
            
            
        }
        
        //create the valueMapper
        RatingCurve ratingCurve = _dataMgr.loadRatingCurve(locationId); 
 
        
        ValueMapper valueMapper = null;
        
        
        if (ratingCurve.exists())
        {
            valueMapper = new StageToFlowValueMapper(ratingCurve); 
            rightAxisLabelString = "cfs";
        }
        else
        {
            valueMapper = new UnitValueMapper(unit, unit);
            rightAxisLabelString = "feet";
        }
        
        //background painter
        String[] textHeaderStringArray = 
                getTextHeaderStringArray(locationId, 
                                          obsTs, 
                                          obsParamCodeString,
                                          fcstTs,
                                          fcstParamCodeString,
                                          unit.toString());
        
        
        TslBackgroundPainter backgroundPainter = 
              new TslBackgroundPainter(outlineColor,
                                         leftAxisLabelString,
                                         rightAxisLabelString);
        
        backgroundPainter.setRightLabelValueMapper(valueMapper);
        backgroundPainter.setShowMinorTicks(true);   
        
     
        backgroundPainter.setTextHeaderStringArray(textHeaderStringArray);
        backgroundPainter.setTitle(titleString);
        
        assignPainterToCanvas(backgroundPainter, canvas);
     
        
        // time line painter   
        TslTimeLinePainter timeLinePainter = new TslTimeLinePainter(timeLineColor, 
                                                 currentTimeHolder);
        assignPainterToCanvas(timeLinePainter, canvas);
        
        
        //significant value line painters for floodstage and actionstage
        SigRiverLevels sigRiverLevels = _dataMgr.loadSigRiverLevels(locationId);
     
        
        //Color floodStageColor = Color.RED;
        Color floodStageColor = Color.ORANGE;
        Color actionStageColor = Color.YELLOW;
         
        
        if (sigRiverLevels != null)
        {   
            
            boolean shouldPaint = false;
            
            Measurement floodStageMeasurement = 
                new Measurement(sigRiverLevels.getFloodStage(), unit);
          
            SignificantLinePainter floodStagePainter = 
                    new SignificantLinePainter(floodStageColor, 
                                               floodStageMeasurement, 
                                               unit );
            
            shouldPaint = isValueAvailable(floodStageMeasurement,  unit);
            
            floodStagePainter.setShouldPaint(shouldPaint);
            
            assignPainterToCanvas(floodStagePainter, canvas);
            
            
            Measurement actionStageMeasurement = 
                new Measurement(sigRiverLevels.getActionStage(), unit);
          
            SignificantLinePainter actionStagePainter = 
                    new SignificantLinePainter(actionStageColor, 
                                               actionStageMeasurement, 
                                               unit );
            
            shouldPaint = isValueAvailable(actionStageMeasurement,  unit);
            
            actionStagePainter.setShouldPaint(shouldPaint);
            
            assignPainterToCanvas(actionStagePainter, canvas);
        }
         
        
        
        //observed data painter 
        if (obsTs != null)
        {
            Color obsColor = Color.YELLOW;
            IrregularTimeSeriesHolder obsTsHolder = new IrregularTimeSeriesHolder();
            obsTsHolder.setTimeSeries(obsTs);
            TsDataPointPainter obsTsPainter = new TsDataPointPainter(obsColor, obsTsHolder);
            assignPainterToCanvas(obsTsPainter, canvas);
        }
        
        // forecast data painters 
        if (fcstTs != null)
        {
            Color fcstColor = Color.GREEN;
            IrregularTimeSeriesHolder fcstTsHolder = new IrregularTimeSeriesHolder();
            fcstTsHolder.setTimeSeries(fcstTs);
            TsDataPointPainter fcstTsPainter = new TsDataPointPainter(fcstColor, fcstTsHolder);
            fcstTsPainter.setDrawLinesBetweenPoints(true);
            assignPainterToCanvas(fcstTsPainter, canvas);
        }
        
        
        addCanvasListeners(canvas, valueMapper);
        
        return;
    }  
    
     
   //------------------------------------------------------------------------------------
    public void initForHeightWithLists(String locationId,
                                       List paramCodeList,
                                       TsPaintableCanvas canvas)
    {
        
        String header = "PDCDrawingMgr.initForHeightWithLists(): ";
        
        MeasuringUnit unit = MeasuringUnit.feet;
        
        canvas.setDisplayedMeasuringUnit(unit);
        
        
        Color outlineColor = Color.WHITE;
        Color timeLineColor = Color.YELLOW;
        
        String leftAxisLabelString = "feet";
        String rightAxisLabelString = "cfs";
        long currentTime = System.currentTimeMillis();
        TimeHolder currentTimeHolder = new TimeHolder();
        currentTimeHolder.setTime(currentTime);        
  
        String physicalElement = "HG";
           
        // set title and measuring units
        
        
        String titleString = "River Stages for " + locationId;
         //read in the observed time series
        
        
        
        IrregularTimeSeries obsTs = null;
        IrregularTimeSeries fcstTs = null;
        
        
        List obsTsList = new ArrayList();
        List fcstTsList = new ArrayList();
        
        ParamCode obsParamCode = null;
        ParamCode fcstParamCode = null;
        
        String obsParamCodeString = null;
        String fcstParamCodeString = null;
        
        for (int i = 0; i < paramCodeList.size(); i++)
        {
            ParamCode paramCode = (ParamCode) paramCodeList.get(i);
             
            if (paramCode.getTypeSource().charAt(0) == 'F')
            {
                
                CodeTimer fcstStageTimer = new CodeTimer();
                fcstStageTimer.start();
                //read in the forecast time series
                fcstTs = 
                    _dataMgr.loadFcstStageTimeSeries(locationId, paramCode);
                fcstStageTimer.stop(header + "loadFcstStageTimeSeries took");
                
                fcstTsList.add(fcstTs);
                
                fcstParamCode = paramCode;
                fcstParamCodeString = fcstParamCode.toString();
            }
            else //observed
            {
                
                CodeTimer stageTimer = new CodeTimer();
                
                stageTimer.start();
                obsTs = 
                    _dataMgr.loadObservedStageTimeSeries(locationId, paramCode, getStartTime()); 
                
                stageTimer.stop(header + "loadObservedStageTimeSeries took ");
                
                
                obsTsList.add(obsTs);
                
                obsParamCode = paramCode;
                obsParamCodeString = obsParamCode.toString();
            }
            
             
           
            
            
        }
        
        //create the valueMapper
        RatingCurve ratingCurve = _dataMgr.loadRatingCurve(locationId);           
        ValueMapper valueMapper = new StageToFlowValueMapper(ratingCurve);       
        
        //background painter
        String[] textHeaderStringArray = 
                getTextHeaderStringArray(locationId, 
                                          obsTs, 
                                          obsParamCodeString,
                                          fcstTs,
                                          fcstParamCodeString,
                                          unit.toString());
        
        
        TslBackgroundPainter backgroundPainter = 
              new TslBackgroundPainter(outlineColor,
                                         leftAxisLabelString,
                                         rightAxisLabelString);
        
        backgroundPainter.setRightLabelValueMapper(valueMapper);
        backgroundPainter.setShowMinorTicks(true);   
        
     
        backgroundPainter.setTextHeaderStringArray(textHeaderStringArray);
        backgroundPainter.setTitle(titleString);
        
        assignPainterToCanvas(backgroundPainter, canvas);
     
        
        // time line painter   
        TslTimeLinePainter timeLinePainter = new TslTimeLinePainter(timeLineColor, 
                                                 currentTimeHolder);
        assignPainterToCanvas(timeLinePainter, canvas);
        
        
        //significant value line painters for floodstage and actionstage
        SigRiverLevels sigRiverLevels = _dataMgr.loadSigRiverLevels(locationId);
     
        
        Color floodStageColor = Color.RED;
        Color actionStageColor = Color.YELLOW;
         
        
        if (sigRiverLevels != null)
        {   
            boolean shouldPaint = true;
            
            Measurement floodStageMeasurement = 
                new Measurement(sigRiverLevels.getFloodStage(), unit);
          
            SignificantLinePainter floodStagePainter = 
                    new SignificantLinePainter(floodStageColor, 
                                               floodStageMeasurement, 
                                               unit );
            
            shouldPaint = isValueAvailable(floodStageMeasurement,  unit);
            
            floodStagePainter.setShouldPaint(shouldPaint);
            
            assignPainterToCanvas(floodStagePainter, canvas);
            
            
            Measurement actionStageMeasurement = 
                new Measurement(sigRiverLevels.getActionStage(), unit);
          
            SignificantLinePainter actionStagePainter = 
                    new SignificantLinePainter(actionStageColor, 
                                               actionStageMeasurement, 
                                               unit );
      
            shouldPaint = isValueAvailable(actionStageMeasurement,  unit);
            
            actionStagePainter.setShouldPaint(shouldPaint);
            
            assignPainterToCanvas(actionStagePainter, canvas);
        }
         
        
        // observed data painter 
        for (int i = 0; i < obsTsList.size(); i++)
        {
            Color obsColor = Color.YELLOW;
            IrregularTimeSeriesHolder obsTsHolder = new IrregularTimeSeriesHolder();
            obsTsHolder.setTimeSeries(obsTs);
            TsDataPointPainter obsTsPainter = new TsDataPointPainter(obsColor, obsTsHolder);
            assignPainterToCanvas(obsTsPainter, canvas);
        }
        
        
        
        // forecast data painters  
        for (int i = 0; i < fcstTsList.size(); i++)
        {
            Color fcstColor = Color.GREEN;        
            IrregularTimeSeriesHolder fcstTsHolder = new IrregularTimeSeriesHolder();
            fcstTsHolder.setTimeSeries(fcstTs);
            TsDataPointPainter fcstTsPainter = new TsDataPointPainter(fcstColor, fcstTsHolder);
            fcstTsPainter.setDrawLinesBetweenPoints(true);
            assignPainterToCanvas(fcstTsPainter, canvas);
        }
        
        
        addCanvasListeners(canvas, valueMapper);
        
        return;
    }  
    
     
   //------------------------------------------------------------------------------------
    public void initForDischarge(String locationId, List paramCodeList,  TsPaintableCanvas canvas)
    {
        
        String header = "PDCDrawingMgr.initForDischarge(): ";
        
        MeasuringUnit unit = MeasuringUnit.cfs;
        
        canvas.setDisplayedMeasuringUnit(unit);
        
        
        Color outlineColor = Color.WHITE;
        Color timeLineColor = Color.YELLOW;
        
        String leftAxisLabelString = "cfs";
        String rightAxisLabelString = null; //actually depends on rating curve existence
        
        //Set time
        long currentTime = System.currentTimeMillis();
        TimeHolder currentTimeHolder = new TimeHolder();
        currentTimeHolder.setTime(currentTime);        
  
        
        // set title and measuring units
        
        
        String titleString = "River Discharge for " + locationId;
         //read in the observed time series
 
        
        IrregularTimeSeries obsTs = null;
        IrregularTimeSeries fcstTs = null;
        
        ParamCode obsParamCode = null;
        ParamCode fcstParamCode = null;
        
        String obsParamCodeString = null;
        String fcstParamCodeString = null;
       
        
        for (int i = 0; i < paramCodeList.size(); i++)
        {
            ParamCode paramCode = (ParamCode) paramCodeList.get(i);
             
            if (paramCode.getTypeSource().charAt(0) == 'F')
            {
                
                CodeTimer fcstStageTimer = new CodeTimer();
                fcstStageTimer.start();
                //read in the forecast time series
                fcstTs = 
                    _dataMgr.loadFcstDischargeTimeSeries(locationId, paramCode);
                
                fcstStageTimer.stop(header + "loadFcstDischargeTimeSeries took");
                
                fcstParamCode = paramCode;
                fcstParamCodeString = fcstParamCode.toString();
            }
            else //observed
            {
                CodeTimer stageTimer = new CodeTimer();
                stageTimer.start();
                
                obsTs = 
                    _dataMgr.loadObservedDischargeTimeSeries(locationId, 
                                                             paramCode,
                                                             getStartTime()); 
                stageTimer.stop(header + "loadObservedDischargeTimeSeries took ");
                
                
                obsParamCode = paramCode;
                obsParamCodeString = obsParamCode.toString();
            }
            
            
            
            
        }
        
        //create the valueMapper
        RatingCurve ratingCurve = _dataMgr.loadRatingCurve(locationId); 
 
        
        ValueMapper valueMapper = null;
        
        
        if (ratingCurve.exists())
        {
            valueMapper = new FlowToStageValueMapper(ratingCurve); 
            rightAxisLabelString = "feet";
        }
        else
        {
            valueMapper = new UnitValueMapper(unit, unit);
            rightAxisLabelString = "cfs";
        }
        
        //background painter
        String[] textHeaderStringArray = 
                getTextHeaderStringArray(locationId, 
                                          obsTs, 
                                          obsParamCodeString,
                                          fcstTs,
                                          fcstParamCodeString,
                                          unit.toString());
        
        
        TslBackgroundPainter backgroundPainter = 
              new TslBackgroundPainter(outlineColor,
                                         leftAxisLabelString,
                                         rightAxisLabelString);
        
        backgroundPainter.setRightLabelValueMapper(valueMapper);
        backgroundPainter.setShowMinorTicks(true);   
        
     
        backgroundPainter.setTextHeaderStringArray(textHeaderStringArray);
        backgroundPainter.setTitle(titleString);
        
        assignPainterToCanvas(backgroundPainter, canvas);
     
        
        // time line painter   
        TslTimeLinePainter timeLinePainter = new TslTimeLinePainter(timeLineColor, 
                                                 currentTimeHolder);
        assignPainterToCanvas(timeLinePainter, canvas);
        
/*        
        //significant value line painters for floodstage and actionstage
        SigRiverLevels sigRiverLevels = _dataMgr.loadSigRiverLevels(locationId);
     
        
        Color floodStageColor = Color.RED;
        Color actionStageColor = Color.YELLOW;
         
        
        if (sigRiverLevels != null)
        {   
            
            boolean shouldPaint = false;
            
            Measurement floodStageMeasurement = 
                new Measurement(sigRiverLevels.getFloodStage(), unit);
          
            SignificantLinePainter floodStagePainter = 
                    new SignificantLinePainter(floodStageColor, 
                                               floodStageMeasurement, 
                                               unit );
            
            shouldPaint = isValueAvailable(floodStageMeasurement,  unit);
            
            floodStagePainter.setShouldPaint(shouldPaint);
            
            assignPainterToCanvas(floodStagePainter, canvas);
            
            
            Measurement actionStageMeasurement = 
                new Measurement(sigRiverLevels.getActionStage(), unit);
          
            SignificantLinePainter actionStagePainter = 
                    new SignificantLinePainter(actionStageColor, 
                                               actionStageMeasurement, 
                                               unit );
            
            shouldPaint = isValueAvailable(actionStageMeasurement,  unit);
            
            actionStagePainter.setShouldPaint(shouldPaint);
            
            assignPainterToCanvas(actionStagePainter, canvas);
        }
  */       
        
        
        //observed data painter 
        Color obsColor = Color.YELLOW;
        IrregularTimeSeriesHolder obsTsHolder = new IrregularTimeSeriesHolder();
        obsTsHolder.setTimeSeries(obsTs);
        TsDataPointPainter obsTsPainter = new TsDataPointPainter(obsColor, obsTsHolder);
        assignPainterToCanvas(obsTsPainter, canvas);
        
        // forecast data painters  
        Color fcstColor = Color.GREEN;        
        IrregularTimeSeriesHolder fcstTsHolder = new IrregularTimeSeriesHolder();
        fcstTsHolder.setTimeSeries(fcstTs);
        TsDataPointPainter fcstTsPainter = new TsDataPointPainter(fcstColor, fcstTsHolder);
        fcstTsPainter.setDrawLinesBetweenPoints(true);
        assignPainterToCanvas(fcstTsPainter, canvas);
        
        
        
        addCanvasListeners(canvas, valueMapper);
        
        return;
    }  
    
     
   //------------------------------------------------------------------------------------
    
    
   //------------------------------------------------------------------------------------
    
    public void initForTemperature(String locationId, List paramCodeList, TsPaintableCanvas canvas)
    {
        MeasuringUnit leftUnit = MeasuringUnit.degreesFahrenheit;
        MeasuringUnit rightUnit = MeasuringUnit.degreesCelsius;
        
        canvas.setDisplayedMeasuringUnit(leftUnit);
      //  canvas.setLeftMargin(30);
      //  canvas.setRightMargin(30);
     
        
        Color outlineColor = Color.WHITE;
        Color timeLineColor = Color.YELLOW;
        
        String leftAxisLabelString = "Deg. F";
        String rightAxisLabelString = "Deg. C";
        long currentTime = System.currentTimeMillis();
        TimeHolder currentTimeHolder = new TimeHolder();
        currentTimeHolder.setTime(currentTime);        
  
           
        // set title and measuring units
        
        String titleString = "Air Temperature for " + locationId;
         
        ParamCode paramCode = getParamCode(paramCodeList);
        
        
        //read in the observed time series
        IrregularTimeSeries obsTs = 
            _dataMgr.loadObservedTemperatureTimeSeries(locationId, paramCode); 
        
         
        //create the valueMapper                 
        ValueMapper valueMapper = new UnitValueMapper(leftUnit, 
                                                       rightUnit); 
        
        //background painter
        String[] textHeaderStringArray = 
                getTextHeaderStringArray(locationId, obsTs, paramCode.toString());
            
        TslBackgroundPainter backgroundPainter = 
              new TslBackgroundPainter(outlineColor,
                                         leftAxisLabelString,
                                         rightAxisLabelString);
        
        backgroundPainter.setRightLabelValueMapper(valueMapper);
        backgroundPainter.setShowMinorTicks(true);   
        
     
        backgroundPainter.setTextHeaderStringArray(textHeaderStringArray);
        backgroundPainter.setTitle(titleString);
        
        assignPainterToCanvas(backgroundPainter, canvas);
     
        
        // time line painter   
        TslTimeLinePainter timeLinePainter = new TslTimeLinePainter(timeLineColor, 
                                                 currentTimeHolder);
        assignPainterToCanvas(timeLinePainter, canvas);
        
        
        //observed data painter 
        Color obsColor = Color.YELLOW;
        IrregularTimeSeriesHolder obsTsHolder = new IrregularTimeSeriesHolder();
        obsTsHolder.setTimeSeries(obsTs);
        TsDataPointPainter obsTsPainter = new TsDataPointPainter(obsColor, obsTsHolder);
        assignPainterToCanvas(obsTsPainter, canvas);
        
      
        addCanvasListeners(canvas, valueMapper);
        
        return;
    }  
  
    //--------------------------------------------------------------------------------------
    
    public void initForWindSpeed(String locationId, List paramCodeList, TsPaintableCanvas canvas)
    {
        MeasuringUnit leftUnit = MeasuringUnit.knots;
        MeasuringUnit rightUnit = leftUnit;
        
        canvas.setDisplayedMeasuringUnit(leftUnit);
         
        Color outlineColor = Color.WHITE;
        Color timeLineColor = Color.YELLOW;
        
        String leftAxisLabelString = "Knots";
        String rightAxisLabelString = "Knots";
        long currentTime = System.currentTimeMillis();
        TimeHolder currentTimeHolder = new TimeHolder();
        currentTimeHolder.setTime(currentTime);        
  
           
        // set title and measuring units
        
        String titleString = "Wind Speed for " + locationId;
         
        ParamCode paramCode = getParamCode(paramCodeList);
        
        
        //read in the observed time series
        IrregularTimeSeries obsTs = 
            _dataMgr.loadObservedWindTimeSeries(locationId, paramCode, leftUnit); 
        
         
        //create the valueMapper                 
         ValueMapper valueMapper = new UnitValueMapper(leftUnit, 
                                                        rightUnit); 
        
        //background painter
        String[] textHeaderStringArray = 
                getTextHeaderStringArray(locationId, obsTs, paramCode.toString());
                
        TslBackgroundPainter backgroundPainter = 
              new TslBackgroundPainter(outlineColor,
                                         leftAxisLabelString,
                                         rightAxisLabelString);
        
       
        backgroundPainter.setRightLabelValueMapper(valueMapper);
        backgroundPainter.setShowMinorTicks(true);   
        
        backgroundPainter.setTextHeaderStringArray(textHeaderStringArray);
        backgroundPainter.setTitle(titleString);
        
        assignPainterToCanvas(backgroundPainter, canvas);
     
        
        // time line painter   
        TslTimeLinePainter timeLinePainter = new TslTimeLinePainter(timeLineColor, 
                                                 currentTimeHolder);
        assignPainterToCanvas(timeLinePainter, canvas);
        
        
        //observed data painter 
        Color obsColor = Color.YELLOW;
        IrregularTimeSeriesHolder obsTsHolder = new IrregularTimeSeriesHolder();
        obsTsHolder.setTimeSeries(obsTs);
        TsDataPointPainter obsTsPainter = new TsDataPointPainter(obsColor, obsTsHolder);
        assignPainterToCanvas(obsTsPainter, canvas);
        
      
        addCanvasListeners(canvas, valueMapper);
        
        return;
    }  
 //--------------------------------------------------------------------------------------
    
    public void initForWindDirection(String locationId, List paramCodeList, TsPaintableCanvas canvas)
    {
        MeasuringUnit leftUnit = MeasuringUnit.degrees;
        MeasuringUnit rightUnit = leftUnit;
        
        canvas.setDisplayedMeasuringUnit(leftUnit);
         
        Color outlineColor = Color.WHITE;
        Color timeLineColor = Color.YELLOW;
        
        String leftAxisLabelString = "Degrees";
        String rightAxisLabelString = "Degrees";
        long currentTime = System.currentTimeMillis();
        TimeHolder currentTimeHolder = new TimeHolder();
        currentTimeHolder.setTime(currentTime);        
  
           
        // set title and measuring units
        
        String titleString = "Wind Direction for " + locationId;
         
        ParamCode paramCode = getParamCode(paramCodeList);
        
        //read in the observed time series
        IrregularTimeSeries obsTs = 
            _dataMgr.loadObservedWindTimeSeries(locationId, paramCode, leftUnit); 
        
         
        //create the valueMapper                 
         ValueMapper valueMapper = new UnitValueMapper(leftUnit, 
                                                        rightUnit); 
        
        //background painter
        String[] textHeaderStringArray = 
                getTextHeaderStringArray(locationId, obsTs, paramCode.toString());
        
        
        TslBackgroundPainter backgroundPainter = 
              new TslBackgroundPainter(outlineColor,
                                         leftAxisLabelString,
                                         rightAxisLabelString);
        
       
        backgroundPainter.setRightLabelValueMapper(valueMapper);
        backgroundPainter.setShowMinorTicks(true);   
        
        backgroundPainter.setTextHeaderStringArray(textHeaderStringArray);
        backgroundPainter.setTitle(titleString);
        
        assignPainterToCanvas(backgroundPainter, canvas);
     
        
        // time line painter   
        TslTimeLinePainter timeLinePainter = new TslTimeLinePainter(timeLineColor, 
                                                 currentTimeHolder);
        assignPainterToCanvas(timeLinePainter, canvas);
        
        
        //observed data painter 
        Color obsColor = Color.YELLOW;
        IrregularTimeSeriesHolder obsTsHolder = new IrregularTimeSeriesHolder();
        obsTsHolder.setTimeSeries(obsTs);
        TsDataPointPainter obsTsPainter = new TsDataPointPainter(obsColor, obsTsHolder);
        assignPainterToCanvas(obsTsPainter, canvas);
        
      
        addCanvasListeners(canvas, valueMapper);
        
        return;
    }  
    
    // --------------------------------------------------------------------------------------------------
 
    public void initForSnow(String locationId, List paramCodeList, TsPaintableCanvas canvas)
    {
        MeasuringUnit leftUnit = MeasuringUnit.inches;
        MeasuringUnit rightUnit = MeasuringUnit.mm;
                
        canvas.setDisplayedMeasuringUnit(leftUnit);
        
        Color outlineColor = Color.WHITE;
        Color timeLineColor = Color.YELLOW;
        
        String leftAxisLabelString = "Inches";
        String rightAxisLabelString = "mm";
        long currentTime = System.currentTimeMillis();
        TimeHolder currentTimeHolder = new TimeHolder();
        currentTimeHolder.setTime(currentTime);        
  
           
        // set title and measuring units
        
        String titleString = "Snow Depth for " + locationId;
         
        
        ParamCode paramCode = getParamCode(paramCodeList);
        
        
        
        //read in the observed time series
        IrregularTimeSeries obsTs = 
            _dataMgr.loadObservedSnowTimeSeries(locationId, paramCode); 
        
         
        //create the valueMapper         
        ValueMapper valueMapper = new UnitValueMapper(leftUnit, 
                                                      rightUnit);       
        
        //background painter
        String[] textHeaderStringArray = 
                getTextHeaderStringArray(locationId, obsTs, paramCode.toString());
        
   
        TslBackgroundPainter backgroundPainter = 
              new TslBackgroundPainter(outlineColor,
                                         leftAxisLabelString,
                                         rightAxisLabelString);
        
        backgroundPainter.setRightLabelValueMapper(valueMapper);
        backgroundPainter.setShowMinorTicks(true);   
        
     
        backgroundPainter.setTextHeaderStringArray(textHeaderStringArray);
        backgroundPainter.setTitle(titleString);
        
        assignPainterToCanvas(backgroundPainter, canvas);
     
        
        // time line painter   
        TslTimeLinePainter timeLinePainter = new TslTimeLinePainter(timeLineColor, 
                                                 currentTimeHolder);
        assignPainterToCanvas(timeLinePainter, canvas);
        
        
        //observed data painter 
        Color obsColor = Color.YELLOW;
        IrregularTimeSeriesHolder obsTsHolder = new IrregularTimeSeriesHolder();
        obsTsHolder.setTimeSeries(obsTs);
        TsDataPointPainter obsTsPainter = new TsDataPointPainter(obsColor, obsTsHolder);
        assignPainterToCanvas(obsTsPainter, canvas);
        
      
        addCanvasListeners(canvas, valueMapper);
        
        return;
    }  
 
    // --------------------------------------------------------------------------------------------------
    
    public void initForDewpoint(String locationId, List paramCodeList, TsPaintableCanvas canvas)
    {
        MeasuringUnit leftUnit = MeasuringUnit.degreesFahrenheit;
        MeasuringUnit rightUnit = MeasuringUnit.degreesCelsius;
        
        
        canvas.setDisplayedMeasuringUnit(leftUnit);
      //  canvas.setLeftMargin(30);
      //  canvas.setRightMargin(30);
     
        
        Color outlineColor = Color.WHITE;
        Color timeLineColor = Color.YELLOW;
        
        String leftAxisLabelString  = "Deg. F";
        String rightAxisLabelString = "Deg. C";
        long currentTime = System.currentTimeMillis();
        TimeHolder currentTimeHolder = new TimeHolder();
        currentTimeHolder.setTime(currentTime);        
  
           
        // set title and measuring units
        
        String titleString = "DewPoint for " + locationId;
         
        ParamCode paramCode = getParamCode(paramCodeList);

        //read in the observed time series
        //dewpoint comes from the temperature table
        IrregularTimeSeries obsTs = 
            _dataMgr.loadObservedTemperatureTimeSeries(locationId, paramCode); 
        
         
        //create the valueMapper         
        ValueMapper valueMapper = new UnitValueMapper(leftUnit, 
                                                      rightUnit);       
        
        //background painter
        String[] textHeaderStringArray = 
                getTextHeaderStringArray(locationId, obsTs, paramCode.toString());
        
          
        TslBackgroundPainter backgroundPainter = 
              new TslBackgroundPainter(outlineColor,
                                         leftAxisLabelString,
                                         rightAxisLabelString);
        
        backgroundPainter.setRightLabelValueMapper(valueMapper);
        backgroundPainter.setShowMinorTicks(true);   
        
     
        backgroundPainter.setTextHeaderStringArray(textHeaderStringArray);
        backgroundPainter.setTitle(titleString);
        
        assignPainterToCanvas(backgroundPainter, canvas);
     
        
        // time line painter   
        TslTimeLinePainter timeLinePainter = new TslTimeLinePainter(timeLineColor, 
                                                 currentTimeHolder);
        assignPainterToCanvas(timeLinePainter, canvas);
        
        
        //observed data painter 
        Color obsColor = Color.YELLOW;
        IrregularTimeSeriesHolder obsTsHolder = new IrregularTimeSeriesHolder();
        obsTsHolder.setTimeSeries(obsTs);
        TsDataPointPainter obsTsPainter = new TsDataPointPainter(obsColor, obsTsHolder);
        assignPainterToCanvas(obsTsPainter, canvas);
        
      
        addCanvasListeners(canvas, valueMapper);
        
        return;
    }  
  
    
    // --------------------------------------------------------------------------------------------------
  
    public void initForHumidity(String locationId,  List paramCodeList, TsPaintableCanvas canvas)
    {
        MeasuringUnit leftUnit = MeasuringUnit.unitless;
        MeasuringUnit rightUnit = MeasuringUnit.unitless;
        
        
        canvas.setDisplayedMeasuringUnit(leftUnit);
      //  canvas.setLeftMargin(30);
      //  canvas.setRightMargin(30);
     
        
        Color outlineColor = Color.WHITE;
        Color timeLineColor = Color.YELLOW;
        
        String leftAxisLabelString = "Percent";
        String rightAxisLabelString = "Percent";
        long currentTime = System.currentTimeMillis();
        TimeHolder currentTimeHolder = new TimeHolder();
        currentTimeHolder.setTime(currentTime);        
           
        ParamCode paramCode = getParamCode(paramCodeList);      
        
        // set title and measuring units
        
        String titleString = "Relative Humidity for " + locationId;
         
        //read in the observed time series
        IrregularTimeSeries obsTs = 
            _dataMgr.loadRelativeHumidityTimeSeries(locationId, paramCode); 
        
         
        //create the valueMapper         
        ValueMapper valueMapper = new UnitValueMapper(leftUnit, 
                                                      rightUnit);       
        
        //background painter
        
        String unitString = "%";
        
        String[] textHeaderStringArray = 
                getTextHeaderStringArray(locationId, obsTs, paramCode.toString(),
                                         unitString);
              
        TslBackgroundPainter backgroundPainter = 
              new TslBackgroundPainter(outlineColor,
                                         leftAxisLabelString,
                                         rightAxisLabelString);
        
        backgroundPainter.setRightLabelValueMapper(valueMapper);
        backgroundPainter.setShowMinorTicks(true);   
        
     
        backgroundPainter.setTextHeaderStringArray(textHeaderStringArray);
        backgroundPainter.setTitle(titleString);
        
        assignPainterToCanvas(backgroundPainter, canvas);
     
        
        // time line painter   
        TslTimeLinePainter timeLinePainter = new TslTimeLinePainter(timeLineColor, 
                                                 currentTimeHolder);
        assignPainterToCanvas(timeLinePainter, canvas);
        
        
        //observed data painter 
        Color obsColor = Color.YELLOW;
        IrregularTimeSeriesHolder obsTsHolder = new IrregularTimeSeriesHolder();
        obsTsHolder.setTimeSeries(obsTs);
        TsDataPointPainter obsTsPainter = new TsDataPointPainter(obsColor, obsTsHolder);
        assignPainterToCanvas(obsTsPainter, canvas);
        
      
        addCanvasListeners(canvas, valueMapper);
        
        return;
    }  
  
    
    // --------------------------------------------------------------------------------------------------
    
    // --------------------------------------------------------------------------------------------------
    private void assignPainterToCanvas(TslCanvasPainter painter, TsPaintableCanvas canvas )
    {
        painter.setCanvas(canvas);
        canvas.addTsCanvasPainter(painter); 
        
        return;
    }
    // --------------------------------------------------------------------------------------------------
    private void assignPainterToCanvas(TslTimeLinePainter painter, TsPaintableCanvas canvas )
    {
        painter.setCanvas(canvas);
        canvas.addCanvasPainter(painter); 
        
        return;
    }
    // --------------------------------------------------------------------------------------------------
    
    private void assignPainterToCanvas(TslBackgroundPainter painter, TsPaintableCanvas canvas )
    {
        painter.setCanvas(canvas);
        canvas.addCanvasPainter(painter); 
        
        return;
    }
    // --------------------------------------------------------------------------------------------------
    
    public void initForPrecipTotal(String locationId, List paramCodeList, TsPaintableCanvas canvas)
    {
        Color outlineColor = Color.WHITE;
        Color timeLineColor = Color.YELLOW;
        
        MeasuringUnit measuringUnit = MeasuringUnit.inches;
        canvas.setDisplayedMeasuringUnit(measuringUnit);
        
        
        String leftAxisLabelString = measuringUnit.toString();
        String rightAxisLabelString = "mm";
        
        long currentTime = System.currentTimeMillis();
        TimeHolder currentTimeHolder = new TimeHolder();
        currentTimeHolder.setTime(currentTime);        
    
        
        // set title and measuring units
        
        String titleString = "Hourly Precip totals for " + locationId;
        //setCanvasTitle("Precip for " + locationId);
        
      // observed data painters 
        
        ParamCode paramCode = getParamCode(paramCodeList);
        
        int durationInHours = getDurationInHours(paramCode.getIhfsDur());
        
        RegularTimeSeries precipTimeSeries = 
                _dataMgr.loadPrecipData(locationId, durationInHours);
  
        IrregularTimeSeries irregularPrecipTimeSeries = 
                    getIrregularTimeSeries(precipTimeSeries);
        
        String[] textHeaderStringArray =
                getTextHeaderStringArray(locationId, 
                                         irregularPrecipTimeSeries,
                                         paramCode.toString());
        
         //background painter
        TslBackgroundPainter backgroundPainter = 
              new TslBackgroundPainter(outlineColor,
                                         leftAxisLabelString,
                                         rightAxisLabelString);
        
        ValueMapper valueMapper = new UnitValueMapper(MeasuringUnit.inches,
                MeasuringUnit.mm);
        backgroundPainter.setRightLabelValueMapper(valueMapper);
        backgroundPainter.setShowMinorTicks(true);     
        backgroundPainter.setTextHeaderStringArray(textHeaderStringArray);
        backgroundPainter.setTitle(titleString);
        
        assignPainterToCanvas(backgroundPainter, canvas);
       
        
        // time line painter   
        TslTimeLinePainter timeLinePainter = new TslTimeLinePainter(timeLineColor, 
                                                 currentTimeHolder);
        assignPainterToCanvas(timeLinePainter, canvas);
        
        // observed data painters 
        
        RegularTimeSeriesHolder precipTsHolder = new RegularTimeSeriesHolder();
        precipTsHolder.setTimeSeries(precipTimeSeries);
        
      
        Color barColor = Color.BLUE;
        TsBarPainter precipPainter = new TsBarPainter(barColor,
                outlineColor,
                precipTsHolder);
        
        assignPainterToCanvas(precipPainter, canvas);
    
        
        addCanvasListeners(canvas, valueMapper);
     
        
        return;
    }
    
    //  ---------------------------------------------------------------------------------- 
    public void initForPrecipDualMode(String locationId, 
                                      List paramCodeList, 
                                      TsPaintableCanvas canvas)
    {
        
 //       String header = "initForPrecipDualMode(): ";
        
        //set up the measuring unit for this method and the canvas
        MeasuringUnit unit = MeasuringUnit.inches;
        canvas.setDisplayedMeasuringUnit(unit);
        
        ParamCode pcParamCode = null;
         
        Color outlineColor = Color.WHITE;
        Color timeLineColor = Color.YELLOW;
        
        String leftAxisLabelString = "PP in.";
        String rightAxisLabelString = "mm";
        long currentTime = System.currentTimeMillis();
        TimeHolder currentTimeHolder = new TimeHolder();
        currentTimeHolder.setTime(currentTime);        
        
               
        // set title and measuring units
        ParamCode paramCode = getParamCode(paramCodeList);
       
        String titleString = "Precip PC and PP for " + locationId;
         
        //read in the observed PC time series
        
        CodeTimer pcTimer = new CodeTimer();
        pcTimer.start();
        IrregularTimeSeries obsTs = 
            _dataMgr.loadObservedPrecipPcTimeSeries(locationId,
                                                    paramCode,
                                                    getStartTime()); 
         
        pcTimer.stop("db retrieval took");
        
        //read in the preprocessed PP time series
        // that comes from PDC's preprocessed files
        int durationInHours = getDurationInHours(paramCode.getIhfsDur());

        CodeTimer pdcPrecipTimer = new CodeTimer();
        pdcPrecipTimer.start();
        
        RegularTimeSeries precipTotalsTimeSeries = 
            _dataMgr.loadPrecipData(locationId, durationInHours);
        
        pdcPrecipTimer.stop("PDC data retrieval took");
        
        //convert to irregular time series
        IrregularTimeSeries irregularPrecipTotalTimeSeries = 
            getIrregularTimeSeries(precipTotalsTimeSeries);
                
        //background painter with its ValueMapper
          //create the valueMapper
        double minPPValue = 0.0;
        
        ValueMapper valueMapper = null;
        
        IrregularTimeSeries normalizedObsTs = null;
        
        
        //set up the appropriate value mapper
        if (obsTs.getMeasurementCount() > 0)
        {
            
            rightAxisLabelString = "PC in.";
            
            Measurement minPCMeasurement = obsTs.getMinMeasurement();
            
            double minPCValue = minPPValue;
            
            if (minPCMeasurement != null)
            { 
                minPCValue = minPCMeasurement.getValue(unit);
            }
            
            valueMapper = new PrecipPPAndPCValueMapper(minPPValue,
                                                       minPCValue, 
                                                       unit);
            
            //This simply subtracts off the minPCValue from all entries,
            // so that the resulting timeseries starts at 0.0.  This will
            //enable it to be drawn on the same graph as the PP routines.
            normalizedObsTs = scaleIrregularTimeSeriesByAddition(obsTs, 
                                                             minPCValue); 
            
            //set up the secondary paramcode
            pcParamCode = new ParamCode(paramCode);
            if (paramCode.getPe().equals("PP"))
            {
                 pcParamCode.setPe("PC");
                 pcParamCode.setIhfsDur(0);
                 pcParamCode.setShefDur("I");
                 pcParamCode.setExtremum("Z");
                 pcParamCode.setTypeSource(paramCode.getTypeSource());
            }
          
        }
        else
        {    
            rightAxisLabelString = "mm";
            normalizedObsTs = obsTs;
            valueMapper = new UnitValueMapper(unit, MeasuringUnit.mm);
        }
          
        String[] textHeaderStringArray = null;
        
        if (pcParamCode != null) //there is pc data
        {
             textHeaderStringArray = getTextHeaderStringArray(locationId, 
            		           obsTs,
            		           pcParamCode.toString(),
            		           irregularPrecipTotalTimeSeries,
                               paramCode.toString(),                                                                                          
                               irregularPrecipTotalTimeSeries.getMeasuringUnit().toString()
                               );
         }
        else  //there is no pc data
        {         
            
            textHeaderStringArray = 
                    getTextHeaderStringArray(locationId, 
                                             irregularPrecipTotalTimeSeries,
                                             paramCode.toString());           
        }
        
        
          TslBackgroundPainter backgroundPainter = 
            new TslBackgroundPainter(outlineColor,
                    leftAxisLabelString,
                    rightAxisLabelString);
        
        backgroundPainter.setRightLabelValueMapper(valueMapper);
        backgroundPainter.setShowMinorTicks(true);   
         
        backgroundPainter.setTextHeaderStringArray(textHeaderStringArray);
        backgroundPainter.setTitle(titleString);
        
        assignPainterToCanvas(backgroundPainter, canvas);
                
        // time line painter   
        TslTimeLinePainter timeLinePainter =
                    new TslTimeLinePainter(timeLineColor, 
                                           currentTimeHolder);
         
        assignPainterToCanvas(timeLinePainter, canvas);
        
        
        // observed PP data painters
        RegularTimeSeriesHolder precipTsHolder = new RegularTimeSeriesHolder();
        precipTsHolder.setTimeSeries(precipTotalsTimeSeries);
        
        Color barColor = Color.BLUE;
        TsBarPainter precipPainter = new TsBarPainter(barColor,
                                                      outlineColor,
                                                      precipTsHolder);
        
        assignPainterToCanvas(precipPainter, canvas);
   
        
        //observed PC  data painter 
        Color obsColor = Color.YELLOW;
        IrregularTimeSeriesHolder obsTsHolder = new IrregularTimeSeriesHolder();
        obsTsHolder.setTimeSeries(normalizedObsTs);
        
        TsDataPointPainter obsTsPainter = new TsDataPointPainter(obsColor, obsTsHolder);
        assignPainterToCanvas(obsTsPainter, canvas);        
        
        //tooltip listener
        addCanvasListeners(canvas, valueMapper);
 
        return;
    } 
    
    //  ---------------------------------------------------------------------------------- 
    public IrregularTimeSeries scaleIrregularTimeSeriesByAddition(IrregularTimeSeries origTs,
                                                                  double additive)
    {
        String header = "PDCDrawingMgr.scaleIrregularTimeSeriesByAddition(): ";
        IrregularTimeSeries newTs = new IrregularTimeSeries(origTs.getMeasuringUnit());
        
        AbsTimeMeasurement valueArray[] = origTs.getMeasurementArray();
        double value = 0;
        
        for (int i = 0 ; i < valueArray.length; i++)
        {
            AbsTimeMeasurement newMeasurement = new AbsTimeMeasurement(valueArray[i]);
            value = newMeasurement.getValue() - additive;
            
            newMeasurement.setValue(value);
            
            newTs.insertMeasurement(newMeasurement);           
        }
        
  //      System.out.println(header + "origTs = " + origTs);
  //      System.out.println(header + "newTs = " + newTs);
        
        return newTs;
    }

    //  ---------------------------------------------------------------------------------- 
    
    public void initForPrecipPC(String locationId, List paramCodeList, TsPaintableCanvas canvas)
    {
        
        //set up the measuring unit for this method and the canvas
        MeasuringUnit unit = MeasuringUnit.inches;
        canvas.setDisplayedMeasuringUnit(unit);
        
        
        Color outlineColor = Color.WHITE;
        Color timeLineColor = Color.YELLOW;
        
        String leftAxisLabelString = "inches";
        String rightAxisLabelString = "mm";
        long currentTime = System.currentTimeMillis();
        TimeHolder currentTimeHolder = new TimeHolder();
        currentTimeHolder.setTime(currentTime);        
  
        // set title and measuring units
        
        ParamCode paramCode = getParamCode(paramCodeList);
        
        
        String titleString = "Precip PC for " + locationId;
         
        //read in the observed time series
        IrregularTimeSeries obsTs = 
            _dataMgr.loadObservedPrecipPcTimeSeries(locationId, paramCode, getStartTime()); 
  
        
        //background painter
        String[] textHeaderStringArray = 
                getTextHeaderStringArray(locationId, obsTs, paramCode.toString());
           
        TslBackgroundPainter backgroundPainter = 
              new TslBackgroundPainter(outlineColor,
                                         leftAxisLabelString,
                                         rightAxisLabelString);
        
        //create the valueMapper
        ValueMapper valueMapper = new UnitValueMapper(unit, MeasuringUnit.mm);
        
        backgroundPainter.setRightLabelValueMapper(valueMapper);
        backgroundPainter.setShowMinorTicks(true);   
        
     
        backgroundPainter.setTextHeaderStringArray(textHeaderStringArray);
        backgroundPainter.setTitle(titleString);
        
        assignPainterToCanvas(backgroundPainter, canvas);
     
        
        // time line painter   
        TslTimeLinePainter timeLinePainter = new TslTimeLinePainter(timeLineColor, 
                                                 currentTimeHolder);
        assignPainterToCanvas(timeLinePainter, canvas);
        
         
        //observed data painter 
        Color obsColor = Color.YELLOW;
        IrregularTimeSeriesHolder obsTsHolder = new IrregularTimeSeriesHolder();
        obsTsHolder.setTimeSeries(obsTs);
        TsDataPointPainter obsTsPainter = new TsDataPointPainter(obsColor, obsTsHolder);
        assignPainterToCanvas(obsTsPainter, canvas);
    
        
        //tooltip listener
        addCanvasListeners(canvas, valueMapper);
        
        return;
    }  

    //  ---------------------------------------------------------------------------------- 
    
    private int getDurationInHours(int ihfsDurationCode)
    {
        int durationInHours = 0;
            
        if ( (ihfsDurationCode >= 1000) && (ihfsDurationCode < 2000) )
        {
            durationInHours = ihfsDurationCode - 1000;
        }
        if (ihfsDurationCode == 2001)
        {
            durationInHours = 24;
        }
        
        return durationInHours;  
    }   
    //  ---------------------------------------------------------------------------------- 
    
    private IrregularTimeSeries getIrregularTimeSeries(RegularTimeSeries origTs)
    {        
        MeasuringUnit unit = origTs.getMeasuringUnit();
        IrregularTimeSeries irregularTimeSeries = new IrregularTimeSeries(unit);
        
        int count = origTs.getMeasurementCount();
        
        AbsTimeMeasurement measurement = null;
        
        for (int i = 0; i < count; i++)
        {
            measurement = origTs.getAbsTimeMeasurementByIndex(i);
            irregularTimeSeries.insertMeasurement(measurement);    
        }
              
        return irregularTimeSeries;
    }
    
    
    //  -------------------------------------------------------------------------------------
    private String[] getTextHeaderStringArray(String locationId,
                                              IrregularTimeSeries obsTs,
                                              String paramCode
                                              )
    
    {
        MeasuringUnit unit = obsTs.getMeasuringUnit();
        String unitString = unit.getName();
        
        String[] textHeaderStringArray = getTextHeaderStringArray(locationId,
                                            obsTs, paramCode, null, null,
                                            unitString);
     
        return textHeaderStringArray;
    }
 
    //  -------------------------------------------------------------------------------------
    
    
    private String[] getTextHeaderStringArray(String locationId,
                                              IrregularTimeSeries obsTs,
                                              String paramCode,
                                              String unitString
                                              )
    
    {
          
        String[] textHeaderStringArray = getTextHeaderStringArray(locationId,
                                            obsTs, paramCode, null, null,
                                            unitString);
     
        return textHeaderStringArray;
    }
 
    //  -------------------------------------------------------------------------------------
    
    private String[] getTextHeaderStringArray(String locationId,
                                              IrregularTimeSeries obsTs,
                                              String paramCodeString,
                                              IrregularTimeSeries fcstTs,
                                              String otherParamCodeString,
                                              String unitString
                                              )
    
    {
        
        String header = "getTextHeadStringArray()[6-args] : "; 
        
        String[] textHeaderStringArray =  null;
        IrregularTimeSeries maxMinTs = null;
        AbsTimeMeasurement max =null;
        AbsTimeMeasurement min =null;
        AbsTimeMeasurement last = null;
        
        if (paramCodeString == null)
        {
            paramCodeString = "";
        }
        StringBuffer paramCodeStringBuffer = new StringBuffer(paramCodeString);
     
        
        Location location = _dataMgr.getLocationInfo(locationId);
        if (location == null)
        {
            location = new Location();
            location.setLid(locationId);
            location.setLocationName("Unknown location");
            location.setElevation(0);
            
        }
        
        //determine units and the max, min, and last values
        MeasuringUnit unit = determineUnit(obsTs, fcstTs);
        
        long startTime = getStartTime();
        long endTime = getEndTime();
        
       
        System.out.println(header + "before get max and min and last measurements ");
        
        // special handle for the PP precipitation data
          	                
    	if (otherParamCodeString != null && (otherParamCodeString.substring(0, 2).equalsIgnoreCase("PP")) 
    			&& fcstTs != null)
    	{    		    	
    	    max = getMaxTsMeasurement(fcstTs, null, unit, startTime, endTime);  
            System.out.println(header + "after get max " + max.getValue());

            min = getMinTsMeasurement(fcstTs, null, unit, startTime, endTime);
            System.out.println(header + "after get min ");

            last = getLastMeasurement(fcstTs, null, unit, startTime, endTime);
            System.out.println(header + "after get last ");

            System.out.println(header + "after get max and min and last measurements ");    		
    		
    	}
    	else
    	{
            max = getMaxTsMeasurement(obsTs, fcstTs, unit, startTime, endTime);  
            System.out.println(header + "after get max " + max.getValue());
        
            min = getMinTsMeasurement(obsTs, fcstTs, unit, startTime, endTime);
            System.out.println(header + "after get min ");
                
            last = getLastMeasurement(obsTs, fcstTs, unit, startTime, endTime);
            System.out.println(header + "after get last ");
                
            System.out.println(header + "after get max and min and last measurements ");
         
    	}
    	
        //append to the param code string
        if (otherParamCodeString != null)
        {
            paramCodeStringBuffer.append("    " + otherParamCodeString);
        }      
        
        // assign values to the textHeaderStringArray
        textHeaderStringArray = new String[5];
        textHeaderStringArray[0] = locationId + " - " + 
        location.getLocationName() + " - " +
        "Elevation: " + location.getElevation() + " feet";
        
        textHeaderStringArray[1] = paramCodeStringBuffer.toString();        
        textHeaderStringArray[2] = "Max = " + getMeasurementString(max, unitString);
        textHeaderStringArray[3] = "Min = " + getMeasurementString(min,  unitString);
        textHeaderStringArray[4] = "Last = " + getMeasurementString(last, unitString);
        
        return textHeaderStringArray;
    }
    // --------------------------------------------------------------------------------------------------
    private MeasuringUnit determineUnit(IrregularTimeSeries ts1, IrregularTimeSeries ts2)
    {
           MeasuringUnit unit = MeasuringUnit.feet;
           IrregularTimeSeries notNullTs = null;
           
           // pick an available time series to use for calculation max and min and last values
           if (ts1 != null)
           {
               notNullTs = ts1;
           }
           else if (ts2 != null)
           {
               notNullTs = ts2;
           }
           else
           {
               notNullTs = null;
           }
           
           if (notNullTs != null)
           {
               unit = notNullTs.getMeasuringUnit();
           }
           
           return unit;
    }
    
    // --------------------------------------------------------------------------------------------------

    private AbsTimeMeasurement getLastMeasurement(IrregularTimeSeries ts1,
                                                  IrregularTimeSeries ts2,
                                                  MeasuringUnit unit,
                                                  long startTime, 
                                                  long endTime)
    {
        AbsTimeMeasurement measurement = null;
        AbsTimeMeasurement m1 = null;
        AbsTimeMeasurement m2 = null;

        m1 = getLastMeasurement(ts1, unit, startTime, endTime);
        m2 = getLastMeasurement(ts2, unit, startTime, endTime);
        
        
        if ((m1 != null) && (m2 != null))
        {                   	
        	if (m1.getTime() > m2.getTime())
            {
                measurement = m1;
            }
            else
            {
                measurement = m2;
            }
        	
        }
        else if (m1 != null)
        {
            measurement = m1;
        }
        else if (m2 != null)
        {
            measurement = m2;
        }
        
        return measurement;
    }
    
    // --------------------------------------------------------------------------------------------------
    
    
    private AbsTimeMeasurement getLastMeasurement(IrregularTimeSeries timeSeries,
                                                  MeasuringUnit unit,
                                                  long startTime, 
                                                  long endTime)
    {
        AbsTimeMeasurement measurement = null;
         
        if (timeSeries != null)
        {
            int count = timeSeries.getMeasurementCount();

            for (int i = count - 1; i >= 0; i--)
            {
                measurement = timeSeries.getAbsTimeMeasurementByIndex(i);
                if (measurement != null)
                {
                    long time = measurement.getTime();

                    if ((time <= endTime) && (time >= startTime))
                    {
                        break; // quit, I found the measurement that I want.
                    }
                }
            }
        }
        
        measurement = getMeasurementByUnit(measurement, unit);
         
        return measurement;
        
    }
    // --------------------------------------------------------------------------------------------------
    private AbsTimeMeasurement getMaxTsMeasurement(IrregularTimeSeries ts1, 
                                                   IrregularTimeSeries ts2,
                                                   MeasuringUnit unit,
                                                   long startTime,
                                                   long endTime)
    {
        AbsTimeMeasurement returnMeasurement = null;
        
        AbsTimeMeasurement m1 = null;
        AbsTimeMeasurement m2 = null;
        int notNullCount = 0;
        
        if (ts1 != null)
        {
            m1 = ts1.getMaxMeasurement(startTime, endTime);
            if (m1 != null)
            {
                returnMeasurement = m1;
                notNullCount++;
            }
        }
        if (ts2 != null)
        {
            m2 = ts2.getMaxMeasurement(startTime, endTime);
            if (m2 != null)
            {
                notNullCount++;
                if (notNullCount == 1) // ts1 was null
                {
                    returnMeasurement = m2;
                }
                else
                // notNullCount == 2, so ts1 was not null, need to compare
                {                	                	
                    if (m2.getValue(unit) > m1.getValue(unit))
                    {
                        returnMeasurement = m2;
                    }
                	
                }
            }
        }
              
        returnMeasurement = getMeasurementByUnit(returnMeasurement, unit);
        
        return returnMeasurement;
    }
    
    // --------------------------------------------------------------------------------------------------
    private AbsTimeMeasurement getMinTsMeasurement(IrregularTimeSeries ts1, 
                                                   IrregularTimeSeries ts2,
                                                   MeasuringUnit unit,
                                                   long startTime,
                                                   long endTime)
    {
        AbsTimeMeasurement returnMeasurement = null;
        
        AbsTimeMeasurement m1 = null;
        AbsTimeMeasurement m2 = null;
        int notNullCount = 0;
            
        if (ts1 != null)
        {
            m1 = ts1.getMinMeasurement(startTime, endTime);
            if (m1 != null)
            {
                returnMeasurement = m1;
                notNullCount++;
            }
        }
        if (ts2 != null)
        {
            m2 = ts2.getMinMeasurement(startTime, endTime);
            if (m2 != null)
            {
                notNullCount++;
                if (notNullCount == 1) // ts1 was null
                {
                    returnMeasurement = m2;
                }
                else
                // notNullCount == 2, so ts1 was not null, need to compare
                {                	                
                    if (m2.getValue(unit) < m1.getValue(unit))
                    {
                        returnMeasurement = m2;
                    }
                	
                }
            }
        }
        
        returnMeasurement = getMeasurementByUnit(returnMeasurement, unit);
                
        return returnMeasurement;
    }
    
    // --------------------------------------------------------------------------------------------------
    private AbsTimeMeasurement getMeasurementByUnit(AbsTimeMeasurement origMeasurement,
                                                    MeasuringUnit unit)
    {
        AbsTimeMeasurement measurement = origMeasurement;
        
        if ( (origMeasurement != null) && (origMeasurement.getUnit() != unit))
        {
            measurement = AbsTimeMeasurement.getConvertedCopy(origMeasurement, unit);
        } 
        
        return measurement;
    }
    
    // --------------------------------------------------------------------------------------------------
    
    
    private String getMeasurementString(AbsTimeMeasurement measurement, String unitString)
    {
        String measurementString = null;
        
        if (measurement != null)
        {
            String timeString =
                DbTimeHelper.getDateTimeStringFromLongTime(measurement.getTime());
            
            //get rid of the seconds
            timeString = timeString.substring(0, timeString.length()-3);
            
            double value = MathHelper.roundToNDecimalPlaces(measurement.getValue(), 2);
            
            measurementString = 
                value + " " +
                unitString + 
                " at " + timeString + "Z";
            
        }
        else
        {
           measurementString = "Not Available";    
        }
        
        return measurementString;
    }
    
    // --------------------------------------------------------------------------------------------------
    private String wrapInHTML(String origString)
    {
        StringBuffer buffer = new StringBuffer("<HTML><CENTER>" + origString +  "</CENTER></HTML>");
        
        return buffer.toString();     
    } 
//  ------------------------------------------------------------
    private String getVerticalText(String origText)
    {
        char[] charArray = origText.toCharArray();
        StringBuffer buffer = new StringBuffer();
        
        buffer.append("<HTML>");
        for (int i = 0; i < charArray.length; i++)
        {
            buffer.append(charArray[i]);
            
            //don't do a break for the last one
            if (i < charArray.length-1)
            {
                buffer.append("<BR>");
            }
        }
        buffer.append("</HTML>");
        
        String verticalText = buffer.toString();
       
        return verticalText;     
    }
//  ------------------------------------------------------------
    private String getVerticalTextFromNewLines(String origText)
    {
        char[] charArray = origText.toCharArray();
        StringBuffer buffer = new StringBuffer();
        
        buffer.append("<HTML>");
        for (int i = 0; i < charArray.length; i++)
        {
            char c = charArray[i];
            if (c == '\n')
            {
                buffer.append("<BR>");
            }
            else
            {
               buffer.append(c);
            }
        }
        buffer.append("</HTML>");
        
        String verticalText = buffer.toString();
       
        return verticalText;     
    }
    // --------------------------------------------------------------------------------------------------
    

    /**
     * @param startTime The startTime to set.
     */
    public void setStartTime(long startTime)
    {
        _startTime = startTime;
    }
    
    // --------------------------------------------------------------------------------------------------
    

    /**
     * @return Returns the startTime.
     */
    public long getStartTime()
    {
        return _startTime;
    }
    
    // --------------------------------------------------------------------------------------------------
    

    /**
     * @param endTime The endTime to set.
     */
    public void setEndTime(long endTime)
    {
        _endTime = endTime;
    }
    /**
     * @return Returns the endTime.
     */
    public long getEndTime()
    {
        return _endTime;
    }
    
    // --------------------------------------------------------------------------------------------------
    
    private class RefreshTimerListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e) 
        {
            refresh();  
        }
    }
    
    //  --------------------------------------------------------------------------------------------------
    
    private void refresh()
    {     
        String header = "PDCDrawingMgr.refresh(): ";
        
        System.out.println(header + "Refreshing");
        
        display(_rememberedArgStringArray, _exitOnClose);
    }
    
    //  --------------------------------------------------------------------------------------------------
 
}
