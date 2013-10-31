package ohd.hseb.timeserieslite.rivermon;

import java.awt.Color;
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
import ohd.hseb.model.FlowToStageValueMapper;
import ohd.hseb.model.RatingCurve;
import ohd.hseb.model.SigRiverLevels;
import ohd.hseb.model.StageToFlowValueMapper;
import ohd.hseb.timeserieslite.Location;
import ohd.hseb.timeserieslite.PDCDataType;
import ohd.hseb.timeserieslite.ParamCode;
import ohd.hseb.timeserieslite.TSLDrawingMgr;
import ohd.hseb.timeserieslite.TSLFrame;
import ohd.hseb.timeserieslite.gui.drawing.SignificantLinePainter;
import ohd.hseb.timeserieslite.gui.drawing.TsCanvasToolTipListener;
import ohd.hseb.timeserieslite.gui.drawing.TsDataPointPainter;
import ohd.hseb.timeserieslite.gui.drawing.TslCanvasPainter;
import ohd.hseb.timeserieslite.gui.drawing.TslTimeLinePainter;
import ohd.hseb.timeserieslite.gui.drawing.TslBackgroundPainter;
import ohd.hseb.util.CodeTimer;
import ohd.hseb.util.MathHelper;
import ohd.hseb.util.TimeHelper;
import ohd.hseb.util.TimeHolder;
import ohd.hseb.util.UnitValueMapper;
import ohd.hseb.util.ValueMapper;
import ohd.hseb.util.gui.drawing.TsPaintableCanvas;

public class RiverMonDrawingMgr implements TSLDrawingMgr
{
    private static final long MILLIS_PER_HOUR = 60*60*1000;
    private static final int TOP_MARGIN = 100;
    private int _hoursBackInTime = 120;
    private int _hoursForwardInTime = 168;
    
    private RiverMonTslDataManager _dataMgr = null;
    private String _locationId = null;
    private List _paramCodeList = new ArrayList();
    private String _jdbcConnectionString = null;
    
  //  private PDCDataType _dataType = null;
    private List _dataTypeList = new ArrayList();
    
    private long _startTime = 0;
    private long _endTime = 0;
 
    private int HOURS_TO_ROUND = 6;
    
    
    private static Map _physicalElementMap = new HashMap();
    static 
    {
        _physicalElementMap.put("HG", PDCDataType.HEIGHT);
        _physicalElementMap.put("HP", PDCDataType.HEIGHT);
        _physicalElementMap.put("HT", PDCDataType.HEIGHT);
        _physicalElementMap.put("QR", PDCDataType.DISCHARGE);
        _physicalElementMap.put("QT", PDCDataType.DISCHARGE);
        _physicalElementMap.put("LS", PDCDataType.LAKE_STORAGE);
       
    }
    
 
    // --------------------------------------------------------------------------------------------------
    
    public RiverMonDrawingMgr()
    {
             
    }
    // --------------------------------------------------------------------------------------------------
    public void display(String[] argStringArray, boolean exitOnClose)
    {
        
        CodeTimer timer = new CodeTimer();
        
        handleCommandLineArgs(argStringArray);
        
        String header = "PDCDrawingMgr.display()";
        
        
        TSLFrame frame = new TSLFrame();
        
        frame.setExitOnClose(exitOnClose);
        
        System.out.println(header + "after TSLFrame()");
        
        TsPaintableCanvas canvas = frame.getCanvas();
        
        
        PDCDataType dataType = null;
            
        if ( _dataTypeList.size() > 0 )
        {
            dataType = (PDCDataType) _dataTypeList.get(0);
        }
        
        initializeCanvas(dataType, _paramCodeList, canvas);
        
        timer.start();
        frame.setVisible(true);
        timer.stop("setVisible() took ");
        
    }
    
    // --------------------------------------------------------------------------------------------------
    
    private void initializeCanvas(PDCDataType dataType,
                                  List paramCodeList,
                                  TsPaintableCanvas canvas)
    {         
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
           _dataMgr = new RiverMonTslDataManager(_jdbcConnectionString);
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
            
            if ((paramCodeString != null) && (paramCodeString.length() == 6)) //valid example = HRIRGZ
            {
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
        }
        
        return; 
    }
    //  -------------------------------------------------------------------------------------
    private ParamCode expandParamCode(String locationId, ParamCode paramCode)
    {
        
        String header = "RiverMonDrawingMgr.expandParamCode(): ";
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

    ParamCode getParamCode(List paramCodeList)
    {
        ParamCode paramCode = null;
        
        
        if  ( (paramCodeList != null) && (paramCodeList.size() > 0) )
        {
             paramCode = (ParamCode) paramCodeList.get(0);    
        }
        
        return paramCode;
    }
    
    
    //  -------------------------------------------------------------------------------------

    PDCDataType getDataTypeFromPhysicalElement(String physicalElement)
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
        
        String header = "RiverMonDrawingMgr.initForHeight(): ";
        
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
        
        String obsParamCodeString = "";
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
    public void initForHeightWithLists(String locationId,
                                       List paramCodeList,
                                       TsPaintableCanvas canvas)
    {
        
        String header = "RiverMonDrawingMgr.initForHeightWithLists(): ";
        
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
        
        String header = "RiverMonDrawingMgr.initForDischarge(): ";
        
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
                                              String fcstParamCodeString,
                                              String unitString
                                              )
    
    {
        AbsTimeMeasurement max =  null;
        AbsTimeMeasurement min =  null;
        AbsTimeMeasurement last =  null;
        
        String[] textHeaderStringArray =  null;
        StringBuffer paramCodeStringBuffer = new StringBuffer(paramCodeString);
        
        Location location = _dataMgr.getLocationInfo(locationId);
        if (location == null)
        {
            location = new Location();
            location.setLid(locationId);
            location.setLocationName("Unknown location");
            location.setElevation(0);
            
        }
       
        
        if (obsTs != null)
        {          
            long startTime = getStartTime();
            long endTime = getEndTime();
            
            max = obsTs.getMaxMeasurement(startTime, endTime);
            min = obsTs.getMinMeasurement(startTime, endTime);
            last = getLastMeasurement(obsTs, startTime, endTime);
        }
        
        if (fcstParamCodeString != null)
        {
            paramCodeStringBuffer.append("    " + fcstParamCodeString);
        }
//      
        
        textHeaderStringArray = new String[5];
        textHeaderStringArray[0] = locationId + " - " + 
        location.getLocationName() + " - " +
        "Elevation: " + location.getElevation() + " feet";
        
        //  textHeaderStringArray[1] = "Elevation: " + location.getElevation() + " feet";
        textHeaderStringArray[1] = paramCodeStringBuffer.toString();
       
        
        textHeaderStringArray[2] = "Max Obs = " + getMeasurementString(max, unitString);
        textHeaderStringArray[3] = "Min Obs = " + getMeasurementString(min,  unitString);
        textHeaderStringArray[4] = "Last Obs = " + getMeasurementString(last, unitString);
        
        return textHeaderStringArray;
    }
    // --------------------------------------------------------------------------------------------------
    private AbsTimeMeasurement getLastMeasurement(IrregularTimeSeries timeSeries,
                                           long startTime, 
                                           long endTime)
    {
        AbsTimeMeasurement measurement = null;
         
        int count = timeSeries.getMeasurementCount();
        
        for (int i = count - 1; i >= 0; i--)
        {
            measurement = timeSeries.getAbsTimeMeasurementByIndex(i);
            if (measurement != null)
            {
                long time = measurement.getTime();
                
                if ((time <= endTime) && (time >= startTime))
                {
                    break;    //quit, I found the measurement that I want.
                }
            }
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
    

}
