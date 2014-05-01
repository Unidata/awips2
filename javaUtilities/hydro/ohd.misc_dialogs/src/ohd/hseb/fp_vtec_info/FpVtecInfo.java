package ohd.hseb.fp_vtec_info;

import java.awt.Color;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import ohd.hseb.measurement.IrregularTimeSeries;
import ohd.hseb.measurement.IrregularTimeSeriesHolder;
import ohd.hseb.measurement.Measurement;
import ohd.hseb.measurement.MeasuringUnit;
//import ohd.hseb.measurement.RegularTimeSeries;
import ohd.hseb.model.FlowToStageValueMapper;
import ohd.hseb.model.RatingCurve;
import ohd.hseb.model.SigRiverLevels;
import ohd.hseb.model.StageToFlowValueMapper;
import ohd.hseb.timeserieslite.PDCDataType;
import ohd.hseb.timeserieslite.ParamCode;

import ohd.hseb.timeserieslite.gui.drawing.SignificantLinePainter;
import ohd.hseb.timeserieslite.gui.drawing.TsCanvasToolTipListener;
import ohd.hseb.timeserieslite.gui.drawing.TsDataPointPainter;
import ohd.hseb.timeserieslite.gui.drawing.TslCanvasPainter;
import ohd.hseb.timeserieslite.gui.drawing.TslTimeLinePainter;
import ohd.hseb.timeserieslite.gui.drawing.TslBackgroundPainter;
import ohd.hseb.util.CodeTimer;
import ohd.hseb.util.TimeHelper;
import ohd.hseb.util.TimeHolder;
import ohd.hseb.util.UnitValueMapper;
import ohd.hseb.util.ValueMapper;
import ohd.hseb.util.gui.drawing.TsPaintableCanvas;
import ohd.hseb.fp_vtec_info.FpCurPrevVtec;
import ohd.hseb.fp_vtec_info.FpVtecInfoFrame;

public class FpVtecInfo {
	private static final long MISSING_INT = -9999;

	private static final long MILLIS_PER_HOUR = 60 * 60 * 1000;

	private static final long MILLIS_PER_SECOND = 1000;

	private static final int TOP_MARGIN = 100;

	private int _defaultHoursBackInTime = 120;

	private int _defaultHoursForwardInTime = 336;

	private FpVtecInfoDataManager _dataMgr = null;

	private String _locationId = null;

	private List _paramCodeList = new ArrayList();

	private String _jdbcConnectionString = null;

	private FpCurPrevVtec _fpCurPrevVtecRecord = null;

	private PDCDataType _dataType = null;

	private List _dataTypeList = new ArrayList();

	private long _startTime = 0;

	private long _endTime = 0;

	private int HOURS_TO_ROUND = 6;

	private FpVtecInfoFrame _frame = null;

	private TsPaintableCanvas _canvas = null;

	private TimeHolder _curVtecBeginTimeHolder = new TimeHolder();

	private TimeHolder _curVtecEndTimeHolder = new TimeHolder();

	private TimeHolder _curVtecRiseTimeHolder = new TimeHolder();

	private TimeHolder _curVtecCrestTimeHolder = new TimeHolder();

	private TimeHolder _curVtecFallTimeHolder = new TimeHolder();

	private static Map _physicalElementMap = new HashMap();
	static {
		_physicalElementMap.put("HG", PDCDataType.HEIGHT);
		_physicalElementMap.put("HP", PDCDataType.HEIGHT);
		_physicalElementMap.put("HT", PDCDataType.HEIGHT);
		_physicalElementMap.put("HM", PDCDataType.HEIGHT);
		_physicalElementMap.put("HL", PDCDataType.HEIGHT);
		_physicalElementMap.put("HZ", PDCDataType.HEIGHT);
		_physicalElementMap.put("HK", PDCDataType.HEIGHT);
		_physicalElementMap.put("QR", PDCDataType.DISCHARGE);
		_physicalElementMap.put("QT", PDCDataType.DISCHARGE);
		_physicalElementMap.put("QS", PDCDataType.DISCHARGE);
		_physicalElementMap.put("QA", PDCDataType.DISCHARGE);
		_physicalElementMap.put("QM", PDCDataType.DISCHARGE);

	}

	// -------------------------------------------------------------------------------------

	public static void show(String dbConnString, String locationId,
			String paramCodeString1, String paramCodeString2) {

		String[] argStringArray = { dbConnString, locationId, paramCodeString1,
				paramCodeString2 };

		FpVtecInfoFrame.setLastUpdateTime(FpVtecInfoFrame.STILL_OPENED);

		for (int i = 0; i < argStringArray.length; i++) {
			System.out.println("argStringArray[" + i + "] = "
					+ argStringArray[i]);
		}

		FpVtecInfo app = new FpVtecInfo();
		app.display(argStringArray, true);

		// need to refresh frame in "Update Graph" and "Save Changes"
		app.getFrame().setApplication(app);

	}

	// --------------------------------------------------------------------------------------------------

	public static void main(String[] argStringArray) {
		FpVtecInfo app = new FpVtecInfo();
		app.display(argStringArray, true);

		// need to refresh frame in "Update Graph" and "Save Changes"
		app.getFrame().setApplication(app);

	}

	// --------------------------------------------------------------------------------------------------
	public void display(String[] argStringArray, boolean exitOnClose) {

		CodeTimer timer = new CodeTimer();
		System.out
				.println("Start to display event's timeseries and VTEC information");

		// retrive the input arguments
		handleCommandLineArgs(argStringArray);

		String header = "FpVtecInfo.display()";

		// read FP, VTEC information from input file
		timer.start();
		_dataMgr.readInputFileFormFpCurPrevVtecInfoMap();
		timer.stop(header + " read input file took");

		// set the timeseries beginTime and endTime based on backhrs and
		// forecasthrs
		// store FP/VTEC information to _fpCurPrevVtecRecord for location
		// _locationId

		_fpCurPrevVtecRecord = setTSBeginEndTimeWindow(_locationId);

		// prepare the JFrame for display window
		if (_frame == null)
			_frame = new FpVtecInfoFrame(_fpCurPrevVtecRecord, _dataMgr);

		_frame.setExitOnClose(exitOnClose);

		// get canvas from frame
		_canvas = _frame.getCanvas();

		// determine the stage or flow data type

		if (_dataTypeList.size() > 0) {
			_dataType = (PDCDataType) _dataTypeList.get(0);
		}

		// draw the canvas
		timer.start();
		initializeCanvas(_dataType, _paramCodeList, _canvas);
		timer.stop(header + " initializeCanvas() took ");

		timer.start();
		_frame.setVisible(true);
		timer.stop(header + "setVisible() took ");

		return;
	}

	// --------------------------------------------------------------------------------------------------

	private void handleCommandLineArgs(String[] argStringArray) {
		String header = "FpVtecInfo.handleCommandLineArgs(): ";

		// includes the optional jdbcConnectionString
		if (argStringArray.length > 0) {
			_jdbcConnectionString = argStringArray[0];
			_dataMgr = new FpVtecInfoDataManager(_jdbcConnectionString);
			System.out.println("_jdbcConnection = " + _jdbcConnectionString);
		}

		// location id
		if (argStringArray.length > 1) {
			_locationId = argStringArray[1];
			System.out.println("_locationId = " + _locationId);
		}

		// create param code list and dataType list
		for (int i = 2; i < argStringArray.length; i++) {
			String paramCodeString = argStringArray[i];
			System.out.println(header + "paramCodeString = " + paramCodeString);

			if ((paramCodeString != null) && (paramCodeString.length() == 6)) // valid
																				// example
																				// =
																				// HRIRGZ
			{
				ParamCode paramCode = new ParamCode(paramCodeString);

				paramCode = expandParamCode(_locationId, paramCode);

				if (paramCode != null) {
					_paramCodeList.add(paramCode);

					// String physicalElement = paramCode.substring(0,2);
					PDCDataType dataType = getDataTypeFromPhysicalElement(paramCode
							.getPe());
					_dataTypeList.add(dataType);
				}
			}
		}

		return;
	}

	// --------------------------------------------------------------------------------------------------

	private void initializeCanvas(PDCDataType dataType, List paramCodeList,
			TsPaintableCanvas canvas) {
		initializeTimeWindow(canvas);

		// make room for the large area required by the text header in the graph
		canvas.setTopMargin(TOP_MARGIN);

		if (dataType == PDCDataType.HEIGHT) {

			initForHeight(_locationId, paramCodeList, canvas);
		}

		else if (dataType == PDCDataType.DISCHARGE) {
			initForDischarge(_locationId, paramCodeList, canvas);
		}

		// call this again to make it redraw everything correctly
		// it wasn't working without this

		canvas.setTimeWindow(getStartTime(), getEndTime());

		return;
	}

	// -----------------------------------------------------------------------
	public void redrawCanvas() {
		_canvas.refresh();

		return;
	}

	// --------------------------------------------------------------------------------------------------
	private void initializeTimeWindow(TsPaintableCanvas canvas) {
		setStartTime(_startTime);
		setEndTime(_endTime);

		canvas.setTimeWindow(getStartTime(), getEndTime());

		return;
	}

	// ----------------------------------------------------------------------------------------------------
	private FpCurPrevVtec setTSBeginEndTimeWindow(String locationId) {
		String header = "FpVtecInfo.setTSBeginEndTime() ";
		Map tempMap = new HashMap();

		// get current time
		long currentTime = System.currentTimeMillis();
		long roundedCurrentTime = TimeHelper.truncateTimeInMillisToNearestHour(
				currentTime, HOURS_TO_ROUND);

		// set up time window
		// get the backhrs and forwardhrs from record for locationId

		tempMap = _dataMgr.getvtecinfoMap();

		FpCurPrevVtec temp = (FpCurPrevVtec) tempMap.get(locationId);

		if (temp != null) {
			long obs_begintime_inmillis = temp.getObsBeginTime()
					* MILLIS_PER_SECOND;
			long fcst_endtime_inmillis = temp.getFcstEndTime()
					* MILLIS_PER_SECOND;

			long rounded_obs_begintime = TimeHelper
					.truncateTimeInMillisToNearestHour(obs_begintime_inmillis,
							HOURS_TO_ROUND);
			long rounded_fcst_endtime = TimeHelper
					.truncateTimeInMillisToNearestHour(fcst_endtime_inmillis,
							HOURS_TO_ROUND);

			if ((temp.getObsBeginTime() == MISSING_INT)
					|| (temp.getObsBeginTime() == 0))
				setStartTime(roundedCurrentTime
						- (_defaultHoursBackInTime * MILLIS_PER_HOUR));
			else
				setStartTime(rounded_obs_begintime);

			if ((temp.getFcstEndTime() == MISSING_INT)
					|| (temp.getFcstEndTime() == 0))
				setEndTime(roundedCurrentTime
						+ (_defaultHoursForwardInTime * MILLIS_PER_HOUR));
			else
				setEndTime(rounded_fcst_endtime);

		}

		else {
			System.out.println(header + "VTEC record not found for "
					+ locationId);
		}

		return temp;

	}

	// --------------------------------------------------------------------------------------------------

	private boolean isValueAvailable(Measurement measurement, MeasuringUnit unit) {
		boolean result = false;

		if (measurement != null) {
			if (measurement.getValue(unit) > 0.0) {
				result = true;
			}
		}
		return result;
	}

	// --------------------------------------------------------------------------------------------------

	private void addCanvasListeners(TsPaintableCanvas canvas,
			ValueMapper valueMapper) {
		// Add listener for the tooltip
		TsCanvasToolTipListener toolTipMgr = new TsCanvasToolTipListener(
				canvas, valueMapper);
		canvas.addMouseMotionListener(toolTipMgr);

		// Add listener for the timelines drag
		TimeLineAdjustmentListener adjustmentListener = new TimeLineAdjustmentListener(
				this, canvas, getCurVtecBeginTimeHolder(),
				getCurVtecEndTimeHolder(), getCurVtecRiseTimeHolder(),
				getCurVtecCrestTimeHolder(), getCurVtecFallTimeHolder());
		canvas.addMouseListener(adjustmentListener);

		return;
	}

	// -------------------------------------------------------------------------------------
	private ParamCode expandParamCode(String locationId, ParamCode paramCode) {

		String header = "FpVtecInfo.expandParamCode(): ";
		ParamCode expandedParamCode = new ParamCode(paramCode);
		String expandedParamCodeString = null;

		String typeSource = paramCode.getTypeSource();
		String peString = paramCode.getPe();

		if (typeSource.indexOf('-') == 1) {
			if (typeSource.charAt(0) == 'F') {
				typeSource = _dataMgr.getPreferredFcstTypeSource(locationId,
						peString);

				if (typeSource != null) {
					expandedParamCode.setTypeSource(typeSource);
					expandedParamCodeString = expandedParamCode.toString();
				} else {
					expandedParamCode = null;
					expandedParamCodeString = null;
				}
			}

		} else {
			expandedParamCodeString = expandedParamCode.toString();
		}

		System.out.println(header + "original paramCode =  "
				+ paramCode.toString());
		System.out.println(header + "expandedParamCode =  "
				+ expandedParamCodeString);

		return expandedParamCode;

	}

	// -------------------------------------------------------------------------------------

	PDCDataType getDataTypeFromPhysicalElement(String physicalElement) {

		PDCDataType dataType = (PDCDataType) _physicalElementMap
				.get(physicalElement);
		if (dataType == null) {
			if (stringStartsWith(physicalElement, 'H')) {
				dataType = PDCDataType.HEIGHT;
			}
			if (stringStartsWith(physicalElement, 'Q')) {
				dataType = PDCDataType.DISCHARGE;
			}
		}

		return dataType;

	}

	// -------------------------------------------------------------------------------------

	private boolean stringStartsWith(String physicalElement, char firstCharacter) {
		boolean result = false;

		if ((physicalElement != null) && (physicalElement.length() > 0)
				&& (physicalElement.charAt(0) == firstCharacter)) {
			result = true;
		}

		return result;
	}

	// -------------------------------------------------------------------------------------

	public void initForHeight(String locationId, List paramCodeList,
			TsPaintableCanvas canvas) {
		long curvtec_beginTime = 0;
		long curvtec_endTime = 0;
		long curvtec_riseTime = 0;
		long curvtec_crestTime = 0;
		long curvtec_fallTime = 0;

		String header = "FpVtecInfo.initForHeight(): ";

		MeasuringUnit unit = MeasuringUnit.feet;

		canvas.setDisplayedMeasuringUnit(unit);

		Color outlineColor = Color.WHITE;
		Color cur_timeLineColor = Color.WHITE;
		Color curvtec_begintimeLineColor = Color.BLUE;
		Color curvtec_endtimeLineColor = Color.GREEN;
		Color curvtec_risetimeLineColor = Color.ORANGE;
		Color curvtec_cresttimeLineColor = Color.RED;
		Color curvtec_falltimeLineColor = Color.MAGENTA;

		String leftAxisLabelString = "feet";
		String rightAxisLabelString = null; // actually depends on rating curve
											// existence
		long currentTime = System.currentTimeMillis();
		TimeHolder currentTimeHolder = new TimeHolder();
		currentTimeHolder.setTime(currentTime);

		// handel the begintime saved as CURRENT time
		if (_fpCurPrevVtecRecord.getcurBeginTime() == -1) {
			getCurVtecBeginTimeHolder().setTime(currentTime);
		} else if (_fpCurPrevVtecRecord.getcurBeginTime() == MISSING_INT) {
			// do not draw the MSG time
			getCurVtecBeginTimeHolder().setTime(MISSING_INT);
		} else {
			curvtec_beginTime = _fpCurPrevVtecRecord.getcurBeginTime()
					* MILLIS_PER_SECOND;
			getCurVtecBeginTimeHolder().setTime(curvtec_beginTime);

		}

		// handel the endtime as CURRENT time
		if (_fpCurPrevVtecRecord.getcurEndTime() == -1) {
			getCurVtecEndTimeHolder().setTime(currentTime);
		} else if (_fpCurPrevVtecRecord.getcurEndTime() == MISSING_INT) {
			// do not draw the MSG time
			getCurVtecEndTimeHolder().setTime(MISSING_INT);
		} else {
			curvtec_endTime = _fpCurPrevVtecRecord.getcurEndTime()
					* MILLIS_PER_SECOND;
			getCurVtecEndTimeHolder().setTime(curvtec_endTime);
		}

		// handle the risetime
		if (_fpCurPrevVtecRecord.getcurRiseTime() == MISSING_INT) {
			// do not draw the MSG time
			getCurVtecRiseTimeHolder().setTime(MISSING_INT);
		} else {
			curvtec_riseTime = _fpCurPrevVtecRecord.getcurRiseTime()
					* MILLIS_PER_SECOND;
			getCurVtecRiseTimeHolder().setTime(curvtec_riseTime);
		}

		// handle the crest time
		if (_fpCurPrevVtecRecord.getcurCrestTime() == MISSING_INT) {
			// do not draw the MSG time
			getCurVtecCrestTimeHolder().setTime(MISSING_INT);
		} else {
			curvtec_crestTime = _fpCurPrevVtecRecord.getcurCrestTime()
					* MILLIS_PER_SECOND;
			getCurVtecCrestTimeHolder().setTime(curvtec_crestTime);
		}

		// handle the falltime
		if (_fpCurPrevVtecRecord.getcurFallTime() == MISSING_INT) {
			// do not draw the MSG time
			getCurVtecFallTimeHolder().setTime(MISSING_INT);
		} else {
			curvtec_fallTime = _fpCurPrevVtecRecord.getcurFallTime()
					* MILLIS_PER_SECOND;
			getCurVtecFallTimeHolder().setTime(curvtec_fallTime);
		}

		// set title and measuring units

		String titleString = "River Stages for " + locationId;

		// read in the observed time series

		IrregularTimeSeries obsTs = null;
		IrregularTimeSeries fcstTs = null;

		ParamCode obsParamCode = null;
		ParamCode fcstParamCode = null;

		String obsParamCodeString = null;
		String fcstParamCodeString = null;

		for (int i = 0; i < paramCodeList.size(); i++) {
			ParamCode paramCode = (ParamCode) paramCodeList.get(i);

			if (paramCode.getTypeSource().charAt(0) == 'F') {

				CodeTimer fcstStageTimer = new CodeTimer();
				fcstStageTimer.start();

				// read in the forecast time series

				fcstTs = _dataMgr.loadFcstStageTimeSeries(locationId,
						paramCode, getEndTime(), _fpCurPrevVtecRecord);

				fcstStageTimer.stop(header + "loadFcstStageTimeSeries took");

				fcstParamCode = paramCode;
				fcstParamCodeString = fcstParamCode.toString();
			} else // observed
			{
				CodeTimer stageTimer = new CodeTimer();
				stageTimer.start();

				obsTs = _dataMgr.loadObservedStageTimeSeries(locationId,
						paramCode, getStartTime(), _fpCurPrevVtecRecord);

				stageTimer.stop(header + "loadObservedStageTimeSeries took ");

				obsParamCode = paramCode;
				obsParamCodeString = obsParamCode.toString();
			}

		}

		// create the valueMapper
		RatingCurve ratingCurve = _dataMgr.loadRatingCurve(locationId);

		ValueMapper valueMapper = null;

		if (ratingCurve.exists()) {
			valueMapper = new StageToFlowValueMapper(ratingCurve);
			rightAxisLabelString = "cfs";
		} else {
			valueMapper = new UnitValueMapper(unit, unit);
			rightAxisLabelString = "feet";
		}

		// the textheader in the TimeSeries/VTEC info window
		String[] textHeaderStringArray = null;

		if (_fpCurPrevVtecRecord != null) {
			textHeaderStringArray = getTextHeaderStringArray(locationId,
					_fpCurPrevVtecRecord, obsParamCodeString.substring(0, 5),
					fcstParamCodeString.substring(0, 5), unit.toString());

		}

		TslBackgroundPainter backgroundPainter = new TslBackgroundPainter(
				outlineColor, leftAxisLabelString, rightAxisLabelString);

		backgroundPainter.setRightLabelValueMapper(valueMapper);
		backgroundPainter.setShowMinorTicks(true);

		backgroundPainter.setTextHeaderStringArray(textHeaderStringArray);
		backgroundPainter.setTitle(titleString);

		assignPainterToCanvas(backgroundPainter, canvas);

		// time lines painter
		// Current time line
		if (currentTimeHolder != null) {
			// TslTimeLinePainter timeLinePainter = new
			// TslTimeLinePainter(cur_timeLineColor,
			// "N", 95, currentTimeHolder);
			TslTimeLinePainter timeLinePainter = new TslTimeLinePainter(
					cur_timeLineColor, "N", 95, currentTimeHolder);

			assignPainterToCanvas(timeLinePainter, canvas);
		}

		// Event begin time line
		
		TslTimeLinePainter curvtec_begintimeLinePainter = new TslTimeLinePainter(
				curvtec_begintimeLineColor, "B", 95,
				getCurVtecBeginTimeHolder());
		assignPainterToCanvas(curvtec_begintimeLinePainter, canvas);
		
		// Event end time line
		
		TslTimeLinePainter curvtec_endtimeLinePainter = new TslTimeLinePainter(
				curvtec_endtimeLineColor, "E", 95, getCurVtecEndTimeHolder());

		assignPainterToCanvas(curvtec_endtimeLinePainter, canvas);
		
		// Event Rise time line
		
		TslTimeLinePainter curvtec_risetimeLinePainter = new TslTimeLinePainter(
					curvtec_risetimeLineColor, "R", 95,
					getCurVtecRiseTimeHolder());

		assignPainterToCanvas(curvtec_risetimeLinePainter, canvas);
		
		// Event Crest time line
		
		TslTimeLinePainter curvtec_cresttimeLinePainter = new TslTimeLinePainter(
					curvtec_cresttimeLineColor, "C", 95,
					getCurVtecCrestTimeHolder());

		assignPainterToCanvas(curvtec_cresttimeLinePainter, canvas);
		

		// Event Fall time line
		
		TslTimeLinePainter curvtec_falltimeLinePainter = new TslTimeLinePainter(
					curvtec_falltimeLineColor, "F", 95,
					getCurVtecFallTimeHolder());

		assignPainterToCanvas(curvtec_falltimeLinePainter, canvas);
		

		// significant value line painters for floodstage and actionstage

		SigRiverLevels sigRiverLevels = _dataMgr.loadSigRiverLevels(locationId);

		Color actionStageColor = Color.YELLOW;
		Color floodStageColor = Color.ORANGE;
		Color moderateStageColor = Color.RED;
		Color majorStageColor = Color.MAGENTA;

		if (sigRiverLevels != null) {
			boolean shouldPaint = false;

			// flood stage
			Measurement floodStageMeasurement = new Measurement(sigRiverLevels
					.getFloodStage(), unit);

			SignificantLinePainter floodStagePainter = new SignificantLinePainter(
					floodStageColor, floodStageMeasurement, unit);

			shouldPaint = isValueAvailable(floodStageMeasurement, unit);
			floodStagePainter.setShouldPaint(shouldPaint);
			assignPainterToCanvas(floodStagePainter, canvas);

			// action stage
			Measurement actionStageMeasurement = new Measurement(sigRiverLevels
					.getActionStage(), unit);

			SignificantLinePainter actionStagePainter = new SignificantLinePainter(
					actionStageColor, actionStageMeasurement, unit);

			shouldPaint = isValueAvailable(actionStageMeasurement, unit);
			actionStagePainter.setShouldPaint(shouldPaint);
			assignPainterToCanvas(actionStagePainter, canvas);

			// moderate stage
			double moderatefs = sigRiverLevels.getModerateFloodStage();
			Measurement moderateStageMeasurement = new Measurement(
					sigRiverLevels.getModerateFloodStage(), unit);

			SignificantLinePainter moderateStagePainter = new SignificantLinePainter(
					moderateStageColor, moderateStageMeasurement, unit);

			shouldPaint = isValueAvailable(moderateStageMeasurement, unit);
			moderateStagePainter.setShouldPaint(shouldPaint);
			assignPainterToCanvas(moderateStagePainter, canvas);

			// major stage
			Measurement majorStageMeasurement = new Measurement(sigRiverLevels
					.getMajorFloodStage(), unit);

			SignificantLinePainter majorStagePainter = new SignificantLinePainter(
					majorStageColor, majorStageMeasurement, unit);

			shouldPaint = isValueAvailable(majorStageMeasurement, unit);
			majorStagePainter.setShouldPaint(shouldPaint);
			assignPainterToCanvas(majorStagePainter, canvas);

		}

		// observed data painter
		Color obsColor = Color.YELLOW;
		IrregularTimeSeriesHolder obsTsHolder = new IrregularTimeSeriesHolder();
		obsTsHolder.setTimeSeries(obsTs);
		TsDataPointPainter obsTsPainter = new TsDataPointPainter(obsColor,
				obsTsHolder);
		obsTsPainter.setDrawLinesBetweenPoints(true);
		assignPainterToCanvas(obsTsPainter, canvas);

		// forecast data painters
		Color fcstColor = Color.GREEN;
		IrregularTimeSeriesHolder fcstTsHolder = new IrregularTimeSeriesHolder();
		fcstTsHolder.setTimeSeries(fcstTs);
		TsDataPointPainter fcstTsPainter = new TsDataPointPainter(fcstColor,
				fcstTsHolder);
		fcstTsPainter.setDrawLinesBetweenPoints(true);
		assignPainterToCanvas(fcstTsPainter, canvas);

		addCanvasListeners(canvas, valueMapper);

		return;
	}

	// ------------------------------------------------------------------------------------
	public void initForDischarge(String locationId, List paramCodeList,
			TsPaintableCanvas canvas) {
		long curvtec_beginTime = 0;
		long curvtec_endTime = 0;
		long curvtec_riseTime = 0;
		long curvtec_crestTime = 0;
		long curvtec_fallTime = 0;

		String header = "FpVtecInfo.initForDischarge(): ";

		MeasuringUnit unit = MeasuringUnit.cfs;

		canvas.setDisplayedMeasuringUnit(unit);

		Color outlineColor = Color.WHITE;
		Color timeLineColor = Color.WHITE;
		Color curvtec_begintimeLineColor = Color.BLUE;
		Color curvtec_endtimeLineColor = Color.GREEN;
		Color curvtec_risetimeLineColor = Color.ORANGE;
		Color curvtec_cresttimeLineColor = Color.RED;
		Color curvtec_falltimeLineColor = Color.MAGENTA;

		String leftAxisLabelString = "cfs";
		String rightAxisLabelString = null; // actually depends on rating curve
											// existence

		// Set time
		long currentTime = System.currentTimeMillis();
		TimeHolder currentTimeHolder = new TimeHolder();
		currentTimeHolder.setTime(currentTime);

		// handel the begintime saved as CURRENT time
		if (_fpCurPrevVtecRecord.getcurBeginTime() == -1) {
			// curvtec_beginTimeHolder = new TimeHolder();
			getCurVtecBeginTimeHolder().setTime(currentTime);
		} else if (_fpCurPrevVtecRecord.getcurBeginTime() != -9999) {
			curvtec_beginTime = _fpCurPrevVtecRecord.getcurBeginTime()
					* MILLIS_PER_SECOND;
			// curvtec_beginTimeHolder = new TimeHolder();
			getCurVtecBeginTimeHolder().setTime(curvtec_beginTime);
		}
		if (_fpCurPrevVtecRecord.getcurEndTime() != -9999) {
			curvtec_endTime = _fpCurPrevVtecRecord.getcurEndTime()
					* MILLIS_PER_SECOND;
			// curvtec_endTimeHolder = new TimeHolder();
			getCurVtecEndTimeHolder().setTime(curvtec_endTime);
		}
		if (_fpCurPrevVtecRecord.getcurRiseTime() != -9999) {
			curvtec_riseTime = _fpCurPrevVtecRecord.getcurRiseTime()
					* MILLIS_PER_SECOND;
			// curvtec_riseTimeHolder = new TimeHolder();
			getCurVtecRiseTimeHolder().setTime(curvtec_riseTime);
		}
		if (_fpCurPrevVtecRecord.getcurCrestTime() != -9999) {
			curvtec_crestTime = _fpCurPrevVtecRecord.getcurCrestTime()
					* MILLIS_PER_SECOND;
			// curvtec_crestTimeHolder = new TimeHolder();
			getCurVtecCrestTimeHolder().setTime(curvtec_crestTime);
		}
		if (_fpCurPrevVtecRecord.getcurFallTime() != -9999) {
			curvtec_fallTime = _fpCurPrevVtecRecord.getcurFallTime()
					* MILLIS_PER_SECOND;
			// curvtec_fallTimeHolder = new TimeHolder();
			getCurVtecFallTimeHolder().setTime(curvtec_fallTime);
		}

		// set title and measuring units

		String titleString = "River Discharge for " + locationId;
		// read in the observed time series

		IrregularTimeSeries obsTs = null;
		IrregularTimeSeries fcstTs = null;

		ParamCode obsParamCode = null;
		ParamCode fcstParamCode = null;

		String obsParamCodeString = null;
		String fcstParamCodeString = null;

		for (int i = 0; i < paramCodeList.size(); i++) {
			ParamCode paramCode = (ParamCode) paramCodeList.get(i);

			if (paramCode.getTypeSource().charAt(0) == 'F') {

				CodeTimer fcstStageTimer = new CodeTimer();
				fcstStageTimer.start();
				// read in the forecast time series
				fcstTs = _dataMgr.loadFcstDischargeTimeSeries(locationId,
						paramCode, getEndTime(), _fpCurPrevVtecRecord);

				fcstStageTimer
						.stop(header + "loadFcstDischargeTimeSeries took");

				fcstParamCode = paramCode;
				fcstParamCodeString = fcstParamCode.toString();
			} else // observed
			{
				CodeTimer stageTimer = new CodeTimer();
				stageTimer.start();

				obsTs = _dataMgr.loadObservedDischargeTimeSeries(locationId,
						paramCode, getStartTime(), _fpCurPrevVtecRecord);
				stageTimer.stop(header
						+ "loadObservedDischargeTimeSeries took ");

				obsParamCode = paramCode;
				obsParamCodeString = obsParamCode.toString();
			}

		}

		// create the valueMapper
		RatingCurve ratingCurve = _dataMgr.loadRatingCurve(locationId);

		ValueMapper valueMapper = null;

		if (ratingCurve.exists()) {
			valueMapper = new FlowToStageValueMapper(ratingCurve);
			rightAxisLabelString = "feet";
		} else {
			valueMapper = new UnitValueMapper(unit, unit);
			rightAxisLabelString = "cfs";
		}

		String[] textHeaderStringArray = null;

		if (_fpCurPrevVtecRecord != null) {
			textHeaderStringArray = getTextHeaderStringArray(locationId,
					_fpCurPrevVtecRecord, obsParamCodeString.substring(0, 5),
					fcstParamCodeString.substring(0, 5), unit.toString());
		}

		TslBackgroundPainter backgroundPainter = new TslBackgroundPainter(
				outlineColor, leftAxisLabelString, rightAxisLabelString);

		backgroundPainter.setRightLabelValueMapper(valueMapper);
		backgroundPainter.setShowMinorTicks(true);

		backgroundPainter.setTextHeaderStringArray(textHeaderStringArray);
		backgroundPainter.setTitle(titleString);

		assignPainterToCanvas(backgroundPainter, canvas);

		// time line painter
		if (currentTimeHolder != null) {
			TslTimeLinePainter nowtimeLinePainter = new TslTimeLinePainter(
					timeLineColor, "N", 95, currentTimeHolder);
			// nowtimeLineLabelPainter.setTimeType("N");
			assignPainterToCanvas(nowtimeLinePainter, canvas);
		}

		if (getCurVtecBeginTimeHolder() != null) {
			TslTimeLinePainter curvtec_begintimeLinePainter = new TslTimeLinePainter(
					curvtec_begintimeLineColor, "B", 95,
					getCurVtecBeginTimeHolder());
			assignPainterToCanvas(curvtec_begintimeLinePainter, canvas);
		}

		if (getCurVtecEndTimeHolder() != null) {
			TslTimeLinePainter curvtec_endtimeLinePainter = new TslTimeLinePainter(
					curvtec_endtimeLineColor, "E", 95,
					getCurVtecEndTimeHolder());

			assignPainterToCanvas(curvtec_endtimeLinePainter, canvas);
		}

		if (getCurVtecRiseTimeHolder() != null) {
			TslTimeLinePainter curvtec_risetimeLinePainter = new TslTimeLinePainter(
					curvtec_risetimeLineColor, "R", 95,
					getCurVtecRiseTimeHolder());

			assignPainterToCanvas(curvtec_risetimeLinePainter, canvas);
		}

		if (getCurVtecCrestTimeHolder() != null) {
			TslTimeLinePainter curvtec_cresttimeLinePainter = new TslTimeLinePainter(
					curvtec_cresttimeLineColor, "C", 95,
					getCurVtecCrestTimeHolder());

			assignPainterToCanvas(curvtec_cresttimeLinePainter, canvas);
		}

		if (getCurVtecFallTimeHolder() != null) {
			TslTimeLinePainter curvtec_falltimeLinePainter = new TslTimeLinePainter(
					curvtec_falltimeLineColor, "F", 95,
					getCurVtecFallTimeHolder());

			assignPainterToCanvas(curvtec_falltimeLinePainter, canvas);
		}

		// significant value line painters for floodflow and actionflow
		SigRiverLevels sigRiverLevels = _dataMgr.loadSigRiverLevels(locationId);

		Color floodFlowColor = Color.RED;
		Color moderateFlowColor = Color.MAGENTA;
		Color majorFlowColor = Color.CYAN;
		Color actionFlowColor = Color.YELLOW;

		if (sigRiverLevels != null) {
			boolean shouldPaint = false;

			// Flood Flow
			Measurement floodFlowMeasurement = new Measurement(sigRiverLevels
					.getFloodFlow(), unit);

			SignificantLinePainter floodFlowPainter = new SignificantLinePainter(
					floodFlowColor, floodFlowMeasurement, unit);

			shouldPaint = isValueAvailable(floodFlowMeasurement, unit);
			floodFlowPainter.setShouldPaint(shouldPaint);
			assignPainterToCanvas(floodFlowPainter, canvas);

			// action flow
			Measurement actionFlowMeasurement = new Measurement(sigRiverLevels
					.getActionFlow(), unit);

			SignificantLinePainter actionFlowPainter = new SignificantLinePainter(
					actionFlowColor, actionFlowMeasurement, unit);

			shouldPaint = isValueAvailable(actionFlowMeasurement, unit);
			actionFlowPainter.setShouldPaint(shouldPaint);
			assignPainterToCanvas(actionFlowPainter, canvas);

			// moderate flow
			Measurement moderateFlowMeasurement = new Measurement(
					sigRiverLevels.getModerateFloodFlow(), unit);

			SignificantLinePainter moderateFlowPainter = new SignificantLinePainter(
					moderateFlowColor, moderateFlowMeasurement, unit);

			shouldPaint = isValueAvailable(moderateFlowMeasurement, unit);
			moderateFlowPainter.setShouldPaint(shouldPaint);
			assignPainterToCanvas(moderateFlowPainter, canvas);

			// major flow
			Measurement majorFlowMeasurement = new Measurement(sigRiverLevels
					.getMajorFloodFlow(), unit);

			SignificantLinePainter majorFlowPainter = new SignificantLinePainter(
					majorFlowColor, majorFlowMeasurement, unit);

			shouldPaint = isValueAvailable(majorFlowMeasurement, unit);
			majorFlowPainter.setShouldPaint(shouldPaint);
			assignPainterToCanvas(majorFlowPainter, canvas);

		}

		// observed data painter
		Color obsColor = Color.YELLOW;
		IrregularTimeSeriesHolder obsTsHolder = new IrregularTimeSeriesHolder();
		obsTsHolder.setTimeSeries(obsTs);
		TsDataPointPainter obsTsPainter = new TsDataPointPainter(obsColor,
				obsTsHolder);
		obsTsPainter.setDrawLinesBetweenPoints(true);
		assignPainterToCanvas(obsTsPainter, canvas);

		// forecast data painters
		Color fcstColor = Color.GREEN;
		IrregularTimeSeriesHolder fcstTsHolder = new IrregularTimeSeriesHolder();
		fcstTsHolder.setTimeSeries(fcstTs);
		TsDataPointPainter fcstTsPainter = new TsDataPointPainter(fcstColor,
				fcstTsHolder);
		fcstTsPainter.setDrawLinesBetweenPoints(true);
		assignPainterToCanvas(fcstTsPainter, canvas);

		addCanvasListeners(canvas, valueMapper);

		return;
	}

	// ------------------------------------------------------------------------------------

	private void assignPainterToCanvas(TslCanvasPainter painter,
			TsPaintableCanvas canvas) {
		painter.setCanvas(canvas);
		canvas.addTsCanvasPainter(painter);

		return;
	}

	// --------------------------------------------------------------------------------------------------
	private void assignPainterToCanvas(TslTimeLinePainter painter,
			TsPaintableCanvas canvas) {
		painter.setCanvas(canvas);
		canvas.addCanvasPainter(painter);

		return;
	}

	// --------------------------------------------------------------------------------------------------

	private void assignPainterToCanvas(TslBackgroundPainter painter,
			TsPaintableCanvas canvas) {
		painter.setCanvas(canvas);
		canvas.addCanvasPainter(painter);

		return;
	}

	// --------------------------------------------------------------------------------------------------

	private String[] getTextHeaderStringArray(String locationId,
			FpCurPrevVtec fpcurprevvtecRecord, String paramCodeString,
			String fcstParamCodeString, String unitString) {
		String[] textHeaderStringArray = null;
		StringBuffer paramCodeStringBuffer = new StringBuffer(paramCodeString);
		if (fcstParamCodeString != null) {
			paramCodeStringBuffer.append("    " + fcstParamCodeString);
		}

		textHeaderStringArray = new String[3];

		// Lid-Name-FldStg
		// if (unitString.equals("feet"))
		if (_dataType == PDCDataType.HEIGHT)
			textHeaderStringArray[0] = locationId + " - "
					+ fpcurprevvtecRecord.getLocName() + " - "
					+ fpcurprevvtecRecord.getLocStream() + " - "
					+ "Flood Stage: " + fpcurprevvtecRecord.getFloodStg() + " "
					+ unitString;
		else
			textHeaderStringArray[0] = locationId + " - "
					+ fpcurprevvtecRecord.getLocName() + " - "
					+ fpcurprevvtecRecord.getLocStream() + " - "
					+ "Flood Flow: " + fpcurprevvtecRecord.getFloodFlow()
					+ unitString;

		// PE-TS
		textHeaderStringArray[1] = "PE/DUR/TS:  "
				+ paramCodeStringBuffer.toString();

		// Current VTEC phenom, signif and ETN

		textHeaderStringArray[2] = "Proposed VTEC Phenom:  "
				+ fpcurprevvtecRecord.getcurPhenom() + "  " + "Signif:  "
				+ fpcurprevvtecRecord.getcurSignif() + "  " + "ETN:  "
				+ fpcurprevvtecRecord.getcurEtn();

		return textHeaderStringArray;

	}

	// --------------------------------------------------------------------------------------------------
	/*
	 * private String wrapInHTML(String origString) { StringBuffer buffer = new
	 * StringBuffer("<HTML><CENTER>" + origString + "</CENTER></HTML>");
	 * 
	 * return buffer.toString(); }
	 */
	// ------------------------------------------------------------
	/*
	 * private String getVerticalText(String origText) { char[] charArray =
	 * origText.toCharArray(); StringBuffer buffer = new StringBuffer();
	 * 
	 * buffer.append("<HTML>"); for (int i = 0; i < charArray.length; i++) {
	 * buffer.append(charArray[i]);
	 * 
	 * //don't do a break for the last one if (i < charArray.length-1) {
	 * buffer.append("<BR>"); } } buffer.append("</HTML>");
	 * 
	 * String verticalText = buffer.toString();
	 * 
	 * return verticalText; }
	 *  //
	 * --------------------------------------------------------------------------------------------------
	 * 
	 * 
	 * /** @param startTime The startTime to set.
	 */
	public void setStartTime(long startTime) {
		_startTime = startTime;
	}

	// --------------------------------------------------------------------------------------------------

	/**
	 * @return Returns the startTime.
	 */
	public long getStartTime() {
		return _startTime;
	}

	// --------------------------------------------------------------------------------------------------

	/**
	 * @param endTime
	 *            The endTime to set.
	 */
	public void setEndTime(long endTime) {
		_endTime = endTime;
	}

	/**
	 * @return Returns the endTime.
	 */
	public long getEndTime() {
		return _endTime;
	}

	// --------------------------------------------------------------------------------------------------
	/**
	 * @return _fpCurPrevVtecRecord
	 */
	public FpCurPrevVtec getFpCurPrevVtecRecord() {
		return _fpCurPrevVtecRecord;
	}

	public FpVtecInfoFrame getFrame() {
		return _frame;
	}

	public void setFrame(FpVtecInfoFrame _frame) {
		this._frame = _frame;
	}

	public FpVtecInfoDataManager get_dataMgr() {
		return _dataMgr;
	}

	public void set_dataMgr(FpVtecInfoDataManager mgr) {
		_dataMgr = mgr;
	}

	public TimeHolder getCurVtecBeginTimeHolder() {
		return _curVtecBeginTimeHolder;
	}

	public TimeHolder getCurVtecEndTimeHolder() {
		return _curVtecEndTimeHolder;
	}

	public TimeHolder getCurVtecRiseTimeHolder() {
		return _curVtecRiseTimeHolder;
	}

	public TimeHolder getCurVtecCrestTimeHolder() {
		return _curVtecCrestTimeHolder;
	}

	public TimeHolder getCurVtecFallTimeHolder() {
		return _curVtecFallTimeHolder;
	}

}
