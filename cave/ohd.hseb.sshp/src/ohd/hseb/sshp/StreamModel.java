/*
 * Created on Nov 3, 2003
 * This class took over nearly all of AppController's old duties.
 * AppController now is in charge of setting up the environment
 * for handling multiple model windows.
 * AppController was created on Sep 17, 2003
 * 
 */
package ohd.hseb.sshp;

import java.util.List;

import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.measurement.AbsTimeMeasurement;
import ohd.hseb.measurement.IrregularTimeSeries;
import ohd.hseb.measurement.IrregularTimeSeriesHolder;
import ohd.hseb.measurement.Measurement;
import ohd.hseb.measurement.MeasurementHolder;
import ohd.hseb.measurement.MeasuringUnit;
import ohd.hseb.measurement.RegularTimeSeries;
import ohd.hseb.measurement.RegularTimeSeriesHolder;
import ohd.hseb.measurement.RelTimeMeasurement;
import ohd.hseb.model.ForecastAdjuster;
import ohd.hseb.model.ForecastAdjusterParams;
import ohd.hseb.model.MonthlyValues;
import ohd.hseb.model.RainfallRunoffModel;
import ohd.hseb.model.RainfallRunoffModelType;
import ohd.hseb.model.RainfallToStageModel;
import ohd.hseb.model.RatingCurve;
import ohd.hseb.model.SigRiverLevels;
import ohd.hseb.model.StageToFlowValueMapper;
import ohd.hseb.model.UnitHydrograph;
import ohd.hseb.model.UnitHydrographDescriptor;
import ohd.hseb.model.mkcapi.KcApiRainfallRunoffModel;
import ohd.hseb.model.mkcapi.KcApiRainfallToStageModel;
import ohd.hseb.model.sacsma.SacRainfallToStageModel;
import ohd.hseb.model.sacsma.SacSmaParameters;
import ohd.hseb.model.sacsma.SacSmaRainfallRunoffModel;
import ohd.hseb.model.sacsma.SacSmaState;
import ohd.hseb.util.CodeTimer;
import ohd.hseb.util.CodeTracer;
import ohd.hseb.util.Logger;
import ohd.hseb.util.TimeHelper;
import ohd.hseb.util.TimeHolder;

import com.raytheon.uf.common.ohd.AppsDefaults;

/**
 * @author Chip Gobs
 * 
 *         This class hooks together all the computational elements for running
 *         a hydrologic model. It is also in charge of making calls to the
 *         DataMgr.
 */
public class StreamModel {
    public static final int DEFAULT_FFG_DURATION_CODE = 1001; // REPRESENTS 1
                                                              // HOUR

    private static final MeasuringUnit _stageUnit = MeasuringUnit.feet;

    private static final MeasuringUnit _dischargeUnit = MeasuringUnit.cfs;

    private static final int MAX_DAYS_OF_TIME_SERIES = 100;

    private static final int MILLIS_PER_HOUR = 60 * 60 * 1000;

    private static final int MILLIS_PER_DAY = 24 * MILLIS_PER_HOUR;

    // FORECAST ADJUSTMENT PARAMETERS (THE BLENDING TIME IS IN THE DATABASE)
    private static final int DEFAULT_ADJUSTMENT_PAIRING_MINUTES = 70;

    private static final int DEFAULT_ADJUSTMENT_INTERPOLATION_HOURS = 3;

    private static final int MAX_BLENDING_HOURS = 1000;

    private int _adjustmentPairingMinutes = 0;

    private int _adjustmentInterpolationHours = 0;

    private DataMgr _dataMgr = null;

    private SSHPConfig _config = null;

    private RainfallRunoffModelType _rainfallRunoffModelType = null;

    private int _modelRunCount = 0;

    // location-related variables
    private String _locationId = null;

    private String _locationName = null;

    private String _basinId = null;

    private String _primaryPe = null; // HG or QR, height or discharge

    private static final int DEFAULT_INITIAL_FORECAST_LENGTH_IN_HOURS = 24;

    private static final int DEFAULT_MAX_FORECAST_LENGTH_IN_HOURS = 120;

    private int _forecastLengthInHours = DEFAULT_INITIAL_FORECAST_LENGTH_IN_HOURS;

    private int _maxForecastLengthInHours = DEFAULT_MAX_FORECAST_LENGTH_IN_HOURS;

    // various time series holders

    // precip
    private RegularTimeSeriesHolder _precipTimeSeriesHolder = new RegularTimeSeriesHolder();

    // evaporation - needed by SAC-SMA model
    private RegularTimeSeriesHolder _evaporationTimeSeriesHolder = new RegularTimeSeriesHolder();

    // calculated runoff
    private RegularTimeSeriesHolder _runoffTimeSeriesHolder = new RegularTimeSeriesHolder();

    // prior runoff
    private RegularTimeSeriesHolder _priorRunoffTimeSeriesHolder = new RegularTimeSeriesHolder();

    // stage
    private IrregularTimeSeriesHolder _observedStageTimeSeriesHolder = new IrregularTimeSeriesHolder();

    private RegularTimeSeriesHolder _simulatedStageTimeSeriesHolder = new RegularTimeSeriesHolder();

    private RegularTimeSeriesHolder _forecastStageTimeSeriesHolder = new RegularTimeSeriesHolder();

    private IrregularTimeSeriesHolder _priorForecastStageTimeSeriesHolder = new IrregularTimeSeriesHolder();

    // discharge
    private RegularTimeSeriesHolder _simulatedDischargeTimeSeriesHolder = new RegularTimeSeriesHolder();

    private SigRiverLevels _sigRiverLevels = null;

    // modeling objects
    private RainfallToStageModel _rainfallToStageModel = null;

    private RainfallRunoffModel _rainfallRunoffModel = null;

    private RatingCurve _ratingCurve = null;

    private UnitHydrograph _unitHydrograph = null;

    private UnitHydrograph _customUnitHydrograph = null;

    private MeasurementHolder _initialStageMeasurementHolder = new MeasurementHolder();

    // a rating-related object
    private StageToFlowValueMapper _stageToFlowValueMapper = null;

    // SAC model variables
    private List _sacStateDescriptorList = null;

    private SacSmaParameters _sacParams = null;

    private SacSmaState _sacState = null;

    private SacSmaStateDescriptor _sacSmaStateDescriptor = null;

    private boolean _isAdjustmentOn = false;

    private ForecastAdjuster _forecastAdjuster = null;

    // KC model variables
    private List _ffhDescriptorList = null;

    private FFHDescriptor _ffhDescriptor = null;

    private static final MeasuringUnit _ffhUnit = MeasuringUnit.inches;

    private static final double _defaultFFHValue = 5.0;

    private double _ffhValue = 0.0;

    private double _thresholdRunoff = 0.0;

    // basistime
    private static final int _intervalInHours = 1;

    // private static final int _hoursBack = 24;
    private static final int _millisPerHour = 1000 * 60 * 60;

    // model run start and end times
    // Note: the end time is currently hardcoded to go out some hours beyond
    // the current time
    private TimeHolder _modelStartTimeHolder = new TimeHolder();

    private TimeHolder _modelEndTimeHolder = new TimeHolder();

    // for debugging
    CodeTracer _tracer = null;

    // -----------------------------------------------------------

    // -----------------------------------------------------------

    public StreamModel(String locationId, RainfallRunoffModelType modelType,
            DataMgr dataMgr) {

        // _tracer = new CodeTracer();

        _locationId = locationId;

        _rainfallRunoffModelType = modelType;

        _dataMgr = dataMgr;

        _config = _dataMgr.getSSHPConfig(_locationId);

        AppsDefaults ad = AppsDefaults.getInstance();

        // load the forecast length parameters
        _forecastLengthInHours = ad.getInt("sshp_initial_forecast_length",
                DEFAULT_INITIAL_FORECAST_LENGTH_IN_HOURS);

        _maxForecastLengthInHours = ad.getInt("sshp_max_forecast_length",
                DEFAULT_MAX_FORECAST_LENGTH_IN_HOURS);

        if (_forecastLengthInHours > _maxForecastLengthInHours) {
            _maxForecastLengthInHours = _forecastLengthInHours;
        }

        // load the adjustment parameters
        _adjustmentPairingMinutes = ad.getInt(
                "sshp_adjustment_pairing_minutes",
                DEFAULT_ADJUSTMENT_PAIRING_MINUTES);
        _adjustmentInterpolationHours = ad.getInt(
                "sshp_adjustment_interpolation_hours",
                DEFAULT_ADJUSTMENT_INTERPOLATION_HOURS);

        // set the basin id associated with the locationId
        if (_config != null) {
            _basinId = _config.getBasinId();
        } else {
            _basinId = "BOGUS_BASIN";
        }

        // get the location name
        _locationName = dataMgr.getLocationName(_locationId);

        // set the model run start time
        long modelStartTime = getTimeOfBestRecentState(_locationId, _basinId);
        if (modelStartTime == 0) {
            modelStartTime = getLatestHourTime();
        } else {
            modelStartTime = TimeHelper.truncateTimeInMillisToNearestHour(
                    modelStartTime, 1);
        }

        setModelStartTime(modelStartTime);

        // init the model states, parameters and input time series data

        initModelData();

        initAllTimeSeries();

        return;
    }

    private void trace() {
        try {
            throw new Exception("trace");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    // -------------------------------------------------------------------------------------
    public String[] getPeArray(char firstLetterOfPhysicalElement) {
        String[] peArray = null;
        String[] heightArray = { "HG", "HP", "HT" };
        String[] dischargeArray = { "QR" };

        peArray = _dataMgr
                .loadPhysicalElementArray(firstLetterOfPhysicalElement);
        if (peArray == null) {
            if (firstLetterOfPhysicalElement == 'H') {
                peArray = heightArray;
            } else if (firstLetterOfPhysicalElement == 'Q') {
                peArray = dischargeArray;
            }
        }

        return peArray;
    }

    // -------------------------------------------------------------------------------------
    public String getRainfallRunoffModelTypeName() {
        String modelTypeName = null;
        if (_rainfallRunoffModelType != null) {
            modelTypeName = _rainfallRunoffModelType.getName();
        }

        return modelTypeName;
    }

    // -------------------------------------------------------------------------------------
    public RainfallRunoffModelType getRainfallRunoffModelType() {
        return _rainfallRunoffModelType;

    }

    // -------------------------------------------------------------------------------------

    public double findThresholdRunoffValue() {

        // _unitHydrograph = getBestUnitHydrograph(_unitHydrograph);

        if (_ratingCurve == null) {
            _ratingCurve = _dataMgr.loadRatingCurve(_locationId);
        }

        return findThresholdRunoffValue(_unitHydrograph, _ratingCurve);
    }

    // -------------------------------------------------------------------------------------

    public boolean setRainfallRunoffModelType(RainfallRunoffModelType modelType) {
        boolean success = false;

        if (isValidModelTypeForLocation(modelType)) {
            success = true;

            _rainfallRunoffModelType = modelType;

            _customUnitHydrograph = null;

            initModelData();

            initAllNonPrecipTimeSeries();
        }

        return success;

    }

    // -------------------------------------------------------------------------------------

    private boolean isValidModelTypeForLocation(RainfallRunoffModelType type) {
        boolean valid = false;

        if (type == RainfallRunoffModelType.API_MKC) {
            valid = true;
        }
        // SshpConfig config = _dataMgr.loadSshpConfig(_locationId);
        else {
            if (_config != null) {
                valid = true;
            }
        }

        return valid;

    }

    // -------------------------------------------------------------------------------------

    public void reloadModelData() {
        // CodeTimer timer = new CodeTimer();

        // timer.start();
        // String header = "StreamModel.reloadModelData():";

        // System.out.println(header + "called");

        _unitHydrograph = null;
        _ratingCurve = null;
        _stageToFlowValueMapper = null;

        initModelData();

        // timer.stop(header + "took");

    }

    // -------------------------------------------------------------------------------------

    private long getTimeOfBestRecentState(String locationId, String basinId) {
        long bestStateTime = 0;

        if (_rainfallRunoffModelType == RainfallRunoffModelType.SAC_SMA) {
            SacSmaState state = getLatestSacSmaStateFromDescriptorList();
            bestStateTime = state.getValidTime();
        } else if (_rainfallRunoffModelType == RainfallRunoffModelType.API_MKC) {
            bestStateTime = _dataMgr
                    .getTimeOfBestRecentFFH(locationId, basinId);
        }

        return bestStateTime;

    }

    // -------------------------------------------------------------------------------------

    private void initModelData() {
        String header = "StreamModel.initModelData(): ";
        // System.out.println(header + "start. ");

        // this method is to be called when the model is going to
        // be run for the first time with this set of settings
        // it is called again when any modelData setting is changed

        // make sure that _unitHydrograph contains the best data available
        if (_customUnitHydrograph != null) {
            _unitHydrograph = _customUnitHydrograph;
        } else {
            _unitHydrograph = getBestUnitHydrograph(_unitHydrograph);
        }

        if (_ratingCurve == null) {
            _ratingCurve = _dataMgr.loadRatingCurve(_locationId);
        }

        if (_stageToFlowValueMapper == null) {
            _stageToFlowValueMapper = new StageToFlowValueMapper(_ratingCurve);
        }

        // init the forecastAdjust object
        if (_forecastAdjuster == null) {
            ForecastAdjusterParams params = new ForecastAdjusterParams();

            params.setPairingTimeMinutes(_adjustmentPairingMinutes);
            params.setInterpolationHours(_adjustmentInterpolationHours);
            params.setBlendingHours(_config.getBlendHours());

            params.setBlendingMethod(_config.getBlendMethod());
            params.setShouldDoAdjustment(_config.useBlend());
            _forecastAdjuster = new ForecastAdjuster(params);
            _isAdjustmentOn = _config.useBlend();
        }

        initRainfallRunoffModelData();

        // timer.stop("StreamModel.initModelData took");

    } // end initModelData

    // -------------------------------------------------------------------------------------

    private void initRainfallRunoffModelData() {
        if (_rainfallRunoffModelType == RainfallRunoffModelType.API_MKC) {
            if (_unitHydrograph == null) {
                trace();
            }

            _rainfallRunoffModel = initKcApiModelData(_unitHydrograph,
                    _ratingCurve);
            _rainfallToStageModel = new KcApiRainfallToStageModel(
                    _rainfallRunoffModel, _unitHydrograph, _ratingCurve,
                    _initialStageMeasurementHolder,
                    _observedStageTimeSeriesHolder, _forecastAdjuster,
                    _simulatedDischargeTimeSeriesHolder);

        } else if (_rainfallRunoffModelType == RainfallRunoffModelType.SAC_SMA) {

            loadPriorRunoffTimeSeries();

            // System.out.println(header + "start. ");

            _rainfallRunoffModel = initSacSmaModelData();
            _rainfallToStageModel = new SacRainfallToStageModel(
                    _rainfallRunoffModel, _unitHydrograph, _ratingCurve,
                    _priorRunoffTimeSeriesHolder,
                    _observedStageTimeSeriesHolder, _forecastAdjuster,
                    _simulatedDischargeTimeSeriesHolder);

        }

    }

    // -------------------------------------------------------------------------------------

    public String[] getUnitHydrographNameArray() {
        List uhgDescList = _dataMgr
                .loadUnitHydrographDescriptorList(getLocationId());
        UnitHydrographDescriptor descriptor;

        String[] nameArray = new String[uhgDescList.size()];

        for (int i = 0; i < uhgDescList.size(); i++) {
            descriptor = (UnitHydrographDescriptor) uhgDescList.get(i);
            nameArray[i] = descriptor.getModel();
        }

        return nameArray;
    }

    // -------------------------------------------------------------------------------------
    public void changeUnitHydrograph(String unitHydrographName) {
        // String header = "StreamModel.changeUnitHydrograph() : ";

        // System.out.println(header + "inside");

        // if there is a change, then load the different unitHydrograph
        if (!_unitHydrograph.getRainfallRunoffModelTypeName().equals(
                unitHydrographName)) {

            // System.out.println(header + "making UHG change to " +
            // unitHydrographName);
            _unitHydrograph = _dataMgr.loadUnitHydrograph(_locationId,
                    _basinId, unitHydrographName);
            _customUnitHydrograph = _unitHydrograph;

            initModelData();

        } else {
            // System.out.println(header + "REFUSING to making UHG change to " +
            // unitHydrographName);
        }

        return;
    }

    // -------------------------------------------------------------------------------------

    public String getUnitHydrographSelectionName() {
        String currentName = null;

        currentName = _unitHydrograph.getRainfallRunoffModelTypeName();

        return currentName;
    }

    // -------------------------------------------------------------------------------------

    private UnitHydrograph getBestUnitHydrograph(
            UnitHydrograph oldUnitHydrograph) {
        String header = "StreamModel.getBestUnitHydrograph() : ";

        /*
         * This method will just return the passed-in unit hydrograph if it is
         * not null and its model type matches the current RainfallRunoffModel
         * type.
         * 
         * If the oldUnitHydrograph is null, then a UHG will be loaded.
         * 
         * else if a customUnitHydrograph has been chosen, then the unitgraph
         * will not change.
         * 
         * else if the model types of the currently selected UHG
         * (oldUnitHydrograph) does not match with the RainfallRunoff Model
         * type, this routine loads the unithydrograph from the DataMgr.
         * 
         * If after that, the unit hydrograph has no points, then the routine
         * tries to load the unithydrograph for the same location and basin but
         * for a different rainfall runoff model. It is anticipated that this
         * last bit of functionality will be used mostly when the user is trying
         * to use the SAC-SMA, and has not define a SAC-SMA unit hydrograph, but
         * does have an API-MKC model unit hydrograph available.
         */
        // final String header = "StreamModel.getBestUnitHydrograph(): ";
        UnitHydrograph newUnitHydrograph = null;

        String oldUnitHydrographModelType = null;
        String modelTypeName = null;

        if (oldUnitHydrograph != null) {
            oldUnitHydrographModelType = oldUnitHydrograph
                    .getRainfallRunoffModelTypeName();
            modelTypeName = _rainfallRunoffModelType.getName();
        }

        // Determine if need to load new unit hydrograph
        boolean needToChangeUhg = false;

        if (oldUnitHydrograph == null) {
            needToChangeUhg = true;
        } else if ((_customUnitHydrograph == null)
                && (!oldUnitHydrographModelType.equals(modelTypeName))) {
            needToChangeUhg = true;
        }

        if (needToChangeUhg) {
            newUnitHydrograph = _dataMgr.loadUnitHydrograph(_locationId,
                    _basinId, _rainfallRunoffModelType.getName());
            _customUnitHydrograph = null; // clear out the custom unit
                                          // hydrograph

            // if did not read any ordinates, try to load a different 1-hour UHG
            if (newUnitHydrograph.getOrdinateCount() == 0) {
                String otherModelTypeName = null;

                newUnitHydrograph = _dataMgr.loadUnitHydrograph(_locationId,
                        _basinId, "ANY");
                _customUnitHydrograph = null;
            } else {
                // System.out.println(header +
                // "Unitgraph loaded successfully for the " +
                // _rainfallRunoffModelType.getName() + " model.");
            }
        }

        else // not null and the rainfall-runoff model types match
        {
            newUnitHydrograph = oldUnitHydrograph;
        }

        return newUnitHydrograph;
    }

    // -------------------------------------------------------------------------------------

    private void refreshModelData() {
        String header = "StreamModel.refreshModelData(): ";
        // System.out.println(header + " started");

        // This is to be called when the model is being re-run

        if (_rainfallRunoffModelType == RainfallRunoffModelType.API_MKC) {
            // _rainfallRunoffModel = initKcApiModelData(_unitHydrograph,
            // _ratingCurve);
        } else if (_rainfallRunoffModelType == RainfallRunoffModelType.SAC_SMA) {
            SacSmaRainfallRunoffModel sacModel = (SacSmaRainfallRunoffModel) _rainfallRunoffModel;
            sacModel.setState(_sacState);
        }

    } // refreshModelData()
    // -------------------------------------------------------------------------------------

    private RainfallRunoffModel initSacSmaModelData() {
        String header = "StreamModel.initSacSmaModelData():";

        // CodeTimer timer1 = new CodeTimer();
        // timer1.start();

        // long lastValidTime = getLatestHourTime();
        long lastValidTime = _modelStartTimeHolder.getTime();

        _sacParams = _dataMgr.loadSacSmaParameters(_basinId, lastValidTime);
        if (_sacParams == null) {
            _sacParams = getDefaultParams();
        }
        // System.out.println("sacParams = " + _sacParams);

        _sacStateDescriptorList = getSacSmaStateDescriptorList();
        _sacState = getSacSmaStateFromDescriptorList(lastValidTime);
        if (_sacState == null) {
            _sacState = getDefaultSacSmaState();
        }

        // _sacState = _dataMgr.loadSacSmaState(_basinId, lastValidTime);

        // System.out.println("sacState = " + _sacState);

        // 
        boolean useStaticMape = true;

        if (_config != null) {
            useStaticMape = _config.useStaticEvap();
        }

        boolean isAdjustment = false;

        if (useStaticMape) // using the 12 monthly values straight
        {
            isAdjustment = false;
        } else // using the MAPE time series. Using the 12 monthly values as
               // adjustment factors
        {
            isAdjustment = true;
        }

        MonthlyValues monthlyValues = _dataMgr.loadMonthlyValues(_basinId,
                isAdjustment);

        double peAdjust = _sacParams.getPeadj();
        double pxAdjust = _sacParams.getPxadj();

        RainfallRunoffModel rainfallRunoffModel = new SacSmaRainfallRunoffModel(
                _sacState, _sacParams, peAdjust, pxAdjust, useStaticMape,
                monthlyValues, _evaporationTimeSeriesHolder);

        // timer1.stop(header + "model setup took");

        return rainfallRunoffModel;

    }

    // -------------------------------------------------------------------------------------

    private RainfallRunoffModel initKcApiModelData(
            UnitHydrograph unitHydrograph, RatingCurve ratingCurve) {
        String header = "StreamModel.initKcApiModelData():";

        // CodeTimer timer1 = new CodeTimer();
        // timer1.start();

        _thresholdRunoff = findThresholdRunoffValue(unitHydrograph, ratingCurve);

        // System.out.println(header + "before getFfgValue() ffhValue = " +
        // _ffhValue);
        _ffhValue = findFfhValue(_ffhDescriptor);
        // System.out.println(header + "after getFfgValue() ffhValue = " +
        // _ffhValue);

        RainfallRunoffModel rainfallRunoffModel = new KcApiRainfallRunoffModel(
                _thresholdRunoff, _ffhValue);

        // timer1.stop(header + "model setup took");

        return rainfallRunoffModel;

    }

    // -------------------------------------------------------------------------------------
    public StageToFlowValueMapper getStageToFlowValueMapper() {
        return _stageToFlowValueMapper;
    }

    // -------------------------------------------------------------------------------------

    public String getLocationName() {
        return _locationName;
    }

    // -------------------------------------------------------------------------------------

    public boolean hasSacSmaParams() {
        return _dataMgr.hasSacSmaParams(_basinId);
    }

    // -------------------------------------------------------------------------------------

    public long getSacParamsValidTime() {
        long time = 0;
        if (_rainfallRunoffModelType == RainfallRunoffModelType.SAC_SMA) {
            time = _sacParams.getValidTime();

        }

        return time;
    }

    // -------------------------------------------------------------------------------------

    public long getSacStateValidTime() {
        long time = 0;
        if (_rainfallRunoffModelType == RainfallRunoffModelType.SAC_SMA) {
            time = _sacState.getValidTime();
        }

        return time;
    }

    // -------------------------------------------------------------------------------------

    private double findThresholdRunoffValue(UnitHydrograph unitHydrograph,
            RatingCurve ratingCurve) {

        String header = "StreamModel.getThresholdRunoffValue()";

        double thresholdRunoff = _dataMgr.loadThresholdRunoffValue(_locationId);

        if (thresholdRunoff == _dataMgr.MISSING) {
            // System.out.println(header +
            // "going to calculate the ThresholdRunoff value");
            SigRiverLevels sigStages = _dataMgr.loadSigRiverLevels(_locationId);

            if (sigStages != null) {
                thresholdRunoff = calculateThresholdRunoffValue(sigStages
                        .getFloodStage(), unitHydrograph, ratingCurve);
            } else {
                thresholdRunoff = _dataMgr.MISSING;
            }
        }
        return thresholdRunoff;
    }

    // ----------------------------------------------------------------------------
    private double calculateThresholdRunoffValue(double floodStage,
            UnitHydrograph unitHydrograph, RatingCurve ratingCurve) {
        String header = "StreamModel.calculateThresholdRunoffValue()";
        // System.out.println(header + " - just starting");

        double thresholdRunoffValue = 0;
        double peakFlow = _dataMgr.MISSING;
        double floodFlow = ratingCurve.getDischargeFromStage(floodStage);
        RelTimeMeasurement peakMeasurement = unitHydrograph
                .getPeakFlowMeasurement();

        if (peakMeasurement != null) {
            peakFlow = peakMeasurement.getValue();
            thresholdRunoffValue = floodFlow / peakFlow;
        }

        // System.out.println(header + "thresholdRunoff = " +
        // thresholdRunoffValue);
        return thresholdRunoffValue;
    }

    // ----------------------------------------------------------------------------
    public double findFfhValue() {
        return findFfhValue(_ffhDescriptor);
    }

    // ----------------------------------------------------------------------------

    private double findFfhValue(FFHDescriptor descriptor) {
        double ffhValue = -999;

        if (descriptor != null) {
            ffhValue = descriptor.getValueInInches();
        }

        else // (descriptor == null)
        {
            AbsTimeMeasurement ffhMeasurement = _dataMgr
                    .loadMostCurrentFfhMeasurement(_locationId, _basinId,
                            _modelStartTimeHolder.getTime());

            // System.out.println(
            // "StreamModel.findFfhValue(): modelStartTimeHolder : " +
            // _modelStartTimeHolder.getTime() );

            if (ffhMeasurement != null) {
                ffhValue = ffhMeasurement.getValue(MeasuringUnit.inches);
            } else {
                ffhValue = getDefaultFFHDescriptor().getValueInInches();
            }
            // System.out.println("StreamModel.getFfhValue() = " + ffhValue);

        }

        return ffhValue;
    }

    // ----------------------------------------------------------------------------
    public List getFFHDescriptorList() {
        _ffhDescriptorList = _dataMgr.loadFfhDescriptorList(_locationId,
                _basinId);

        if (_ffhDescriptorList.size() == 0) {
            _ffhDescriptorList.add(getDefaultFFHDescriptor());
        }

        return _ffhDescriptorList;
    }

    // ----------------------------------------------------------------------------
    private FFHDescriptor getDefaultFFHDescriptor() {
        AbsTimeMeasurement measurement = new AbsTimeMeasurement(
                _defaultFFHValue, getLatestHourTime(), _ffhUnit);
        FFHDescriptor descriptor = new FFHDescriptor(_locationId, measurement,
                DEFAULT_FFG_DURATION_CODE, false);
        descriptor.setIsDefaultValue(true);

        return descriptor;
    }

    // ----------------------------------------------------------------------------
    public List getSacSmaStateDescriptorList() {
        List list = _dataMgr
                .loadSacSmaStateDescriptorListByBasinId(getBasinId());
        if (list.size() == 0) {
            SacSmaState state = getDefaultSacSmaState();
            SacSmaStateDescriptor descriptor = new SacSmaStateDescriptor(state);

            list.add(descriptor);
        }
        return list;
    }

    // ----------------------------------------------------------------------------
    private SacSmaState getSacSmaStateFromDescriptorList(long modelRunStartTime) {
        // trace();
        String header = "StreamModel.getSacSmaStateFromDescriptorList(): ";
        SacSmaState returnState = null;

        if (_sacStateDescriptorList == null) {
            _sacStateDescriptorList = getSacSmaStateDescriptorList();
        }

        for (int i = 0; i < _sacStateDescriptorList.size(); i++) {
            SacSmaStateDescriptor descriptor = (SacSmaStateDescriptor) _sacStateDescriptorList
                    .get(i);
            SacSmaState state = descriptor.getState();

            // System.out.println(header + " modelRunStartTime = " +
            // DbTimeHelper.getDateTimeStringFromLongTime(modelRunStartTime));
            // System.out.println(header + " i = " + i + " state = " + state);

            if (state.getValidTime() <= modelRunStartTime) {
                returnState = state;
                // System.out.println(header + "will return this state i = " + i
                // + " state = " + state);

                break;
            }

        }

        return returnState;
    }

    // ----------------------------------------------------------------------------
    private SacSmaState getLatestSacSmaStateFromDescriptorList() {
        // trace();
        String header = "StreamModel.getLatestSacSmaStateFromDescriptorList(): ";
        SacSmaState returnState = null;

        if (_sacStateDescriptorList == null) {
            _sacStateDescriptorList = getSacSmaStateDescriptorList();
        }

        if (_sacStateDescriptorList.size() > 0) {
            SacSmaStateDescriptor descriptor = (SacSmaStateDescriptor) _sacStateDescriptorList
                    .get(0);
            SacSmaState state = descriptor.getState();
            returnState = state;
            // System.out.println(header + "will return state = " + state);
        }

        return returnState;
    }

    // ----------------------------------------------------------------------------
    private SacSmaState getDefaultSacSmaState() {
        SacSmaState state = new SacSmaState();
        state.setSource("BOGUS DEFAULT");

        state.setBasinId(_basinId);
        state.setValidTime(getLatestHourTime());

        state.setUztwc(31.02);
        state.setUzfwc(0.009);
        state.setLztwc(31.94);
        state.setLzfsc(0.060);
        state.setLzfpc(74.74);
        state.setAdimc(62.04);

        return state;
    }

    // ----------------------------------------------------------------------------
    private static SacSmaParameters getDefaultParams() {
        // uses sept93 data for ELDO2)
        /*
         * PXADJ = 1.000 PEADJ = 1.000 UZTWM = 60.000 UZFWM = 25.000 UZK = 0.350
         * PCTIM = 0.000 ADIMP = 0.000 RIVA = 0.003 ZPERC = 210.000 REXP = 1.40
         * LZTWM = 120.000 LZFSM = 25.000 LZFPM = 150.000 LZSK = 0.080 LZPK =
         * 0.004 PFREE = 0.250 RSERV = 0.300 SIDE = 0.000 IOPTET=0 //we don't
         * have this one, so ignore it EFC = 0.50
         */

        SacSmaParameters params = new SacSmaParameters();

        params.setPxadj(1.0);
        params.setPeadj(1.0);
        params.setUztwm(60.0);
        params.setUzfwm(25.0);
        params.setUzk(0.35);
        params.setPctim(0.000);
        params.setAdimp(0.000);
        params.setRiva(0.003);
        params.setZperc(210.000);
        params.setRexp(1.40);
        params.setLztwm(120.000);
        params.setLzfsm(25.0);
        params.setLzfpm(150.000);
        params.setLzsk(0.080);
        params.setLzpk(0.004);
        params.setPfree(0.250);
        params.setRserv(0.300);
        params.setSide(0.000);
        params.setEfc(.50);

        return params;
    }

    // -----------------------------------------------------------------------

    public void reloadPrecipTimeSeries() {
        initDbPrecipTimeSeries();

        return;
    }

    // ---------------------------------------------------------------------

    public void reloadFilePrecipTimeSeries(String filePathString) {
        initFilePrecipTimeSeries(filePathString);

        return;
    }

    // ---------------------------------------------------------------------

    private void initDbPrecipTimeSeries() {
        String header = "StreamModel.initDbPrecipTimeSeries() : ";

        long lastTimeSeriesHour = _modelEndTimeHolder.getTime();
        long firstTimeSeriesHour = lastTimeSeriesHour - MAX_DAYS_OF_TIME_SERIES
                * 24 * MILLIS_PER_HOUR;

        // precip
        long lastObsTime = TimeHelper.truncateTimeInMillisToNearestHour(System
                .currentTimeMillis(), 1);
        long firstObsTime = lastObsTime - MAX_DAYS_OF_TIME_SERIES * 24
                * MILLIS_PER_HOUR;
        RegularTimeSeries obsPrecipTs = loadObservedPrecipTimeSeries(
                firstObsTime, lastObsTime);
        obsPrecipTs.setName("Original Observed Precip Ts");
        // System.out.println(header +
        // "after loadObservedPrecipTimeSeries,  obs precip ts = " +
        // obsPrecipTs);

        // new code
        // CodeTimer timer = new CodeTimer();
        // timer.start();
        RegularTimeSeries fcstPrecipTs = _dataMgr.loadFcstMapTimeSeries(
                _basinId, getLatestHourTime() + _millisPerHour);
        // timer.stop("_dataMgr.loadFcstMapTimeSeries took ");

        RegularTimeSeries fullPrecipTs = RegularTimeSeries.concatenate(
                obsPrecipTs, fcstPrecipTs);
        // end new code

        long precipStartTime = obsPrecipTs.getStartTime();
        if ((precipStartTime < firstTimeSeriesHour)
                || (precipStartTime > lastTimeSeriesHour)) {
            // System.out.println(header + " creating a new time series");
            // obsPrecipTs = new RegularTimeSeries(firstTimeSeriesHour,
            // lastTimeSeriesHour,
            // 1, MeasuringUnit.inches);
            // System.out.println(header + "after re-creation obs precip ts = "
            // + obsPrecipTs);
        } else {
            // System.out.println(header + "before stretching obs precip ts = "
            // + obsPrecipTs);

            // obsPrecipTs.stretchTimeSeries(firstTimeSeriesHour,
            // lastTimeSeriesHour, 0.0);
            fullPrecipTs.stretchTimeSeries(firstTimeSeriesHour,
                    lastTimeSeriesHour, 0.0);

            // System.out.println(header + "after stretching obs precip ts = " +
            // obsPrecipTs);
        }

        // _precipTimeSeriesHolder.setTimeSeries(obsPrecipTs);
        _precipTimeSeriesHolder.setTimeSeries(fullPrecipTs);

        return;
    }

    // ----------------------------------------------------------------------------

    private void initFilePrecipTimeSeries(String filePathString) {
        String header = "StreamModel.initFilePrecipTimeSeries() : ";

        long lastTimeSeriesHour = _modelEndTimeHolder.getTime();
        long firstTimeSeriesHour = lastTimeSeriesHour - MAX_DAYS_OF_TIME_SERIES
                * 24 * MILLIS_PER_HOUR;

        // precip
        long lastObsTime = TimeHelper.truncateTimeInMillisToNearestHour(System
                .currentTimeMillis(), 1);
        long firstObsTime = lastObsTime - MAX_DAYS_OF_TIME_SERIES * 24
                * MILLIS_PER_HOUR;
        RegularTimeSeries obsPrecipTs = loadObservedPrecipTimeSeries(
                firstObsTime, lastObsTime);
        obsPrecipTs.setName("Original Observed Precip Ts");
        // System.out.println(header +
        // "after loadObservedPrecipTimeSeries,  obs precip ts = " +
        // obsPrecipTs);

        RegularTimeSeries fcstPrecipTs = _dataMgr.loadFcstMapTimeSeries(
                _basinId, getLatestHourTime() + _millisPerHour);

        RegularTimeSeries fullPrecipTs = _dataMgr
                .loadFileMapTimeSeries(filePathString);

        long precipStartTime = obsPrecipTs.getStartTime();
        if ((precipStartTime < firstTimeSeriesHour)
                || (precipStartTime > lastTimeSeriesHour)) {

        } else {
            fullPrecipTs.stretchTimeSeries(firstTimeSeriesHour,
                    lastTimeSeriesHour, 0.0);

        }

        _precipTimeSeriesHolder.setTimeSeries(fullPrecipTs);

        return;
    }

    // ----------------------------------------------------------------------------

    // --------------------------------------------------------

    private void initAllNonPrecipTimeSeries() {
        // long lastEvenHour = getLatestHourTime();
        long lastEvenHour = _modelStartTimeHolder.getTime();
        long nextEvenHour = lastEvenHour + _millisPerHour;
        long endOfPrecipForecastTime = _modelEndTimeHolder.getTime();

        // potential evaporation
        RegularTimeSeries evaporationTimeSeries = _dataMgr
                .loadPotentialEvaporationTimeSeries(_basinId,
                        _modelStartTimeHolder.getTime(), _modelEndTimeHolder
                                .getTime());

        _evaporationTimeSeriesHolder.setTimeSeries(evaporationTimeSeries);

        // stage
        if (_observedStageTimeSeriesHolder.getTimeSeries() == null) {
            IrregularTimeSeries obsStageTimeSeries = loadObservedStageTimeSeries();
            _observedStageTimeSeriesHolder.setTimeSeries(obsStageTimeSeries);

        }

        // stage
        if (_priorForecastStageTimeSeriesHolder.getTimeSeries() == null) {
            IrregularTimeSeries priorForecastTimeSeries = loadPriorForecastTimeSeries();
            _priorForecastStageTimeSeriesHolder
                    .setTimeSeries(priorForecastTimeSeries);

        }

        // used for some rainfall-runoff models
        setInitialStageMeasurement(findInitialStageMeasurement());

        // runoff, both calculated now and prior runoff read from the database
        if (_runoffTimeSeriesHolder.getTimeSeries() == null) {
            RegularTimeSeries runoffTimeSeries = new RegularTimeSeries(
                    nextEvenHour, endOfPrecipForecastTime, _intervalInHours,
                    MeasuringUnit.mm);

            _runoffTimeSeriesHolder.setTimeSeries(runoffTimeSeries);

        }

        // used in initRainfallRunoffModelData() now
        // remove after 6/1/07
        // loadPriorRunoffTimeSeries();

    }

    // ----------------------------------------------------------------------------
    private void loadPriorRunoffTimeSeries() {
        String header = "StreamModel.loadPriorRunoffTimeSeries() ";
        System.out.println(header + " - called.");
        // load prior runoff
        String typeSource = determineTypeSourceForPriorRunoffTimeSeries();

        _unitHydrograph = getBestUnitHydrograph(_unitHydrograph);

        RegularTimeSeries priorRunoffTimeSeries = _dataMgr
                .loadPriorRunoffTimeSeries(_locationId, typeSource,
                        getModelStartTime(), _unitHydrograph.getOrdinateCount());

        _priorRunoffTimeSeriesHolder.setTimeSeries(priorRunoffTimeSeries);
    }

    // ----------------------------------------------------------------------------

    private String determineTypeSourceForPriorRunoffTimeSeries() {
        String typeSource = null;

        if (_rainfallRunoffModelType == RainfallRunoffModelType.SAC_SMA) {
            if (_sacState != null) {
                SSHPSource source = SSHPSource
                        .findMatchingSSHPSourceBySacSmaSource(_sacState
                                .getSource());

                if (source != null) {
                    typeSource = source.getTypeSource();
                }
            }
        }

        return typeSource;
    }

    // ----------------------------------------------------------------------------

    private void initAllTimeSeries() {
        // _tracer.trace();

        setModelStartTime(getLatestHourTime());

        initDbPrecipTimeSeries();

        initAllNonPrecipTimeSeries();

        return;
    }

    // -----------------------------------------------------------
    public TimeHolder getModelStartTimeHolder() {
        return _modelStartTimeHolder;
    }

    // ----------------------------------------------------------------------------

    public long getModelStartTime() {
        return _modelStartTimeHolder.getTime();
    }

    // ----------------------------------------------------------------------------

    public void setFFHProductDescriptor(FFHDescriptor descriptor) {
        _ffhDescriptor = descriptor;

    }

    // ----------------------------------------------------------------------------
    public void setSacSmaStateDescriptor(SacSmaStateDescriptor descriptor) {
        String header = "StreamModel.setSacSmaStateDescriptor(): ";
        _sacSmaStateDescriptor = descriptor;

        _sacState = _sacSmaStateDescriptor.getState();
    }

    // ----------------------------------------------------------------------------

    public void setModelStartTime(long modelStartTime) {
        modelStartTime = TimeHelper.truncateTimeInMillisToNearestHour(
                modelStartTime, 1);
        // trace();
        _modelStartTimeHolder.setTime(modelStartTime);

        // sets the model end time
        setForecastLengthInHours(_forecastLengthInHours);

        /*
         * int baseHours = 72; int unitHydrographDuration = 0;
         * 
         * if (_unitHydrograph != null) { unitHydrographDuration =
         * _unitHydrograph.getOrdinateCount(); }
         * 
         * int totalHours = baseHours + unitHydrographDuration;
         * 
         * //make sure that the time series are not too huge // 100 days should
         * be enough if (totalHours > 2400) { totalHours = 2400; }
         */

        return;
    }

    // -----------------------------------------------------------
    public void setForecastLengthInHours(int lengthInHours) {
        // String header = "StreamModel.setForecastLengthInHours(): ";

        // System.out.println(header + "lengthInHours = " + lengthInHours);

        if (lengthInHours > 0) {
            _forecastLengthInHours = lengthInHours;
            long duration = ((long) (lengthInHours) * (long) MILLIS_PER_HOUR);
            long newEndTime = _modelStartTimeHolder.getTime() + duration;

            setModelEndTime(newEndTime);
        }

        return;
    }

    // -----------------------------------------------------------

    public int getForecastHours() {
        return _forecastLengthInHours;
    }

    // -----------------------------------------------------------

    public void setModelEndTime(long modelEndTime) {
        _modelEndTimeHolder.setTime(modelEndTime);
    }

    // -----------------------------------------------------------
    public long getLatestHourTime() {
        return TimeHelper.truncateTimeInMillisToNearestHour(System
                .currentTimeMillis(), 1);
    }

    // -----------------------------------------------------------

    public String getLocationId() {
        return _locationId;

    }

    public String getBasinId() {
        return _basinId;
    }

    // -------------------------------------------------------------

    public SigRiverLevels getSigRiverLevels() {
        // if needed, retrieve and cache the data
        // then return the value
        if (_sigRiverLevels == null) {
            _sigRiverLevels = _dataMgr.loadSigRiverLevels(_locationId);

            boolean stagesAreMissing = false;
            if (!_sigRiverLevels.hasFloodStage()
                    || !_sigRiverLevels.hasActionStage()) {
                stagesAreMissing = true;
            }

            // if the site is set up to use discharge (flow) instead of stage,
            // convert from flow to stage and populate the floodStage and
            // actionStage
            // (if possible),
            // so that they are available to other parts of the program
            if (stagesAreMissing) {
                RatingCurve ratingCurve = getRatingCurve();
                if (_sigRiverLevels.hasFloodFlow()) {
                    double floodFlow = _sigRiverLevels.getFloodFlow();
                    double floodStage = ratingCurve
                            .getStageFromDischarge(floodFlow);
                    _sigRiverLevels.setFloodStage(floodStage);
                }
                if (_sigRiverLevels.hasActionFlow()) {
                    double actionFlow = _sigRiverLevels.getActionFlow();
                    double actionStage = ratingCurve
                            .getStageFromDischarge(actionFlow);
                    _sigRiverLevels.setActionStage(actionStage);
                }
            }

        }

        return _sigRiverLevels;
    }

    // -----------------------------------------------------------
    private IrregularTimeSeries loadPriorForecastTimeSeries() {
        IrregularTimeSeries forecastStageTimeSeries = null;

        forecastStageTimeSeries = _dataMgr.loadFcstStageTimeSeries(
                getLocationId(), getPrimaryPe());

        return forecastStageTimeSeries;
    }

    // -----------------------------------------------------------

    private RegularTimeSeries loadObservedPrecipTimeSeries(long startTime,
            long endTime) {
        RegularTimeSeries observedPrecipTimeSeries = null;

        // CodeTimer timer = new CodeTimer();
        // timer.start();
        observedPrecipTimeSeries = _dataMgr
                .loadObsMapTimeSeriesFromDb(_basinId);

        if (observedPrecipTimeSeries == null) {
            observedPrecipTimeSeries = new RegularTimeSeries(startTime,
                    endTime, 1, MeasuringUnit.inches);
        }

        // timer.stop("StreamModel.loadObservedPrecipTimeSeries() called DataMgr.loadObsMapTimeSeries() which took ");

        // System.out.println("StreamModel.loadObservedPrecipTimeSeries() = " +
        // observedPrecipTimeSeries);

        return observedPrecipTimeSeries;
    }

    // -----------------------------------------------------------

    // -----------------------------------------------------------

    private IrregularTimeSeries loadObservedStageTimeSeries() {
        String header = "StreamModel.loadObservedStageTimeSeries() ";
        CodeTimer timer = new CodeTimer();
        timer.start();

        IrregularTimeSeries observedStageTimeSeries = null;

        if (getSigRiverLevels().useStage()) {

            observedStageTimeSeries = _dataMgr
                    .loadObservedStageTimeSeries(_locationId);
        } else // use flow and then convert it
        {
            IrregularTimeSeries observedDischargeTimeSeries = null;
            observedDischargeTimeSeries = _dataMgr
                    .loadObservedDischargeTimeSeries(_locationId);
            if (observedDischargeTimeSeries.getMeasurementCount() > 0) {
                observedStageTimeSeries = getRatingCurve().getStageTimeSeries(
                        observedDischargeTimeSeries);
            } else // there is no discharge time series
            {
                observedStageTimeSeries = _dataMgr
                        .loadObservedStageTimeSeries(_locationId);
            }
        }

        setLastObservedTimeForAdjustment(observedStageTimeSeries.getEndTime());

        timer.stop("StreamModel.loadObservedStageTimeSeries() took ");

        return observedStageTimeSeries;
    }

    // -----------------------------------------------------------

    public void reloadObservedStageTimeSeries() {
        IrregularTimeSeries observedStageTimeSeries = loadObservedStageTimeSeries();
        _observedStageTimeSeriesHolder.setTimeSeries(observedStageTimeSeries);

    }

    // -----------------------------------------------------------

    public DataMgr getDataMgr() {
        return _dataMgr;
    }

    // -----------------------------------------------------------
    public RegularTimeSeries getDischargeTimeSeries() {
        RegularTimeSeries fcstStageTimeSeries = _forecastStageTimeSeriesHolder
                .getTimeSeries();
        RegularTimeSeries timeSeries = _ratingCurve
                .getDischargeRegularTimeSeries(fcstStageTimeSeries);

        return timeSeries;

    }

    // -----------------------------------------------------------

    public RegularTimeSeriesHolder getForecastStageTimeSeriesHolder() {
        return _forecastStageTimeSeriesHolder;
    }

    public IrregularTimeSeriesHolder getPriorForecastStageTimeSeriesHolder() {
        return _priorForecastStageTimeSeriesHolder;
    }

    // -----------------------------------------------------------
    public RegularTimeSeriesHolder getSimulationStageTimeSeriesHolder() {
        return _simulatedStageTimeSeriesHolder;
    }

    // -----------------------------------------------------------

    /*
     * public RegularTimeSeriesHolder getObservedPrecipTimeSeriesHolder() {
     * return _observedPrecipTimeSeriesHolder; }
     */
    // -----------------------------------------------------------
    public IrregularTimeSeriesHolder getObservedStageTimeSeriesHolder() {
        return _observedStageTimeSeriesHolder;
    }

    // -----------------------------------------------------------
    public RegularTimeSeriesHolder getPrecipTimeSeriesHolder() {
        return _precipTimeSeriesHolder;
    }

    /*
     * public RegularTimeSeriesHolder getForecastPrecipTimeSeriesHolder() {
     * return _forecastPrecipTimeSeriesHolder; }
     */

    // -----------------------------------------------------------
    public RegularTimeSeriesHolder getEvaporationTimeSeriesHolder() {
        return _evaporationTimeSeriesHolder;
    }

    // -----------------------------------------------------------

    public RegularTimeSeriesHolder getRunoffTimeSeriesHolder() {
        return _runoffTimeSeriesHolder;
    }

    public RegularTimeSeriesHolder getPriorRunoffTimeSeriesHolder() {
        return _priorRunoffTimeSeriesHolder;
    }

    // -----------------------------------------------------------
    public String getPrimaryPe() {

        if (_primaryPe == null) {
            _primaryPe = _dataMgr.loadPrimaryPe(_locationId);

        }

        return _primaryPe;
    }

    // -----------------------------------------------------------
    // load the baseflow

    public Measurement findBaseflowMeasurementOld() {

        String header = "StreamModel.getBaseflowMeasurementOld():";

        long modelRunStartTime = _modelStartTimeHolder.getTime();
        IrregularTimeSeries observedStageTs = _observedStageTimeSeriesHolder
                .getTimeSeries();

        AbsTimeMeasurement latestMeasurement = null;
        Measurement baseflowMeasurement = null;

        boolean done = false;

        // find the latestHeightValue that is not beyond the modelRunStartTime
        for (int i = 0; i < observedStageTs.getMeasurementCount() && (!done); i++) {
            AbsTimeMeasurement m = observedStageTs
                    .getAbsTimeMeasurementByIndex(i);

            long measurementTime = m.getTime();

            if (measurementTime < modelRunStartTime) {
                latestMeasurement = m;
            } else if (measurementTime > modelRunStartTime) {
                done = true;
            }

        } // for i

        Measurement heightMeasurement = latestMeasurement;

        if (heightMeasurement != null) {
            double heightValue = heightMeasurement.getValue();

            if (_ratingCurve == null) {
                _ratingCurve = getRatingCurve();
            }

            if (_ratingCurve != null) {
                double baseflowValue = _ratingCurve
                        .getDischargeFromStage(heightValue);

                // System.out.println(header + " heightValue = " + heightValue +
                // " baseflowValue = " + baseflowValue );

                // System.out.println(header + "stage measurement = " +
                // latestMeasurement);

                baseflowMeasurement = new Measurement(baseflowValue,
                        MeasuringUnit.cfs);
            }
        }

        // timer.stop(header + "baseflow retrieval took" );

        if (baseflowMeasurement == null) {
            baseflowMeasurement = new Measurement(0.0, MeasuringUnit.cfs);
        }

        return baseflowMeasurement;

    } // end getBaseflowMeasurement()

    // -----------------------------------------------------------
    // load the initial stage measurement from the time series

    public Measurement findInitialStageMeasurement() {

        String header = "StreamModel.findInitialStageMeasurement():";

        long modelRunStartTime = _modelStartTimeHolder.getTime();
        IrregularTimeSeries observedStageTs = _observedStageTimeSeriesHolder
                .getTimeSeries();

        AbsTimeMeasurement latestMeasurement = null;
        Measurement initialStageMeasurement = null;

        boolean done = false;

        // find the latestHeightValue that is not beyond the modelRunStartTime
        for (int i = 0; i < observedStageTs.getMeasurementCount() && (!done); i++) {
            AbsTimeMeasurement m = observedStageTs
                    .getAbsTimeMeasurementByIndex(i);

            long measurementTime = m.getTime();

            if (measurementTime <= modelRunStartTime) {
                latestMeasurement = m;
            } else if (measurementTime > modelRunStartTime) {
                done = true;
            }

        } // for i

        initialStageMeasurement = latestMeasurement;

        if (initialStageMeasurement == null) {
            initialStageMeasurement = new Measurement(0.0, _stageUnit);
        }

        return initialStageMeasurement;

    } // end findInitialStageMeasurement()

    // -----------------------------------------------------------
    public void setBaseflowMeasurementByStageMeasurementOld(
            Measurement stageMeasurement) {
        double stageValue = 0.0;
        double baseflowValue = 0.0;

        if (stageMeasurement != null) {
            stageValue = stageMeasurement.getValue(_stageUnit);
        }

        if (_ratingCurve != null) {
            baseflowValue = _ratingCurve.getDischargeFromStage(stageValue);
        }

        Measurement baseflowMeasurement = new Measurement(baseflowValue,
                _dischargeUnit);
        _initialStageMeasurementHolder.setMeasurement(baseflowMeasurement);
    }

    // -----------------------------------------------------------
    public void setInitialStageMeasurement(Measurement stageMeasurement) {

        if (stageMeasurement == null) {
            stageMeasurement = new Measurement(0.0, _stageUnit);
        }

        _initialStageMeasurementHolder.setMeasurement(stageMeasurement);
    }

    // -----------------------------------------------------------
    public void setBaseflowMeasurementOld(Measurement baseflowMeasurement) {
        _initialStageMeasurementHolder.setMeasurement(baseflowMeasurement);
    }

    // -----------------------------------------------------------

    public void setFfhValue(double ffhValue) {
        String header = "StreamModel.setFfgValue(): ";
        // System.out.println(header + " ffhValue = " + ffhValue);
        _ffhValue = ffhValue;

        if (_rainfallRunoffModelType == RainfallRunoffModelType.API_MKC) {
            KcApiRainfallRunoffModel model = (KcApiRainfallRunoffModel) _rainfallRunoffModel;
            model.setFfhValue(_ffhValue);
        }
    }

    // -----------------------------------------------------------

    public RegularTimeSeries runModel() {
        String header = "StreamModel.runModel()";

        // System.out.println(header + "inside method");

        // make sure that model states start out fresh
        refreshModelData();

        _modelRunCount++;

        Measurement initialStageMeasurement = _initialStageMeasurementHolder
                .getMeasurement();

        if (initialStageMeasurement == null) {
            initialStageMeasurement = findInitialStageMeasurement();
            /*
             * if (_ratingCurve != null) { double stageValue =
             * initialStageMeasurement.getValue(MeasuringUnit.feet); double
             * baseflowValue = _ratingCurve.getDischargeFromStage(stageValue);
             * baseflowMeasurement = new Measurement(baseflowValue,
             * MeasuringUnit.cfs); }
             */

            _initialStageMeasurementHolder
                    .setMeasurement(initialStageMeasurement);
        }

        // System.out.println(header + "baseflowMeasurement =  " +
        // baseflowMeasurement);

        // do the actual calculations
        RegularTimeSeries forecastStageTimeSeries = _rainfallToStageModel
                .calculateStageTimeSeries(_modelStartTimeHolder.getTime(),
                        _modelEndTimeHolder.getTime(), _precipTimeSeriesHolder
                                .getTimeSeries(), _runoffTimeSeriesHolder);

        forecastStageTimeSeries = forecastStageTimeSeries.getSubTimeSeries(
                _modelStartTimeHolder.getTime(), _modelEndTimeHolder.getTime());

        // trim the simulated dischargeTimeSeries
        RegularTimeSeries simulatedDischargeTimeSeries = _simulatedDischargeTimeSeriesHolder
                .getTimeSeries();
        simulatedDischargeTimeSeries = simulatedDischargeTimeSeries
                .getSubTimeSeries(_modelStartTimeHolder.getTime(),
                        _modelEndTimeHolder.getTime());
        _simulatedDischargeTimeSeriesHolder
                .setTimeSeries(simulatedDischargeTimeSeries);

        // convert simulated to stage and put in holder
        RegularTimeSeries simulatedStageTimeSeries = _ratingCurve
                .getStageRegularTimeSeries(simulatedDischargeTimeSeries);
        _simulatedStageTimeSeriesHolder.setTimeSeries(simulatedStageTimeSeries);

        forecastStageTimeSeries.setName("Forecast#" + _modelRunCount);

        _forecastStageTimeSeriesHolder.setTimeSeries(forecastStageTimeSeries);

        return forecastStageTimeSeries;

    } // end runModel

    // -----------------------------------------------------------

    // -----------------------------------------------------------
    public RatingCurve getRatingCurve() {
        return _dataMgr.loadRatingCurve(_locationId);
    }

    // -----------------------------------------------------------

    public void setThresholdRunoff(double thresholdRunoff) {
        _thresholdRunoff = thresholdRunoff;

        if (_rainfallRunoffModelType == RainfallRunoffModelType.API_MKC) {
            KcApiRainfallRunoffModel model = (KcApiRainfallRunoffModel) _rainfallRunoffModel;
            model.setThresholdRunoff(_thresholdRunoff);
        }
    }

    // -----------------------------------------------------------
    public void saveModelStates(Logger logger) {
        if (_rainfallRunoffModelType == RainfallRunoffModelType.SAC_SMA) {
            SacSmaRainfallRunoffModel model = (SacSmaRainfallRunoffModel) _rainfallRunoffModel;

            SacSmaState state = model.getState();

            state.setSource("LOCAL");
            logger.log("StreamModel.saveModelStates(): saved State = " + state);

            _dataMgr.saveSacState(state);

        }
    }

    // -----------------------------------------------------------
    public void saveRunoffTimeSeries(Logger logger) {
        RegularTimeSeries runoffTimeSeries = getRunoffTimeSeriesHolder()
                .getTimeSeries();
        RegularTimeSeries priorRunoffTimeSeries = getPriorRunoffTimeSeriesHolder()
                .getTimeSeries();

        RegularTimeSeries allRunoffTs = null;

        if ((runoffTimeSeries != null) && (priorRunoffTimeSeries != null)) {
            allRunoffTs = RegularTimeSeries.concatenate(runoffTimeSeries,
                    priorRunoffTimeSeries);
        } else if (runoffTimeSeries != null) {
            allRunoffTs = runoffTimeSeries;
        } else if (priorRunoffTimeSeries != null) {
            allRunoffTs = priorRunoffTimeSeries;
        }

        logger
                .log("StreamModel.saveRunoffTimeSeries(): saved runoff time series = "
                        + allRunoffTs);

        _dataMgr.saveRunoffTimeSeries(_locationId, allRunoffTs);

    }

    // -----------------------------------------------------------
    public void savePrecipTimeSeriesToFile(String filePath) {
        RegularTimeSeries precipTimeSeries = getPrecipTimeSeriesHolder()
                .getTimeSeries();

        if (precipTimeSeries != null) {
            _dataMgr.saveMapTimeSeriesToFile(precipTimeSeries, filePath);
        }

        return;
    }

    // -----------------------------------------------------------

    /**
     * @param isBlendingOn
     *            The isBlendingOn to set.
     */
    public void setAdjustmentOn(boolean isAdjustmentOn) {

        _isAdjustmentOn = isAdjustmentOn;

        // Note: params refers to the same object, not a copy
        ForecastAdjusterParams params = _forecastAdjuster.getParams();
        params.setShouldDoAdjustment(_isAdjustmentOn);

        // System.out.println("StreamModel.setAdjustmentOn(): new value = " +
        // _isAdjustmentOn);
    }

    // -----------------------------------------------------------
    public void setLastObservedTimeForAdjustment(long lastTime) {
        String header = "StreamModel.setLastObservedTimeForAdjustment():";

        System.out.println(header + "lastTime = "
                + DbTimeHelper.getDateTimeStringFromLongTime(lastTime));
        if (_forecastAdjuster != null) {
            _forecastAdjuster.setLastObservedTimeToUseAsInput(lastTime);
        }
    }

    // -----------------------------------------------------------
    public int getBlendingHours() {
        int hoursToAdjust = -1;
        final String header = "StreamModel.getBlendingHours():";

        if (_forecastAdjuster != null) {
            hoursToAdjust = _forecastAdjuster.getParams().getBlendingHours();
        }

        return hoursToAdjust;
    }

    // -----------------------------------------------------------
    public int getMaxBlendingHours() {
        return MAX_BLENDING_HOURS;
    }

    // -----------------------------------------------------------
    public void setAdjustmentHours(int hoursToAdjust) {
        // final String header = "StreamModel.setAdjustmentHours():";

        if (_forecastAdjuster != null) {
            _forecastAdjuster.getParams().setBlendingHours(hoursToAdjust);
            // System.out.println(header + "value = " + hoursToAdjust);
        }

        return;
    }

    // -----------------------------------------------------------

    /**
     * @return Returns the isBlendingOn.
     */
    public boolean isAdjustmentOn() {
        return _isAdjustmentOn;
    }

    /**
     * @return Returns the sacParams.
     */
    public SacSmaParameters getSacParams() {
        return _sacParams;
    }

    /**
     * @return Returns the sacState.
     */
    public SacSmaState getSacState() {
        return _sacState;
    }

    public void setSacState(SacSmaState state) {
        _sacState = state;
    }

    /**
     * @param maxForecastLengthInHours
     *            The maxForecastLengthInHours to set.
     */
    public void setMaxForecastLengthInHours(int maxForecastLengthInHours) {
        _maxForecastLengthInHours = maxForecastLengthInHours;
    }

    /**
     * @return Returns the maxForecastLengthInHours.
     */
    public int getMaxForecastLengthInHours() {
        return _maxForecastLengthInHours;
    }

}
