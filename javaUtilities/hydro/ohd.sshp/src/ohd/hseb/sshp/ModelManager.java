package ohd.hseb.sshp;

import java.util.HashMap;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;

import ohd.hseb.model.RainfallRunoffModelType;
import ohd.hseb.model.sacsma.SacSmaState;
import ohd.hseb.util.AppsDefaults;
import ohd.hseb.util.CodeTimer;
import ohd.hseb.util.TimeHelper;
import ohd.hseb.util.TimeHolder;


/**
 * 
 * @author cgobs
 * @date August 20, 2007
 * This class acts as the Manager of various StreamModels.
 * Some StreamModels may be for alternate current time simulations, others for past model times.
 */
public class ModelManager
{
    private DataMgr _dataMgr = null;
    private StreamModel _primaryStreamModel = null;
    private Map<SacSmaStateDescriptor, StreamModel> _modelMap = new HashMap<SacSmaStateDescriptor, StreamModel>();
    
  //  private List<StreamModel> _currentStreamModelList = new ArrayList<StreamModel>();
  //  private List<StreamModel> _pastStreamModelList = new ArrayList<StreamModel>();
    
    private List<StreamModel> _streamModelList = new ArrayList<StreamModel>();
    
    private boolean _showAlternateForecasts = false;
    private boolean _showPastForecasts = false;
    
    private SSHPConfig _config = null;
  
    //location-specific info
    private String _locationId = null;
    private String _locationName = null;
    private String _basinId = null;
    
    //model run time related variables
    
    private static final int MAX_DAYS_OF_TIME_SERIES = 100;
    private static final int MILLIS_PER_HOUR = 60 * 60 * 1000;
    private static final int MILLIS_PER_DAY =  24 * MILLIS_PER_HOUR;

    
    private TimeHolder _modelStartTimeHolder = new TimeHolder();
    private TimeHolder _modelEndTimeHolder = new TimeHolder();
    
    private static final int DEFAULT_INITIAL_FORECAST_LENGTH_IN_HOURS = 24;
    private static final int DEFAULT_MAX_FORECAST_LENGTH_IN_HOURS = 120;

    private int _forecastLengthInHours = DEFAULT_INITIAL_FORECAST_LENGTH_IN_HOURS;
    private int _maxForecastLengthInHours = DEFAULT_MAX_FORECAST_LENGTH_IN_HOURS;
    
    private int _hoursBackForPreviousForecastStates = 24;
    private int _intervalInHoursForPreviousForecastStates = 6;
    
      
    // ------------------------------------------------------------------------

    public ModelManager(String locationId, DataMgr dataMgr)
    {
         _dataMgr = dataMgr;
        
        
         //set up location-specific data
         setLocationId(locationId);
         
        _config = _dataMgr.getSSHPConfig(_locationId);       
        
        
        //set the basin id associated with the locationId
        if (_config != null)
        {
            _basinId = _config.getBasinId();   
        }
        else
        {
            _basinId = "BOGUS_BASIN";   
        }
     
        //get the location name
        _locationName = dataMgr.getLocationName(_locationId);
        
        
        AppsDefaults ad = new AppsDefaults();
        
        
        //load the forecast length parameters
        _forecastLengthInHours = ad.getInt("sshp_initial_forecast_length", 
                                    DEFAULT_INITIAL_FORECAST_LENGTH_IN_HOURS);

        _maxForecastLengthInHours = ad.getInt("sshp_max_forecast_length",
                                    DEFAULT_MAX_FORECAST_LENGTH_IN_HOURS);

        if (_forecastLengthInHours > _maxForecastLengthInHours)
        {
            _maxForecastLengthInHours = _forecastLengthInHours;
        }
        
        
        
    }
    // ------------------------------------------------------------------------
    
    public DataMgr getDataManager()
    {
        return _dataMgr;
    }
    //  -----------------------------------------------------------
    
    public String getLocationId()
    {
        return _locationId;    
        
    }
    
    // ------------------------------------------------------------------------
    public void setLocationId(String locationId)
    {
       _locationId = locationId;  
    }
    
    // ------------------------------------------------------------------------
    public void setBasinId(String basinId)
    {
       _basinId = basinId;  
    }
    
    // ------------------------------------------------------------------------
    
    public String getBasinId()
    {
        return _basinId;
    }

    // -------------------------------------------------------------
    public void setPrimaryModelStartTime(long modelStartTime)
    {
        modelStartTime = TimeHelper.truncateTimeInMillisToNearestHour(modelStartTime, 1);
       // trace();
        getPrimaryStreamModel().setModelStartTime(modelStartTime);
        
        _modelStartTimeHolder.setTime(modelStartTime); 
        
        //sets the model end time
        setForecastLengthInHours(_forecastLengthInHours);
        
        for (StreamModel streamModel : _streamModelList)
        {
            
            if (streamModel.getRainfallRunoffModelType() == RainfallRunoffModelType.SAC_SMA)
            { 
                streamModel.setModelStartTime(streamModel.getSacStateValidTime());
            }
        }
            
        return;
    }
    // ------------------------------------------------------------------------
    public void setForecastLengthInHours(int lengthInHours)
    {
       // String header = "ModelManager.setForecastLengthInHours(): ";
        
      //  System.out.println(header + "lengthInHours = " + lengthInHours);
        
        if (lengthInHours > 0)
        {
            _forecastLengthInHours = lengthInHours;
            long duration = ((long)(lengthInHours) * (long)MILLIS_PER_HOUR);
            long newEndTime =  _modelStartTimeHolder.getTime() + duration;
                               
            setModelEndTime(newEndTime);
        }
        
        for (StreamModel streamModel : getCompleteStreamModelList())
        {
            streamModel.setForecastLengthInHours(_forecastLengthInHours);
        }
        
        return;
    }
    //  -----------------------------------------------------------
    
    public int getForecastHours()
    {
        return _forecastLengthInHours;
    }
    
    //  -----------------------------------------------------------
    
   
    public void setModelEndTime(long modelEndTime)
    {
         _modelEndTimeHolder.setTime(modelEndTime); 
    }
 
    //  -----------------------------------------------------------
    
    public void addStreamModel(StreamModel streamModel)
    {
        _streamModelList.add(streamModel);
    }
    // ------------------------------------------------------------------------
    public void removeAllPastStreamModels()
    {

        String header = "ModelManager.removeAllPastStreamModels(): ";

        List<StreamModel> modelsToRemoveList = new ArrayList<StreamModel>();

        //if the model has a different source than the primary stream model, then it is an alternate StreamModel
        for (StreamModel streamModel : _streamModelList)
        {
            if (streamModel.getModelStartTime() != getPrimaryStreamModel().getModelStartTime()) //this line is just for efficiency
            {

                modelsToRemoveList.add(streamModel);

            }
        }

        //remove the past streamModels
        for (StreamModel streamModel : modelsToRemoveList)
        {
                _streamModelList.remove(streamModel);
        }

        System.out.println(header + " removed " + modelsToRemoveList.size() + " streamModels. " );

    } //end removeAllAlternateStreamModels()



    // ------------------------------------------------------------------------
    public void removeAllStreamModels()
    {
    
        
       _streamModelList.clear();
        
      // removeAllCurrentStreamModels();
      // removeAllPastStreamModels();
       
     //  printAllModels();
    }
    // ------------------------------------------------------------------------
    public void removeAllStreamModelsExceptPrimary()
    {
       _streamModelList.clear();
       setPrimaryStreamModel(getPrimaryStreamModel());
    }
    // ------------------------------------------------------------------------
    public void setPrimaryStreamModel(StreamModel primaryStreamModel)
    {
        this._primaryStreamModel = primaryStreamModel;
        
        primaryStreamModel.setIsAlternateStreamModel(false);
        
        // add to the list of currentStreamModels
        if (! _streamModelList.contains(primaryStreamModel))
        {
            _streamModelList.add(primaryStreamModel);
        }
    }
    // ------------------------------------------------------------------------
    
    public StreamModel getPrimaryStreamModel()
    {
        return _primaryStreamModel;
    } 
    // ------------------------------------------------------------------------
    public List<StreamModel> getCompleteStreamModelList()
    { 
        return _streamModelList;
    }
    
    // ------------------------------------------------------------------------
  
    public void runStreamModels()
    {
        String header = "ModelManager.runStreamModels(): ";
        int count = 0;
        
        for (StreamModel model : _streamModelList)
        {
            //System.out.println(header + " model[" + count + "] = " + model);
            model.runModel();
          //  count++;
        }

    }
    // ------------------------------------------------------------------------
    public List<SacSmaStateDescriptor> getActualSacSmaStateDescriptorList()
    {
        boolean forceCacheUpdate = true;
        List list = _dataMgr.getSacSmaStateDescriptorListByBasinId(getBasinId(), forceCacheUpdate); 
        return list;
    }
    
    // ------------------------------------------------------------------------   
    
    public List<SacSmaStateDescriptor> getSacSmaStateDescriptorListWithBogusStateIfEmpty(boolean showUnadjustedStates)
    {
        List list = getActualSacSmaStateDescriptorList(); 
        
        
        if (! showUnadjustedStates)
        {
            list = filterOutUnadjustedStates(list);
        }
      
        
        if (list.size() == 0)
        {
            SacSmaState state = getDefaultSacSmaState();
            SacSmaStateDescriptor descriptor = new SacSmaStateDescriptor(state);
            
            list.add(descriptor);
        }
        return list;
    }
    
    //  ----------------------------------------------------------------------------  
    public List<SacSmaStateDescriptor> filterOutUnadjustedStates(List<SacSmaStateDescriptor> sacStateDescriptorList)
    {
        
        List<SacSmaStateDescriptor> filteredList = new ArrayList<SacSmaStateDescriptor>();
        
        String header = "ModelManager.filterOutUnadjustedStates(): ";
        
        final String unadjustedSourceString = SSHPSource.UNADJUSTED_VAR.getSacSmaSource();
        
        
        for (SacSmaStateDescriptor descriptor : sacStateDescriptorList)
        {
            String stateSourceString = descriptor.getState().getSource();
            
            if (! stateSourceString.equals(unadjustedSourceString))
            {
                filteredList.add(descriptor);
            }

        }      
        return filteredList;
   
    }
    
     //  ----------------------------------------------------------------------------  
    private SacSmaState getDefaultSacSmaState()
    {
        SacSmaState state = new SacSmaState();
        state.setSource("BOGUS DEFAULT");
        
        state.setBasinId(_basinId);
        state.setValidTime(getLatestHourTime());
        
        state.setUztwc(0);
        state.setUzfwc(0);
        state.setLztwc(0);
        state.setLzfsc(0);
        state.setLzfpc(0);
        state.setAdimc(0);
        
        return state;
    }
    //  ----------------------------------------------------------------------------
    public long getLatestHourTime()
    {
        return TimeHelper.truncateTimeInMillisToNearestHour(System.currentTimeMillis(), 1);
    }
    //  -----------------------------------------------------------
 
    public void removeAllAlternateStreamModels()
    {      
        
        String header = "ModelManager.removeAllAlternateStreamModels(): ";
        List<StreamModel> modelsToRemoveList = new ArrayList<StreamModel>();
               
        //if the model has a different source than the primary stream model, then it is an alternate StreamModel
        for (StreamModel streamModel : _streamModelList)
        {
            if (streamModel != getPrimaryStreamModel()) //this line is just for efficiency, it doesn't really change anything
            {
                if (! haveSameSource(streamModel, getPrimaryStreamModel()))
                {
                    modelsToRemoveList.add(streamModel);
                }
            }
        }
        
        //remove the alternate streamModels
        for (StreamModel streamModel : modelsToRemoveList)
        {
            if (streamModel != getPrimaryStreamModel())
            {           
                _streamModelList.remove(streamModel);
            }
        }
        
        System.out.println(header + " removed " + modelsToRemoveList.size() + " streamModels. " );
      
    } //end removeAllAlternateStreamModels()
    
    //  -----------------------------------------------------------
    public boolean haveSameSource(SacSmaStateDescriptor descriptor1, 
                                  SacSmaStateDescriptor descriptor2)
    {
        boolean result = false;
        
        String sourceString1 = descriptor1.getState().getSource();
        String sourceString2 = descriptor2.getState().getSource();
        
        if (sourceString1.equals(sourceString2))
        {
            result = true;
        }
        
        return result;
    }
    //  -----------------------------------------------------------
    public boolean haveSameSource(StreamModel model1, 
                              StreamModel model2)
    {
        boolean result = false;

        String sourceString1 = model1.getSacState().getSource();
        String sourceString2 = model2.getSacState().getSource();
        
        if (sourceString1.equals(sourceString2))
        {
            result = true;
        }
        

        return result;
    }
//  -----------------------------------------------------------
    
    public void printAllModels()
    {
        String header = "ModelManager.printAllModels();";
  
        int i = 0;
        
        for (StreamModel streamModel : _streamModelList)
        {
            System.out.println(header + "model[ " + i + "] = " + streamModel);
            i++;
        }
          
    }
        
    //  -----------------------------------------------------------
    public void setHoursBackAndIntervalForPreviousForecastStates(int hoursBack, int intervalInHours)
    {
        _hoursBackForPreviousForecastStates = hoursBack;
        _intervalInHoursForPreviousForecastStates = intervalInHours;
        
        StreamModel primaryStreamModel = getPrimaryStreamModel();
        SacSmaState state = primaryStreamModel.getSacState();
        SacSmaStateDescriptor descriptor = new SacSmaStateDescriptor(state);
        
        configureStreamModelsForSacSma(primaryStreamModel, descriptor, _showAlternateForecasts, _showPastForecasts);
        
        return;
    }
    
    //  -----------------------------------------------------------
    
    
    //  -----------------------------------------------------------
    public void setShowAlternateForecasts(boolean showAlternateForecasts)
    {
        String header = "ModelManager.setShowAlternateForecasts(): ";
     
        _showAlternateForecasts = showAlternateForecasts;

        if (_showAlternateForecasts)
        {
            StreamModel primaryStreamModel = getPrimaryStreamModel();
            SacSmaState state = primaryStreamModel.getSacState();
            SacSmaStateDescriptor descriptor = new SacSmaStateDescriptor(state);

            configureStreamModelsForSacSma(primaryStreamModel, descriptor, _showAlternateForecasts, _showPastForecasts);
        }
        else //set not to display the alternate forecasts
        {
            System.out.println(header + "removing all alternate StreamModels. ");
            removeAllAlternateStreamModels();
        }
     
    }
    //  -----------------------------------------------------------
    public void setShowPastForecasts(boolean showPastForecasts)
    {
        String header = "ModelManager.setShowPastForecasts(): "; 
       
        _showPastForecasts = showPastForecasts;

        if (_showPastForecasts) //changed to display the states
        {
            StreamModel primaryStreamModel = getPrimaryStreamModel();
            SacSmaState state = primaryStreamModel.getSacState();
            SacSmaStateDescriptor descriptor = new SacSmaStateDescriptor(state);

            CodeTimer timer = new CodeTimer();
            timer.start();
            configureStreamModelsForSacSma(primaryStreamModel, descriptor, _showAlternateForecasts, _showPastForecasts);
            timer.stop(header + " configureStreamModelsForSacSma() took");
        }
        else //changed not to display the alternate forecasts
        {
            removeAllPastStreamModels();
        }
     
    }
  
    //  -----------------------------------------------------------   
    
    public void  configureStreamModelsForSacSma(StreamModel primaryStreamModel, 
                                                SacSmaStateDescriptor primaryDescriptor,
                                                boolean showAlternateForecasts, boolean showPastForecasts)
    {
        String  header = "ModelManager.configureCurrentStreamModelsForSacSma(): ";

      //  System.out.println(header + "---------------------------------------------at the top of the method");
        
        removeAllStreamModels();

//      for each SacSmaStateDescriptor
//      if the descriptor is not the primary descriptor, but has the same time, then
//      create a StreamModel for it and set the descriptor.
//      add the stream model to the current stream model list


//      add in streamModels that have the same time as the primary streamModel

        if (primaryStreamModel.getRainfallRunoffModelType() == RainfallRunoffModelType.SAC_SMA)
        {
       //     System.out.println(header + "using  SAC-SMA Rainfall-Runoff model");

       //     System.out.println(header + " --------------------------------------------------------");      
       //     System.out.println(header + "primaryDescriptor = " + primaryDescriptor);
        //    System.out.println(header + "primaryDescriptor  state = " + primaryDescriptor.getState());

            for (SacSmaStateDescriptor descriptor : getActualSacSmaStateDescriptorList())
            {
                if  (! sameTimeAndSource(descriptor, primaryDescriptor) )
                {
                    if (showAlternateForecasts) //if should show alternate forecasts
                    {
                        if (descriptor.getValidTime() == primaryDescriptor.getValidTime() )
                        {

                            //create a StreamModel for this time and add to the list of models
                            getAndAddAlternateStreamModel(getLocationId(), descriptor, primaryDescriptor, primaryStreamModel);

                        } //end if times are the same
                    } //end if showAllCurrentForecasts


                    if (showPastForecasts)
                    {
                         final int millisBack = _hoursBackForPreviousForecastStates * MILLIS_PER_HOUR;
                         final int intervalInMillis = _intervalInHoursForPreviousForecastStates * MILLIS_PER_HOUR;

                        if (
                                (descriptor.getValidTime() < primaryDescriptor.getValidTime() )  &&
                                (descriptor.getValidTime() >=  (primaryDescriptor.getValidTime() - millisBack) )
                        )
                        {
                            long timeDiff = primaryDescriptor.getValidTime() - descriptor.getValidTime();

                            //if at the right interval
                            if ( ( timeDiff >= intervalInMillis) && (timeDiff % intervalInMillis) == 0) 
                            {

                                if (showAlternateForecasts) //show alternate and other past forecasts
                                {
                                    //create a StreamModel for this time and add to the list of models
                                    getAndAddAlternateStreamModel(getLocationId(), descriptor, primaryDescriptor, primaryStreamModel);
                                }

                                else //just show ones that match the primary Source
                                {
                                    if (haveSameSource(descriptor, primaryDescriptor))
                                    {
                                        //create a StreamModel for this time and add to the list of models
                                        getAndAddAlternateStreamModel(getLocationId(), descriptor, primaryDescriptor, primaryStreamModel);
                                    }
                                }
                            }
                        } //end if times are the same
                    }

                } //end if not sameTimeAndSource, meaning -> not the primaryStreamModel

                else //this is the primaryStreamModel
                { 
                    CodeTimer timer = new CodeTimer();
                   // timer.start();
                    primaryStreamModel.setSacSmaStateDescriptor(descriptor); 
                   // timer.stop(header + "  primaryStreamModel.setSacSmaStateDescriptor() took: ");
                    addStreamModel(primaryStreamModel);
                 //   System.out.println(header + "resetting descriptor = primaryDescriptor " + descriptor);

                }
            } //end for

        } //end if using SAC-SMA
        else
        {
            System.out.println(header + "using a different Rainfall-Runoff model, so doing nothing here");
        }


      //  printAllModels();

       // System.out.println(header + " --------------------------------------------------------");




    } // end configureCurrentStreamModelsForSacSma
//  -----------------------------------------------------------------
    public void  configureStreamModelsForMkcApi(StreamModel primaryStreamModel, 
            FFHDescriptor primaryDescriptor)
    {
        String  header = "ModelManager.configureStreamModelsMkcApi(): ";

        removeAllStreamModels();

        addStreamModel(primaryStreamModel);
    
    } // end configureStreamModelsForMkcApi
//  -----------------------------------------------------------------
    private void getAndAddAlternateStreamModel(String locationId,
                                                  SacSmaStateDescriptor descriptor,
                                                  SacSmaStateDescriptor primaryDescriptor,
                                                  StreamModel primaryStreamModel
                                                  )
    {
        
        String  header = "ModelManager.getAndAddAlternateStreamModel(): ";
        CodeTimer getAndAddAlternateStreamModelTimer = new CodeTimer();
        getAndAddAlternateStreamModelTimer.start();
        
       // System.out.println(header + "alternateDescriptor = " + descriptor);
       // System.out.println(header + "descriptor  state = " + descriptor.getState());

        StreamModel streamModel = null;
        
        //first look for the stream model in the cache, if not there, create it
        streamModel = getStreamModelFromCache(descriptor);
        
        if (streamModel == null)
        {
            CodeTimer createAlternateStreamModelTimer = new CodeTimer();
            createAlternateStreamModelTimer.start();
            streamModel = createAlternateStreamModel(locationId, descriptor, primaryDescriptor, primaryStreamModel);
            createAlternateStreamModelTimer.stop(header + "createAlternateStreamModel() took ");
            _modelMap.put(descriptor, streamModel);
            
           // System.out.println(header + "did NOT find in cache, alternateDescriptor = " + descriptor);
        }
        else
        {
           // System.out.println(header + "found in cache, alternateDescriptor = " + descriptor);
        }
        
        
        if (streamModel != null)
        {
            streamModel.setForecastLengthInHours(_forecastLengthInHours);
            
            addStreamModel(streamModel);
        } 
        
        getAndAddAlternateStreamModelTimer.stop(header + " took ");
     
    }
//  -----------------------------------------------------------------
    
    private StreamModel createAlternateStreamModel(String locationId,
            SacSmaStateDescriptor descriptor,
            SacSmaStateDescriptor primaryDescriptor,
            StreamModel primaryStreamModel)
    {
        
        String  header = "ModelManager.createAlternateStreamModel(): ";

        
        final boolean isAlternateStreamModel = true;

        
        StreamModel streamModel = new StreamModel(locationId,
                primaryStreamModel.getRainfallRunoffModelType(),
                getDataManager(),
                isAlternateStreamModel);


        System.out.println(header + "descriptor = " + descriptor);

        streamModel.setSacSmaStateDescriptor(descriptor);
        streamModel.setModelStartTime(descriptor.getValidTime());

        System.out.println(header + "before loadPriorRunoffTimeSeries,  alternate streamModel = " + streamModel + "\n");

        streamModel.loadPriorRunoffTimeSeries("loadPriorRunoffTimeSeries called from configureCurrentStreamModelsForSacSma()");

        System.out.println(header + "after loadPriorRunoffTimeSeries,  alternate streamModel = " + streamModel + "\n");


        streamModel.setPrecipTimeSeriesHolder(primaryStreamModel.getPrecipTimeSeriesHolder());
        
        return streamModel;
    }
    
//  -----------------------------------------------------------------
    StreamModel getStreamModelFromCache(SacSmaStateDescriptor descriptor)
    {
        StreamModel streamModel = _modelMap.get(descriptor);
   
        return streamModel;
    }
    
//  -----------------------------------------------------------------
    
    private boolean sameTimeAndSource(SacSmaStateDescriptor desc1, SacSmaStateDescriptor desc2)
    {
        boolean result = false;

        String source1 = desc1.getState().getSource();
        String source2 = desc2.getState().getSource();

        if (desc1.getValidTime() == desc2.getValidTime())
        {

            if (source1.equals(source2))
            {
                result = true;
            }

        }

        return result;
    }
//  -----------------------------------------------------------------

} //end ModelManager
