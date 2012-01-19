package ohd.hseb.timeserieslite.rivermon;

import java.sql.SQLException;
import java.text.DecimalFormat;

import java.util.List;

import ohd.hseb.db.Database;
import ohd.hseb.db.DbTable;
import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.ihfsdb.generated.DischargeRecord;
import ohd.hseb.ihfsdb.generated.DischargeTable;
import ohd.hseb.ihfsdb.generated.FcstDischargeRecord;
import ohd.hseb.ihfsdb.generated.FcstDischargeTable;
import ohd.hseb.ihfsdb.generated.FcstHeightRecord;
import ohd.hseb.ihfsdb.generated.FcstHeightTable;
import ohd.hseb.ihfsdb.generated.HeightRecord;
import ohd.hseb.ihfsdb.generated.HeightTable;
import ohd.hseb.ihfsdb.generated.IngestFilterRecord;
import ohd.hseb.ihfsdb.generated.IngestFilterTable;
import ohd.hseb.ihfsdb.generated.LocPDCRecord;
import ohd.hseb.ihfsdb.generated.LocPDCView;
import ohd.hseb.ihfsdb.generated.LocationRecord;
import ohd.hseb.ihfsdb.generated.LocationTable;
import ohd.hseb.ihfsdb.generated.RatingRecord;
import ohd.hseb.ihfsdb.generated.RatingShiftRecord;
import ohd.hseb.ihfsdb.generated.RatingShiftTable;
import ohd.hseb.ihfsdb.generated.RatingTable;
import ohd.hseb.ihfsdb.generated.RiverstatRecord;
import ohd.hseb.ihfsdb.generated.RiverstatTable;
import ohd.hseb.ihfsdb.generated.StnClassTable;
import ohd.hseb.measurement.AbsTimeMeasurement;
import ohd.hseb.measurement.IrregularTimeSeries;
import ohd.hseb.measurement.Measurement;
import ohd.hseb.measurement.MeasuringUnit;
import ohd.hseb.measurement.RegularTimeSeries;
import ohd.hseb.model.RatingCurve;
import ohd.hseb.model.RatingPoint;
import ohd.hseb.model.SigRiverLevels;
import ohd.hseb.pdc_pp.RegularObsTimeSeries;
import ohd.hseb.pdc_pp.TimeValuePair;
import ohd.hseb.timeserieslite.Location;
import ohd.hseb.timeserieslite.PDCDataType;
import ohd.hseb.timeserieslite.ParamCode;
import ohd.hseb.util.AppsDefaults;
import ohd.hseb.util.FileLogger;
import ohd.hseb.util.Logger;
import ohd.hseb.util.TimeHelper;


public class RiverMonTslDataManager
{
    
    // --------------------------------------------------------------------------------------------
    
    public  static final double MISSING = -999.0;
    
    private static final int _defaultQualityCode = 1879048191;
    private static final int _questionable_bad_threshold =  1073741824;
    private static final long MILLIS_PER_HOUR = 60*60*1000;     
    private static final long MILLIS_PER_YEAR = (365L * 24L * 60L * 60L * 1000L);
    
    private Database _db = null;
    private Logger _logger = null;
    
    private String _jdbcConnectionString = null;   
    private PDCDataType _dataType = null;
    
    private String _physicalElement = null;
    
    private String _preprocessedFilePath = null;
    private double MAX_REASONABLE_PRECIP = 30; //inches per hour
 
    // ---------------------------------------------------------------------------------
  
    public RiverMonTslDataManager(String jdbcConnectionString)
    {
        _logger  = new FileLogger(true);
        
        _jdbcConnectionString = jdbcConnectionString;
        
        _db = new Database();     
      
        _db.connect(_jdbcConnectionString);
        
        getAppsDefaults();
    }
    // ---------------------------------------------------------------------------------

    private void getAppsDefaults()
    {
        AppsDefaults ad = new AppsDefaults();
     
        _preprocessedFilePath = ad.getToken( "pdc_pp_dir" ) + "/";
    }
        
    //  -------------------------------------------------------------------------------------
    
    private void connect(String connectionString)
    {
        _db.connectWithDriverSearch(connectionString);
    }  
    
    //  -------------------------------------------------------------------------------------
    
    public void disconnect()
    {
        _db.disconnect();   
    }

    //  -------------------------------------------------------------------------------------
    public String getPreferredFcstTypeSource(String lid,
                                                  String pe)
    {
        String header = "PdcTslDataManager.getPreferredFcstTypeSource(): ";
        
        String newTypeSource = null;
        
        IngestFilterTable table = new IngestFilterTable(_db);
        
        IngestFilterRecord record = null;
        
        String whereClause = "WHERE lid = '" + lid + "'" +
                             " AND pe = '" + pe + "'" +
                             " AND dur = 0 " +
                             " AND ts like 'F%' " + 
                             " order by ts_rank ASC" ;
        
        try
        {
            List recordList = table.select(whereClause);
             
            if (recordList.size() > 0)
            {
                if (pe.charAt(0)=='H')
                {
                       for (int i = 0; i < recordList.size(); i++)
                    {
                        record = (IngestFilterRecord) recordList.get(i);
                        if (fcstHeightRecordExists(record))
                        {
                            newTypeSource = record.getTs();
                            break;
                        }
                    } 
                
                }
                else //pe is not 'H*', so don't look at fcstHeight to see if it exists
                {
                    newTypeSource = record.getTs();
                }
                
            }
        }
        catch(SQLException e)
        {
            logSQLException(e);
        }
        
  //      System.out.println(header + " newTypeSource = " + newTypeSource); 
        
        return newTypeSource;
    }
 
    //  ----------------------------------------------------------------------------------
    public boolean fcstHeightRecordExists(IngestFilterRecord ingestRecord)
    {
         boolean result = false;
         
         String header = "PdcTslDataManager.fcstHeightRecordExists(): ";
        
         FcstHeightTable table = new FcstHeightTable(_db);
      
         String whereClause = "WHERE lid = '" + ingestRecord.getLid() + "'" +
                              " AND pe = '" + ingestRecord.getPe() + "'" +
                              " AND dur = " + ingestRecord.getDur() + 
                              " AND ts = '" + ingestRecord.getTs() + "'";
                             
     
         try
         {
             int recordCount = table.selectCount(whereClause);
              
             if (recordCount > 0)
             {
                 result = true;
             }
         }
         catch(SQLException e)
         {
              logSQLException(e);
         }
 
         return result;
    }
    //  ----------------------------------------------------------------------------------
 /*   public String getPreferredFcstTypeSourceByIngestFilter(String lid,
                                             ParamCode paramCode)
    {
        String header = "PdcTslDataManager.getPreferredFcstTypeSourceByIngestFilter(): ";
        
        String newTypeSource = null;
     
        IngestFilterTable table = new IngestFilterTable(_db);
        
        IngestFilterRecord record = null;
    
        String whereClause = "WHERE lid = '" + lid +
                             "' AND pe = '" + paramCode.getPe() + "'" +
                             "AND dur = 0 " +
                             "AND ts like 'F%' " + 
                             " order by ts_rank ASC" ;
    
        try
        {
            List recordList = table.select(whereClause);
            if (recordList.size() > 0)
            {
                int i = 0;
                //for (int i = 0; i < recordList.size(); i++)
                {
                    record = (IngestFilterRecord) recordList.get(i);
                    newTypeSource = record.getTs();
                }   
            }
        }
        catch(SQLException e)
        {
             logSQLException(e);
        }
   
        System.out.println(header + " newTypeSource = " + newTypeSource); 
        
        return newTypeSource;
    }
    
    //  -------------------------------------------------------------------------------------
    public String getPreferredFcstTypeSourceByFcstHeight(String lid,
                                             ParamCode paramCode)
    {
        String header = "PdcTslDataManager.getPreferredFcstTypeSource(): ";
        
        String newTypeSource = null;
     
        FcstHeightTable table = new FcstHeightTable(_db);
        
        FcstHeightRecord record = null;
    
        String whereClause = "WHERE lid = '" + lid +
                             "' AND pe = '" + paramCode.getPe() + "'" +
                             "AND dur = 0 " +
                             " order by basistime DESC, validtime ASC" ;
    
        try
        {
            List recordList = table.selectNRecords(whereClause,1);
            if (recordList.size() > 0)
            {
                int i = 0;
                //for (int i = 0; i < recordList.size(); i++)
                {
                    record = (FcstHeightRecord) recordList.get(i);
                    newTypeSource = record.getTs();
                }   
            }
        }
        catch(SQLException e)
        {
             logSQLException(e);
        }
   
        System.out.println(header + " newTypeSource = " + newTypeSource); 
        
        return newTypeSource;
    }
 */   
    //  -------------------------------------------------------------------------------------
     
    public SigRiverLevels loadSigRiverLevels(String locationId)
    {
        String header = "PdcTslDataManager.loadSigRiverLevels(): ";
        RiverstatTable table = new RiverstatTable(_db);
        RiverstatRecord record = null;
        
        SigRiverLevels levels = null;
        
        String whereClause = "WHERE lid = '" + locationId + "'";
    
        try
        {
            List recordList = table.select(whereClause);
            if (recordList.size() > 0)
            {
                record = (RiverstatRecord) recordList.get(0);
                
                String primaryPe = record.getPrimary_pe();
                
                
                double floodStage = record.getFs();
                double actionStage = record.getWstg();
                double floodFlow = record.getFq();
                double actionFlow = record.getAction_flow();
                
                
                if (DbTable.isNull(actionStage))
                {
                     System.out.println(header + " actionStage is null for " + locationId);                
                }
                if (DbTable.isNull(record.getPrimary_pe()))
                {
                    System.out.println(header + " primary_pe is null for " + locationId);
                    record.setPrimary_pe("  ");
                }
                
                
                
                levels = new SigRiverLevels(locationId, 
                            record.getPrimary_pe(),
                            floodStage, 
                            actionStage,
                            floodFlow,
                            actionFlow);
                
            }   
        }
        catch(SQLException e)
        {
             logSQLException(e);
        }
        
        return levels;
    }
    
    // --------------------------------------------------------------------------------------------------
    
    public RatingCurve loadRatingCurve(String locationId)
    {
        RatingCurve ratingCurve = null;
         
        RatingTable ratingTable = new RatingTable(_db); 
        RatingShiftTable ratingShiftTable = new RatingShiftTable(_db);
       
        RiverstatTable riverstatTable = new RiverstatTable(_db);
        
        List ratingRecordList = null;
        List ratingShiftList = null;
        List riverstatList = null;
        
        
        try
        {
            ratingRecordList = ratingTable.select("WHERE LID = '" +
                                               locationId + "' ORDER by stage");
                                               
            ratingShiftList  = ratingShiftTable.selectNRecords("WHERE LID = '" + 
                                               locationId + "' AND active = 'T' ORDER BY date desc", 1 );
                                               
                  
            riverstatList = riverstatTable.select("WHERE LID =  '" +  locationId + "'");  
      
            //determine total amount of active 
            //rating shifts 
            double totalShiftAmount = 0.0;
            
            for (int i = 0; i < ratingShiftList.size(); i++)
            {
               RatingShiftRecord shiftRecord = (RatingShiftRecord) ratingShiftList.get(i);
               totalShiftAmount = shiftRecord.getShift_amount();    

               //System.out.println("totalShiftAmount = " + totalShiftAmount);
            }
            

            //initialize the RatingCurve object with data from the database, including
            // the totalShiftAmount
            ratingCurve = new RatingCurve(locationId);
            
    
            ratingCurve.setShiftAmount(totalShiftAmount);
        
            for (int i = 0; i < ratingRecordList.size(); i++)
            {
                RatingRecord record = (RatingRecord) ratingRecordList.get(i);
            
                RatingPoint ratingPoint = new RatingPoint();
                ratingPoint.setDischarge(record.getDischarge());
                ratingPoint.setUnshiftedStage(record.getStage());
    
                //done by addRatingPoint
                //ratingCurve.setShiftAmount(totalShiftAmount);
                
                ratingCurve.addRatingPoint(ratingPoint);
            } //end for
            
            
            // set the USGS fields in the rating curve
            if (riverstatList.size() > 0)
            {
                  RiverstatRecord riverstatRecord = (RiverstatRecord) riverstatList.get(0);
                  ratingCurve.setUsgsRatingNumber(riverstatRecord.getUsgs_ratenum());
                  ratingCurve.setRatingDate(riverstatRecord.getRatedat());  
            }    
    
        }
        catch (SQLException e)
        {
            logSQLException(e);
            ratingCurve = null;
        }
        
        return ratingCurve;
    }
  
    
//  -------------------------------------------------------------------------------------    
     
    public RegularTimeSeries getRegularTimeSeries(RegularObsTimeSeries obsTimeSeries)
    {
             
        long startTime = obsTimeSeries.getDescriptor().getStartTime();
        long endTime = obsTimeSeries.getDescriptor().getEndTime();
        long timeStepIntervalInMillis = obsTimeSeries.getTimeStepIntervalInMillis();
        long timeStepIntervalInHours = timeStepIntervalInMillis / MILLIS_PER_HOUR;
        MeasuringUnit unit = MeasuringUnit.inches;
        
        String startTimeString = DbTimeHelper.getDateTimeStringFromLongTime(startTime);
        String endTimeString = DbTimeHelper.getDateTimeStringFromLongTime(endTime);
              
        RegularTimeSeries regularTimeSeries = 
                new RegularTimeSeries(startTime, endTime, (int) timeStepIntervalInHours , unit);  
        
        List timeValuePairList = obsTimeSeries.getTimeValuePairList(true);
        
        for (int i = 0; i < timeValuePairList.size(); i++)
        {
             TimeValuePair pair = (TimeValuePair) timeValuePairList.get(i);
             long time = pair.getDateTime();
             double value = pair.getValue();
             
             Measurement measurement = new Measurement(value, unit);
             
             regularTimeSeries.setMeasurementByTime(measurement, time);        
        }
        
        regularTimeSeries.shiftStartEndTimeHours(12);
        
        return regularTimeSeries;
    }
    
//  -------------------------------------------------------------------------------------

    private void logSQLException(SQLException exception)
    {
        _logger.log("SQL ERROR = " +
                    exception.getErrorCode() +  " " +
                    exception.getMessage());
                         
        exception.printStackTrace(_logger.getPrintWriter());
        
        _logger.log("End of stack trace");
         
    }  
//  ------------------------------------------------------- 
    public String getLocationName(String locationId)
    {
        return loadLocationName(locationId);    
    }
//  ------------------------------------------------------- 
    private String loadLocationName(String locationId)
    {
               
        String locationName = null;

        LocationRecord record = null;
        LocationTable table = new LocationTable(_db);


        try
        {
            String whereClause = "WHERE lid = '" + locationId + "'";
            List recordList = table.select(whereClause);


            if (recordList.size() > 0)
            {
                record = (LocationRecord) recordList.get(0);
                locationName = record.getName();
            }
        }
        catch (SQLException e)
        {
            logSQLException(e);
        }

        return locationName;
        
    }
    //  ------------------------------------------------------------------------
    
    public IrregularTimeSeries loadObservedStageTimeSeries(String locationId, 
                                                           ParamCode  paramCode,
                                                           long startTime)
    {
        MeasuringUnit unit = MeasuringUnit.feet;
        String header = "PdcTslDataManager.loadObservedStageTimeSeries(): ";
        IrregularTimeSeries stageTimeSeries = new IrregularTimeSeries(unit);
        
        final double missingValue = -9999.0;
        HeightTable table = new HeightTable(_db);
        HeightRecord record = null;
        List recordList = null;
        AbsTimeMeasurement measurement = null;
        
//        ParamCode paramCode = new ParamCode(paramCodeString);
        
        String startTimeString = DbTimeHelper.getDateTimeStringFromLongTime(startTime);
      
        String whereClause = " WHERE lid = '" + locationId + "'" +
         " AND pe = '" + paramCode.getPe() + "'" +  
         " AND ts = '" + paramCode.getTypeSource() + "'" +  
         " AND dur = " +  paramCode.getIhfsDur() +
         " AND extremum = '" + paramCode.getExtremum() + "'" +  
         " AND quality_code >= " + _questionable_bad_threshold + "";
      //   " AND obstime >= '" + startTimeString + "'";
    
        
        System.out.println(header + "whereClause = " + whereClause);
        
        try
        {       
            recordList = table.select(whereClause);
            
            for (int i = 0; i < recordList.size(); i++)
            {
                record = (HeightRecord) recordList.get(i);
                
              
                //don't let the missing values be included          
                if (record.getValue() != missingValue)
                {
                    measurement = new AbsTimeMeasurement(record.getValue(),
                                                     record.getObstime(),
                                                     unit); 
                                                     
                    stageTimeSeries.insertMeasurement(measurement);
                }   
            }
        }
        catch(SQLException e)
        {
            logSQLException(e);
        }
        
        return stageTimeSeries;
    
    }
    
    //  ------------------------------------------------------------------------

    public IrregularTimeSeries loadObservedDischargeTimeSeries(String locationId, 
                                                           ParamCode  paramCode,
                                                           long startTime)
    {
        MeasuringUnit unit = MeasuringUnit.cfs;
        String header = "PdcTslDataManager.loadObservedDischargeTimeSeries(): ";
        IrregularTimeSeries stageTimeSeries = new IrregularTimeSeries(unit);
        
        final double missingValue = -9999.0;
        DischargeTable table = new DischargeTable(_db);
        DischargeRecord record = null;
        List recordList = null;
        AbsTimeMeasurement measurement = null;
        
//        ParamCode paramCode = new ParamCode(paramCodeString);
        
        String startTimeString = DbTimeHelper.getDateTimeStringFromLongTime(startTime);
      
        String whereClause = " WHERE lid = '" + locationId + "'" +
         " AND pe = '" + paramCode.getPe() + "'" +  
         " AND ts = '" + paramCode.getTypeSource() + "'" +  
         " AND dur = " +  paramCode.getIhfsDur() +
         " AND extremum = '" + paramCode.getExtremum() + "'" +  
         " AND quality_code >= " + _questionable_bad_threshold + "";
      //   " AND obstime >= '" + startTimeString + "'";
    
        
        System.out.println(header + "whereClause = " + whereClause);
        
        try
        {       
            recordList = table.select(whereClause);
            
            for (int i = 0; i < recordList.size(); i++)
            {
                record = (DischargeRecord) recordList.get(i);
                
              
                //don't let the missing values be included          
                if (record.getValue() != missingValue)
                {
                    measurement = new AbsTimeMeasurement(record.getValue(),
                                                     record.getObstime(),
                                                     unit); 
                                                     
                    stageTimeSeries.insertMeasurement(measurement);
                }   
            }
        }
        catch(SQLException e)
        {
            logSQLException(e);
        }
        
        return stageTimeSeries;
    
    }
    
    //  ------------------------------------------------------------------------

   
//  -----------------------------------------------------------------------------------   
    
    public IrregularTimeSeries loadFcstStageTimeSeries(String locationId,
                                                       ParamCode paramCode)
    {
        MeasuringUnit unit = MeasuringUnit.feet;
        String header = "PdcTslDataManager.loadFcstStageTimeSeries(): ";
        IrregularTimeSeries stageTimeSeries = new IrregularTimeSeries(unit);
        
        final double missingValue = -9999.0;
        FcstHeightTable table = new FcstHeightTable(_db);
        FcstHeightRecord record = null;
        List recordList = null;
        AbsTimeMeasurement measurement = null;
        
        final long maxLong = Long.MAX_VALUE;
    
    
        String whereClause = " WHERE lid = '" + locationId + "'" +
        " AND pe = '" + paramCode.getPe() + "'" +  
        " AND ts = '" + paramCode.getTypeSource() + "'" +  
        " AND dur = " +  paramCode.getIhfsDur() +
        " AND extremum = '" + paramCode.getExtremum() + "'" +  
        " AND quality_code >= " + _questionable_bad_threshold +
        " ORDER BY basistime DESC ";
     //   " AND obstime >= '" + startTimeString + "'";

         
        long originalBasisTime = 0;
        
        try
        {       
            recordList = table.select(whereClause);
            
            for (int i = 0; i < recordList.size(); i++)
            {
                
                
                record = (FcstHeightRecord) recordList.get(i);
                
                if (i == 0)
                {
                    originalBasisTime = record.getBasistime();   
                }
                else
                {
                    if (record.getBasistime() != originalBasisTime)
                    {
                        break;    
                    }
                }
                
                //don't let the missing values be included          
                if (record.getValue() != missingValue)
                {
                    measurement = new AbsTimeMeasurement(record.getValue(),
                                                     record.getValidtime(),
                                                     unit); 
                                                     
                    stageTimeSeries.insertMeasurement(measurement);
                    
                   // System.out.println(header + "measurement = " + measurement);
                    
                }   
            }
        }
        catch(SQLException e)
        {
            logSQLException(e);
        }
        
        return stageTimeSeries;
    }
    
//  -----------------------------------------------------------------------------------   
    
    public IrregularTimeSeries loadFcstDischargeTimeSeries(String locationId,
                                                       ParamCode paramCode)
    {
        MeasuringUnit unit = MeasuringUnit.feet;
        String header = "PdcTslDataManager.loadFcstDischargeTimeSeries(): ";
        IrregularTimeSeries dischargeTimeSeries = new IrregularTimeSeries(unit);
        
        final double missingValue = -9999.0;
        FcstDischargeTable table = new FcstDischargeTable(_db);
        FcstDischargeRecord record = null;
        List recordList = null;
        AbsTimeMeasurement measurement = null;
        
        final long maxLong = Long.MAX_VALUE;
    
    
        String whereClause = " WHERE lid = '" + locationId + "'" +
        " AND pe = '" + paramCode.getPe() + "'" +  
        " AND ts = '" + paramCode.getTypeSource() + "'" +  
        " AND dur = " +  paramCode.getIhfsDur() +
        " AND extremum = '" + paramCode.getExtremum() + "'" +  
        " AND quality_code >= " + _questionable_bad_threshold +
        " ORDER BY basistime DESC ";
     //   " AND obstime >= '" + startTimeString + "'";

         
        long originalBasisTime = 0;
        
        try
        {       
            recordList = table.select(whereClause);
            
            for (int i = 0; i < recordList.size(); i++)
            {
                
                
                record = (FcstDischargeRecord) recordList.get(i);
                
                if (i == 0)
                {
                    originalBasisTime = record.getBasistime();   
                }
                else
                {
                    if (record.getBasistime() != originalBasisTime)
                    {
                        break;    
                    }
                }
                
                //don't let the missing values be included          
                if (record.getValue() != missingValue)
                {
                    measurement = new AbsTimeMeasurement(record.getValue(),
                                                     record.getValidtime(),
                                                     unit); 
                                                     
                    dischargeTimeSeries.insertMeasurement(measurement);
                    
                   // System.out.println(header + "measurement = " + measurement);
                    
                }   
            }
        }
        catch(SQLException e)
        {
            logSQLException(e);
        }
        
        return dischargeTimeSeries;
    }
    
//  -----------------------------------------------------------------------------------   
    
    
    public Location getLocationInfo(String locationId)
    {
        DecimalFormat decimalFormat = new DecimalFormat( "0.00" );
  
        Location location = null;
        String whereClause = " WHERE lid = '" + locationId + "'";
        
        LocPDCView locPDCView = new LocPDCView( _db );
        RiverstatTable riverStatTable = new RiverstatTable( _db );
        StnClassTable stationClassTable = new StnClassTable(_db);
       
        
        List locPDCViewList = null;
        List riverStatRecordList = null;
        List stationClassList = null;
        
        
        try
        {
            locPDCViewList = locPDCView.select( whereClause );
            riverStatRecordList = riverStatTable.select( whereClause );
          
            stationClassList = stationClassTable.select( whereClause );
              
        }
        catch( SQLException e )
        {
            logSQLException(e);
            e.printStackTrace();
        }
        
      
        
        for ( int i = 0; i < locPDCViewList.size(); i++ )
        {
            LocPDCRecord locPDCRecord = (LocPDCRecord) locPDCViewList.get( i );
            location = new Location();
            location.setLid( locPDCRecord.getLid().trim() );
            location.setHsa( locPDCRecord.getHsa().trim() );
            
          
            location.setLat( Double.parseDouble( decimalFormat.format( locPDCRecord.getLat() ) ) );
            location.setLon( Double.parseDouble( decimalFormat.format( locPDCRecord.getLon() ) ) );
            
            
            location.setElevation( locPDCRecord.getElev() );
            location.setLocationName( locPDCRecord.getName() );
            location.setRiverStation( false );
            
            double fq = locPDCRecord.getFq();
            double fs = locPDCRecord.getFs();
            
            if ( fq == 0 ) // missing value
            {
                location.setFloodFlow( MISSING );
            }
            else
            {
                location.setFloodFlow( fq );
            }
            
            if ( fs == 0 ) // missing value
            {
                location.setFloodStage( MISSING );
            }
            else
            {
                location.setFloodStage( fs );
            }
            
            if ( ( fq == 0.0 ) && ( fs != 0.0 ) && (location.getRatingCurve() != null ) )
            {
                location.setFloodFlow( location.getRatingCurve().getDischargeFromStage( fs ) );
            }
            
            String dispClass = locPDCRecord.getDisp_class();
            
            if ( ( dispClass.indexOf( "F" ) ) != -1 )
            {
                location.setFcstPoint( true );
            }
            else
            {
                location.setFcstPoint( false );
            }
            
            String dcp = locPDCRecord.getIs_dcp();
            
            if ( ( dcp.indexOf( "T" ) ) != -1 )
            {
                location.setDCP( true );
            }
            else
            {
                location.setDCP( false );
            }
            
            String observer = locPDCRecord.getIs_observer();
            
            if ( ( observer.indexOf( "T" ) ) != -1 )
            {
                location.setObserver( true );
            }
            else
            {
                location.setObserver( false );
            }
            
            location.setTelemType( locPDCRecord.getTelem_type() );
        
            
        }
      
      
      
        return location;
    }
//  ------------------------------------------------------------------------
    private String getPrecipFileName(int durationInHours)
    {
        String precipFileName = _preprocessedFilePath + 
                                "/Precip" + durationInHours +"Hour.dat";
        return precipFileName;
    }
//  ------------------------------------------------------------------------

    public long getLatestHourTime()
    {
        return TimeHelper.truncateTimeInMillisToNearestHour(System.currentTimeMillis(), 1);
    }
//  -----------------------------------------------------------------------------------   
    
} //end PdcTslDataManager
