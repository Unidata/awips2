package ohd.hseb.fp_vtec_info;

import java.sql.SQLException;


import java.util.List;

import ohd.hseb.db.Database;
import ohd.hseb.db.DbTable;
import ohd.hseb.ihfsdb.generated.FcstHeightTable;
import ohd.hseb.ihfsdb.generated.FloodcatRecord;
import ohd.hseb.ihfsdb.generated.FloodcatTable;
import ohd.hseb.ihfsdb.generated.IngestFilterRecord;
import ohd.hseb.ihfsdb.generated.IngestFilterTable;
import ohd.hseb.ihfsdb.generated.RatingRecord;
import ohd.hseb.ihfsdb.generated.RatingShiftRecord;
import ohd.hseb.ihfsdb.generated.RatingShiftTable;
import ohd.hseb.ihfsdb.generated.RatingTable;
import ohd.hseb.ihfsdb.generated.RiverstatRecord;
import ohd.hseb.ihfsdb.generated.RiverstatTable;
import ohd.hseb.measurement.AbsTimeMeasurement;
import ohd.hseb.measurement.IrregularTimeSeries;
import ohd.hseb.measurement.MeasuringUnit;
import ohd.hseb.model.RatingCurve;
import ohd.hseb.model.RatingPoint;
import ohd.hseb.model.SigRiverLevels;
import ohd.hseb.timeserieslite.ParamCode;
import ohd.hseb.util.AppsDefaults;
import ohd.hseb.util.FileLogger;
import ohd.hseb.util.Logger;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;

public class FpVtecInfoDataManager
{
              
    private static final long MILLIS_PER_SECOND = 1000;
        
    private Database _db = null;
    private Logger _logger = null;
    
    private String _jdbcConnectionString = null;      
    
    private String _rpfdataFilePath = null;
    private Map    _vtecinfoMap = new HashMap();
   
    // ---------------------------------------------------------------------------------
  
    public FpVtecInfoDataManager(String jdbcConnectionString)
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
     
        _rpfdataFilePath = ad.getToken( "rpf_template_dir" ) + "/";
	
   	    return;
    }
    //----------------------------------------------------------------------------------
                      
    public void readInputFileFormFpCurPrevVtecInfoMap()
    {         
	    //String header = "FpVtecInfoDataManager.readInputFileFormCurPrevVtecInfoMap(): ";
        String fpvtecinfoFileName = getFpVtecInfoFileName();
        FpVtecInfoFileReader reader = new FpVtecInfoFileReader(fpvtecinfoFileName);
                
        _vtecinfoMap = reader.read();
        
        return;
    } 
          
//  ------------------------------------------------------------------------
    private String getFpVtecInfoFileName()
    {
        String fpvtecinfoFileName = _rpfdataFilePath + 
                                "fpvtecinfo.dat";
        System.out.println("FpVtecInfoDataManager: getFpVtecInfoFileName()" + 
        		                 fpvtecinfoFileName);
        
        return fpvtecinfoFileName;
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

    //  ------------------------------------------------------------------------
    
    public IrregularTimeSeries loadObservedStageTimeSeries(String locationId, 
                                                           ParamCode  paramCode,
                                                           long startTime,
                                                           FpCurPrevVtec fpCurPrevVtecRecord)
    {
    	final double missingValue = -9999.0;
    	String header = "FpVtecInfoDataManager.loadObservedStageTimeSeries(): ";
    	
    	MeasuringUnit unit = MeasuringUnit.feet;        
        IrregularTimeSeries stageTimeSeries = new IrregularTimeSeries(unit);
        AbsTimeMeasurement measurement = null;
        ArrayList obsValueTimeList = new ArrayList();
        String obsValueTimeRecord = null;
        String [] temp = null;
        double obsValue = missingValue;
        long   obsTime = 0;
             
        
        try
        {       
            obsValueTimeList = fpCurPrevVtecRecord.getObsValuetTimeList();
            
            if (obsValueTimeList != null)
            {	
	            for (int i = 0; i < obsValueTimeList.size(); i++)
	            {
	                obsValueTimeRecord = obsValueTimeList.get(i).toString();
	                              
	                //don't let the missing values be included          
	                
	                temp = obsValueTimeRecord.split(",");
	                obsValue = Double.parseDouble(temp[0]);
	                obsTime = (Long.parseLong(temp[1])) * MILLIS_PER_SECOND;
	                
	                measurement = new AbsTimeMeasurement(obsValue,
	                                                     obsTime,
	                                                     unit); 
	                                                     
	                stageTimeSeries.insertMeasurement(measurement);
	                
	            }
            }
        }
        
        catch(Exception e)
        {
            System.err.println(header + "error loading observed data from obsValueTimeList");
            e.printStackTrace();
        }
        
        return stageTimeSeries;
    
    }
    
    //  ------------------------------------------------------------------------

    public IrregularTimeSeries loadObservedDischargeTimeSeries(String locationId, 
                                                           ParamCode  paramCode,
                                                           long startTime,
                                                           FpCurPrevVtec fpCurPrevVtecRecord)
    {
    	final double missingValue = -9999.0;
    	
        MeasuringUnit unit = MeasuringUnit.cfs;
        String header = "FpVtecInfoDataManager.loadObservedDischargeTimeSeries(): ";
        
        AbsTimeMeasurement measurement = null;
        ArrayList obsValueTimeList = new ArrayList();
        String obsValueTimeRecord = null;
        String [] temp = null;
        double obsValue = missingValue;
        long   obsTime = 0;
        
        IrregularTimeSeries dischargeTimeSeries = new IrregularTimeSeries(unit);
                
        try
        {       
            obsValueTimeList = fpCurPrevVtecRecord.getObsValuetTimeList();
            
            if (obsValueTimeList != null)
            {	
	            for (int i = 0; i < obsValueTimeList.size(); i++)
	            {
	                obsValueTimeRecord = obsValueTimeList.get(i).toString();
	                              	               	                
	                temp = obsValueTimeRecord.split(",");
	                obsValue = Double.parseDouble(temp[0]);
	                obsTime = (Long.parseLong(temp[1])) * MILLIS_PER_SECOND;
	                
	                measurement = new AbsTimeMeasurement(obsValue,
	                                                     obsTime,
	                                                     unit); 
	                                                     
	                dischargeTimeSeries.insertMeasurement(measurement);
	                
	            }
            }
        }
        
        catch(Exception e)
        {
            System.err.println(header + "error loading observed data from obsValueTimeList");
            e.printStackTrace();
        }
        
                
        return dischargeTimeSeries;
    
    }
    
    //  ------------------------------------------------------------------------
      
    public IrregularTimeSeries loadFcstStageTimeSeries(String locationId,
                                                       ParamCode paramCode,
						                               long      endTime,
						                               FpCurPrevVtec fpCurPrevVtecRecord)
    {
    	long originalBasisTime = 0;
    	final double missingValue = -9999.0;
    	
        MeasuringUnit unit = MeasuringUnit.feet;
        AbsTimeMeasurement measurement = null;
        String header = "FpVtecInfoDataManager.loadFcstStageTimeSeries(): ";        
        ArrayList fcstValueTimeList = new ArrayList();
        String fcstValueTimeRecord = null;
        String [] temp = null;
        double fcstValue = missingValue;
        long   fcstTime = 0;
        
        IrregularTimeSeries stageTimeSeries = new IrregularTimeSeries(unit);
        
               
        try
        {       
            fcstValueTimeList = fpCurPrevVtecRecord.getFcstValueTimeList();            
            
            if (fcstValueTimeList != null)
            {	
	            for (int i = 0; i < fcstValueTimeList.size(); i++)
	            {
	                fcstValueTimeRecord = fcstValueTimeList.get(i).toString();
	                
	                temp = fcstValueTimeRecord.split(",");
	                fcstValue = Double.parseDouble(temp[0]);
	                fcstTime = (Long.parseLong(temp[1])) * MILLIS_PER_SECOND;
	                
	                measurement = new AbsTimeMeasurement(fcstValue,
	                                                     fcstTime,
	                                                     unit); 
	                                                     
	                stageTimeSeries.insertMeasurement(measurement);
	                
	            }
            }
        }
        
        catch(Exception e)
        {
            System.err.println(header + "error loading forecast data from fcstValueTimeList");
            e.printStackTrace();
        }
        
        
        return stageTimeSeries;
    }
    
//  -----------------------------------------------------------------------------------   
    
    public IrregularTimeSeries loadFcstDischargeTimeSeries(String locationId,
                                                           ParamCode paramCode,
						                                   long      endTime,
						                                   FpCurPrevVtec fpCurPrevVtecRecord)
    {
    	long originalBasisTime = 0;
    	final double missingValue = -9999.0;
    	
        MeasuringUnit unit = MeasuringUnit.cfs;
        String header = "FpVtecInfoDataManager.loadFcstDischargeTimeSeries(): ";
       
        ArrayList fcstValueTimeList = new ArrayList();
        String fcstValueTimeRecord = null;
        String [] temp = null;
        double fcstValue = missingValue;
        long   fcstTime = 0;
        AbsTimeMeasurement measurement = null;
        IrregularTimeSeries dischargeTimeSeries = new IrregularTimeSeries(unit);
                    
       
        try
        {       
            fcstValueTimeList = fpCurPrevVtecRecord.getFcstValueTimeList();            
            
            if (fcstValueTimeList != null)
            {	
	            for (int i = 0; i < fcstValueTimeList.size(); i++)
	            {
	                fcstValueTimeRecord = fcstValueTimeList.get(i).toString();
	                
	                temp = fcstValueTimeRecord.split(",");
	                fcstValue = Double.parseDouble(temp[0]);
	                fcstTime = (Long.parseLong(temp[1])) * MILLIS_PER_SECOND;
	                
	                measurement = new AbsTimeMeasurement(fcstValue,
	                                                     fcstTime,
	                                                     unit); 
	                                                     
	                dischargeTimeSeries.insertMeasurement(measurement);
	                
	            }
            }
        }
        
        catch(Exception e)
        {
            System.err.println(header + "error loading forecast data from fcstValueTimeList");
            e.printStackTrace();
        }
        
        return dischargeTimeSeries;
    }
    
       
    // ------------------------------------------------------------------------------------
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
  
    // ------------------------------------------------------------------------------
    
    
    public SigRiverLevels loadSigRiverLevels(String locationId)
    {
        String header = "FpVtecInfoDataManager.loadSigRiverLevels(): ";
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
                
                double floodStage = record.getFs();
                double actionStage = record.getWstg();
                double floodFlow = record.getFq();
                double actionFlow = record.getAction_flow();
                // need to change the common lib SigRiverLevels.java
                                
                
                if (DbTable.isNull(actionStage))
                {
                     System.out.println(header + " actionStage is null for " + locationId);                
                }
                if (DbTable.isNull(record.getPrimary_pe()))
                {
                    System.out.println(header + " primary_pe is null for " + locationId);
                    record.setPrimary_pe("HG");
                }                                
                
                levels = new SigRiverLevels(locationId, 
                            record.getPrimary_pe(),
                            floodStage, 
                            actionStage,
                            floodFlow,
                            actionFlow);
                // load moderate and major floods
                
                loadFloodCategories(levels);
            }   
        }
        catch(SQLException e)
        {
             logSQLException(e);
        }
        
        return levels;
    }
    
    // --------------------------------------------------------------------------------------------------
    private void loadFloodCategories(SigRiverLevels levels)
    {
        FloodcatTable table = new FloodcatTable(_db);
        FloodcatRecord record = null;
          
        String whereClause = "WHERE lid = '" + levels.getLocationId() + "'";
    
        try
        {
            List recordList = table.select(whereClause);
            if (recordList.size() > 0)
            {
                record = (FloodcatRecord) recordList.get(0);
                
                levels.setMinorFloodFlow(record.getMinor_flow());
                levels.setModerateFloodFlow(record.getModerate_flow());
                levels.setMajorFloodFlow(record.getMajor_flow());
                
                levels.setMinorFloodStage(record.getMinor_stage());
                levels.setModerateFloodStage(record.getModerate_stage());
                levels.setMajorFloodStage(record.getMajor_stage());
            }   
        }
        catch(SQLException e)
        {
             logSQLException(e);
        }
        
        return;
    }
    //  -------------------------------------------------------------------------------------
    public String getPreferredFcstTypeSource(String lid,
                                                  String pe)
    {
        String header = "FpVtecInfoDataManager.getPreferredFcstTypeSource(): ";
        
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
    
    // --------------------------------------------------------------------------
    
    //  ----------------------------------------------------------------------------------
    public boolean fcstHeightRecordExists(IngestFilterRecord ingestRecord)
    {
         boolean result = false;
         
         String header = "FpVtecInfoDataManager.fcstHeightRecordExists(): ";
        
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
    
    public  Map getvtecinfoMap() 
	{
		return _vtecinfoMap;
	}

	public String get_rpfdataFilePath() {
		return _rpfdataFilePath;
	}

	public void set_rpfdataFilePath(String filePath) {
		_rpfdataFilePath = filePath;
	}

	public Database get_db() {
		return _db;
	}

	public void set_db(Database _db) {
		this._db = _db;
	}
	
} //end FpVtecInfoDataManager
