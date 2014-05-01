package ohd.hseb.raxdb_sync;

import java.sql.SQLException;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import ohd.hseb.db.Database;
import ohd.hseb.db.DbTable;
import ohd.hseb.ihfsdb.generated.AdjustFactorTable;
import ohd.hseb.ihfsdb.generated.CountiesTable;
import ohd.hseb.ihfsdb.generated.CrestTable;
import ohd.hseb.ihfsdb.generated.DataLimitsTable;
import ohd.hseb.ihfsdb.generated.FloodcatTable;
import ohd.hseb.ihfsdb.generated.IngestFilterTable;
import ohd.hseb.ihfsdb.generated.LocDataLimitsTable;
import ohd.hseb.ihfsdb.generated.LocationTable;
import ohd.hseb.ihfsdb.generated.RatingShiftTable;
import ohd.hseb.ihfsdb.generated.RatingTable;
import ohd.hseb.ihfsdb.generated.ReservoirTable;
import ohd.hseb.ihfsdb.generated.RiverstatTable;
import ohd.hseb.raxbase.db.CustomRaxRatingTable;
import ohd.hseb.raxdb.generated.RaxAdjustFactorTable;
import ohd.hseb.raxdb.generated.RaxCrestTable;
import ohd.hseb.raxdb.generated.RaxDataLimitsTable;
import ohd.hseb.raxdb.generated.RaxIngestFilterTable;
import ohd.hseb.raxdb.generated.RaxLocDataLimitsTable;
import ohd.hseb.raxdb.generated.RaxLocationTable;
import ohd.hseb.raxdb.generated.RaxRatingShiftTable;
import ohd.hseb.raxdb.generated.RaxRatingTable;
import ohd.hseb.raxdb.generated.RaxReservoirTable;
import ohd.hseb.raxdb.generated.RaxRiverCritTable;
import ohd.hseb.util.AppsDefaults;
import ohd.hseb.util.FileLogger;

public class RaxSyncDataMgr
{
    private Database _dbIhfs = null;
    private Database _dbRax = null;
    
    private AppsDefaults _ad = null;

    private FileLogger _loggerForApp = null;
    private FileLogger _loggerForUpdates = null;
    
    private static final String dateFormatString = "yyyyMMdd";
    private static final SimpleDateFormat _dateFormatter = new SimpleDateFormat(dateFormatString);
    
    private static final String dateTimeFormatString = "yyyy/MM/dd HH:mm:ss";
    private static final SimpleDateFormat _dateTimeFormatter = new SimpleDateFormat(dateTimeFormatString);
    
    private static final String latlonFormatString = ".0000";
    private static final DecimalFormat _latlonFormatter = new DecimalFormat(latlonFormatString);

    public RaxSyncDataMgr(String connectionStringIhfs, String connectionStringRax)
    {
        _ad = new AppsDefaults();
        
        setupMainLogFile("RaxSync.log");
        setupUpdateInsertLogFile();

        _dbIhfs = new Database(connectionStringIhfs);
        _dbRax = new Database(connectionStringRax);

    } // end of constructor which takes two connection strings
    
    public RaxSyncDataMgr(String connectionStringIhfs, Database raxDb)
    {
        _ad = new AppsDefaults();
        
        setupMainLogFile("RaxBaseSync.log");
        setupUpdateInsertLogFile();

        _dbIhfs = new Database(connectionStringIhfs);
        _dbRax = raxDb;

    } // end of constructor which takes a connection string and database object
    
    //---------------------------------------------------------------------
    public String formatLatLon(double latlon)
    {
        return _latlonFormatter.format(latlon); 
    } // end of method
    
    //---------------------------------------------------------------------
    public String getDateStamp()
    {
        Date date = new Date();
      
        return _dateFormatter.format(date); 
    } // end of method
    
    //---------------------------------------------------------------------
    public String getDateTimeStamp()
    {
        Date date = new Date();
      
        return _dateTimeFormatter.format(date); 
    } // end of method
    
    //---------------------------------------------------------------------

    public String getIhfsDbName()
    {
        String tokenValue = getRawTokenValue("db_name");
      
        return tokenValue; 
    } // end of method
    
    //---------------------------------------------------------------------
    public String getRaxDbName()
    {
        String tokenValue = getRawTokenValue("adb_name");
      
        return tokenValue; 
    } // end of method
    
    //---------------------------------------------------------------------
    public boolean inAnalysisOnlyMode()
    {
        boolean returnValue = true;

        String tokenValue = getRawTokenValue("adb_sync_mode");
           
        if (tokenValue.compareToIgnoreCase("UPDATE") == 0)
            returnValue = false;
                      
        return returnValue; 

    } // end of method
    
    //---------------------------------------------------------------------
    public String getTablenames()
    {
        String returnString = "NONE";

        String tokenValue = getRawTokenValue("adb_sync_tablenames");
           
        if (tokenValue.length() > 0)
           returnString = tokenValue;
                      
        return returnString; 

    } // end of method
    
    //---------------------------------------------------------------------
    public String getRiverCritPreference()
    {
        String returnString = "ACTION";

        String tokenValue = getRawTokenValue("adb_sync_rivercrit");
           
        if (tokenValue.compareToIgnoreCase("FIS") == 0)
            returnString = "FIS";
        else if (tokenValue.compareToIgnoreCase("BOTH") == 0)
            returnString = "BOTH";
                
        return returnString;
        
    } // end of method
    
    //---------------------------------------------------------------------
    public String getRatingCurveUnits()
    {
        String returnString = "ENGL";

        String tokenValue = getRawTokenValue("adb_sync_ihfs_units");
           
        if (tokenValue.compareToIgnoreCase("METR") ==0)
            returnString = "METR";
                            
        return returnString;
        
    } // end of method
    
    //---------------------------------------------------------------------
    public String getInterpolateType()
    {
        String returnString = "LIN";

        String tokenValue = getRawTokenValue("adb_sync_ihfs_interpolate");
           
        if (tokenValue.compareToIgnoreCase("LOG") == 0)
            returnString = "LOG";
                
        return returnString;
        
    } // end of method
    
    //---------------------------------------------------------------------
    public boolean useIngestColumnForComparison()
    {
        boolean returnValue = true;

        String tokenValue = getRawTokenValue("adb_sync_ihfs_ingest");
           
        if (tokenValue.compareToIgnoreCase("IGNORE") == 0)
            returnValue = false;

        return returnValue;
        
    } // end of method
    
    //---------------------------------------------------------------------
    public boolean isDebugOutputOn()
    {
        boolean returnValue = false;

        String tokenValue = getRawTokenValue("adb_sync_ihfs_debug");
           
        if (tokenValue.compareToIgnoreCase("ON") == 0)
            returnValue = true;
                
        return returnValue;
        
    } // end of method
    
    //---------------------------------------------------------------------
    public String getRawTokenValue(String tokenName)
    {
        String returnString = "";
        String tokenValue = null;

        tokenValue = _ad.getToken(tokenName);
           
        if (tokenValue != null)
            returnString = tokenValue;
                
        return returnString;
        
    } // end of method
    
    //---------------------------------------------------------------------
    private void setupMainLogFile(String filename)
    {
        String logDirectory = null;
        String applicationLogFile = null;

        logDirectory = getLogsDirectory();
        applicationLogFile = logDirectory + "/" + filename + "." + getDateStamp();
 
        _loggerForApp = new FileLogger(applicationLogFile, true, false);

    } // end of setupMainLogFile method

    //---------------------------------------------------------------------
    public FileLogger getMainAppLogger()
    {
        return _loggerForApp;
    } // end of method

    //---------------------------------------------------------------------
    private void setupUpdateInsertLogFile()
    {
        String updateInsertLogFile = null;

        updateInsertLogFile = getInsertUpdateLogFilename();

        _loggerForUpdates = new FileLogger(updateInsertLogFile, true, false);

    } // end of setupInsertupdateLogFile method

    //---------------------------------------------------------------------
    public String getInsertUpdateLogFilename()
    {
        String logDirectory = null;
        String filename = null;

        logDirectory = getLogsDirectory();
        filename = logDirectory + "/RaxDbMods.log." + getDateStamp();

        return filename;
    } // end of method

    //---------------------------------------------------------------------
    public FileLogger getInsertUpdateLogger()
    {
        return _loggerForUpdates;
    } // end of method

    //---------------------------------------------------------------------
    public String getLogsDirectory()
    {
        String logDirectory = null;
        String returnString = null;
        
        logDirectory = _ad.getToken("adb_sync_logs_dir");
        
        if (logDirectory != null)
            returnString = logDirectory;
        else
            returnString = "/rfc_arc/logs/dbsync";
                
        return returnString;
        
    } // end of method
        
    //---------------------------------------------------------------------
    public DiffSet findAllDifferences(String tablenames)
    {
        DiffSet allDiffSets = new DiffSet();
        List diffFinderList = getDifferenceFinderList(tablenames);
        
        for (int i=0; i < diffFinderList.size(); i++)
        {
            IDifferenceMgr diffFinder = (IDifferenceMgr) diffFinderList.get(i);
            DiffSet diffSet = diffFinder.findDifferences();
            allDiffSets.addDiffSet(diffSet);
            System.gc();         
        }
       
        return allDiffSets;
                
    } // end of findAllDifferences method

    //---------------------------------------------------------------------
    public List getDifferenceFinderList(String tablenames)
    {
        List list = new ArrayList();
        
        String splitTablenames[] = tablenames.split("\\s+");
        
        Set nameSet = new HashSet();
        for (int i=0; i < splitTablenames.length; i++)
        {
            nameSet.add(splitTablenames[i].toUpperCase());
        }
        
        if (nameSet.contains("LOCATION") || nameSet.contains("ALL"))
            list.add(new LocationDifferenceMgr(this));
        if (nameSet.contains("INGESTFILTER") || nameSet.contains("ALL"))
            list.add(new IngestFilterDifferenceMgr(this));
        if (nameSet.contains("CREST") || nameSet.contains("ALL"))
            list.add(new CrestDifferenceMgr(this));
        if (nameSet.contains("RESERVOIR") || nameSet.contains("ALL"))
            list.add(new ReservoirDifferenceMgr(this));
        if (nameSet.contains("DATALIMITS") || nameSet.contains("ALL"))
            list.add(new DataLimitsDifferenceMgr(this));
        if (nameSet.contains("LOCDATALIMITS") || nameSet.contains("ALL"))
            list.add(new LocDataLimitsDifferenceMgr(this));
        if (nameSet.contains("ADJUSTFACTOR") || nameSet.contains("ALL"))
            list.add(new AdjustFactorDifferenceMgr(this));
        if (nameSet.contains("RIVERCRIT") || nameSet.contains("ALL"))
            list.add(new RiverCritDifferenceMgr(this));
        if (nameSet.contains("RATING") || nameSet.contains("ALL"))
            list.add(new RatingDifferenceMgr(this));
        
        return list;
        
    } // end of getDifferenceFinderList method

    //---------------------------------------------------------------------
    public void reportAllDifferences(DiffSet setOfAllDifferences)
    {
        RecordDifference oneRecordDifference = new RecordDifference();
        
        for (int i=0; i<setOfAllDifferences.getList().size(); i++)
        {
            oneRecordDifference = (RecordDifference) setOfAllDifferences.getList().get(i);
            oneRecordDifference.getDifferenceMgr().reportDifferences(oneRecordDifference);
        } // for AllDifferences

    } // end of reportAllDifferences method

    //---------------------------------------------------------------------
    public int proccessAllDifferences(DiffSet setOfAllDifferences, boolean batchProcessing)
    {
        int numberOfFailedUpdates = 0;
        int totalNumberOfFailedUpdates = 0;
        RecordDifference oneRecordDifference = new RecordDifference();
        String message = null;
        
        if (batchProcessing == true)
            message = "\nThe Batch Synchronization application executed the following transaction(s) on the RAX database:";            
        else
            message = "\nThe RaxBase application executed the following transaction(s) on the RAX database:";
        
        getInsertUpdateLogger().log(message);
        for (int i=0; i<setOfAllDifferences.getList().size(); i++)
        {
            oneRecordDifference = (RecordDifference) setOfAllDifferences.getList().get(i);
            numberOfFailedUpdates = oneRecordDifference.getDifferenceMgr().processDifferences(oneRecordDifference);
            totalNumberOfFailedUpdates += numberOfFailedUpdates;
        } // for AllDifferences
        
        return totalNumberOfFailedUpdates;

    } // end of processAllDifferences method

    //---------------------------------------------------------------------
    public List getIhfsLocationRecordList()
    {
        String whereClause = "ORDER BY lid";
        return getRecordList(new LocationTable(_dbIhfs), whereClause);
    }

    //---------------------------------------------------------------------
    public List getRaxLocationRecordList()
    {
        String whereClause = "WHERE sed IS NULL ORDER BY lid, sbd";
        return getRecordList(new RaxLocationTable(_dbRax), whereClause);
    }

    //---------------------------------------------------------------------
    public List getIhfsCountiesRecordList()
    {
        String whereClause = "ORDER BY county";
        return getRecordList(new CountiesTable(_dbIhfs), whereClause);
    }

    //---------------------------------------------------------------------
    public List getIhfsIngestFilterRecordList(String first, String last)
    {
        String whereClause = "WHERE lid >= '"+ first + "' AND lid < '" + last + "' ORDER BY lid";
        return getRecordList(new IngestFilterTable(_dbIhfs), whereClause);
    }

    //---------------------------------------------------------------------
    public List getRaxIngestFilterRecordList(String first, String last)
    {
        String whereClause = "WHERE lid >= '"+ first + "' AND lid < '" + last + "' ORDER BY lid";
        return getRecordList(new RaxIngestFilterTable(_dbRax), whereClause);
    }

    //---------------------------------------------------------------------
    public List getIhfsCrestRecordList()
    {
        String whereClause = "ORDER BY lid, datcrst, stage DESC";
        return getRecordList(new CrestTable(_dbIhfs), whereClause);
    }

    //---------------------------------------------------------------------
    public List getRaxCrestRecordList()
    {
        String whereClause = "ORDER BY lid";
        return getRecordList(new RaxCrestTable(_dbRax), whereClause);
    }

    //---------------------------------------------------------------------
    public List getIhfsReservoirRecordList()
    {
        String whereClause = "ORDER BY lid";
        return getRecordList(new ReservoirTable(_dbIhfs), whereClause);
    }

    //---------------------------------------------------------------------
    public List getRaxReservoirRecordList()
    {
        String whereClause = "WHERE sed IS NULL ORDER BY lid, sbd DESC";
        return getRecordList(new RaxReservoirTable(_dbRax), whereClause);
    }

    //---------------------------------------------------------------------
    public List getIhfsDataLimitsRecordList()
    {
        String whereClause = "ORDER BY pe";
        return getRecordList(new DataLimitsTable(_dbIhfs), whereClause);
    }

    //---------------------------------------------------------------------
    public List getRaxDataLimitsRecordList()
    {
        String whereClause = "ORDER BY pe1, pe2";
        return getRecordList(new RaxDataLimitsTable(_dbRax), whereClause);
    }

    //---------------------------------------------------------------------
    public List getIhfsLocDataLimitsRecordList()
    {
        String whereClause = "ORDER BY lid, pe";
        return getRecordList(new LocDataLimitsTable(_dbIhfs), whereClause);
    }

    //---------------------------------------------------------------------
    public List getRaxLocDataLimitsRecordList()
    {
        String whereClause = "ORDER BY lid, pe1, pe2";
        return getRecordList(new RaxLocDataLimitsTable(_dbRax), whereClause);
    }

    //---------------------------------------------------------------------
    public List getIhfsAdjustFactorRecordList()
    {
        String whereClause = "ORDER BY lid, pe";
        return getRecordList(new AdjustFactorTable(_dbIhfs), whereClause);
    }

    //---------------------------------------------------------------------
    public List getRaxAdjustFactorRecordList()
    {
        String whereClause = "ORDER BY lid, pe1, pe2, begin_date";
        return getRecordList(new RaxAdjustFactorTable(_dbRax), whereClause);
    }

    //---------------------------------------------------------------------
    public List getIhfsRiverstatRecordList()
    {
        String whereClause = "ORDER BY lid";
        return getRecordList(new RiverstatTable(_dbIhfs), whereClause);
    }

    //---------------------------------------------------------------------
    public List getIhfsFloodcatRecordList()
    {
        String whereClause = "ORDER BY lid";
        return getRecordList(new FloodcatTable(_dbIhfs), whereClause);
    }

    //---------------------------------------------------------------------
    public List getRaxRiverCritRecordList()
    {
        String whereClause = "ORDER BY lid, vdtime";
        return getRecordList(new RaxRiverCritTable(_dbRax), whereClause);
    }

    //---------------------------------------------------------------------
    public List getIhfsRatingRecordList()
    {
        String whereClause = "ORDER BY lid, stage";
        return getRecordList(new RatingTable(_dbIhfs), whereClause);
    }

    //---------------------------------------------------------------------
    public List getRaxRatingRecordList()
    {
        String whereClause = "WHERE src='IHFS' ORDER BY lid, valid_date";
        return getRecordList(new RaxRatingTable(_dbRax), whereClause);
    }

    //---------------------------------------------------------------------
    public List getIhfsRatingShiftRecordList()
    {
        String whereClause = "WHERE active='T' ORDER BY lid, date";
        return getRecordList(new RatingShiftTable(_dbIhfs), whereClause);
    }

    //---------------------------------------------------------------------
    public List getRaxRatingShiftRecordList()
    {
        String whereClause = "WHERE src='IHFS' ORDER BY lid, begin_date";
        return getRecordList(new RaxRatingShiftTable(_dbRax), whereClause);
    }

    //---------------------------------------------------------------------
    private List getRecordList(DbTable table, String where)
    {
        List recordList = null;
        
        try
        {
            recordList = table.select(where);
        }
        catch (SQLException e)
        {
            e.printStackTrace();
        }
        
        return recordList;
        
    } // end of getRecordList method

    //---------------------------------------------------------------------
    public RaxLocationTable getRaxLocationTable()
    {
        return new RaxLocationTable(_dbRax);
    }

    //---------------------------------------------------------------------
    public RaxIngestFilterTable getRaxIngestFilterTable()
    {
        return new RaxIngestFilterTable(_dbRax);
    }

    //---------------------------------------------------------------------
    public RaxCrestTable getRaxCrestTable()
    {
        return new RaxCrestTable(_dbRax);
    }

    //---------------------------------------------------------------------
    public RaxReservoirTable getRaxReservoirTable()
    {
        return new RaxReservoirTable(_dbRax);
    }

    //---------------------------------------------------------------------
    public RaxDataLimitsTable getRaxDataLimitsTable()
    {
        return new RaxDataLimitsTable(_dbRax);
    }

    //---------------------------------------------------------------------
    public RaxLocDataLimitsTable getRaxLocDataLimitsTable()
    {
        return new RaxLocDataLimitsTable(_dbRax);
    }

    //---------------------------------------------------------------------
    public RaxAdjustFactorTable getRaxAdjustFactorTable()
    {
        return new RaxAdjustFactorTable(_dbRax);
    }

    //---------------------------------------------------------------------
    public RaxRiverCritTable getRaxRiverCritTable()
    {
        return new RaxRiverCritTable(_dbRax);
    }

    //---------------------------------------------------------------------
    public RaxRatingTable getRaxRatingTable()
    {
        return new CustomRaxRatingTable(_dbRax);
    }

    //---------------------------------------------------------------------
    public RaxRatingShiftTable getRaxRatingShiftTable()
    {
        return new RaxRatingShiftTable(_dbRax);
    }

} // end of RaxSyncDataMgr class
