/*
 * Created on September 10, 2005
 *
 * 
 */
package ohd.hseb.pdc_pp;



import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.sql.SQLException;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import ohd.hseb.db.Database;
import ohd.hseb.db.DbTable;
import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.ihfsdb.generated.LocPDCRecord;
import ohd.hseb.ihfsdb.generated.LocPDCView;
import ohd.hseb.ihfsdb.generated.RatingRecord;
import ohd.hseb.ihfsdb.generated.RatingShiftRecord;
import ohd.hseb.ihfsdb.generated.RatingShiftTable;
import ohd.hseb.ihfsdb.generated.RatingTable;
import ohd.hseb.ihfsdb.generated.RiverstatRecord;
import ohd.hseb.ihfsdb.generated.RiverstatTable;
import ohd.hseb.ihfsdb.generated.ShefDurRecord;
import ohd.hseb.ihfsdb.generated.ShefDurTable;
import ohd.hseb.ihfsdb.generated.StnClassRecord;
import ohd.hseb.ihfsdb.generated.StnClassTable;
import ohd.hseb.model.RatingCurve;
import ohd.hseb.model.RatingPoint;
import ohd.hseb.pdc_pp.Location;
import ohd.hseb.util.AppsDefaults;
import ohd.hseb.util.CodeTimer;
import ohd.hseb.util.EnvHelper;
import ohd.hseb.util.IntegerHolder;
import ohd.hseb.util.Logger;
import ohd.hseb.util.MathHelper;
import ohd.hseb.util.StringParser;
import ohd.hseb.util.TimeHelper;

public class PDCPreprocessorDataMgr
{
    public static final int MISSING = -9999;
   // private static final double MISSING_DOUBLE = MISSING;
    private static String OS = System.getProperty("os.name").toLowerCase();
    
    
    public static final long MILLIS_PER_HALF_HOUR = 30 * 60 * 1000;
    public static final long MILLIS_PER_HOUR = 60 * 60 * 1000;
    public static final long MILLIS_PER_DAY = MILLIS_PER_HOUR * 24;
    public static final long MILLIS_PER_12_HOURS = MILLIS_PER_HOUR * 12;
    public static final long MILLIS_PER_MINUTE = 60 * 1000;
    public static long _minutesBeforeTopOfHour;
    public static long _minutesAfterTopOfHour;
    public static boolean _DEBUG = false;

    
//  Variables used for the preprocessed file
    private String _preprocessedFilePath = null;
  //  private long _endTime = System.currentTimeMillis(); 
    
    private Database _db = null;
    private Logger _logger = null;
    private Map _locationHashMap = null;
    private Map _shefDurMap = null;
    private Map _shefDurToDurcodeMap = null;
       
      
    private int _nonPrecipHoursToReadWhenCaching = 24;
    private int _precipHoursToReadWhenCaching = 48; //currently, must be a multiple of 24 hours
    private long _cacheRefreshAgeMaxInMillis = 60 * 60 * 1000;
    
  //  private String _dataDir = null;
    private String _refreshIndicatorBaseFileName = "CacheRefreshIndicator.dat";
    
    
    
    
    
// ------------------------------------------------------------------------	
    
    public PDCPreprocessorDataMgr( String baseConnectionString, 
                                   Logger logger, 
                                   String preprocessedFilePath, 
                                   long minutesBeforeTopOfHour, 
                                   long minutesAfterTopOfHour, 
                                   boolean debug )
    {		
        super();
        
    	_db = new Database();
        _logger = logger;
        _DEBUG = debug;
        _preprocessedFilePath = preprocessedFilePath;
        _minutesBeforeTopOfHour = minutesBeforeTopOfHour;
        _minutesAfterTopOfHour = minutesAfterTopOfHour;

        getAppsDefaults();
        
        if ( baseConnectionString == null )
        {
            baseConnectionString = getBaseConnectionString();  
        }
    	_db.connectWithDriverSearch( baseConnectionString );	
    }
    
    // ------------------------------------------------------------------------------------
    
    private void getAppsDefaults()
    {
        AppsDefaults ad = new AppsDefaults();
      
        _preprocessedFilePath = ad.getToken( "pdc_pp_dir" ) + "/";
        
        int cacheRefreshAgeInMinutes = ad.getInt("pdc_clean_cache_minutes", 59);
        _cacheRefreshAgeMaxInMillis = cacheRefreshAgeInMinutes * MILLIS_PER_MINUTE; 
        
        System.out.println("_cacheRefreshAgeMax = " + _cacheRefreshAgeMaxInMillis);
        
        return;
    }
// ------------------------------------------------------------------------
  
    public String getDatabaseName()
    {
            String dbName = _db.getDatabaseName();
            return dbName;
    }
	
// ------------------------------------------------------------------------
  	
  	private String getBaseConnectionString()
	{
  	    //this is used in testing mode, not the production mode
		String connectionURLPart = "jdbc:postgresql://dx1-nhdr:5432/hd_ob6ounx?user=oper";
		return connectionURLPart;
	}

// ------------------------------------------------------------------------

/**
* createLidMap() - Creates a Hashmap of Location objects.  Used for the RegularObsTimeSeriesDescriptor objects
*/
  	public void createLidMap()
  	{
  	    DecimalFormat decimalFormat = new DecimalFormat( "0.00" );
  	    
  	    CodeTimer lidHashMapTimer = new CodeTimer();
  	    CodeTimer locPDCViewTimer = new CodeTimer();
  	    CodeTimer riverStatTableTimer = new CodeTimer();
  	    CodeTimer putHashMapTimer = new CodeTimer();
  	    CodeTimer parseTimer = new CodeTimer();
  	    CodeTimer ratingCurveTimer = new CodeTimer();
  	    
  	    Location location = null;
  	    String whereClause = "";
  	    
  	    LocPDCView locPDCView = new LocPDCView( _db );
  	    RiverstatTable riverStatTable = new RiverstatTable( _db );
        StnClassTable stationClassTable = new StnClassTable(_db);
  	   
        
  	    List locPDCViewList = null;
  	    List riverStatRecordList = null;
        List stationClassList = null;
  	    
  	    if ( _DEBUG )
  	    {
  	        lidHashMapTimer.start();
  	    }
  	    
  	    try
  	    {
  	        if ( _DEBUG )
  	        {
  	            locPDCViewTimer.start();
  	        }
  	        locPDCViewList = locPDCView.select( whereClause );
  	        if ( _DEBUG )
  	        {
  	            locPDCViewTimer.stop( "Finished LocPDCView query in " );
  	        }
  	        
  	        if ( _DEBUG )
  	        {
  	            riverStatTableTimer.start();
  	        }
  	        riverStatRecordList = riverStatTable.select( whereClause );
  	        if ( _DEBUG )
  	        {
  	            riverStatTableTimer.stop( "Finished RiverStatTable query in " );
  	        }
            
  	    
            stationClassList = stationClassTable.select( whereClause );
              
  	    }
  	    catch( SQLException e )
  	    {
  	        logSQLException(e);
  	        e.printStackTrace();
  	    }
  	    
  	    if ( _locationHashMap == null )
  	    {
  	        _locationHashMap = new HashMap();
  	    }
  	    if ( _DEBUG )
  	    {
  	        System.out.println( "Size of recordslist = " + locPDCViewList.size() );
  	        System.out.flush();
  	    }
  	    
  	    for ( int i = 0; i < locPDCViewList.size(); i++ )
  	    {
  	        LocPDCRecord locPDCRecord = (LocPDCRecord) locPDCViewList.get( i );
  	        location = new Location();
  	        location.setLid( locPDCRecord.getLid().trim() );
  	        location.setHsa( locPDCRecord.getHsa().trim() );
  	        
  	        if ( _DEBUG )
  	        {
  	            parseTimer.restart();
  	        }
            
            double formattedLat = getFormattedDouble(locPDCRecord.getLat(), 2, DbTable.getNullDouble(), 0.0);
            location.setLat(formattedLat);
            
            double formattedLon =  getFormattedDouble(locPDCRecord.getLon(), 2, DbTable.getNullDouble(), 0.0);
            location.setLon(formattedLon);
            
            if ( _DEBUG )
  	        {
  	            parseTimer.stop();
  	        }
  	        
  	        location.setElevation( locPDCRecord.getElev() );
  	        location.setLocationName( locPDCRecord.getName() );
  	        location.setRiverStation( false );
  	        
  	        double fq = locPDCRecord.getFq();
  	        double fs = locPDCRecord.getFs();
  	        
            if (locPDCRecord.getLid().equals("ESFO2"))
            {
                System.out.println("fs = " + fs);
            }
            
  	        if ( ( fq == 0 ) || (DbTable.isNull(fq)) )// missing value
  	        {
  	            location.setFloodFlow( MISSING );
  	        }
  	        else
  	        {
  	            location.setFloodFlow( fq );
  	        }
  	        
  	        if (( fs == 0 ) || (DbTable.isNull(fs)))// missing value
  	        {
  	            location.setFloodStage( MISSING );
  	        }
  	        else
  	        {
  	            location.setFloodStage( fs );
  	        }
  	        
  	        if ( ( fq == MISSING ) && ( fs != MISSING ) && (location.getRatingCurve() != null ) )
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
  	        if ( _DEBUG )
  	        {
  	            putHashMapTimer.restart();
  	        }
  	        _locationHashMap.put( location.getLid(), location );
  	        if ( _DEBUG )
  	        {
  	            putHashMapTimer.stop();
  	        }
  	    }
  	    
  	   /*
         for ( int i = 0; i < riverStatRecordList.size(); i++ )
  	    {
  	        RiverstatRecord riverStatRecord = (RiverstatRecord) riverStatRecordList.get( i );
  	        
  	        Location selectedLoc = (Location) _locationHashMap.get( riverStatRecord.getLid() );
  	        
  	        if ( ( selectedLoc != null ) && ( riverStatRecord != null ) )
  	        {
  	            selectedLoc.setRiverStation( true );
  	        }
        }
           */
        
        
        //For each stationClassRecord, match it up to a location record.
        // if there is a match, set the river station indicator to true if
        // stnClassRecord's display class contains F, D, or R.
        
        for ( int i = 0; i < stationClassList.size(); i++ )
        {
            StnClassRecord stnClassRecord = (StnClassRecord) stationClassList.get( i );
            
            Location selectedLoc = (Location) _locationHashMap.get( stnClassRecord.getLid() );
            
            
            if ( ( selectedLoc != null ) && ( stnClassRecord != null ) )
            {
                String displayClassString = stnClassRecord.getDisp_class();
                if ( (displayClassString.contains("R")) ||
                     (displayClassString.contains("D")) ||
                     (displayClassString.contains("F"))
                   )
                   {
                       selectedLoc.setRiverStation( true );
                   }
                
            }
         }
  	    
  	    if ( _DEBUG )
  	    {
  	        ratingCurveTimer.restart();
  	    }
  	    
  	    loadRatingCurves();
  	    if ( _DEBUG )
  	    {
  	        ratingCurveTimer.stop();
  	    }
  	    
  	    if ( _DEBUG )
  	    {
  	        System.out.println( "push HashMap finished in " + putHashMapTimer.getElapsedTime() + " millis" );
  	        System.out.println( "parseTimer finished in " + parseTimer.getElapsedTime() + " millis" );
  	        System.out.println( "ratingCurveTimer finished in " + ratingCurveTimer.getElapsedTime() + " millis" );
  	        lidHashMapTimer.stop( "Finished creating Lid HashMap in " );
  	    }
  	}

    //--------------------------------------------------------------------------------
    
    private double  getFormattedDouble(double originalValue, int decimalPlacesToMaintain,
                                       double missingValue, double missingValueReplacement)
    {
        double value = missingValueReplacement;
        
        if (originalValue == missingValue)
        {
            originalValue = missingValueReplacement;
        }
        else
        {
            value = MathHelper.roundToNDecimalPlaces(originalValue, decimalPlacesToMaintain);
        }
        
        return value;
    }
    
    // ------------------------------------------------------------------------

    public Set getCompleteLidSet()
    {
        Set completeLidSet = _locationHashMap.keySet();

        return completeLidSet;
    }
    
    public Set getRiverStationLidSet()
    {
        List locationKeyList = new ArrayList(_locationHashMap.keySet());
        
        Set completeRiverStationLidSet = new HashSet();
        
        //find all river stations and add their lids to the lid set.
        for (int i = 0; i < locationKeyList.size(); i++)
        {
            String lid = (String) locationKeyList.get(i);
            Location location = (Location) _locationHashMap.get(lid);  
            if (location.isRiverStation())
            {
                completeRiverStationLidSet.add(lid);    
            }
        }

        return completeRiverStationLidSet;
    }
    
//  ------------------------------------------------------------------------

    public RegularObsTimeSeries getDischargeTimeSeriesFromHeightTimeSeries( RegularObsTimeSeries obsTimeSeries )
    {
        RegularObsTimeSeries newTimeSeries = null;
        RegularObsTimeSeriesDescriptor descriptor = 
                    new RegularObsTimeSeriesDescriptor(obsTimeSeries.getDescriptor());
       
        Location location = (Location) _locationHashMap.get( descriptor.getLid() );
      //  DecimalFormat decimalFormat = new DecimalFormat( "0.00" );
        String dischargeString = null;
        RatingCurve ratingCurve = location.getRatingCurve();
        boolean wrongPe = false;
        
        //change the descriptor and check for a nonconvertable PE
        if (descriptor.getPe().equalsIgnoreCase("HG"))
        {
            descriptor.setPe("QR");
        }
        else if (descriptor.getPe().equalsIgnoreCase("HT"))
        {
            descriptor.setPe("QT");
        }
        else
        {
            wrongPe = true;
        }
        
        
        if ( !wrongPe &&  ratingCurve != null && ratingCurve.exists())
        {
            
           
            
            HourlyTimeSlotPolicy hourlyTimeSlotPolicy = new HourlyTimeSlotPolicy();
            
            newTimeSeries = new RegularObsTimeSeries( descriptor, 1, hourlyTimeSlotPolicy );
            
            List timeValuePairList = obsTimeSeries.getTimeValuePairList( true );
            
            double dischargeValue = 0.0;
            
            for ( int i = 0; i < timeValuePairList.size(); i++ )
            {
                TimeValuePair timeValuePair = (TimeValuePair) timeValuePairList.get( i );
                if ( timeValuePair != null )
                {
                    TimeValuePair newTimeValuePair = new TimeValuePair( timeValuePair.getDateTime() );
                    
                    dischargeValue = ratingCurve.getDischargeFromStage( timeValuePair.getValue() );
                    dischargeValue = MathHelper.roundToNDecimalPlaces(dischargeValue, 2);
                    newTimeValuePair.setValue(dischargeValue);
                                         
                    // dischargeString = decimalFormat.format( ratingCurve.getDischargeFromStage( timeValuePair.getValue() ) );
                    // newTimeValuePair.setValue( Double.parseDouble( dischargeString ) );                 
                      
                    newTimeSeries.addTimeValuePairIfBetterMatch( newTimeValuePair );
                }
            }
        }
        
        return newTimeSeries;
    }
    
//  ------------------------------------------------------------------------

    public RegularObsTimeSeries getPercentFloodFlowTimeSeriesListFromFlowStorageTimeSeries( RegularObsTimeSeries obsTimeSeries )
    {
        RegularObsTimeSeries newTimeSeries = null;
        RegularObsTimeSeriesDescriptor descriptor = obsTimeSeries.getDescriptor();
        Location location = (Location) _locationHashMap.get( descriptor.getLid() );
      //  DecimalFormat decimalFormat = new DecimalFormat( "0.00" );
        
        if ( location.getFloodFlow() > 0.0 )
        {
            HourlyTimeSlotPolicy hourlyTimeSlotPolicy = new HourlyTimeSlotPolicy();
            
            newTimeSeries = new RegularObsTimeSeries( descriptor, 1, hourlyTimeSlotPolicy );
            
            List timeValuePairList = obsTimeSeries.getTimeValuePairList( true );
            
            double percentFloodFlow = 0.0;
            
            for ( int i = 0; i < timeValuePairList.size(); i++ )
            {
                TimeValuePair timeValuePair = (TimeValuePair) timeValuePairList.get( i );
                if ( timeValuePair != null )
                {
                    TimeValuePair newTimeValuePair = new TimeValuePair( timeValuePair.getDateTime() );
                    
                    
                    if (! timeValuePair.isMissingValue())
                    {
                        percentFloodFlow = timeValuePair.getValue() / location.getFloodFlow();                        
                        percentFloodFlow = 100.0 * MathHelper.roundToNDecimalPlaces(percentFloodFlow, 2);
                        
                        //String valueString = decimalFormat.format( timeValuePair.getValue() / location.getFloodFlow() );
                        //newTimeValuePair.setValue( Double.parseDouble( valueString ) );
                        
                        if ((percentFloodFlow >= 0.0)  && (percentFloodFlow <= 100.0) )
                        {
                            newTimeValuePair.setValue(percentFloodFlow );
                            newTimeSeries.addTimeValuePairIfBetterMatch( newTimeValuePair );
                        }
                    }
                }
            }
        }
        return newTimeSeries;
    }
    
//  ------------------------------------------------------------------------
    
    public void loadRatingCurves()
    {
        RatingCurve ratingCurve = null;
        
        RatingTable ratingTable = new RatingTable(_db); 
        RatingShiftTable ratingShiftTable = new RatingShiftTable(_db);
        
        RiverstatTable riverstatTable = new RiverstatTable(_db);
        
        List allRatingRecordList = null;
        List singleStationRatingRecordList = new ArrayList();
        
        List allRatingShiftRecordList = null;
        List singleStationShiftList = null;
        
        List allRiverstatRecordList = null;
        RiverstatRecord riverStatRecord = null;
        
        String prevLid = null;
        
        int riverStatIndex = 0;
        int ratingShiftIndex = 0;
        IntegerHolder ratingShiftIndexHolder = new IntegerHolder( ratingShiftIndex );
        IntegerHolder riverStatIndexHolder = new IntegerHolder( riverStatIndex );
        
        try
        {
            allRatingRecordList = ratingTable.select( "ORDER BY lid, stage" );
            allRatingShiftRecordList = ratingShiftTable.select( "WHERE active = 'T' ORDER BY lid, date desc" );
            allRiverstatRecordList = riverstatTable.select( "ORDER BY lid" );
            
            for ( int i = 0; i < allRatingRecordList.size(); i++ )
            {
                RatingRecord ratingRecord = (RatingRecord) allRatingRecordList.get( i );
                // you have at least one rating curve that you have dealt with
                // and you are still going
                if ((prevLid != null) && (! ratingRecord.getLid().equalsIgnoreCase(prevLid)))
                {
                    singleStationShiftList =
                            getStationRatingShiftRecordListFromRatingShiftRecordList( prevLid,
                                                                                      allRatingShiftRecordList,
                                                                                      ratingShiftIndexHolder );
                    riverStatRecord = 
                             getRiverStatRecordFromRiverStatRecordList( prevLid,
                                                                        allRiverstatRecordList,
                                                                        riverStatIndexHolder );
                    //process old 
                    loadRatingCurveByRecordList( singleStationRatingRecordList, singleStationShiftList, riverStatRecord, prevLid );
                    
                    singleStationRatingRecordList.clear();   
                }
                
                singleStationRatingRecordList.add(ratingRecord);     
                prevLid = ratingRecord.getLid();
            }
            
        }
        catch (SQLException e)
        {
            logSQLException(e);
            ratingCurve = null;
        }
    }

//  ------------------------------------------------------------------------

    
    private void loadRatingCurveByRecordList( List ratingRecordList, 
                                              List ratingShiftList, 
                                              RiverstatRecord riverStatRecord, 
                                              String locationId )
    {
        String header = "loadRatingCurveByRecordList(): ";
        
        RatingCurve ratingCurve = null;
        
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
        ratingCurve = new RatingCurve( locationId );
        
        
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
        if ( riverStatRecord != null )
        {
            ratingCurve.setUsgsRatingNumber(riverStatRecord.getUsgs_ratenum());
            ratingCurve.setRatingDate(riverStatRecord.getRatedat());
        }
       
        Location location = (Location) _locationHashMap.get( locationId );
        
          
        if (location != null)
        {
            location.setRatingCurve( ratingCurve );
        }
        else
        {
            System.out.println(header + 
                        " Station classification missing ERROR for locationId = " +
                          locationId + " +  Run set_stnclass.LX to update station classifications.");
        }
        
    }
   
//  ------------------------------------------------------------------------

/**
 * @param lid - location id
 * @param allRatingShiftRecordList - Complete list of ratingShiftRecords sorted by lid 
 * @param ratingShiftIndexHolder - Index holder for the current ratingShift Index
 * @return Single station ratingShiftRecordList
 */    
    private List getStationRatingShiftRecordListFromRatingShiftRecordList( String lid, 
                                                                    List allRatingShiftRecordList, 
                                                                    IntegerHolder ratingShiftIndexHolder )
    {
        List ratingShiftRecordList = new ArrayList();
        RatingShiftRecord record = null;
        
        boolean found = false;
        boolean done = false;
        
        for ( int i = ratingShiftIndexHolder.getValue(); i < allRatingShiftRecordList.size() && !done; i++ )
        {
            record = (RatingShiftRecord) allRatingShiftRecordList.get( i );
            if ( record.getLid().equalsIgnoreCase( lid ) )
            {
                ratingShiftRecordList.add( record );
                found = true;
                ratingShiftIndexHolder.setValue( i );
            }
            else if ( found ) // prevlid does not match current lid and lid has already been found so we've found all the ratingshifts for the lid
            {
                done = true;
            }
        }
        return ratingShiftRecordList;
    }

//  ------------------------------------------------------------------------

/**
 * Get's the matching riverstatrecord for the passed in lid
 * @param lid - location id
 * @param allRiverStatRecordList - Complete list of RiverstatRecords 
 * @param riverStatIndexHolder - Index holder for the current riverstat Index
 */    
    private RiverstatRecord getRiverStatRecordFromRiverStatRecordList( String lid,
                                                                       List allRiverStatRecordList,
                                                                       IntegerHolder riverStatIndexHolder )
    {
        RiverstatRecord record = null;
        
        boolean found = false;
        
        for ( int i = riverStatIndexHolder.getValue(); i < allRiverStatRecordList.size() && !found; i++ )
        {
            record = (RiverstatRecord) allRiverStatRecordList.get( i );
            if ( record.getLid().equalsIgnoreCase( lid ) )
            {
                found = true;
                riverStatIndexHolder.setValue( i );
            }
        }
        return record;
    }
    

//  ------------------------------------------------------------------------

/**
 * createShefDurMap() - Creates a Hashmap of Shef durations.  Used for creating PEDTSEPs when writing out the data to 
 *                      .DAT files
 */    
    public Map getShefDurCodeToDurMap()
    {
        return _shefDurToDurcodeMap;
    }
    
    public void createShefDurMap()
    {
        CodeTimer timer1 = new CodeTimer();
        
        if ( _DEBUG )
        {
            timer1.start();
        }
        
        ShefDurTable table = new ShefDurTable( _db );
        List shefDurRecordList = null;
        
        try
        {
            shefDurRecordList = table.select( "" );
        }
        catch(SQLException e)
        {
            logSQLException(e);
        }
        
        if ( _shefDurMap == null )
        {
            _shefDurMap = new HashMap();
            _shefDurToDurcodeMap = new HashMap();
        }

        for ( int i = 0; i < shefDurRecordList.size(); i++ )
        {
            ShefDurRecord record = (ShefDurRecord) shefDurRecordList.get( i );
            _shefDurMap.put( "" + record.getDur(), record.getDurcode() ); 
            _shefDurToDurcodeMap.put(record.getDurcode(), "" + record.getDur() ); 
        }
        
        if ( _DEBUG )
        {
            timer1.stop( "Finished creating ShefDurMap in " );
        }
    }
    
//  ------------------------------------------------------------------------

    
    /**
     * getCachedNonPrecipObsTimeSeriesList - Queries the database and returns a List of Observed Time Series.
     * @param tableName - Name of the table to query.
     * @param numHoursToPreProcess - The number of hours of data to preprocess.
     * 
     * @return obsTimeSeriesList - List of RegularObsTimeSeries objects
     */
    private List readNonPrecipObsTimeSeriesListFromCachedFile(String fullFilePath)
    {

        PDCFileReader reader = new PDCFileReader(fullFilePath, _shefDurToDurcodeMap);
        
        List obsTimeSeriesList = reader.read();

        return obsTimeSeriesList;
    }
//  ------------------------------------------------------------------------
   
    private List readPrecipObsTimeSeriesListFromCachedFile(String fullFilePath)
    {

        PDCFileReader reader = new PDCFileReader(fullFilePath, _shefDurToDurcodeMap);
        
        List obsTimeSeriesList = reader.read();

        return obsTimeSeriesList;
    }

// ------------------------------------------------------------------------
    public List getNonPrecipObsTimeSeriesListUsingCache(String tableName,
                                                        long endTime,
                                                        int numHoursToPreprocess)
    {
        String header = "PDCPreprocessorDataMgr.getNonPrecipObsTimeSeriesListUsingCache(): ";
        List resultantTimeSeriesList = null;
        List fileTimeSeriesList = null;
        List dbTimeSeriesList = null;
        CodeTimer dbTimer = new CodeTimer();
        CodeTimer writeTimer = new CodeTimer();
        
        int numHoursToRetrieveFromDb = 0;
        
        String fullFilePath = _preprocessedFilePath + "/cached" + tableName + ".dat";
        
        fileTimeSeriesList = readNonPrecipObsTimeSeriesListFromCachedFile(fullFilePath);
        
        if  (isCachedDataCurrentEnough(fileTimeSeriesList,
                                       _nonPrecipHoursToReadWhenCaching))
            
        {       
            numHoursToRetrieveFromDb = _nonPrecipHoursToReadWhenCaching;
        }
        else //re-retrieve everything from the db
        {
            numHoursToRetrieveFromDb = numHoursToPreprocess;
        }
        
      
        System.out.println(header + "for table = " + tableName + 
                           ", numHoursToRetrieveFromDb = " +
                           numHoursToRetrieveFromDb);
        
        dbTimer.start();   
        dbTimeSeriesList = getNonPrecipObsTimeSeriesListFromDb( tableName, endTime, numHoursToRetrieveFromDb );
        dbTimer.stop(header + "db read within cache read took "); 
        
        long newStartTime = getNewStartTime(endTime, 1, numHoursToPreprocess);
       
        resultantTimeSeriesList = TimeSeriesListMgr.mergeTimeSeriesLists(dbTimeSeriesList,
                                                       fileTimeSeriesList, 
                                                       newStartTime,
                                                       endTime);
        
        
         
        // for the next time this program is run, write out the newly cached
        // data
        // I don't want the missing data to be saved
        
        writeTimer.start();
        writeRegularObsDataToFile( resultantTimeSeriesList, "cached" + tableName);
        writeTimer.stop(header + "writing to data file within cache read took ");
        
        return resultantTimeSeriesList;
    }  
    //  --------------------------------------------------------------------------------------------
    public List getPrecipObsTimeSeriesListFromCache(int timeStepIntervalInHours)
    {
        List cachedDataTimeSeriesList = null;
  
        String fullFilePath = _preprocessedFilePath + "/cached" + timeStepIntervalInHours + "HourPrecip.dat";
        
        cachedDataTimeSeriesList = readPrecipObsTimeSeriesListFromCachedFile(fullFilePath);
   
        return cachedDataTimeSeriesList;
    }  
    //  --------------------------------------------------------------------------------------------
    
    public void handleCacheAging()
    {
        if (! isCacheNewEnough())
        {
            deleteOldCacheFiles();
            updateCacheRefreshIndicator();
        }
    }
    
    //  --------------------------------------------------------------------------------------------
    
    private boolean isCacheNewEnough()
    {
        boolean result = false;
        
        File file = new File( getCacheRefreshIndicatorFileName() );
        if (file.exists())
        {
            long fileCreationTime = file.lastModified();   
            long currentTime = System.currentTimeMillis();
            
            long ageInMillis = currentTime - fileCreationTime;
            if (ageInMillis < _cacheRefreshAgeMaxInMillis )
            {
                result = true;
            }
            else
            {
                result = false;
            }
        }
        
        return result;
    }
    //  --------------------------------------------------------------------------------------------
    
    private void deleteOldCacheFiles()
    {
        File directory = new File(_preprocessedFilePath);
     
        File fileArray[] = directory.listFiles();
        
        if (fileArray != null)
        {
            
            for (int i = 0; i < fileArray.length; i++)
            {
                File file = fileArray[i];
                String fileName = file.getName();
                if (fileName.startsWith("cached") && 
                        fileName.endsWith(".dat"))
                {
                    file.delete();   
                }
                
            }
        }
        
    }
    
    //  --------------------------------------------------------------------------------------------

    
    
    private void updateCacheRefreshIndicator()
    {
        File file = new File( getCacheRefreshIndicatorFileName() );       
        try
        {
            if (file.exists())
            {
                file.delete();
            }
            
            file.createNewFile();
        }
        catch (IOException e)
        {
            System.out.println(e.getMessage());
        }
    }
    //  --------------------------------------------------------------------------------------------
    
    private  String getCacheRefreshIndicatorFileName()
    {
        return _preprocessedFilePath + "/" + _refreshIndicatorBaseFileName;  
    }
    //  --------------------------------------------------------------------------------------------

    private boolean isCachedDataCurrentEnough(List fileTimeSeriesList, int cacheHours)
    {
        /*
         *  This examines the times of the data in the cache.
         *  shouldCacheBeAccepted() examines the last time that the cache itself was
         *  completely recreated from scratch.
         * 
         */
        String header = "PDCPreprocessorDataMgr.isCachedDataCurrentEnough() :";
        boolean result = false; 
         
        if (fileTimeSeriesList != null) //if there is no file, then it is not current enough
        {
            RegularObsTimeSeries firstFileTs = (RegularObsTimeSeries) fileTimeSeriesList.get(0);
            
    //        System.out.println(header + " firstFileTs = " + firstFileTs);
            
            long lastFileObsTime = firstFileTs.getLatestTimeValue(0);
            
            
            long currentTime = System.currentTimeMillis();
            
   //         System.out.println(header + " lastFileObsTime = " +
   //                DbTimeHelper.getDateTimeStringFromLongTime(lastFileObsTime) +
   //                " currentTime = " + DbTimeHelper.getDateTimeStringFromLongTime(currentTime) );
   
            
            long stalenessTime = currentTime - lastFileObsTime;
            
            if (stalenessTime < cacheHours * MILLIS_PER_HOUR)
            {
                result = true;
            }
        }
        else
        {
            System.out.println(header + 
                    "fileTimeSeriesList is null, so need to read more data from db");
        }
        
        return result;
    }
    // --------------------------------------------------------------------------------------------
   
//  -------------------------------------------------------------------------    
  
// -------------------------------------------------------------------------
/**
 * getNonPrecipObsTimeSeriesListFromDb - Queries the database and returns a List of
 * Observed data.
 * 
 * @param tableName -
 *            Name of the table to query.
 * @param numHoursToPreProcess -
 *            The number of hours of data to preprocess.
 * 
 * @return obsTimeSeriesList - List of RegularObsTimeSeries data
 */
    public List getNonPrecipObsTimeSeriesListFromDb( String tableName,
                                                     long endTime, int numHoursToPreProcess )
    {
        String header = "PDCPreprocessorDataMgr.getNonPrecipObsTimeSeriesListFromDb(): ";
        NonPrecipObsTableManager table = new NonPrecipObsTableManager( _db, tableName );
        List obsTimeSeriesList = null;
        
        if ( numHoursToPreProcess > 0 )
        {
            try
            {
                obsTimeSeriesList = table.select( tableName, numHoursToPreProcess, endTime );
            }
            catch(SQLException e)
            {
                logSQLException(e);
            }
        }
        
        System.out.println(header + 
                "for table = " + tableName +
                ", records retrieved = " + obsTimeSeriesList.size());
        
        return obsTimeSeriesList;
    }

// ------------------------------------------------------------------------

    public RegularObsTimeSeries getDepthAboveFSTimeSeries( RegularObsTimeSeries obsTimeSeries )
    {
        RegularObsTimeSeries newTimeSeries = null;
        Location location = (Location) _locationHashMap.get( obsTimeSeries.getDescriptor().getLid() );
        double floodStage = MISSING;
        HourlyTimeSlotPolicy hourlyTimeSlotPolicy = new HourlyTimeSlotPolicy();
        
        if ( location == null )
        {
            System.out.println( "Pause" );
        }
        if ( location.isRiverStation() )
        {
            newTimeSeries = new RegularObsTimeSeries( obsTimeSeries.getDescriptor(), 1, hourlyTimeSlotPolicy );
            floodStage = location.getFloodStage();
            List timeValuePairList = obsTimeSeries.getTimeValuePairList( true );
            
            for( int i = 0; i < timeValuePairList.size(); i++ )
            {
                TimeValuePair timeValuePair = (TimeValuePair) timeValuePairList.get( i );
                if ( timeValuePair != null )
                {
                    TimeValuePair newTimeValuePair = new TimeValuePair( timeValuePair.getDateTime() );
                    if ( ( floodStage == MISSING ) || ( timeValuePair.getValue() == MISSING ) )
                    {
                        newTimeValuePair.setValue( MISSING );
                    }
                    else
                    {
                        newTimeValuePair.setValue( timeValuePair.getValue() - floodStage );
                    }
                    newTimeSeries.addTimeValuePairIfBetterMatch( newTimeValuePair );
                }
            }
        }
        else
        {
            newTimeSeries = new RegularObsTimeSeries( obsTimeSeries.getDescriptor(), 1, hourlyTimeSlotPolicy );
            List timeValuePairList = obsTimeSeries.getTimeValuePairList( true );
            for( int i = 0; i < timeValuePairList.size(); i++ )
            {
                TimeValuePair timeValuePair = (TimeValuePair) timeValuePairList.get( i );
                TimeValuePair newTimeValuePair = null;
                if ( timeValuePair != null )
                {
                    newTimeValuePair = new TimeValuePair( timeValuePair.getDateTime(), MISSING );
                    newTimeSeries.addTimeValuePairIfBetterMatch( newTimeValuePair );
                }
            }
        }
        return newTimeSeries;
    }

//  ------------------------------------------------------------------------

    public RegularObsTimeSeries getDeltaTimeSeries( RegularObsTimeSeries obsTimeSeries )
    {
        DecimalFormat decimalFormat = new DecimalFormat( "0.00" );

        RegularObsTimeSeriesDescriptor descriptor = obsTimeSeries.getDescriptor();
        long previousDayTime = descriptor.getStartTime();
        
        HourlyTimeSlotPolicy hourlyTimeSlotPolicy = new HourlyTimeSlotPolicy();

        RegularObsTimeSeries newTimeSeries = new RegularObsTimeSeries( obsTimeSeries.getDescriptor(), 1, 
                                                  hourlyTimeSlotPolicy );
        
        descriptor.setStartTime( previousDayTime + ( MILLIS_PER_DAY ) );
        
        Map timeValuePairMap = obsTimeSeries.getTopOfTheHourTimeValuePairMap();

        for ( long indexTime = descriptor.getStartTime(); indexTime <= descriptor.getEndTime(); indexTime+=MILLIS_PER_HOUR )
        {
            TimeValuePair timeValuePair = (TimeValuePair) timeValuePairMap.get( new Long( indexTime ) );
            if ( timeValuePair != null )
            {
                TimeValuePair newTimeValuePair = new TimeValuePair( timeValuePair.getDateTime() );
                TimeValuePair previousTimeValuePair = (TimeValuePair) timeValuePairMap.get( new Long( previousDayTime) );
                
                if ( ( timeValuePair == null ) 
                        || ( previousTimeValuePair == null )  
                        || ( previousTimeValuePair.getValue() == MISSING ) 
                        || ( timeValuePair.getValue() == MISSING ) )
                {
                    
                }
                else 
                {
                    double newValue = timeValuePair.getValue() - previousTimeValuePair.getValue();
                    
                    newTimeValuePair.setValue( Double.parseDouble( decimalFormat.format( newValue ) ) ); 
                    newTimeSeries.addTimeValuePairIfBetterMatch( newTimeValuePair );
                }
                
                previousDayTime += MILLIS_PER_HOUR;
            }

        }

        return newTimeSeries;
    }

//  ------------------------------------------------------------------------

    public void executePDCPrecipPP(long endTime, int precipHoursToReadFromDB)
    {
        CodeTimer executePDCPrecipPPTimer = new CodeTimer();
        
        if ( _DEBUG )
        {
            executePDCPrecipPPTimer.start();
        }
       
        EnvHelper envHelper = new EnvHelper();

        String dirString = envHelper.getProperty("WHFS_BIN_DIR");
        String commandString = dirString + "/run_pdc_precip_pp" + 
                               " " + ( endTime / 1000 ) +
                               " " + precipHoursToReadFromDB;
        
        if ( (OS.indexOf("nt") > -1)
                || (OS.indexOf("windows 2000") > -1)
                || (OS.indexOf("windows xp") > -1) )  // Checking to see if running on Linux or Windows
        {
        }
        else
        {
            try
            {
                System.out.println("Command string = :" + commandString + ":");
                Process process = Runtime.getRuntime().exec(commandString);
                process.waitFor();
            }
            catch(IOException e)
            {
                logException( e );
                if ( _DEBUG )
                {
                    e.printStackTrace();
                }
            }

            catch( InterruptedException e )
            {
                logException( e );
                if ( _DEBUG )
                {
                    e.printStackTrace();
                }
            }
        }
        if ( _DEBUG )
        {
            executePDCPrecipPPTimer.stop( "Completed call of PDCPrecipPP in " );
        }
    }

    
    public List getInstantaneousPrecipTimeSeriesList(long endTime)
    {
        List instantPrecipTimeSeriesList = new ArrayList();
        String instantPrecipInputFileName = null;
   
        if ( (OS.indexOf("nt") > -1)
                || (OS.indexOf("windows 2000") > -1)
                || (OS.indexOf("windows xp") > -1) )
         {
            instantPrecipInputFileName = "D:/data/pdcpreprocessor/geninstantprecip.out";
         }
        else // assume it's unix
        {
            instantPrecipInputFileName = _preprocessedFilePath + "/geninstantprecip.out";
        }

        try 
        {
            File instantPrecipFile = new File( instantPrecipInputFileName );
            
            BufferedReader _reader = new BufferedReader( new FileReader( instantPrecipFile ) );
            String line = _reader.readLine();  //discard the first line
            line = _reader.readLine();         //discard the second line
            line = _reader.readLine();         //discard the third line
            line = _reader.readLine();

            while ( ( line = _reader.readLine() ) != null )  //reads the next line and checks if it's null
            {
                RegularObsTimeSeriesDescriptor descriptor = new RegularObsTimeSeriesDescriptor();
                String[] tokenizedLine = StringParser.tokenize( line );

                descriptor.setLid( tokenizedLine[ 0 ] );
                descriptor.setPe( tokenizedLine[ 1 ] );
                descriptor.setDur( (short) 1001 );
                descriptor.setExtremum( "Z" );
                descriptor.setTs( tokenizedLine[ 2 ] );
                descriptor.setStartTime( endTime - MILLIS_PER_DAY );
                descriptor.setEndTime( endTime );

                line = _reader.readLine();
                tokenizedLine = StringParser.tokenize( line );
                
                
                IrregularObsTimeSeries obsTimeSeries = new IrregularObsTimeSeries( descriptor, 1 );
                
                long startTime = endTime - MILLIS_PER_DAY;
                int counter = 0;
                
                // add all of the regular hours for the last 24 hours, 1..24
                for ( long indexTime = startTime; indexTime < endTime; indexTime+=MILLIS_PER_HOUR )
                {
                    TimeValuePair timeValuePair = new TimeValuePair( indexTime );
                    timeValuePair.setValue( Double.parseDouble( tokenizedLine[ counter++ ] ) );
                    obsTimeSeries.addTimeValuePairIfBetterMatch( timeValuePair );
                }
                
                // add the special entry that is for the last half hour
                TimeValuePair timeValuePair = new TimeValuePair( endTime - (MILLIS_PER_HALF_HOUR) );
                timeValuePair.setValue( Double.parseDouble( tokenizedLine[ counter++ ] ) );
                obsTimeSeries.addTimeValuePairIfBetterMatch( timeValuePair );
                
                instantPrecipTimeSeriesList.add( obsTimeSeries );
            }
        }
        catch( FileNotFoundException e )
        {
            logException(e);
            if ( _DEBUG )
            {
                e.printStackTrace();
            }
        }
        catch( IOException e )
        {
            logException(e);
            if ( _DEBUG )
            {
                e.printStackTrace();
            }
        }
        return instantPrecipTimeSeriesList;
    }
    
//  ------------------------------------------------------------------------

  

//  ------------------------------------------------------------------------
    public List  get1HourPrecipTimeSeriesListFromCFile()
    {
        return readPrecipTimeSeriesListFromCGeneratedFile(1, 1001);
    }
//  ------------------------------------------------------------------------  
    public List get3HourPrecipTimeSeriesListFromCFile()
    {
        return readPrecipTimeSeriesListFromCGeneratedFile(3, 1003);
    }  
//  ------------------------------------------------------------------------
    public List get6HourPrecipTimeSeriesListFromCFile()
    {
        return readPrecipTimeSeriesListFromCGeneratedFile(6, 1006);
    }   
//  ------------------------------------------------------------------------
    public List get24HourPrecipTimeSeriesListFromCFile()
    {
        return readPrecipTimeSeriesListFromCGeneratedFile(24, 2001);
    }  
//  ------------------------------------------------------------------------
        

    public List readPrecipTimeSeriesListFromCGeneratedFile(int timeStepIntervalInHours,
                                                           int durationCode)
    {
        List precipTimeSeriesList = new ArrayList();
        
        String fileName = "gen"+ timeStepIntervalInHours+ "hourprecip.out";
   
        String precipInputFileName = null;
        String[] tokenizedLine = null;
   
        if ( (OS.indexOf("nt") > -1)
                || (OS.indexOf("windows 2000") > -1)
                || (OS.indexOf("windows xp") > -1) )
         {
            precipInputFileName = "D:/data/pdcpreprocessor/" + fileName;
         }
        else // assume it's unix
        {
            precipInputFileName = _preprocessedFilePath + "/" + fileName;
        }

        try 
        {
            File precipInputFile = new File( precipInputFileName );
            
            BufferedReader _reader = new BufferedReader( new FileReader( precipInputFile ) );
            String line = _reader.readLine();  //discard the first line
            line = _reader.readLine();         //discard the second line
            line = _reader.readLine();         //discard the third line
            line = _reader.readLine();         //discard the fourth line
            line = _reader.readLine();
            tokenizedLine = StringParser.tokenize( line );
            
            long startTime = Long.parseLong( tokenizedLine[ 0 ] ) * 1000;
            long endTime = Long.parseLong( tokenizedLine[ 1 ] ) * 1000;
             
            while ( ( line = _reader.readLine() ) != null )  //reads the next line and checks if it's null
            {
                RegularObsTimeSeriesDescriptor descriptor = new RegularObsTimeSeriesDescriptor();
                tokenizedLine = StringParser.tokenize( line );

                descriptor.setLid( tokenizedLine[ 0 ] );
                //descriptor.setPe( "PP" ); //added 5/16/06
               
                descriptor.setPe( tokenizedLine[ 1 ] ); //commented out because I want acumm. precip to display as PP
                descriptor.setDur( (short) durationCode );
                descriptor.setExtremum( "Z" );
                descriptor.setTs( tokenizedLine[ 2 ] );
                descriptor.setStartTime( startTime );
                descriptor.setEndTime( endTime );

                line = _reader.readLine();
                tokenizedLine = StringParser.tokenize( line );
                              
                TimeSlotPolicy timeSlotPolicy = new HourlyTimeSlotPolicy();
                
                RegularObsTimeSeries obsTimeSeries =
                            new RegularObsTimeSeries(descriptor,
                                                     timeStepIntervalInHours,
                                                     timeSlotPolicy);
                
                int counter = 0;
                
//                System.out.println( ( _endTime - startTime ) / ( 1000 * 60 ) );

                long indexTime = startTime;
                
                // add in all the TimeValuePairs for the complete periods
                for ( int i = 0; i < tokenizedLine.length-1; i++ )
                {
                    TimeValuePair timeValuePair = new TimeValuePair( indexTime );
                    timeValuePair.setValue( Double.parseDouble( tokenizedLine[ counter++ ] ) );
                    obsTimeSeries.addTimeValuePairIfBetterMatch( timeValuePair );
                    indexTime += timeStepIntervalInHours * MILLIS_PER_HOUR;
                }
                
                // add in the tvp for the last (possibly) incomplete period
                TimeValuePair timeValuePair = new TimeValuePair( endTime );
                timeValuePair.setValue( Double.parseDouble( tokenizedLine[ counter++ ] ) );
                obsTimeSeries.addTimeValuePairIfBetterMatch( timeValuePair );
                
                precipTimeSeriesList.add( obsTimeSeries );
            }
        }
        catch( FileNotFoundException e )
        {
            logException( e );
            if ( _DEBUG )
            {
                e.printStackTrace();
            }
        }
        catch( IOException e )
        {
            logException( e );
            if ( _DEBUG )
            {
                e.printStackTrace();
            }
        }

        
        return precipTimeSeriesList;
    }
    
//  ------------------------------------------------------------------------   
    
    private void logSQLException(SQLException exception)
    {
        _logger.log("SQL ERROR = " +
                    exception.getErrorCode() +  " " +
                    exception.getMessage());
        exception.printStackTrace(_logger.getPrintWriter());
        
        _logger.log("End of stack trace");
         
    }
//  ------------------------------------------------------------------------
 
    private void logException( Exception exception )
    {
        _logger.log( "ERROR = " + 
                     exception.getMessage() );
        exception.printStackTrace( _logger.getPrintWriter() );
        
        _logger.log( "End of stack trace" );
    }

// ------------------------------------------------------------------------
    public PrecipListHolder processPrecipDataWithCaching(long endTime, int numHoursToPreprocess)
    {        
        CodeTimer cCodeTimer = new CodeTimer();
        CodeTimer cacheReadTimer = new CodeTimer();
        CodeTimer processingTimer = new CodeTimer();
        CodeTimer instTimer = new CodeTimer();
        CodeTimer mergingTimer = new CodeTimer();
        
        PrecipListHolder precipListHolder = new PrecipListHolder();
        
        String header = "PDCPreprocessor.processPrecipDataWithCaching(): ";
        int hoursToReadWhenCaching = _precipHoursToReadWhenCaching; //Note, MUST be a multiple of 24 hours
        int numHoursToRetrieveUsingCRoutines = 0;
        //read in cached Precip Data into all 4  Lists of RegularObsTimeSeries objects
        //adjust the number of hours to read for the C program,
                // based on how much I read in from the cache and how current it is
        
        // call the c program
        // read some the list of observed time series data from the c file
        
          
        // merge the c file data and the cached file data
        // save the merged data into the cache
        
        // write out the merged data 
        
        cacheReadTimer.start();
        
        List cached1HourPrecipTimeSeriesList =
                       getPrecipObsTimeSeriesListFromCache(1);
        
        List cached3HourPrecipTimeSeriesList =
                       getPrecipObsTimeSeriesListFromCache(3);

        List cached6HourPrecipTimeSeriesList =
                       getPrecipObsTimeSeriesListFromCache(6);
        
        List cached24HourPrecipTimeSeriesList =
                       getPrecipObsTimeSeriesListFromCache(24);
        
        cacheReadTimer.stop(header + "reading the cache took ");

        
        List instantPrecipTimeSeriesList = null;
        List oneHourPrecipTimeSeriesList = null;
        List threeHourPrecipTimeSeriesList = null;
        List sixHourPrecipTimeSeriesList = null;
        List twentyFourHourPrecipTimeSeriesList = null;
        
       
        //determine how many hours to retrieve fresh from the database
        if ( 
           (isCachedDataCurrentEnough(cached1HourPrecipTimeSeriesList, hoursToReadWhenCaching)) &&
           (isCachedDataCurrentEnough(cached3HourPrecipTimeSeriesList, hoursToReadWhenCaching)) &&
           (isCachedDataCurrentEnough(cached6HourPrecipTimeSeriesList, hoursToReadWhenCaching)) &&
           (isCachedDataCurrentEnough(cached24HourPrecipTimeSeriesList, hoursToReadWhenCaching))
           )
        {       
            numHoursToRetrieveUsingCRoutines = hoursToReadWhenCaching;    
        }
        else //re-retrieve everything from the db
        {
            numHoursToRetrieveUsingCRoutines = numHoursToPreprocess;
        }
        
        
        
        // retrieve fresh data and merge it with the data previously retrieved from the cache.
        if ( numHoursToPreprocess > 0 )
        {
            System.out.println(header + " asking for " + 
                               numHoursToRetrieveUsingCRoutines + " from C code");
  
            cCodeTimer.start();
            
            executePDCPrecipPP(endTime, numHoursToRetrieveUsingCRoutines);
            
            cCodeTimer.stop(header + "Running the C Precip preprocessor alone took");
            
            
            instTimer.start();
            instantPrecipTimeSeriesList = getInstantaneousPrecipTimeSeriesList(endTime);
            precipListHolder.setInstantPrecipTimeSeriesList(instantPrecipTimeSeriesList);
            instTimer.stop(header + " calculating Instantaneous Precip took ");
            
            mergingTimer.start();
            
             // Merge all the time series (plural)
            long newStartTime = 0;
            oneHourPrecipTimeSeriesList = getMergedPrecipTimeSeriesList(endTime, 1, 
                                                            numHoursToPreprocess,
                                                            1001,
                                                            cached1HourPrecipTimeSeriesList);
            precipListHolder.setOneHourPrecipTimeSeriesList(oneHourPrecipTimeSeriesList);
            
            
            threeHourPrecipTimeSeriesList = getMergedPrecipTimeSeriesList(endTime, 3, 
                    numHoursToPreprocess,
                    1003,
                    cached3HourPrecipTimeSeriesList);
            precipListHolder.setThreeHourPrecipTimeSeriesList(threeHourPrecipTimeSeriesList);
            
            
            sixHourPrecipTimeSeriesList = getMergedPrecipTimeSeriesList(endTime, 6, 
                    numHoursToPreprocess,
                    1006,
                    cached6HourPrecipTimeSeriesList);
            precipListHolder.setSixHourPrecipTimeSeriesList(sixHourPrecipTimeSeriesList);
            
            
            twentyFourHourPrecipTimeSeriesList= getMergedPrecipTimeSeriesList(endTime, 24, 
                    numHoursToPreprocess,
                    2001,
                    cached24HourPrecipTimeSeriesList);
            precipListHolder.setTwentyForHourPrecipTimeSeriesList(twentyFourHourPrecipTimeSeriesList);
            
            mergingTimer.stop(header + "merging the 1,3,6, and 24 hour time series took");
         
        }
        
        return precipListHolder;
    }
    
//  ------------------------------------------------------------------------
    private List getMergedPrecipTimeSeriesList(long endTime, int timeStepInHours,  
            int numHoursToPreprocess,
            int durationCode,
            List cachedPrecipTimeSeriesList)
    {
        String header = "PDCPreprocessorDataMgr.getMergedPrecipTimeSeriesList() ";
        List mergedDataList = null;
        List freshDataList = null;
        long newStartTime = 0;
        
        String cachedDataFileName = "cached" + timeStepInHours + "HourPrecip";
        
         
        freshDataList = readPrecipTimeSeriesListFromCGeneratedFile(timeStepInHours, durationCode);
        
        newStartTime = getNewStartTime(endTime, timeStepInHours, numHoursToPreprocess);
        long newEndTime = getNewEndTime(endTime, timeStepInHours);
        
        TimeSeriesListMgr.setNewStartTime(freshDataList, newStartTime);
         
        String newStartTimeString = DbTimeHelper.getDateTimeStringFromLongTime(newStartTime);
        String newEndTimeString = DbTimeHelper.getDateTimeStringFromLongTime(newEndTime);
        
        System.out.println(header + " " + 
                            timeStepInHours + "-hour timestep,  newStartTime = " +
                            newStartTimeString + " newEndTime = " + newEndTimeString);
        
        mergedDataList = TimeSeriesListMgr.mergeTimeSeriesLists(freshDataList, 
                                                           cachedPrecipTimeSeriesList,
                                                           newStartTime,
                                                           newEndTime);
        
        // copy to the cache for the next run
        writeRegularObsDataToFile(mergedDataList, cachedDataFileName);
        
        return mergedDataList;
        
    }
//  ------------------------------------------------------------------------
    long getNewStartTime(long endTime, int timeStepIntervalInHours, int numHoursToPreprocess)
    {
       long newStartTime  = 0;
       long hoursBack = 0;
                  
       //----examine this section for simplification and correction-
      //  long numPeriods = numHoursToPreprocess / timeStepIntervalInHours;
      //  long millisBackInTime = numPeriods * timeStepIntervalInHours * MILLIS_PER_HOUR;
   
        if (timeStepIntervalInHours == 1)
        {
            hoursBack = numHoursToPreprocess - 1;
        }
        else
        {
            hoursBack = numHoursToPreprocess - 1;
        }
       
        long timeStepIntervalInMillis = timeStepIntervalInHours * MILLIS_PER_HOUR;
        long millisBackInTime = hoursBack * MILLIS_PER_HOUR;
       
        newStartTime = endTime - millisBackInTime;
        newStartTime /= timeStepIntervalInMillis;
        newStartTime *= timeStepIntervalInMillis;
   //----------------
   
       if (timeStepIntervalInHours == 24)
       {
           newStartTime += MILLIS_PER_12_HOURS; //12z is used instead of 0z
       }
       
       return newStartTime;
         
    }
    
//  ------------------------------------------------------------------------

    long getNewEndTime(long origEndTime, int timeStepIntervalInHours)
    {
        String header = "PDCPreprocessor.getNewEndTime(): ";
        long endTime = 0;
        
        if (timeStepIntervalInHours != 24)
        {
            // System.out.println(header + "orig end time was " + DbTimeHelper.getDateTimeStringFromLongTime(origEndTime));
            endTime = TimeHelper.truncateTimeInMillisToNearestHour(origEndTime, timeStepIntervalInHours);
            endTime += timeStepIntervalInHours * MILLIS_PER_HOUR;
        }

        //        System.out.println(header + "new time is " + DbTimeHelper.getDateTimeStringFromLongTime(endTime));
        else //if (timeStepIntervalInHours == 24)
        {
            long twelveHourEndTime = TimeHelper.truncateTimeInMillisToNearestHour(origEndTime, 12);
            long twentyFourHourEndTime = TimeHelper.truncateTimeInMillisToNearestHour(origEndTime, 24);
            
            if (twelveHourEndTime == twentyFourHourEndTime) //before 12 Z
            {
                endTime = twelveHourEndTime + MILLIS_PER_12_HOURS;
                
            }
            else //after 12Z
            {
                endTime = twelveHourEndTime + MILLIS_PER_DAY;
            }
            
          
        }

        return endTime;
    }
//  ------------------------------------------------------------------------

/**
 * disconnect() - Closes the active connection to the database
 */
    public void disconnect()
    {
        _db.disconnect();   
    }
  	
//  ------------------------------------------------------------------------

    public void finalize()
    {
        disconnect();	
    }
    
//  ------------------------------------------------------------------------

// -------------------------------------------------------------------------
    public void writeIrregularObsDataToFile( List obsTimeSeriesList, String fileName )
    {
        PDCFileWriter writer = new PDCFileWriter(_locationHashMap, 
                                                 _shefDurMap,
                                                 _shefDurToDurcodeMap,
                                                 _preprocessedFilePath);
        writer.writeIrregularObsDataToFile(obsTimeSeriesList, fileName);
    }
    
    
    public void writeRegularObsDataToFile( List obsTimeSeriesList, String fileName )
    {
        PDCFileWriter writer = new PDCFileWriter(_locationHashMap, 
                                                _shefDurMap, 
                                                _shefDurToDurcodeMap,
                                                _preprocessedFilePath);
        writer.writeRegularObsDataToFile(obsTimeSeriesList, fileName);
    }
    
   
    public void setPreprocessedFilePath( String preprocessedFilePath )
    {
        _preprocessedFilePath = preprocessedFilePath;
    }

//  ------------------------------------------------------------------------
    public void testPDCFiles()
    {
         String fullFilePath = _preprocessedFilePath + "/cachedHeight.dat";
        
         PDCFileReaderSlow reader = new PDCFileReaderSlow(fullFilePath, _shefDurToDurcodeMap);
         
         List obsTimeSeriesList = reader.read();
         
         
         PDCFileWriter writer = new PDCFileWriter(_locationHashMap,
                                                  _shefDurMap,
                                                  _shefDurToDurcodeMap, 
                                                  _preprocessedFilePath);
         
         writer.writeRegularObsDataToFile(obsTimeSeriesList, "cachedHeight2");
    }
    
// ------------------------------------------------------------------------

    /**
    * @return Returns whether in debug mode or not.
    */
}