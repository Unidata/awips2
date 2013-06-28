package ohd.hseb.pdc_pp;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import ohd.hseb.db.Database;
import ohd.hseb.db.DbTable;
import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.util.MathHelper;
import ohd.hseb.util.TimeHelper;

import ohd.hseb.util.CodeTimer;



public class NonPrecipObsTableManager extends DbTable
{
    public static final int DECIMAL_PLACES = 2;
    
    public NonPrecipObsTableManager(Database database, String tableName ) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName( tableName );
    }
    
    
    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //     a Vector of ObservedData objects
    //-----------------------------------------------------------------
    public List select( String tableName, int numHoursToProcess, long endTime ) throws SQLException
    {
        String header = "NonPrecipObsTableManager.select: ";
        
        CodeTimer nonPrecipObsTableTimer = new CodeTimer();
        if ( PDCPreprocessorDataMgr._DEBUG )
        {
            nonPrecipObsTableTimer.start();
        }
        RegularObsTimeSeriesDescriptor obsTimeSeriesDescriptor =
                                       new RegularObsTimeSeriesDescriptor();
        RegularObsTimeSeries obsTimeSeries = null;
        double value = 0.0;
        long obsTime = 0;
        TimeValuePair timeValuePair = null;
       
        long hourlyEndTime = TimeHelper.roundTimeInMillisToNearestHour( endTime );
        long hourlyStartTime = hourlyEndTime - 
            ( ( numHoursToProcess - 1) * PDCPreprocessorDataMgr.MILLIS_PER_HOUR );
       
        long dailyEndTime = truncateTimeInMillisToDay( endTime );
        dailyEndTime += PDCPreprocessorDataMgr.MILLIS_PER_12_HOURS;  //12z to 12z
        long dailyStartTime = dailyEndTime -
            ( ( numHoursToProcess ) * PDCPreprocessorDataMgr.MILLIS_PER_HOUR );
        
        String pe = null;
        String extremum = null;
        HourlyTimeSlotPolicy hourlyTimeSlotPolicy = new HourlyTimeSlotPolicy();
        DailyTimeSlotPolicy dailyTimeSlotPolicy = new DailyTimeSlotPolicy();
        CodeTimer queryTimer = new CodeTimer();
        
        List obsTimeSeriesList = new ArrayList();
        int recordsFound = 0;
        
        String tabbedheader = "    " + header;
        
        Map obsTimeSeriesMap = new HashMap();
        
        // create a List to hold Height Records
        
        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = 
            "Select lid, pe, dur, extremum, ts, value, obstime from " +
                    tableName + " WHERE obstime >= '" + 
                    DbTimeHelper.getDateTimeStringFromLongTime( hourlyStartTime ) + "'";
       
        
        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery( selectStatement );

        // loop through the result set
        boolean rsNext = rs.next();

        // Setting the start and end time for the timeseries
        
        while ( rsNext )
        {
            obsTimeSeriesDescriptor.setLid( getTrimmedString( rs.getString( 1 ) ) );
            pe = getTrimmedString( rs.getString( 2 ) );
            obsTimeSeriesDescriptor.setPe( pe );
            obsTimeSeriesDescriptor.setDur( rs.getShort( 3 ) );
            extremum = getTrimmedString( rs.getString( 4 ) );
            obsTimeSeriesDescriptor.setExtremum( extremum );
            obsTimeSeriesDescriptor.setTs( getTrimmedString( rs.getString( 5 ) ) );
            value = MathHelper.roundToNDecimalPlaces(rs.getDouble( 6 ), DECIMAL_PLACES) ;
            obsTime = getLongTime( rs.getString( 7 ) );

            timeValuePair = new TimeValuePair( obsTime, value );

            obsTimeSeries = (RegularObsTimeSeries) obsTimeSeriesMap.get( obsTimeSeriesDescriptor );
            
            if ( obsTimeSeries == null )
            {
                // Check for Air Temperature Daily MAX and MIN
                if ( (pe.equalsIgnoreCase("TA")) && 
                        ((extremum.equalsIgnoreCase("X")) || (extremum.equalsIgnoreCase("N"))) )
                {
                    obsTimeSeriesDescriptor.setEndTime( dailyEndTime );
                    obsTimeSeriesDescriptor.setStartTime( dailyStartTime );
                    
                    //System.out.println(header + "dailyEndTime = " +
                    //          DbTimeHelper.getDateTimeStringFromLongTime(dailyEndTime));

                    RegularObsTimeSeriesDescriptor newDescriptor = 
                                new RegularObsTimeSeriesDescriptor( obsTimeSeriesDescriptor );

                    obsTimeSeries = 
                            new RegularObsTimeSeries( newDescriptor, 24,
                                                      dailyTimeSlotPolicy );  // 24 hour data
                }
                else
                {
                    obsTimeSeriesDescriptor.setEndTime( hourlyEndTime );
                    obsTimeSeriesDescriptor.setStartTime( hourlyStartTime );
                    RegularObsTimeSeriesDescriptor newDescriptor = 
                            new RegularObsTimeSeriesDescriptor( obsTimeSeriesDescriptor );

                    obsTimeSeries = new RegularObsTimeSeries( newDescriptor, 1,
                                                              hourlyTimeSlotPolicy ); // hourly data
                }

                addTimeSeriesToOrderedList( obsTimeSeriesList, obsTimeSeries );
                
                obsTimeSeriesMap.put( obsTimeSeries.getDescriptor(), obsTimeSeries );
            }
            
            
            obsTimeSeries.addTimeValuePairIfBetterMatch( timeValuePair );

            // increment the number of records found
            recordsFound++;
            rsNext = rs.next();
        }
        
        System.out.println(header + "table = " + tableName +
                            " raw recordsFound = " + recordsFound);
   
        // Close the result set
        rs.close();
        
   
        return obsTimeSeriesList;
    } // end of select method
    
 
// -------------------------------------------------------------------------

/**
 * 
 * @param dateTime - date/time in milliseconds
 * @return - the date/time truncated to the day.  e.g. 10-30-2005 5:00pm becomes 10-30-2005 12:00am
 */
    private long truncateTimeInMillisToDay( long dateTime )
    {
        long dateTimeReturnValue = dateTime;
        
        dateTimeReturnValue /= PDCPreprocessorDataMgr.MILLIS_PER_DAY;
        dateTimeReturnValue *= PDCPreprocessorDataMgr.MILLIS_PER_DAY;
        
        return dateTimeReturnValue;
    }

    
    // --------------------------------------------------------------------------------------------
    public void addTimeSeriesToOrderedList(List obsTimeSeriesList, RegularObsTimeSeries newTimeSeries )
    {
        String header = "NonPrecipobsTableManager.addTimeSeriesToOrderedList(): ";
        RegularObsTimeSeries obsTimeSeries = null;
        String newLid = newTimeSeries.getDescriptor().getLid();
        int index = -1;
        boolean found = false;
        
        Comparator comparator = new RegularObsTimeSeriesComparator();
        
        index = Collections.binarySearch(obsTimeSeriesList, newTimeSeries, comparator);      // -4
        
        // Add the non-existent item to the list
        if (index < 0) 
        {
            obsTimeSeriesList.add(-index-1, newTimeSeries);
        }
        else if (index == 0)
        {
            obsTimeSeriesList.add(index, newTimeSeries);
            System.out.println(header + "Item added at 0 index in the list");
        }

        else
        {
            System.out.println(header + "Item aleady in the list");
        }
    }

    // --------------------------------------------------------------------------------------------
    
    
    public void addTimeSeriesToOrderedList_old( List obsTimeSeriesList, 
                                            RegularObsTimeSeries newTimeSeries )
    {
        RegularObsTimeSeries obsTimeSeries = null;
        String newLid = newTimeSeries.getDescriptor().getLid();
        int index = -1;
        boolean found = false;
        
        for ( int i = 0; i < obsTimeSeriesList.size(); i++ )
        {
            obsTimeSeries = (RegularObsTimeSeries) obsTimeSeriesList.get( i );
            
            if ( newLid.compareToIgnoreCase( obsTimeSeries.getDescriptor().getLid() ) < 0 )
            {
                index = i;
                found = true;
                break;
            }
        }
        
        if ( found )   //descriptor inserted at index 
        {
            obsTimeSeriesList.add( index, newTimeSeries );
        }
        else // descriptorlist is either empty or descriptor belongs at the end of the list
        {
            index = 0;
            obsTimeSeriesList.add( newTimeSeries );
        }
    }
    
    public long getLongTime( String timeString )
    {
        long longTime = 0;
        
        longTime = getLongTimeFromDateTimeString( timeString );
        return longTime;
    }

    public List select( String arg0 ) throws SQLException
    {
        throw new Error( "NonPrecipObsTable.select(String) should not be used" );
    }
} // end of HeightTable class
