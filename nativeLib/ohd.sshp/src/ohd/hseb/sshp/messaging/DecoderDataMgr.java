/*
 * Created on Jul 30, 2003
 *
 * 
 */
package ohd.hseb.sshp.messaging;

import ohd.hseb.db.*;
import ohd.hseb.sshp.FcstTsDescriptor;
import ohd.hseb.util.CodeTimer;
import ohd.hseb.util.FileLogger;
import ohd.hseb.ihfsdb.generated.FcstDischargeRecord;
import ohd.hseb.ihfsdb.generated.FcstDischargeTable;
import ohd.hseb.ihfsdb.generated.FcstOtherRecord;
import ohd.hseb.ihfsdb.generated.FcstOtherTable;
import ohd.hseb.ihfsdb.generated.MonthlyValuesRecord;
import ohd.hseb.ihfsdb.generated.MonthlyValuesTable;
import ohd.hseb.ihfsdb.generated.SacSmaParamsRecord;
import ohd.hseb.ihfsdb.generated.SacSmaParamsTable;
import ohd.hseb.ihfsdb.generated.SacSmaStateRecord;
import ohd.hseb.ihfsdb.generated.SacSmaStateTable;
import ohd.hseb.measurement.*;
import ohd.hseb.model.*;
import ohd.hseb.model.sacsma.*;

import java.util.*;
import java.sql.*;

// These are explicity imported in order to show what
// database tables are being used.


public class DecoderDataMgr
{
    
    public final double MISSING = -999.0;
    private static final int RECORD_COUNT_FOR_RECONNECT = 500;
    private FileLogger _logger = null;
    private Database _db = null;
    private String _connectionString = null;
    private int _timeSeriesRecordInsertionCounter = 0;
    
    private Map _oldFcstDischargeMap = new HashMap();   
      
// ------------------------------------------------------------------------
    
    public DecoderDataMgr( FileLogger logger )
    {
        this( null, logger );
    }

// ------------------------------------------------------------------------
    
    public DecoderDataMgr( String baseConnectionString, FileLogger logger )
    {
		_db = new Database();
		
		_logger = logger;
        
		_connectionString = baseConnectionString;
        	
		_db.connect( _connectionString );
	}
    
	// ------------------------------------------------------------------------
    
    public void disconnect()
    {
        _db.disconnect();   
    }
    
//  ------------------------------------------------------------------------
    
    public void finalize()
    {
        disconnect();	
    }
    
//	-------------------------------------------------------	

    private void logSQLException( SQLException exception )
    {
/*
        System.out.println("ERROR = " +
                           exception.getErrorCode() +  " " +
                           exception.getMessage());
*/
		_logger.log( "ERROR = " +
					 exception.getErrorCode() +  " " +
					 exception.getMessage());
    }
    
//  -------------------------------------------------------     

    public SacSmaParameters loadNewestSacSmaParams( String basinId, String source )
    {
        List sacSmaParamsRecordList = null;
        List sacSmaParamsList = null;
        SacSmaParamsTable table = new SacSmaParamsTable( _db );
        
        String where = " WHERE basin_id='" + basinId + "' AND source='" + source + "' ORDER BY validTime desc ";
        
        try 
        {
            sacSmaParamsRecordList = table.selectNRecords( where, 1 );
        }
        catch( SQLException e )
        {
            logSQLException(e);
        }
        sacSmaParamsList = getSacSmaParamsListFromSacSmaParamsRecordList( sacSmaParamsRecordList );
        
        if ( sacSmaParamsList.size() == 0 )
        {
        	return null;
        }
        else
        {
        	return (SacSmaParameters) sacSmaParamsList.get( 0 );
        }
    }
    
//  -------------------------------------------------------     

    private List getSacSmaParamsListFromSacSmaParamsRecordList( List sacSmaParamsRecordList )
    {
        List sacSmaParamsList = new ArrayList();
        
        for( int i = 0; i < sacSmaParamsRecordList.size(); i++ )
        {
            SacSmaParameters addSacSmaParams = new SacSmaParameters();
            SacSmaParamsRecord sacSmaParamsRecord = (SacSmaParamsRecord) sacSmaParamsRecordList.get( i );
            
            addSacSmaParams.setBasinId(sacSmaParamsRecord.getBasin_id());
            addSacSmaParams.setSource(sacSmaParamsRecord.getSource());
            addSacSmaParams.setValidTime(sacSmaParamsRecord.getValidtime());
            addSacSmaParams.setPostingTime(sacSmaParamsRecord.getPostingtime());
            addSacSmaParams.setUztwm(sacSmaParamsRecord.getUztwm());
            addSacSmaParams.setUzfwm(sacSmaParamsRecord.getUzfwm());
            addSacSmaParams.setUzk(sacSmaParamsRecord.getUzk());
            addSacSmaParams.setPctim(sacSmaParamsRecord.getPctim());
            addSacSmaParams.setAdimp(sacSmaParamsRecord.getAdimp());
            addSacSmaParams.setRiva(sacSmaParamsRecord.getRiva());
            addSacSmaParams.setZperc(sacSmaParamsRecord.getZperc());
            addSacSmaParams.setRexp(sacSmaParamsRecord.getRexp());
            addSacSmaParams.setLztwm(sacSmaParamsRecord.getLztwm());
            addSacSmaParams.setLzfsm(sacSmaParamsRecord.getLzfsm());
            addSacSmaParams.setLzfpm(sacSmaParamsRecord.getLzfpm());
            addSacSmaParams.setLzsk(sacSmaParamsRecord.getLzsk());
            addSacSmaParams.setLzpk(sacSmaParamsRecord.getLzpk());
            addSacSmaParams.setPfree(sacSmaParamsRecord.getPfree());
            addSacSmaParams.setRserv(sacSmaParamsRecord.getRserv());
            addSacSmaParams.setSide(sacSmaParamsRecord.getSide());
            
            addSacSmaParams.setPeadj(sacSmaParamsRecord.getPeadj());
            addSacSmaParams.setPxadj(sacSmaParamsRecord.getPxadj());
            addSacSmaParams.setEfc(sacSmaParamsRecord.getEfc());
        
            sacSmaParamsList.add( addSacSmaParams );
        }
    return sacSmaParamsList;    
    }

//  -------------------------------------------------------     

    
	private SacSmaParamsRecord getSacSmaParamsRecord( SacSmaParameters sacSmaParams )
	{
		SacSmaParamsRecord record = new SacSmaParamsRecord();
      
		record.setBasin_id( sacSmaParams.getBasinId() );
		record.setSource(sacSmaParams.getSource());
		record.setValidtime(sacSmaParams.getValidTime() );
		record.setPostingtime(sacSmaParams.getPostingTime());

		record.setUztwm(sacSmaParams.getUztwm());
		record.setUzfwm(sacSmaParams.getUzfwm());
		record.setUzk(sacSmaParams.getUzk());
		record.setPctim(sacSmaParams.getPctim());
		record.setAdimp(sacSmaParams.getAdimp());
		record.setRiva(sacSmaParams.getRiva());
		record.setZperc(sacSmaParams.getZperc());
		record.setRexp(sacSmaParams.getRexp());
		record.setLztwm(sacSmaParams.getLztwm());
		record.setLzfsm(sacSmaParams.getLzfsm());
		record.setLzfpm(sacSmaParams.getLzfpm());
		record.setLzsk(sacSmaParams.getLzsk());
		record.setLzpk(sacSmaParams.getLzpk());
		record.setPfree(sacSmaParams.getPfree());
		record.setRserv(sacSmaParams.getRserv());
		record.setSide(sacSmaParams.getSide());
		record.setPeadj(sacSmaParams.getPeadj());
		record.setPxadj(sacSmaParams.getPxadj());
		record.setEfc(sacSmaParams.getEfc());
        
		return record;
	}


//  ------------------------------------------------------- 


    public void saveParams( SacSmaParameters sacSmaParams )
    {
        SacSmaParamsTable table = new SacSmaParamsTable( _db );
        SacSmaParamsRecord record = getSacSmaParamsRecord( sacSmaParams );
        
        try
        {   
            table.insertOrUpdate( record ); 
			_logger.log( "     Successfully inserted or updated Params for " + record.getBasin_id() );
        }
        catch ( SQLException e )
        {
            logSQLException(e);
            _logger.log("failed to insert or update for " + record);
        }   
    }   
//  -------------------------------------------------------     

    public void saveState( SacSmaState sacSmaState )
    {
        SacSmaStateTable table = new SacSmaStateTable( _db );
        SacSmaStateRecord record = new SacSmaStateRecord();

        record.setBasin_id( sacSmaState.getBasinId() );
        record.setSource( sacSmaState.getSource() );
        record.setValidtime( sacSmaState.getValidTime() );
        record.setBasistime( sacSmaState.getBasisTime() );
        record.setPostingtime( sacSmaState.getPostingTime() );
        
        record.setUztwc( sacSmaState.getUztwc() );
        record.setUzfwc( sacSmaState.getUzfwc() );
        record.setLztwc( sacSmaState.getLztwc() );
        record.setLzfsc( sacSmaState.getLzfsc() );
        record.setLzfpc( sacSmaState.getLzfpc() );
        record.setAdimc( sacSmaState.getAdimc() );

        try
        {       
            table.insertOrUpdate( record );
			_logger.log( "     Successfully inserted or updated State for " + record.getBasin_id() );
        }
        catch ( SQLException e )
        {
            logSQLException(e);
            _logger.log("failed to insert or update for " + record);
        }   
    }

//  -------------------------------------------------------     

       public void saveMonthlyValues( MonthlyValues monthlyValues )
       {
           MonthlyValuesTable table = new MonthlyValuesTable( _db );
           MonthlyValuesRecord record = new MonthlyValuesRecord();
           long currentTime = System.currentTimeMillis();
           double[] valueArray = monthlyValues.getValueArray();

           record.setLid( monthlyValues.getBasinId() );
           record.setPe( monthlyValues.getPe() );
           record.setDur( monthlyValues.getDur() );
           record.setTs( monthlyValues.getTs() );
           record.setExtremum( monthlyValues.getExtremum() );
           if ( monthlyValues.isAdjustment() )
           {
               record.setAdjustment( "Y" );
           }
           else
           {
               record.setAdjustment( "N" );
           }
           record.setPostingtime( currentTime );
           record.setJan_value( valueArray[ 0 ] );
           record.setFeb_value( valueArray[ 1 ] );
           record.setMar_value( valueArray[ 2 ] );
           record.setApr_value( valueArray[ 3 ] );
           record.setMay_value( valueArray[ 4 ] );
           record.setJun_value( valueArray[ 5 ] );
           record.setJul_value( valueArray[ 6 ] );
           record.setAug_value( valueArray[ 7 ] );
           record.setSep_value( valueArray[ 8 ] );
           record.setOct_value( valueArray[ 9 ] );
           record.setNov_value( valueArray[ 10 ] );
           record.setDec_value( valueArray[ 11 ] );
           try
           {       
               table.insertOrUpdate( record );
   			   _logger.log( "     Successfully inserted or updated Monthly Values for " + record.getLid() );
           }
           catch ( SQLException e )
           {
               logSQLException(e);
               _logger.log("failed to insert or update for " + record);
           }   
       }
//  -------------------------------------------------------     
	public void saveFcstTimeSeries( FcstTsDescriptor descriptor, RegularTimeSeries timeSeries )
	{
		_timeSeriesRecordInsertionCounter = 0;
		
		if ( descriptor.getTableName().equalsIgnoreCase( "FcstDischarge" ) )
		{
			saveFcstRunoffTs( descriptor, timeSeries );    
		}
		else if ( descriptor.getTableName().equalsIgnoreCase( "FcstOther" ) )
		{
			saveFcstOtherTs( descriptor, timeSeries );   
		}
	}

//	-------------------------------------------------------     

	private void saveFcstOtherTs( FcstTsDescriptor descriptor, RegularTimeSeries timeSeries )
	{
		FcstOtherTable table = new FcstOtherTable(_db);
		FcstOtherRecord record = new FcstOtherRecord();
        
        FcstOtherRecord oldRecord = null;

        String space = "     ";
        long startTime = 0;
        long endTime = 0;
        long interval = 0;
        long maxBasisTime = 0;
        final long HOUR_IN_MILLIS = 60 * 60 * 1000;
        CodeTimer timer1 = new CodeTimer( _logger );
 
        MeasuringUnit dbRunoffUnit = MeasuringUnit.inches;


        timer1.start();

        startTime = timeSeries.getStartTime();
        endTime = timeSeries.getEndTime();
        interval = timeSeries.getIntervalInHours() * HOUR_IN_MILLIS;
    


        //initialize the record key fields to save from the descriptor
        setRecordFromDescriptor(record, descriptor);

        //delete records from the same or previous basistime
        String deleteWhereString = null;
        deleteWhereString = getDeleteWhereString( descriptor );

        try
        {
            table.delete( deleteWhereString );
        }
        catch ( SQLException e )
        {
            logSQLException(e);
        }
        
        
        
        int insertCount = 0;

        //insert records
        for (long time = startTime; time <= endTime; time += interval)
        {
            reconnectIfNeeded();

            Measurement newMeasurement = timeSeries.getMeasurementByTime(time);
            double value = newMeasurement.getValue(dbRunoffUnit);
    
            record.setValue(value);
            record.setValidtime(time);

            try
            {
                table.insert(record);
                _timeSeriesRecordInsertionCounter++; //used for reconnections
                insertCount++;
            }
    
            catch ( SQLException e )
            {
                logSQLException( e );
                _logger.log( space + "error with insert for FcstOther " + record);
                _logger.log( space + "insertCount = " + insertCount);
            }
        }

        _logger.log( space + insertCount + " records inserted" + 
                    " for " + record.getLid() + "." );
    
       
	}
//  -------------------------------------------------------     

  
//	-------------------------------------------------------     
    private void setRecordFromDescriptor(FcstDischargeRecord record, FcstTsDescriptor descriptor)
    {
        record.setLid(descriptor.getLid());
        record.setPe(descriptor.getPe());
        record.setTs(descriptor.getTs());
        record.setExtremum(descriptor.getExtremum());
        record.setShef_qual_code(descriptor.getShef_qual_code());
        record.setProduct_id(descriptor.getProduct_id());
        record.setProbability(descriptor.getProbability());
        record.setQuality_code(descriptor.getQuality_code());
        record.setRevision(descriptor.getRevision());
        record.setDur(descriptor.getDur());
        record.setProducttime(descriptor.getProducttime());
        record.setBasistime(descriptor.getBasistime());
        record.setPostingtime(System.currentTimeMillis());
        
        return;
        
    }

//  -------------------------------------------------------     
    private void setRecordFromDescriptor(FcstOtherRecord record, FcstTsDescriptor descriptor)
    {
           record.setLid(descriptor.getLid());
           record.setPe(descriptor.getPe());
           record.setTs(descriptor.getTs());
           record.setExtremum(descriptor.getExtremum());
           record.setShef_qual_code(descriptor.getShef_qual_code());
           record.setProduct_id(descriptor.getProduct_id());
           record.setProbability(descriptor.getProbability());
           record.setQuality_code(descriptor.getQuality_code());
           record.setRevision(descriptor.getRevision());
           record.setDur(descriptor.getDur());
           record.setProducttime(descriptor.getProducttime());
           record.setBasistime(descriptor.getBasistime());
           record.setPostingtime(System.currentTimeMillis());
        
           return;
        
    }

    

//	-------------------------------------------------------     

	private void saveFcstRunoffTs( FcstTsDescriptor descriptor, RegularTimeSeries timeSeries )
	{
		FcstDischargeTable table = new FcstDischargeTable( _db );
		FcstDischargeRecord record = new FcstDischargeRecord();
		   
		String space = "     ";
		long startTime = 0;
		long endTime = 0;
		long interval = 0;
		long maxBasisTime = 0;
		final long HOUR_IN_MILLIS = 60 * 60 * 1000;
		CodeTimer timer1 = new CodeTimer( _logger );
     
		MeasuringUnit dbRunoffUnit = MeasuringUnit.inches;

    
		timer1.start();
    
		startTime = timeSeries.getStartTime();
		endTime = timeSeries.getEndTime();
		interval = timeSeries.getIntervalInHours() * HOUR_IN_MILLIS;
        
    
    
		//initialize the record key fields to save from the descriptor
		setRecordFromDescriptor(record, descriptor);
        
        
        
        //delete records from the same or previous basistime
        String deleteWhereString = null;
        
		deleteWhereString = getDeleteWhereString( descriptor );
		
		try
		{
			table.delete( deleteWhereString );
		}
		catch ( SQLException e )
		{
            logSQLException(e);
		}
        
        
        //insert all records
		int insertCount = 0;	 
		for (long time = startTime; time <= endTime; time += interval)
		{
			reconnectIfNeeded();

			Measurement newMeasurement = timeSeries.getMeasurementByTime(time);
			double value = newMeasurement.getValue(dbRunoffUnit);
            
            record.setValue(value);
            record.setValidtime(time);

			try
			{
				table.insert(record);
				_timeSeriesRecordInsertionCounter++;  //used for reconnections
				insertCount++;
			}
            
			catch ( SQLException e )
			{
				logSQLException( e );
				_logger.log( space + "error with insert for FcstOther " + record);
				_logger.log( space + "insertCount = " + insertCount);
			}
		}
        
	   
        _logger.log( space + insertCount + " records inserted" + 
                  " for " + record.getLid() + "." );
        
        
        
        
	} //end saveFcstDischarge


// ------------------------------------------

	private String getDeleteWhereString( FcstTsDescriptor descriptor )
	{
		String whereString = 
				 "WHERE lid = '" + descriptor.getLid() + "'" 
				  + " AND pe = '" + descriptor.getPe() + "'" 
				  + " AND dur = '" + descriptor.getDur() + "'" 
				  + " AND ts = '" + descriptor.getTs() + "'" 
				  + " AND extremum = '" + descriptor.getExtremum() + "'" 
				  + " AND probability = '" + descriptor.getProbability() + "'"
                  + " AND product_id = '" +  descriptor.getProduct_id() + "'" 
				  + " AND basistime <= '" + DbTimeHelper.getDateTimeStringFromLongTime( descriptor.getBasistime() ) + "'" ; 
			 
		 return whereString;
	}
	
     
 
//  -------------------------------------------------------     



    void addRecordToMap(FcstDischargeRecord record)
    {
        Long key = new Long(record.getValidtime());
        Object value = record;

        _oldFcstDischargeMap.put(key, value);

        return;
    }  
   
//-------------------------------------------------------     
     
      
   
      // -------------------------------------------------------------------------------
    
 
//  -------------------------------------------------------     
    

//  -------------------------------------------------------     
 	
	private void testFcstOtherRecordInsertion()
	{
		FcstOtherRecord record = null;
		FcstOtherTable table = new FcstOtherTable( _db );
		String where = " WHERE PE='EA' ";
		
		try
		{
			List recordList = table.select( where );
			if ( recordList.size() > 0 )
			{
				record = (FcstOtherRecord) recordList.get( 0 );
				System.out.println( "DecoderDataMgr.testFcst: record: " + record );
				table.insertOrUpdate( record );
			}
		}
		catch ( SQLException e )
		{
			e.printStackTrace();
		}
		
	}

//  -------------------------------------------------------     
 	
	private void reconnectIfNeeded()
	{
	    // This was old code that was written only because the Informix JDBC driver had a memory leak that
	    // could only be fixed by disconnecting and reconnecting.
	    /*
		int total = _timeSeriesRecordInsertionCounter;
		if ( ( total > 0 ) && ( total % RECORD_COUNT_FOR_RECONNECT == 0 ) )
		{
			reconnect();
		}
		*/
	}

//  -------------------------------------------------------     
 	
	private void reconnect()
	{
		_db.disconnect();
		_db.connect( _connectionString );
		_logger.log( "     Reconnected to the Database for performance reasons" );
	}

//  -------------------------------------------------------     
 
	
	public static void main( String args[] )
	{
		DecoderDataMgr decoderMgr = new DecoderDataMgr(new FileLogger( "DecoderDataMgrtesting.log" ) );
		decoderMgr.testFcstOtherRecordInsertion();
	}
} //end class DecoderDataMgr
