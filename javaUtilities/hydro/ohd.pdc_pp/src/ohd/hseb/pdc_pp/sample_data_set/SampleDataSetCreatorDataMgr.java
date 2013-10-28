package ohd.hseb.pdc_pp.sample_data_set;

import java.sql.SQLException;
import java.util.List;

import ohd.hseb.db.Database;
import ohd.hseb.ihfsdb.generated.CurPCRecord;
import ohd.hseb.ihfsdb.generated.CurPCTable;
import ohd.hseb.ihfsdb.generated.CurPPRecord;
import ohd.hseb.ihfsdb.generated.CurPPTable;
import ohd.hseb.ihfsdb.generated.DischargeRecord;
import ohd.hseb.ihfsdb.generated.DischargeTable;
import ohd.hseb.ihfsdb.generated.HeightRecord;
import ohd.hseb.ihfsdb.generated.HeightTable;
import ohd.hseb.ihfsdb.generated.LakeRecord;
import ohd.hseb.ihfsdb.generated.LakeTable;
import ohd.hseb.ihfsdb.generated.LocationRecord;
import ohd.hseb.ihfsdb.generated.LocationTable;
import ohd.hseb.ihfsdb.generated.RatingRecord;
import ohd.hseb.ihfsdb.generated.RatingTable;
import ohd.hseb.ihfsdb.generated.RiverstatRecord;
import ohd.hseb.ihfsdb.generated.RiverstatTable;
import ohd.hseb.ihfsdb.generated.SnowRecord;
import ohd.hseb.ihfsdb.generated.SnowTable;
import ohd.hseb.ihfsdb.generated.StnClassRecord;
import ohd.hseb.ihfsdb.generated.StnClassTable;
import ohd.hseb.ihfsdb.generated.TemperatureRecord;
import ohd.hseb.ihfsdb.generated.TemperatureTable;
import ohd.hseb.ihfsdb.generated.WeatherRecord;
import ohd.hseb.ihfsdb.generated.WeatherTable;
import ohd.hseb.ihfsdb.generated.WindRecord;
import ohd.hseb.ihfsdb.generated.WindTable;
import ohd.hseb.util.Logger;
import ohd.hseb.util.TimeHelper;

public class SampleDataSetCreatorDataMgr
{
    private Database _db = null;
    private Logger _logger = null;
    private static String OS = System.getProperty("os.name").toLowerCase();
    private String _connectionString = null;
    private long _endTime = System.currentTimeMillis();
    private long _TOTHEndTime = TimeHelper.roundTimeInMillisToNearestHour( _endTime );
    private long _startTime = _TOTHEndTime - ( 24 * TimeHelper.MILLIS_PER_HOUR );
    private String _lid1 = "1PDC1";
    private String _lid2 = "1PDC2";
    
    public SampleDataSetCreatorDataMgr( String baseConnectionString, Logger logger )
    {
        super();
        
        _db = new Database();
        _logger = logger;

        if ( baseConnectionString == null )
        {
            baseConnectionString = getBaseConnectionString();  
        }
        _connectionString = baseConnectionString;
        _db.connectWithDriverSearch( _connectionString );
    }
    
    public void create_test_location()
    {
        LocationRecord locationRecord1 = new LocationRecord();
        LocationRecord locationRecord2 = new LocationRecord();
        StnClassRecord stnClassRecord1 = new StnClassRecord();
        StnClassRecord stnClassRecord2 = new StnClassRecord();
        RiverstatRecord riverStatRecord1 = new RiverstatRecord();
        RiverstatRecord riverStatRecord2 = new RiverstatRecord();
        RatingRecord ratingRecord1 = new RatingRecord();
        RatingRecord ratingRecord2 = new RatingRecord();
        RatingTable ratingTable = new RatingTable( _db );
        LocationTable locationTable = new LocationTable( _db );
        StnClassTable stnClassTable = new StnClassTable( _db );
        RiverstatTable riverstatTable = new RiverstatTable( _db ); 
        List recordList = null;
        
        locationRecord1.setLid( _lid1 );
        locationRecord1.setCounty( "BRYAN" );
        locationRecord1.setDetail( "1 W" );
        locationRecord1.setElev( 503 );
        locationRecord1.setHsa( "OUN" );
        locationRecord1.setLat( 32.34343456 );
        locationRecord1.setLon( 97.232341123 );
        locationRecord1.setLremark( "US HWY 70  NR LT BANK ON DS SIDE OF BRIDGE OVER BLUE RIVER, 1 MI. W OF BLUE, OK AND 8 MI. E OF DURANT, OKDURANT/BRYAN COUNTY CEMA IS BACKUP OBSERVER TO DCP" );
        locationRecord1.setLrevise( System.currentTimeMillis() );
        locationRecord1.setName( "PDC Test Site 1" );
        locationRecord1.setNetwork( "B" );
        locationRecord1.setRb( "RED 3" );
        locationRecord1.setRfc( "ABRFC" );
        locationRecord1.setState( "OK" );
        locationRecord1.setWfo( "OUN" );
        locationRecord1.setTzone( "MST7" );
        stnClassRecord1.setLid( _lid1 );
        stnClassRecord1.setDisp_class( "RPTO" );
        stnClassRecord1.setDcp( "T" );
        stnClassRecord1.setObserver( "T" );
        stnClassRecord1.setTelem_type( "NONE" );
        riverStatRecord1.setLid( _lid1 );
        riverStatRecord1.setFq( 42000 );
        riverStatRecord1.setFs( 22 );
        ratingRecord1.setLid( _lid1 );
        
        
        locationRecord2.setLid( _lid2 );
        locationRecord2.setCounty( "BRYAN" );
        locationRecord2.setDetail( "1 W" );
        locationRecord2.setElev( 503 );
        locationRecord2.setHsa( "OUN" );
        locationRecord2.setLat( 30.34343456 );
        locationRecord2.setLon( 99.232341123 );
        locationRecord2.setLremark( "US HWY 70  NR LT BANK ON DS SIDE OF BRIDGE OVER BLUE RIVER, 1 MI. W OF BLUE, OK AND 8 MI. E OF DURANT, OKDURANT/BRYAN COUNTY CEMA IS BACKUP OBSERVER TO DCP" );
        locationRecord2.setLrevise( System.currentTimeMillis() );
        locationRecord2.setName( "PDC Test Site 2" );
        locationRecord2.setNetwork( "B" );
        locationRecord2.setRb( "RED 3" );
        locationRecord2.setRfc( "ABRFC" );
        locationRecord2.setState( "OK" );
        locationRecord2.setWfo( "OUN" );
        locationRecord2.setTzone( "MST7" );
        stnClassRecord2.setLid( _lid2 );
        stnClassRecord2.setDisp_class( "RPTO" );
        stnClassRecord2.setDcp( "T" );
        stnClassRecord2.setObserver( "T" );
        stnClassRecord2.setTelem_type( "NONE" );
        riverStatRecord2.setLid( _lid2 );
        riverStatRecord2.setFq( 5750 );
        riverStatRecord2.setFs( 25 );
        ratingRecord2.setLid( _lid2 );

        
        try
        {
            recordList = locationTable.select( "WHERE LID = '" + _lid1 + "' or LID = '" + _lid2 + "' " );
            
            if ( recordList.size() == 0 )
            {
                System.out.println( "Inserting Location/StnClass/Riverstat/Rating records for locations \"" + _lid1 + "\" and \"" + _lid2 + "\" " );
                locationTable.insert( locationRecord1 );
                locationTable.insert( locationRecord2 );
                stnClassTable.insert( stnClassRecord1 );
                stnClassTable.insert( stnClassRecord2 );
                riverstatTable.insert( riverStatRecord1 );
                riverstatTable.insert( riverStatRecord2 );
                for ( double i = 0; i < 5.0; i+=0.1 )
                {
                    ratingRecord1.setStage( i );
                    ratingRecord1.setDischarge( i*1.5 );
                    ratingTable.insert( ratingRecord1 );
                }
                
                for ( double i = 0; i < 5.0; i+=0.2 )
                {
                    ratingRecord2.setStage( i );
                    ratingRecord2.setDischarge( i*2.5 );
                    ratingTable.insert( ratingRecord2 );
                }
            }
            else
            {
                System.out.println( "Test locations already exist in the Location/StnClass/Riverstat/Rating tables...\n" + 
                                    "Removing and reinserting Location/StnClass/Riverstat/Rating records for locations " +
                                    "\"" + _lid1 + "\" and \"" + _lid2 + "\" " );
                ratingTable.delete( "WHERE lid = '" + _lid1 + "' OR lid = '" + _lid2 + "'" );
                riverstatTable.delete( riverStatRecord1 );
                riverstatTable.delete( riverStatRecord2 );
                stnClassTable.delete( stnClassRecord1 );
                stnClassTable.delete( stnClassRecord2 );
                locationTable.delete( locationRecord1 );
                locationTable.delete( locationRecord2 );
                locationTable.insert( locationRecord1 );
                locationTable.insert( locationRecord2 );
                stnClassTable.insert( stnClassRecord1 );
                stnClassTable.insert( stnClassRecord2 );
                riverstatTable.insert( riverStatRecord1 );
                riverstatTable.insert( riverStatRecord2 );
                for ( double i = 0; i < 5.0; i+=0.1 )
                {
                    ratingRecord1.setStage( i );
                    ratingRecord1.setDischarge( i*1.5 );
                    ratingTable.insert( ratingRecord1 );
                }
                
                for ( double i = 0; i < 5.0; i+=0.2 )
                {
                    ratingRecord2.setStage( i );
                    ratingRecord2.setDischarge( i*2.5 );
                    ratingTable.insert( ratingRecord2 );
                }
            }
        }
        catch (SQLException e )
        {
            e.printStackTrace();
        }
    }
    
    public void create_test_data()
    {
        deleteTestData();
        createHeightData();
        createTemperatureData();
        createWindData();
        createSnowData();
        createWeatherData();
        createLakeData();
        createDischargeData();
        createPCData();
        createPPData();
    }
    
    public void removeAllData()
    {
        RatingTable ratingTable = new RatingTable( _db );
        LocationTable locationTable = new LocationTable( _db );
        StnClassTable stnClassTable = new StnClassTable( _db );
        RiverstatTable riverstatTable = new RiverstatTable( _db ); 

        System.out.println( "Deleting data from the Location/Riverstat/StnClass/Rating tables" );
        deleteTestData();
        try
        {
            ratingTable.delete( "WHERE lid = '" + _lid1 + "' OR lid = '" + _lid2 + "'" );
            stnClassTable.delete( "WHERE lid = '" + _lid1 + "' OR lid = '" + _lid2 + "'" );
            riverstatTable.delete( "WHERE lid = '" + _lid1 + "' OR lid = '" + _lid2 + "'" );
            locationTable.delete( "WHERE lid = '" + _lid1 + "' OR lid = '" + _lid2 + "'" );
        }
        catch (SQLException e )
        {
            e.printStackTrace();
        }
    }
    private void deleteTestData()
    {
        HeightTable heightTable = new HeightTable( _db );
        TemperatureTable tempTable = new TemperatureTable( _db );
        WindTable windTable = new WindTable( _db );
        SnowTable snowTable = new SnowTable( _db );
        WeatherTable weatherTable = new WeatherTable( _db );
        LakeTable lakeTable = new LakeTable( _db );
        DischargeTable dischargeTable = new DischargeTable( _db );
        CurPCTable curPCTable = new CurPCTable( _db );
        CurPPTable curPPTable = new CurPPTable( _db );
        System.out.println( "Clearing out old test data" );
        try
        {
            heightTable.delete( "WHERE LID='" + _lid1 + "' OR LID='" + _lid2 + "'" );
            tempTable.delete( "WHERE LID='" + _lid1 + "' OR LID='" + _lid2 + "'" );
            windTable.delete( "WHERE LID='" + _lid1 + "' OR LID='" + _lid2 + "'" );
            snowTable.delete( "WHERE LID='" + _lid1 + "' OR LID='" + _lid2 + "'" );
            weatherTable.delete( "WHERE LID='" + _lid1 + "' OR LID='" + _lid2 + "'" );
            lakeTable.delete( "WHERE LID='" + _lid1 + "' OR LID='" + _lid2 + "'" );
            dischargeTable.delete( "WHERE LID='" + _lid1 + "' OR LID='" + _lid2 + "'" );
            curPCTable.delete( "WHERE LID='" + _lid1 + "' OR LID='" + _lid2 + "'" );
            curPPTable.delete( "WHERE LID='" + _lid1 + "' OR LID='" + _lid2 + "'" );
        }
        catch ( SQLException e )
        {
            e.printStackTrace();
        }
    }
    
    private void createHeightData()
    {
        System.out.println( "Inserting test Height data for locations \"" + _lid1 + "\" and \"" + _lid2 + "\" " );
        HeightRecord record = new HeightRecord();
        HeightTable heightTable = new HeightTable( _db );
        short dur = 1001;
        short revision = 0;
        double value = 0.0;
        record.setLid( _lid1 );
        record.setDur( dur );
        record.setPe( "HG" );
        record.setTs( "RG" );
        record.setExtremum( "Z" );
        record.setShef_qual_code( "Z" );
        record.setQuality_code( 1879048191 );
        record.setRevision( revision );
        record.setProduct_id( "KWOHRRSOUN" );
        record.setProducttime( _endTime );
        record.setPostingtime( _endTime );
        
        for ( long indexTime = _startTime; indexTime <= _TOTHEndTime; indexTime += TimeHelper.MILLIS_PER_HOUR )
        {
            record.setPe( "HG" );
            record.setObstime( indexTime );
            record.setValue( value );
            try
            {
                heightTable.insert( record );
                record.setPe( "HP" );
                record.setValue( value*1000 );
                heightTable.insert( record );
            }
            catch ( SQLException e )
            {
                e.printStackTrace();
            }
            value += 0.1;
        }
        
        record.setLid( _lid2 );
        value = 0.2;
        
        for ( long indexTime = _startTime; indexTime <= _TOTHEndTime; indexTime += TimeHelper.MILLIS_PER_HOUR )
        {
            record.setPe( "HG" );
            record.setObstime( indexTime );
            record.setValue( value );
            try
            {
                heightTable.insert( record );
                record.setPe( "HP" );
                record.setValue( value*1000 );
                heightTable.insert( record );
            }
            catch ( SQLException e )
            {
                e.printStackTrace();
            }
            value += 0.2;
        }
    }
    
    private void createTemperatureData()
    {
        System.out.println( "Inserting test Temperature data for locations \"" + _lid1 + "\" and \"" + _lid2 + "\" " );
        TemperatureRecord record = new TemperatureRecord();
        TemperatureTable temperatureTable = new TemperatureTable( _db );
        short dur = 1001;
        short revision = 0;
        double value = 50.0;
        
        record.setLid( _lid1 );
        record.setDur( dur );
        record.setPe( "TA" );
        record.setTs( "RG" );
        record.setExtremum( "Z" );
        record.setShef_qual_code( "Z" );
        record.setQuality_code( 1879048191 );
        record.setRevision( revision );
        record.setProduct_id( "KWOHRRSOUN" );
        record.setProducttime( _endTime );
        record.setPostingtime( _endTime );
        
        for ( long indexTime = ( _startTime - 24 * TimeHelper.MILLIS_PER_HOUR ); indexTime <= _TOTHEndTime; indexTime += TimeHelper.MILLIS_PER_HOUR )
        {
            record.setExtremum( "Z" );
            record.setObstime( indexTime );
            record.setValue( value );
            try
            {
                temperatureTable.insert( record );
                if ( indexTime == _startTime )
                {
                    record.setExtremum( "N" );
                    temperatureTable.insert( record );
                }
                if ( indexTime == _TOTHEndTime )
                {
                    record.setExtremum( "X" );
                    temperatureTable.insert( record );
                }
            }
            catch ( SQLException e )
            {
                e.printStackTrace();
            }
            value += 1;
        }
        
        value = 2;
        
        record.setPe( "TD" );
        
        for ( long indexTime = _startTime-(24*TimeHelper.MILLIS_PER_HOUR); indexTime <= _TOTHEndTime; indexTime += TimeHelper.MILLIS_PER_HOUR )
        {
            record.setValue( value );
            try
            {
                record.setObstime( indexTime );
                temperatureTable.insert( record );
            }
            catch ( SQLException e )
            {
                e.printStackTrace();
            }
            value++;
        }
        
        record.setPe( "TA" );
        record.setLid( _lid2 );
        value = 60;
        
        for ( long indexTime = ( _startTime - 24 * TimeHelper.MILLIS_PER_HOUR ); indexTime <= _TOTHEndTime; indexTime += TimeHelper.MILLIS_PER_HOUR )
        {
            record.setExtremum( "Z" );
            record.setObstime( indexTime );
            record.setValue( value );
            try
            {
                temperatureTable.insert( record );
                if ( indexTime == _startTime )
                {
                    record.setExtremum( "N" );
                    temperatureTable.insert( record );
                }
                if ( indexTime == _TOTHEndTime )
                {
                    record.setExtremum( "X" );
                    temperatureTable.insert( record );
                }
            }
            catch ( SQLException e )
            {
                e.printStackTrace();
            }
            value += 2;
        }
        
        value = 2;
        
        record.setPe( "TD" );
        for ( long indexTime = _startTime-(24*TimeHelper.MILLIS_PER_HOUR); indexTime <= _TOTHEndTime; indexTime += TimeHelper.MILLIS_PER_HOUR )
        {
            record.setValue( value );
            record.setObstime( indexTime );
            try
            {
                temperatureTable.insert( record );
            }
            catch ( SQLException e )
            {
                e.printStackTrace();
            }
            value += 2;
        }

    }

    private void createWindData()
    {
        System.out.println( "Inserting test Wind data for locations \"" + _lid1 + "\" and \"" + _lid2 + "\" " );
        WindRecord record = new WindRecord();
        WindTable windTable = new WindTable( _db );
        short dur = 1001;
        short revision = 0;
        double value = 0.0;
        record.setLid( _lid1 );
        record.setDur( dur );
        record.setPe( "US" );
        record.setTs( "RG" );
        record.setExtremum( "Z" );
        record.setShef_qual_code( "Z" );
        record.setQuality_code( 1879048191 );
        record.setRevision( revision );
        record.setProduct_id( "KWOHRRSOUN" );
        record.setProducttime( _endTime );
        record.setPostingtime( _endTime );
        
        for ( long indexTime = _startTime; indexTime <= _TOTHEndTime; indexTime += TimeHelper.MILLIS_PER_HOUR )
        {
            record.setPe( "US" );
            record.setObstime( indexTime );
            record.setValue( value );
            try
            {
                windTable.insert( record );
            }
            catch ( SQLException e )
            {
                e.printStackTrace();
            }
            record.setPe( "UD" );
            try
            {
                windTable.insert( record );
            }
            catch ( SQLException e )
            {
                e.printStackTrace();
            }
            value += 0.1;
        }
        
        record.setLid( _lid2 );
        value = 0.2;
        
        for ( long indexTime = _startTime; indexTime <= _TOTHEndTime; indexTime += TimeHelper.MILLIS_PER_HOUR )
        {
            record.setPe( "US" );
            record.setObstime( indexTime );
            record.setValue( value );
            try
            {
                windTable.insert( record );
            }
            catch ( SQLException e )
            {
                e.printStackTrace();
            }
            record.setPe( "UD" );
            try
            {
                windTable.insert( record );
            }
            catch ( SQLException e )
            {
                e.printStackTrace();
            }
            value += 0.2;
        }
    }
    
    private void createSnowData()
    {
        System.out.println( "Inserting test Snow data for locations \"" + _lid1 + "\" and \"" + _lid2 + "\" " );
        SnowRecord record = new SnowRecord();
        SnowTable snowTable = new SnowTable( _db );
        short dur = 1001;
        short revision = 0;
        double value = 0.0;
        record.setLid( _lid1 );
        record.setDur( dur );
        record.setPe( "SW" );
        record.setTs( "RG" );
        record.setExtremum( "Z" );
        record.setShef_qual_code( "Z" );
        record.setQuality_code( 1879048191 );
        record.setRevision( revision );
        record.setProduct_id( "KWOHRRSOUN" );
        record.setProducttime( _endTime );
        record.setPostingtime( _endTime );
        
        for ( long indexTime = ( _startTime - 24*TimeHelper.MILLIS_PER_HOUR ); indexTime <= _TOTHEndTime; indexTime += TimeHelper.MILLIS_PER_HOUR )
        {
            record.setObstime( indexTime );
            record.setValue( value );
            try
            {
                snowTable.insert( record );
            }
            catch ( SQLException e )
            {
                e.printStackTrace();
            }
            value += 0.1;
        }
        
        record.setLid( _lid2 );
        value = 0.2;
        
        for ( long indexTime = ( _startTime - 24*TimeHelper.MILLIS_PER_HOUR ); indexTime <= _TOTHEndTime; indexTime += TimeHelper.MILLIS_PER_HOUR )
        {
            record.setObstime( indexTime );
            record.setValue( value );
            try
            {
                snowTable.insert( record );
            }
            catch ( SQLException e )
            {
                e.printStackTrace();
            }
            value += 0.2;
        }
    }

    private void createWeatherData()
    {
        System.out.println( "Inserting test Weather data for locations \"" + _lid1 + "\" and \"" + _lid2 + "\" " );
        WeatherRecord record = new WeatherRecord();
        WeatherTable weatherTable = new WeatherTable( _db );
        short dur = 1001;
        short revision = 0;
        double value = 0.0;
        record.setLid( _lid1 );
        record.setDur( dur );
        record.setPe( "XR" );
        record.setTs( "RG" );
        record.setExtremum( "Z" );
        record.setShef_qual_code( "Z" );
        record.setQuality_code( 1879048191 );
        record.setRevision( revision );
        record.setProduct_id( "KWOHRRSOUN" );
        record.setProducttime( _endTime );
        record.setPostingtime( _endTime );
        
        for ( long indexTime = _startTime; indexTime <= _TOTHEndTime; indexTime += TimeHelper.MILLIS_PER_HOUR )
        {
            record.setObstime( indexTime );
            record.setValue( value );
            try
            {
                weatherTable.insert( record );
            }
            catch ( SQLException e )
            {
                e.printStackTrace();
            }
            value += 0.1;
        }
        
        record.setLid( _lid2 );
        value = 0.2;
        
        for ( long indexTime = _startTime; indexTime <= _TOTHEndTime; indexTime += TimeHelper.MILLIS_PER_HOUR )
        {
            record.setObstime( indexTime );
            record.setValue( value );
            try
            {
                weatherTable.insert( record );
            }
            catch ( SQLException e )
            {
                e.printStackTrace();
            }
            value += 0.2;
        }
    }

    private void createLakeData()
    {
        System.out.println( "Inserting test Lake data for locations \"" + _lid1 + "\" and \"" + _lid2 + "\" " );
        LakeRecord record = new LakeRecord();
        LakeTable lakeTable = new LakeTable( _db );
        short dur = 1001;
        short revision = 0;
        double value = 0.0;
        record.setLid( _lid1 );
        record.setDur( dur );
        record.setPe( "LS" );
        record.setTs( "RG" );
        record.setExtremum( "Z" );
        record.setShef_qual_code( "Z" );
        record.setQuality_code( 1879048191 );
        record.setRevision( revision );
        record.setProduct_id( "KWOHRRSOUN" );
        record.setProducttime( _endTime );
        record.setPostingtime( _endTime );
        
        for ( long indexTime = _startTime; indexTime <= _TOTHEndTime; indexTime += TimeHelper.MILLIS_PER_HOUR )
        {
            record.setObstime( indexTime );
            record.setValue( value );
            try
            {
                lakeTable.insert( record );
            }
            catch ( SQLException e )
            {
                e.printStackTrace();
            }
            value += 0.1;
        }
        
        record.setLid( _lid2 );
        value = 0.2;
        
        for ( long indexTime = _startTime; indexTime <= _TOTHEndTime; indexTime += TimeHelper.MILLIS_PER_HOUR )
        {
            record.setObstime( indexTime );
            record.setValue( value );
            try
            {
                lakeTable.insert( record );
            }
            catch ( SQLException e )
            {
                e.printStackTrace();
            }
            value += 0.2;
        }
    }

    private void createDischargeData()
    {
        System.out.println( "Inserting test Discharge data for locations \"" + _lid1 + "\" and \"" + _lid2 + "\" " );
        DischargeRecord record = new DischargeRecord();
        DischargeTable dischargeTable = new DischargeTable( _db );
        short dur = 1001;
        short revision = 0;
        double value = 0.0;
        record.setLid( _lid1 );
        record.setDur( dur );
        record.setPe( "QR" );
        record.setTs( "RG" );
        record.setExtremum( "Z" );
        record.setShef_qual_code( "Z" );
        record.setQuality_code( 1879048191 );
        record.setRevision( revision );
        record.setProduct_id( "KWOHRRSOUN" );
        record.setProducttime( _endTime );
        record.setPostingtime( _endTime );
        
        for ( long indexTime = _startTime; indexTime <= _TOTHEndTime; indexTime += TimeHelper.MILLIS_PER_HOUR )
        {
            record.setPe( "QR" );
            record.setObstime( indexTime );
            record.setValue( value );
            try
            {
                dischargeTable.insert( record );
                record.setPe( "QT" );
                dischargeTable.insert( record );
            }
            catch ( SQLException e )
            {
                e.printStackTrace();
            }
            value += 0.1;
        }
        
        record.setLid( _lid2 );
        value = 0.2;
        
        for ( long indexTime = _startTime; indexTime <= _TOTHEndTime; indexTime += TimeHelper.MILLIS_PER_HOUR )
        {
            record.setPe( "QR" );
            record.setObstime( indexTime );
            record.setValue( value );
            try
            {
                dischargeTable.insert( record );
                record.setPe( "QT" );
                dischargeTable.insert( record );
            }
            catch ( SQLException e )
            {
                e.printStackTrace();
            }
            value += 0.2;
        }
    }

    private void createPCData()
    {
        System.out.println( "Inserting test PC data for locations \"" + _lid1 + "\" and \"" + _lid2 + "\" " );
        CurPCRecord record = new CurPCRecord();
        CurPCTable curPCTable = new CurPCTable( _db );
        short dur = 0;
        short revision = 0;
        double value = 0.0;
        record.setLid( _lid1 );
        record.setDur( dur );
        record.setPe( "PC" );
        record.setTs( "RG" );
        record.setExtremum( "Z" );
        record.setShef_qual_code( "Z" );
        record.setQuality_code( 1879048191 );
        record.setRevision( revision );
        record.setProduct_id( "KWOHRRSOUN" );
        record.setProducttime( _endTime );
        record.setPostingtime( _endTime );
        
        for ( long indexTime = _startTime; indexTime <= _TOTHEndTime; indexTime += TimeHelper.MILLIS_PER_HOUR )
        {
            record.setObstime( indexTime );
            record.setValue( value );
            try
            {
                curPCTable.insert( record );
            }
            catch ( SQLException e )
            {
                e.printStackTrace();
            }
            value += 0.1;
        }
        
        record.setLid( _lid2 );
        value = 0.2;
        
        for ( long indexTime = _startTime; indexTime <= _TOTHEndTime; indexTime += TimeHelper.MILLIS_PER_HOUR )
        {
            record.setObstime( indexTime );
            record.setValue( value );
            try
            {
                curPCTable.insert( record );
            }
            catch ( SQLException e )
            {
                e.printStackTrace();
            }
            value += 0.2;
        }
    }

    private void createPPData()
    {
        System.out.println( "Inserting test PP data for locations \"" + _lid1 + "\" and \"" + _lid2 + "\" " );
        CurPPRecord record = new CurPPRecord();
        CurPPTable curPPTable = new CurPPTable( _db );
        short dur = 1001;
        short revision = 0;
        double value = 0.0;
        record.setLid( _lid1 );
        record.setDur( dur );
        record.setPe( "PP" );
        record.setTs( "RG" );
        record.setExtremum( "Z" );
        record.setShef_qual_code( "Z" );
        record.setQuality_code( 1879048191 );
        record.setRevision( revision );
        record.setProduct_id( "KWOHRRSOUN" );
        record.setProducttime( _endTime );
        record.setPostingtime( _endTime );
        
        for ( long indexTime = _startTime; indexTime <= _TOTHEndTime; indexTime += TimeHelper.MILLIS_PER_HOUR )
        {
            record.setObstime( indexTime );
            record.setValue( value );
            try
            {
                curPPTable.insert( record );
            }
            catch ( SQLException e )
            {
                e.printStackTrace();
            }
        }
        
        record.setLid( _lid2 );
        value = 0.2;
        
        for ( long indexTime = _startTime; indexTime <= _TOTHEndTime; indexTime += TimeHelper.MILLIS_PER_HOUR )
        {
            record.setObstime( indexTime );
            record.setValue( value );
            try
            {
                curPPTable.insert( record );
            }
            catch ( SQLException e )
            {
                e.printStackTrace();
            }
        }
    }

    private void logSQLException(SQLException exception)
    {
        _logger.log("SQL ERROR = " +
                    exception.getErrorCode() +  " " +
                    exception.getMessage());
                         
        exception.printStackTrace(_logger.getPrintWriter());
        
        _logger.log("End of stack trace");
         
    }
    
    private void logException( Exception exception )
    {
        _logger.log( "ERROR = " + 
                     exception.getMessage() );
        exception.printStackTrace( _logger.getPrintWriter() );
        
        _logger.log( "End of stack trace" );
    }

    private String getBaseConnectionString()
    {
        //this is used in testing mode, not the production mode
        String connectionURLPart = "jdbc:postgresql://dx1-nhdr:5432/hd_ob7ounx?user=pguser";
        return connectionURLPart;
    }
}
