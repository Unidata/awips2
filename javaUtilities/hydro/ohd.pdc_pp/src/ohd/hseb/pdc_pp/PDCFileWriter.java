package ohd.hseb.pdc_pp;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.text.DecimalFormat;
import java.util.List;
import java.util.Map;

import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.util.CodeTimer;
import ohd.hseb.util.MathHelper;

public class PDCFileWriter
{
    public static final int MISSING = -9999;
    private static final double MISSING_DOUBLE = MISSING;

    
    private String _preprocessedFilePath = null;
    private boolean _fileIsOpen = false;
    private boolean _keepPreprocessedFileOpen = false;
    private OutputStream _outputStream = null;
    private PrintWriter _writer = null;
    
    private Map _locationHashMap = null;
    private Map _shefDurMap = null;
  //  private Map _shefDurToDurcodeMap = null;
  
    
    private boolean _DEBUG = false;
    
    CodeTimer _timeSeriesOutputStringTimer = null;
    
    private long _startTime = 0;
    private long _endTime = 0;
    
    
    //this will change during the processing
    //it exists solely for monitor different parts of the processing
    private int _timeSeriesValueCount = 0;
  
  
    // ------------------------------------------------------------------------------
    
    public PDCFileWriter(Map locationHashMap,
                         Map shefDurMap, 
                         Map shefDurToDurcodeMap,
                         String preprocessedFilePath)
    {
        _locationHashMap = locationHashMap;
        _shefDurMap = shefDurMap;
       // _shefDurToDurcodeMap = shefDurToDurcodeMap;
        _preprocessedFilePath = preprocessedFilePath;
        
        return;
    }
    // ----------------------------------------------------------------------------------

    public void writeIrregularObsDataToFile( List obsTimeSeriesList, String fileName )
    {
        
        _timeSeriesOutputStringTimer = new CodeTimer();
        //_decimalFormatTimer = new CodeTimer();
   
        if ( ( obsTimeSeriesList != null ) && (! obsTimeSeriesList.isEmpty() ) )
        {
            String header = "PDCFileWriter.writeIrregularObsDataToFile(): ";
            StringBuffer outputStringBuffer = new StringBuffer();
            String finalPreProcessedFileName = _preprocessedFilePath + fileName + ".dat";
            String preProcessedWorkFileName = finalPreProcessedFileName + ".work";
            
            
            //open if needed
            if (!isPreprocessedFileOpen())
            {
               // System.out.println("writeIrregularObsDataToFile() : Opening " + fileName);
                openPreProcessedFile( preProcessedWorkFileName );
               // _writer = new PrintWriter( _outputStream );
            }
            
            // write out the message, pre-pending date time stamp if required
            if ( _DEBUG )
            {
                System.out.println( "Writing out to file: " + _preprocessedFilePath + fileName + ".dat" );
                System.out.flush();
            }

            long curTime = System.currentTimeMillis();
            outputStringBuffer.append( "Created on: " + 
                    DbTimeHelper.getDateTimeStringFromLongTime( curTime ) + "\n" );
            
            outputStringBuffer.append( "####(file header)NUM_OF_VALUES START_TIME TIME_INTERVAL_BETWEEN_VALUES_IN_SECONDS\n" );
            outputStringBuffer.append( "####LID HSA PEDSTE ELEV LAT LON FCST_PNT(T/F) STATION_NAME\n" );
            outputStringBuffer.append( "####DCP_FLAG OBSERVER_FLAG TELEM_TYPE_STRINGS \n" );
            outputStringBuffer.append( "####VALUES\n" );
            if ( obsTimeSeriesList != null )
            {
                IrregularObsTimeSeries firstObsTimeSeries = (IrregularObsTimeSeries) obsTimeSeriesList.get( 0 );
                int num_of_values = firstObsTimeSeries.getTimeValuePairList( true ).size();
                long start_time = firstObsTimeSeries.getDescriptor().getStartTime();
                long timeStepIntervalInMillis = firstObsTimeSeries.getTimeStepIntervalInMillis();
                long timeStepIntervalInSeconds = timeStepIntervalInMillis / 1000;
               
                long end_time = firstObsTimeSeries.getDescriptor().getEndTime();
                
              //  long end_time = start_time + ( (num_of_values - 1) * timeStepIntervalInMillis );
                
               
                /*
                 *   long start_time = firstObsTimeSeries.getDescriptor().getStartTime();
                long end_time = firstObsTimeSeries.getDescriptor().getEndTime();
                 * 
                 */
                
                outputStringBuffer.append("#### start time = " +
                        DbTimeHelper.getDateTimeStringFromLongTime( start_time ) +
                        " end time =  " + 
                        DbTimeHelper.getDateTimeStringFromLongTime(end_time) + '\n');

                
                
                if ( _DEBUG )
                {
                    outputStringBuffer.append( num_of_values + " " + 
                            DbTimeHelper.getDateTimeStringFromLongTime( start_time ) + 
                            " " + timeStepIntervalInSeconds + "\n" );
                }
                else
                {
                    outputStringBuffer.append( num_of_values + " " + start_time/1000 + 
                            " " + timeStepIntervalInSeconds + "\n" );
                }
                for ( int i = 0; i < obsTimeSeriesList.size(); i++ )
                {
                    IrregularObsTimeSeries obsTimeSeries = (IrregularObsTimeSeries) obsTimeSeriesList.get( i );
                    if ( obsTimeSeries != null )
                    {
                        outputStringBuffer.append( getOutputStringFromLocationAndIrregularObsTimeSeries( obsTimeSeries ) );
                    }
                }
            }
            else
            {
                outputStringBuffer.append("#### start time = UNKNOWN " +
                                           " end time = UNKNOWN \n"); 
                
            }
            
            _writer.println( outputStringBuffer );
            _writer.flush();
            
            //close if supposed to do so
            if (! _keepPreprocessedFileOpen )
            {
                // System.out.println("writeIrregularObsDataToFile() closing the file");
                closePreprocessedFile();    
                File workFile = new File( preProcessedWorkFileName );
                File finalFile = new File ( finalPreProcessedFileName );
                finalFile.delete();
                workFile.renameTo( new File( finalPreProcessedFileName ) );
            }
        }
    }

//  ------------------------------------------------------------------------

    private String getOutputStringFromLocationAndIrregularObsTimeSeries( IrregularObsTimeSeries obsTimeSeries )
    {
        RegularObsTimeSeriesDescriptor descriptor = obsTimeSeries.getDescriptor();
        String lid = descriptor.getLid();
        String shef_param_code = null;
        Location location = (Location) _locationHashMap.get( lid );
        StringBuffer outputString = new StringBuffer();
        
        shef_param_code = descriptor.getPe() + _shefDurMap.get( "" + descriptor.getDur() ) + 
                          descriptor.getTs() + descriptor.getExtremum();
        descriptor.setShef_param_code( shef_param_code );
        outputString.append( location.getLid() + " " + location.getHsa() + " " + 
                descriptor.getShef_param_code() + " " + location.getElevation() + " " + 
                location.getLat() + " " + location.getLon() + " " + location.isFcstPoint() + " " + 
                location.getLocationName() + "\n" );
        
        if ( location.isDCP() )
        {
            outputString.append( "T " );
        }
        else
        {       
            outputString.append( "F " );
        }
        
        if ( location.isObserver() )
        {
            outputString.append( "T " );
        }
        else
        {       
            outputString.append( "F " );
        }
        
        if ( ( location.getTelemType() == null ) || (location.getTelemType().length() == 0) )
        {
            outputString.append( "NO_TELEM_TYPE" );
        }
        else
        {
            outputString.append( location.getTelemType() );
        }
        outputString.append( "\n" + getIrregularObsTimeSeriesOutputString( obsTimeSeries ) + "\n" );


        return outputString.toString();
    }

//  ------------------------------------------------------------------------
    private String getIrregularObsTimeSeriesOutputString( IrregularObsTimeSeries obsTimeSeries )
    {
        String header = "PDCFileWriter.getIrregularObsTimeSeriesOutputString(): ";
        List timeValuePairList = obsTimeSeries.getTimeValuePairList( true );
        DecimalFormat decimalFormat = new DecimalFormat( "0.00" );
        
        String missingDoubleString = getFormattedNumber(MISSING_DOUBLE, decimalFormat) + "";
        
        StringBuffer outputStringBuffer = new StringBuffer();

        double formattedNumber = 0.0;
        String dateTimeString = null;
        
        for ( int i = 0; i < timeValuePairList.size(); i++ )
        {
            TimeValuePair timeValuePair = (TimeValuePair) timeValuePairList.get( i );
            if ( timeValuePair != null )
            {
                if ( _DEBUG )
                {
                    
                    dateTimeString = DbTimeHelper.getDateTimeStringFromLongTime( timeValuePair.getDateTime());
                    // formattedNumber = getFormattedNumber(timeValuePair.getValue(), decimalFormat );
                     outputStringBuffer.append( dateTimeString + "|" +
                             timeValuePair.getValue() + "\n");
                    
           
                }
                else
                {
                    outputStringBuffer.append(timeValuePair.getValue() + " ");
                }
            }
            else
            {
                if ( _DEBUG )
                {
                    outputStringBuffer.append(missingDoubleString + "\n");
                }
                else
                {
                    outputStringBuffer.append(missingDoubleString + " "); 
                }
                  
            }
        }
        return outputStringBuffer.toString();
    }
    
    
    private String old_getIrregularObsTimeSeriesOutputString( IrregularObsTimeSeries obsTimeSeries )
    {
        List timeValuePairList = obsTimeSeries.getTimeValuePairList( true );
        DecimalFormat decimalFormat = new DecimalFormat( "0.00" );

        String outputString = "";

        for ( int i = 0; i < timeValuePairList.size(); i++ )
        {
            TimeValuePair timeValuePair = (TimeValuePair) timeValuePairList.get( i );
            if ( timeValuePair != null )
            {
                if ( _DEBUG )
                {
                    outputString += DbTimeHelper.getDateTimeStringFromLongTime( timeValuePair.getDateTime() ) + 
                                    "|" + decimalFormat.format( timeValuePair.getValue() ) + "\n";
                }
                else
                {
                    outputString += decimalFormat.format( timeValuePair.getValue() ) + " ";
                }
            }
            else
            {
                if ( _DEBUG )
                {
                    outputString += MISSING_DOUBLE + "\n";
                }
                else
                {
                    outputString += MISSING_DOUBLE + " ";
                }
            }
        }
        return outputString;
    }
    
//  ------------------------------------------------------------------------
    public void writeRegularObsDataToFile( List obsTimeSeriesList, String fileName )
    {
        String header = "PDCFileWriter.writeRegularObsDataToFile(): ";
        CodeTimer fileTimer = new CodeTimer();
    //    CodeTimer outputStringTimer = new CodeTimer();
        
        _timeSeriesOutputStringTimer = new CodeTimer();
      //  _decimalFormatTimer = new CodeTimer();
        
        
        if ( ( obsTimeSeriesList != null ) && (! obsTimeSeriesList.isEmpty() ) )
        {
           
            StringBuffer outputStringBuffer = new StringBuffer();
            String finalPreProcessedFileName = _preprocessedFilePath + fileName + ".dat";
            String preProcessedWorkFileName = finalPreProcessedFileName + ".work";
            
            //open if needed
            if (!isPreprocessedFileOpen())
            {
                openPreProcessedFile( preProcessedWorkFileName );
               // _writer = new PrintWriter(_outputStream);
            }
            
            if ( _DEBUG )
            {
                System.out.println( "Writing out to file: " + _preprocessedFilePath + fileName + ".dat" );
                System.out.flush();
            }
            
            long curTime = System.currentTimeMillis();
            // write out the message, pre-pending date time stamp if required
            if ( _DEBUG )
            {
                
                outputStringBuffer.append( "DEBUGGABLE OUTPUT FILE Created on: " + 
                        DbTimeHelper.getDateTimeStringFromLongTime( curTime ) + "\n" );
            }
            else
            {
                outputStringBuffer.append( "Created on: " + 
                        DbTimeHelper.getDateTimeStringFromLongTime( curTime ) + "\n" );
            }
            
            outputStringBuffer.append( "####(file header)NUM_OF_VALUES START_TIME TIME_INTERVAL_BETWEEN_VALUES_IN_SECONDS\n" );
            outputStringBuffer.append( "####LID HSA PEDSTE ELEV LAT LON FCST_PNT(T/F) STATION_NAME\n" );
            outputStringBuffer.append( "####DCP_FLAG OBSERVER_FLAG TELEM_TYPE_STRINGS \n" );
            outputStringBuffer.append( "####VALUES\n" );
            if  ( ( obsTimeSeriesList != null ) && (obsTimeSeriesList.size() > 0) )
            {
                RegularObsTimeSeries firstObsTimeSeries = (RegularObsTimeSeries) obsTimeSeriesList.get( 0 );
                int num_of_values = firstObsTimeSeries.getTimeValuePairList( true ).size();
                
                _timeSeriesValueCount = num_of_values;
                
                long start_time = firstObsTimeSeries.getDescriptor().getStartTime();
                long timeStepIntervalInMillis = firstObsTimeSeries.getTimeStepIntervalInMillis();
                long timeStepIntervalInSeconds = timeStepIntervalInMillis / 1000;
                long end_time = start_time + ( (num_of_values - 1) * timeStepIntervalInMillis );
                
                _startTime = start_time;
                _endTime = end_time;
                
                outputStringBuffer.append("#### start time = " +
                                        DbTimeHelper.getDateTimeStringFromLongTime( start_time ) +
                                        " end time =  " + 
                                        DbTimeHelper.getDateTimeStringFromLongTime(end_time) + '\n');
                
                if ( _DEBUG )
                {
                    outputStringBuffer.append( num_of_values + " " + 
                            DbTimeHelper.getDateTimeStringFromLongTime( start_time ) + 
                            " " + timeStepIntervalInSeconds + "\n" );
                }
                else
                {
                    outputStringBuffer.append( num_of_values + " " + start_time/1000 + 
                            " " + timeStepIntervalInSeconds + "\n" );
                }
                
                
                for ( int i = 0; i < obsTimeSeriesList.size(); i++ )
                {
                    RegularObsTimeSeries obsTimeSeries = (RegularObsTimeSeries) obsTimeSeriesList.get( i );
                    if ( obsTimeSeries != null )
                    {
                        outputStringBuffer.append(
                                   getRegularObsTimeSeriesOutputStringWhenMergedWithLocation( obsTimeSeries ) );
                        
                    }
                }
            }
            else
            {
                outputStringBuffer.append("#### start time = UNKNOWN" +    
                                          " end time = UNKNOWN \n");
                  
                return;
            }
             
            
            
            //System.out.println(header + 
            //        "creating the output string from each obsTimeSeries and its location entry took a total of " + 
            //        outputStringTimer.getElapsedTime() + " millis.");
              
      //      System.out.println(header + 
      //              "creating the output string from each obsTimeSeries alone took a total of " + 
      //              _timeSeriesOutputStringTimer.getElapsedTime() + " millis.");
        
   
            
    //        fileTimer.start();
            _writer.println( outputStringBuffer );
   //         fileTimer.stop(header + "writing the outputStringBuffer to the file took");
            _writer.flush();
            
            // close if supposed to do so
            if (!_keepPreprocessedFileOpen)
            {
                closePreprocessedFile();
                File workFile = new File(preProcessedWorkFileName);
                File finalFile = new File(finalPreProcessedFileName);
                finalFile.delete();
                workFile.renameTo(new File(finalPreProcessedFileName));
            }
        }
    }
    
//  ------------------------------------------------------------------------

    private StringBuffer getRegularObsTimeSeriesOutputStringWhenMergedWithLocation( RegularObsTimeSeries obsTimeSeries )
    {
        String header = "PDCFileWriter.getRegularObsTimeSeriesOutputStringWhenMergedWithLocation(): ";
        
        RegularObsTimeSeriesDescriptor descriptor = obsTimeSeries.getDescriptor();
        
        if (descriptor == null)
        {
            System.out.println(header + " ***************** the time series descriptor is null");    
        }
        
        String lid = descriptor.getLid();
        String shef_param_code = null;
        Location location = (Location) _locationHashMap.get( lid );
        
        
        StringBuffer outputStringBuffer = new StringBuffer();
        
        
        if (location == null)
        {
            System.out.println(header + " ***************** the location object is null for lid = "  + lid);    
        }
        
 
        //Make sure that a pe of PC ends up with a duration of 0,
        //even though within the preprocessor, before writing, it has a different duration
        //, matching the calculated accumulations it applies to. The "I" duration in this case
        //indicates the source of the data, not the period for which accumulations were computed
        if (descriptor.getPe().equals("PC"))
        {
            descriptor.setDur((short)0);
        }
        String durationString = (String) _shefDurMap.get( "" + descriptor.getDur() );
         
        shef_param_code = descriptor.getPe() + durationString +
                          descriptor.getTs() + descriptor.getExtremum();
        descriptor.setShef_param_code( shef_param_code );
        
        outputStringBuffer.append(location.getLid() + " " + location.getHsa() + " " + 
                descriptor.getShef_param_code() + " " + location.getElevation() + " " + 
                location.getLat() + " " + location.getLon() + " " + location.isFcstPoint() + " " + 
                location.getLocationName() + "\n" );
        
        if ( location.isDCP() )
        {
            outputStringBuffer.append( "T " );
        }
        else
        {       
            outputStringBuffer.append( "F " );
        }
        
        if ( location.isObserver() )
        {
            outputStringBuffer.append( "T " );
        }
        else
        {       
            outputStringBuffer.append( "F " );
        }
        
        if ( ( location.getTelemType() == null ) || (location.getTelemType().length() == 0) )
        {
            outputStringBuffer.append( "NO_TELEM_TYPE" );
        }
        else
        {
            outputStringBuffer.append( location.getTelemType() );
        }
        
         
    //    _timeSeriesOutputStringTimer.restart();
        
        outputStringBuffer.append( "\n" + getRegularObsTimeSeriesTimeValuePairListOutputString( obsTimeSeries ) + "\n" );
    //    _timeSeriesOutputStringTimer.stop();

        return outputStringBuffer;
    }

//  ------------------------------------------------------------------------
    private String getObsTimeSeriesOutputString_orig_way( RegularObsTimeSeries obsTimeSeries )
    {
        String header = "PDCFileWriter.getObsTimeSeriesOutputString_orig_way(): ";
        List timeValuePairList = obsTimeSeries.getTimeValuePairList( true );
        DecimalFormat decimalFormat = new DecimalFormat( "0.00" );
        
        String outputString = "";

        for ( int i = 0; i < timeValuePairList.size(); i++ )
        {
            TimeValuePair timeValuePair = (TimeValuePair) timeValuePairList.get( i );
            if ( timeValuePair != null )
            {
                if ( _DEBUG )
                {
                    outputString += DbTimeHelper.getDateTimeStringFromLongTime( timeValuePair.getDateTime() ) + 
                                "|" + decimalFormat.format( timeValuePair.getValue() ) + "\n";
                }
                else
                {
                    outputString += decimalFormat.format( timeValuePair.getValue() ) + " ";
                }
            }
            else
            {
                if ( _DEBUG )
                {
                    outputString += decimalFormat.format( MISSING_DOUBLE ) + "\n";
                }
                else
                {
                    outputString += decimalFormat.format( MISSING_DOUBLE ) + " ";
                }
            }
        }
        return outputString;
    } //orig way
    
    
    private String getObsTimeSeriesOutputString_old( RegularObsTimeSeries obsTimeSeries )
    {
        String header = "PDCFileWriter.getObsTimeSeriesOutputString_old(): ";
        List timeValuePairList = obsTimeSeries.getTimeValuePairList( true );
        DecimalFormat decimalFormat = new DecimalFormat( "0.00" );
        
        String outputString = "";
        String formattedNumberString = null;
        String dateTimeString = null;

        for ( int i = 0; i < timeValuePairList.size(); i++ )
        {
            TimeValuePair timeValuePair = (TimeValuePair) timeValuePairList.get( i );
            if ( timeValuePair != null )
            {
                if ( _DEBUG )
                {
                    dateTimeString= DbTimeHelper.getDateTimeStringFromLongTime( timeValuePair.getDateTime() );
                    formattedNumberString = getFormattedNumber_old(timeValuePair.getValue(), decimalFormat );
                    outputString += dateTimeString + 
                                "|" + formattedNumberString + "\n";
                }
                else
                {
                    formattedNumberString = getFormattedNumber_old(timeValuePair.getValue(), decimalFormat );
                    outputString += formattedNumberString + " ";
                }
            }
            else
            {
                if ( _DEBUG )
                {
                    formattedNumberString = getFormattedNumber_old(MISSING_DOUBLE, decimalFormat );
                    outputString += formattedNumberString + "\n";
                }
                else
                {
                    formattedNumberString = getFormattedNumber_old(MISSING_DOUBLE, decimalFormat );
                    outputString += formattedNumberString + " ";
                }
            }
        }
        return outputString;
    } //old way
    
    // --------------------------------------------------------------------------------------------  
    
    private String getRegularObsTimeSeriesTimeValuePairListOutputString( RegularObsTimeSeries obsTimeSeries )
    {
        String header = "PDCFileWriter.getRegularObsTimeSeriesTimeValuePairListOutputString(): ";
        List timeValuePairList = obsTimeSeries.getTimeValuePairList( true );
        //Map timeValuePairMap = ob
        
        
        DecimalFormat decimalFormat = new DecimalFormat( "0.00" );
        
        String missingDoubleString = getFormattedNumber(MISSING_DOUBLE, decimalFormat) + "";
        
        StringBuffer outputStringBuffer = new StringBuffer();

        //double formattedNumber = 0.0;
        String dateTimeString = null;
         
        if (timeValuePairList.size() > _timeSeriesValueCount)
        {
          //  System.out.println(header + "\nFor " + obsTimeSeries.getDescriptor().toString() + 
          //          " timeValuePairList.size() = " + timeValuePairList.size() + 
         //           ">  intended _timeSeriesValueCount = " + _timeSeriesValueCount);
        }
        else if (timeValuePairList.size() < _timeSeriesValueCount)
        {
            System.out.println(header + "\n *******For " + obsTimeSeries.getDescriptor().toString() + 
                    " timeValuePairList.size() = " + timeValuePairList.size() + 
                    " <  intended _timeSeriesValueCount = " + _timeSeriesValueCount);
            
         }
        else
        {
            //System.out.println(header + "\nFor " + obsTimeSeries.getDescriptor().toString() + 
            //        " timeValuePairList.size() = intended _timeSeriesValueCount"); 
        }
        
        RegularObsTimeSeriesDescriptor descriptor = obsTimeSeries.getDescriptor();
        
        if (
           (descriptor.getStartTime() > _startTime) ||
           (descriptor.getEndTime() < _endTime)
           )
        {
            String startTimeString = DbTimeHelper.getDateTimeStringFromLongTime(descriptor.getStartTime());
            String endTimeString = DbTimeHelper.getDateTimeStringFromLongTime(descriptor.getEndTime());
            System.out.println(header + descriptor.toString() + " goes from " +
                               startTimeString + " to " + endTimeString );
            
            
            String origStartTimeString = DbTimeHelper.getDateTimeStringFromLongTime(_startTime);
            String origEndTimeString = DbTimeHelper.getDateTimeStringFromLongTime(_endTime);
         
            System.out.println(header +  " and should have gone from  " +
                    origStartTimeString + " to " + origEndTimeString );
            
            System.out.println(header + " merger string = " + obsTimeSeries.getMergerString() + '\n');
            
            
        }
        
        if (! descriptor.isMissing() ) //NOT MISSING
        {
            
            for ( int i = 0; i < timeValuePairList.size(); i++ )
            {
                TimeValuePair timeValuePair = (TimeValuePair) timeValuePairList.get( i );
                if ( timeValuePair != null )
                {
                    if ( _DEBUG )
                    {
                        dateTimeString = DbTimeHelper.getDateTimeStringFromLongTime( timeValuePair.getDateTime());
                        // formattedNumber = getFormattedNumber(timeValuePair.getValue(), decimalFormat );
                        outputStringBuffer.append( dateTimeString + "|" +
                                timeValuePair.getValue() + "\n");
                    }
                    else
                    {
                        // formattedNumber = getFormattedNumber(timeValuePair.getValue(), decimalFormat );
                        outputStringBuffer.append(timeValuePair.getValue() + " ");
                    }
                }
                else
                {
                    if ( _DEBUG )
                    {
                        outputStringBuffer.append(missingDoubleString + "\n");
                    }
                    else
                    {
                        outputStringBuffer.append(missingDoubleString + " "); 
                    }
                    
                }
            }
            
        }
        else //the time series is missing
        {
            outputStringBuffer.append("MISSING");
            /*
            for ( long indexTime = descriptor.getStartTime();
                       indexTime < descriptor.getEndTime();
                       indexTime += PDCPreprocessorDataMgr.MILLIS_PER_HOUR )
            {
                outputStringBuffer.append(missingDoubleString + " ");
            }
            */
            
        }
        
        
        return outputStringBuffer.toString();
    }
//  -------------------------------------------------------------------------
    public double getFormattedNumber(double value, DecimalFormat decimalFormat)
    {
    //    _decimalFormatTimer.restart();
        
       // value = roundToPlaces(value, 2);
        
       // String numberString = decimalFormat.format(value);
        
    //    _decimalFormatTimer.stop();
        
        return value;
    }
 
    
    //  -------------------------------------------------------------------------
    
    public String getFormattedNumber_old(double value, DecimalFormat decimalFormat)
    {
        //_decimalFormatTimer.restart();
             
         String numberString = decimalFormat.format(value);
        
       // _decimalFormatTimer.stop();
        
        return value + "";
    }
//  -------------------------------------------------------------------------

    public double getFormattedNumberWithRounding(double value, DecimalFormat decimalFormat)
    {
       // _decimalFormatTimer.restart();
        
        value = MathHelper.roundToNDecimalPlaces(value, 2);
        
       // String numberString = decimalFormat.format(value);
        
       // _decimalFormatTimer.stop();
        
        return value;
    }


// -------------------------------------------------------------------------

    private boolean isPreprocessedFileOpen()
    {
        return _fileIsOpen; 
    }

//  ------------------------------------------------------------------------

    public void closePreprocessedFile() 
    {
        if (isPreprocessedFileOpen() )
        {
             _fileIsOpen = false;
             _writer.close();
         }
    }

//  ------------------------------------------------------------------------

    public void closePreprocessedFile( PrintWriter writer ) 
    {
        if (isPreprocessedFileOpen() )
        {
             _fileIsOpen = false;
             writer.close();
         }
    }

//  ------------------------------------------------------------------------

    private void openPreProcessedFile(String fileName)
    {
         try
         {
             if (fileName != null)
             {
                 _outputStream = new BufferedOutputStream( new FileOutputStream(fileName, false) );
                 
                 _fileIsOpen = true;
             }
             else //fileName == null
             {
                 _outputStream = System.out;
             }

             _writer = new PrintWriter( _outputStream );
         }
         catch (java.io.IOException e)
         {
             System.out.println( e );
             if ( _DEBUG )
             {
                 e.printStackTrace();
             }
         }
    }

    //  ------------------------------------------------------------------------

    

}
