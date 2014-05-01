package ohd.hseb.bias_trans;

import ohd.hseb.util.CodeTimer;
import ohd.hseb.db.*;

public class BiasMessageCreator
{
    public static final String VERSION = "OB82";
    public static final String RELEASE_DATE = "April 11, 2007";
    public static final String PROGRAM_NAME = "BiasMessageCreator";
    
    private static final String _usage = "Usage: " + PROGRAM_NAME +
    " <database_name> <YYYYMMDDHH>";
    
    private BiasDataMgr _dataMgr = null;
    
    private String _dateTime; // YYYYMMDDHH
    private String _jdbcUrl;
    // -----------------------------------------------------------------------------------------------------    
    
    public BiasMessageCreator ( String[] args ) throws Exception
    {
        getBiasArguments ( args );
        
        // Create the data manager
        setDataMgr( new BiasDataMgr ( _jdbcUrl ));
    }
    // -----------------------------------------------------------------------------------------------------    
    
    private void getBiasArguments ( String args [ ]) throws Exception
    {
        boolean failed = false;
        int datetimeAsInteger;
                
        if ( args.length != 2)
        {
            failed = true;
        }
        else
        {

            // Process the database name supplied as the first argument.
            _jdbcUrl = args[0];
            _dateTime = args[1];

            // Make sure the time string is the correct length.
            if ( _dateTime.length() != RfcBiasConstants.DATEHOUR_LENGTH )
            {
                failed = true;
            }
            
            // Make sure the time string is an integer.
            try
            {
               datetimeAsInteger = Integer.parseInt(_dateTime);
            }
            catch ( NumberFormatException e)
            {
                failed = true;
            }
        }
        
        if ( failed )
        {
            throw new Exception (_usage); 
        }
    }
    // -----------------------------------------------------------------------------------------------------    
    
    private void setDataMgr(BiasDataMgr dataMgr)
    {
        _dataMgr = dataMgr;
    }
    // -----------------------------------------------------------------------------------------------------    
    
    private BiasDataMgr getDataMgr()
    {
        return _dataMgr;
    }
    // -----------------------------------------------------------------------------------------------------    
    
    public void process () throws Exception
    {
        RadarBias radarBias = _dataMgr.getRadarBias( _dateTime );
        
        System.out.println ("Bias Message Retrieved.\n" + radarBias );
          
        _dataMgr.writeRadarBiasToFile ( radarBias, _dateTime );
    }
    // -----------------------------------------------------------------------------------------------------    
    /**
     * @param args
     */
    public static void main(String[] args)
    {
        long startingTime = System.currentTimeMillis();
        String startingTimeString = DbTimeHelper.getDateTimeStringFromLongTime(startingTime);
        
        System.out.println("The Starting Time is: " + startingTimeString);
        try
        {
           CodeTimer codeTimer = new CodeTimer();
           codeTimer.start();
            
           BiasMessageCreator messageCreator = new BiasMessageCreator( args);
                   
           System.out.println("Starting Program " + PROGRAM_NAME );
           System.out.println("Version: " + VERSION + " Release Date: " + 
                               RELEASE_DATE );
           
           messageCreator.process();
           
           codeTimer.stop("Create_rfc_bias_message completed in ");

        }
        catch ( Exception e)
        {
            
            System.out.println (e.getMessage());
            System.out.println (e.getStackTrace());
        }
    }
}
