package ohd.hseb.bias_trans;

import java.io.File;

import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.util.CodeTimer;

public class BiasMessageReader
{
    private static final String VERSION = "OB82";
    private static final String RELEASE_DATE = "April 11, 2007";
    private static final String PROGRAM_NAME = "BiasMessageReader";
    
    private static final String _usage = "Usage: " + PROGRAM_NAME +
    " <database_name> <filepath>";
    
    private BiasDataMgr _dataMgr = null;
    private String _jdbcUrl = null;
    private File _filePath = null;
    
    public BiasMessageReader ( String[] args, String PROGRAM_NAME ) throws Exception
    {
        getBiasArguments ( args, PROGRAM_NAME);
        
        // Create the data manager
        setDataMgr( new BiasDataMgr ( _jdbcUrl ));
    }
    
    /**
     * @param args
     */
    private void getBiasArguments ( String args [ ], final String program_name ) throws Exception
    {
        boolean failed = false;
       
        if (args.length != 2)
        {
            failed = true;
        }
        else
        {
           // Process the database name supplied via the -d argument.
           _jdbcUrl = args[0];
           
           // Process the filename/path supplied via the -f argument.
           
           _filePath = new File ( args[1] );
           System.out.println ("File path for file reader = " + _filePath);
        }
                    
        if ( failed )
        {
            throw new Exception (_usage); 
        }
    }

    public void process ( ) throws Exception
    {
        RadarBias radarBias = _dataMgr.readRadarBiasFromFile ( _filePath );
        
        System.out.println ("Bias Message Retrieved.\n" + radarBias );

        _dataMgr.writeRadarBiasToDatabase ( radarBias );
    }
    
    public static void main(String[] args)
    {
        long startingTime = System.currentTimeMillis();
        String startingTimeString = DbTimeHelper.getDateTimeStringFromLongTime(startingTime);
        System.out.println("The Starting Time is: " + startingTimeString);
        
        try
        {
           CodeTimer codeTimer = new CodeTimer();
           codeTimer.start();
            
           BiasMessageReader messageReader = new BiasMessageReader( args, PROGRAM_NAME);
           
           System.out.println("Starting Program " + PROGRAM_NAME );
           System.out.println("Version: " + VERSION + " Release Date: " + 
                               RELEASE_DATE );
           
           messageReader.process();
         
           codeTimer.stop("process_rfc_bias_message completed in ");
        }
        catch  ( Exception e)
        {
            System.out.println (e.getMessage());
            System.out.println (e.getStackTrace());
        }

    }

    public void setDataMgr(BiasDataMgr dataMgr)
    {
        _dataMgr = dataMgr;
    }

    public BiasDataMgr getDataMgr()
    {
        return _dataMgr;
    }

}

