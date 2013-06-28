package ohd.hseb.pdc_pp.sample_data_set;

import ohd.hseb.util.FileLogger;
import ohd.hseb.util.Logger;

public class CreateSampleDataSet
{
    private SampleDataSetCreatorDataMgr _dataMgr = null;
    private boolean _createData = true;
    
    public CreateSampleDataSet()
    {
        
    }
    
    /**
     * initialize( String[] args ) - Used to process the command line arguments and set appropriate variables.
     * @param args - Command line arguments
     */
    private void initialize( String[] args )
    {
        StringBuffer baseConnectionStringBuffer = new StringBuffer();  //gotten from the command line
        StringBuffer logDirName = new StringBuffer();  
        String logFileName = null;  
        Logger logger = null;
        
        processCommandLineArguments( baseConnectionStringBuffer, logDirName, args );
        
        logFileName = logDirName + "/SampleDataSetCreator.log";
        
        logger = new FileLogger(logFileName, true, true); 
        
        _dataMgr = new SampleDataSetCreatorDataMgr(baseConnectionStringBuffer.toString(), logger );
        if ( _createData )
        {
            _dataMgr.create_test_location();
            _dataMgr.create_test_data();
        }
        else
        {
            _dataMgr.removeAllData();
        }
    }

    /**
     * 
     * @param baseConnectionStringBuffer - Postgres database connection string
     * @param logDirName - Directory where the log file will be created
     * @param args - Command line arguments
     */    
    private void processCommandLineArguments( StringBuffer baseConnectionStringBuffer, 
            StringBuffer logDirName, String[] args )
    {
        if ( args.length < 2 )
        {
            System.err.println("Usage: ohd.hseb.PDCPreprocessor.sample_data_set.CreateSampleDataSet connection_string logFileDir <true|false - create sample data>"); 
            System.exit(0);
        }    
        
        if ( args.length == 3 )
        {
            if ( args[2].equalsIgnoreCase( "true" ) )
            {
                _createData = true;
            }
            else 
            {
                _createData = false;
            }
        }

        baseConnectionStringBuffer.append( args[0] );
        logDirName.append( args[1] );
    }
    
    public static void main( String[] args )
    {
        CreateSampleDataSet dataCreator = new CreateSampleDataSet();
        
        dataCreator.initialize( args );
        
        
    }
}
