package ohd.hseb.raxdb_sync;

public class RaxDbSyncBatch
{
    
    private RaxSyncDataMgr _dataMgr = null;
    private RaxDbSync _rdbs = null;
    private String _syncMode = "ANALYSIS ONLY";
    private String _tablenames = null;
    
    public RaxDbSyncBatch(String ihfsConnString, String raxConnString)
    {
        // Create a Data Manager object
        _dataMgr = new RaxSyncDataMgr(ihfsConnString, raxConnString);

        _rdbs = new RaxDbSync(_dataMgr);
        
        _dataMgr.getMainAppLogger().log( "###################################################################################");
        _dataMgr.getMainAppLogger().log(_dataMgr.getDateTimeStamp() + " Starting Batch Synchronization program version date 02/01/2008.\n");
        
        _dataMgr.getMainAppLogger().log("IHFS database used for synchronization : " + _dataMgr.getIhfsDbName());
        _dataMgr.getMainAppLogger().log("RAX  database used for synchronization : " + _dataMgr.getRaxDbName());
        String tokenName = "adb_sync_ihfs_debug";
        _dataMgr.getMainAppLogger().log("Apps_defaults Token " + tokenName + " set to: " + _dataMgr.getRawTokenValue(tokenName) +
                                        "\nGenerate (tablenameDebug.out) debug output files : " + _dataMgr.isDebugOutputOn());        

        // Check if sync mode token is set to "Update"
        if (! _dataMgr.inAnalysisOnlyMode())
        {
            _syncMode = "UPDATE";
        }
        tokenName = "adb_sync_mode";
        _dataMgr.getMainAppLogger().log("Apps_defaults Token " + tokenName + " set to: " + _dataMgr.getRawTokenValue(tokenName) +
                                        "\nValue being used by this application is : " + _syncMode);

        // get the list of table names to process
        _tablenames = _dataMgr.getTablenames();
        tokenName = "adb_sync_tablenames";
        _dataMgr.getMainAppLogger().log("Apps_defaults Token " + tokenName + " set to: " + _dataMgr.getRawTokenValue(tokenName) +
                                        "\nList of Tables to process : " + _tablenames.toUpperCase());
        
        
        _rdbs.process(_syncMode, _tablenames);

        _dataMgr.getMainAppLogger().log("\n" + _dataMgr.getDateTimeStamp() + " Batch Synchronization program successfully completed.\n");

    } // end of RaxDbSyncBatch constructor
    
    /**
     * @param args
     */
    public static void main(String[] args)
    {
        final RaxDbSyncBatch _raxBatch;
        
//      Read tokens & command line arguments
        if (args.length < 2)
        {
            System.err.println("Usage: ohd.hseb.raxdb_synch IHFSdb_connection_string RAXdb_connection_string"); 
            System.exit(0);
        }    
        
        String baseConnectionStringIHFS = args[0];
        String baseConnectionStringRAX = args[1];
        
        _raxBatch = new RaxDbSyncBatch(baseConnectionStringIHFS, baseConnectionStringRAX);
        
    } // end of psvm()

} // end of RaxDbSyncBatch class
