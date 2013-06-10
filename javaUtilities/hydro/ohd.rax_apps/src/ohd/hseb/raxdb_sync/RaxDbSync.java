package ohd.hseb.raxdb_sync;


public class RaxDbSync
{
    
    private RaxSyncDataMgr _dataManager = null;
    
    
    public RaxDbSync(RaxSyncDataMgr dm)
    {
        _dataManager = dm;
    } // end of RaxDbSync constructor


    public void process(String syncMode, String tablenames)
    {
        DiffSet allTheDifferencesFound = null;
        
        allTheDifferencesFound = findAllDifferences(tablenames);
        
        reportAllDifferences(allTheDifferencesFound);
        
        if (syncMode.equalsIgnoreCase("UPDATE"))
            proccessAllDifferences(allTheDifferencesFound);
    }
    
    private DiffSet findAllDifferences(String tablenames)
    {
        return _dataManager.findAllDifferences(tablenames);
    }
    
    private void reportAllDifferences(DiffSet differencesToWorkOn)
    {
        _dataManager.reportAllDifferences(differencesToWorkOn);
    }

    private void proccessAllDifferences(DiffSet differencesToWorkOn)
    {
        _dataManager.proccessAllDifferences(differencesToWorkOn, true);
    }
    
} // end of RaxDbSync class
