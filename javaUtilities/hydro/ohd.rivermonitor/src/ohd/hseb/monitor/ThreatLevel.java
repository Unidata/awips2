package ohd.hseb.monitor;

public enum ThreatLevel
{
   NO_THREAT,
    MISSING_DATA,
    AGED_DATA,
    CAUTION,
    ALERT;
    
    static boolean isDataMissingOrAged(ThreatLevel level)
    {
        boolean result = false;
        
        if ((level == ThreatLevel.AGED_DATA) || (level == ThreatLevel.MISSING_DATA) )
        {
            result = true;
        }
            
        return result;
    }
    // -------------------------------------------------------------------------------------
    public boolean isGreater( ThreatLevel otherLevel)
    {
        boolean result = false;
        if (compareTo(otherLevel) > 0)
        {
            result = true;
        }
        
        return result;
    }
}


