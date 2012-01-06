package ohd.hseb.raxbase.model;

import ohd.hseb.raxbase.util.PEManager;
import ohd.hseb.raxdb.generated.RaxLocDataLimitsRecord;

public class RaxLocDataLimits extends RaxDataLimits
{
    private String _lid = null;
    
    public RaxLocDataLimits(){}

    public RaxLocDataLimits( RaxDataLimits raxDataLimits )
    {
        super( raxDataLimits );
    }
    
    public RaxLocDataLimits( RaxLocDataLimits raxLocDataLimits )
    {
        super( raxLocDataLimits );
        setLid( raxLocDataLimits.getLid() );
    }

    public RaxLocDataLimits( RaxLocDataLimitsRecord raxLocDataLimitsRecord )
    {
        super( raxLocDataLimitsRecord );
        setLid( raxLocDataLimitsRecord.getLid() );
    }

    public static RaxLocDataLimits getRaxLocDataLimits( RaxLocDataLimitsRecord raxLocDataLimitsRecord )
    {
        return ( new RaxLocDataLimits( raxLocDataLimitsRecord ) );
    }
    
    public static RaxLocDataLimitsRecord getRaxLocDataLimitsRecord( RaxLocDataLimits raxLocDataLimits )
    {
        RaxLocDataLimitsRecord raxLocDataLimitsRecord = new RaxLocDataLimitsRecord();
        
        raxLocDataLimitsRecord.setLid( raxLocDataLimits.getLid() );
        raxLocDataLimitsRecord.setPe1( PEManager.getPe1FromPE( raxLocDataLimits.getPe() ) );
        raxLocDataLimitsRecord.setPe2( PEManager.getPe2FromPE( raxLocDataLimits.getPe() ) );
        raxLocDataLimitsRecord.setDur( raxLocDataLimits.getDur() );
        raxLocDataLimitsRecord.setIdur( raxLocDataLimits.getIdur() );
        raxLocDataLimitsRecord.setMonthdaystart( raxLocDataLimits.getMonthDayStart() );
        raxLocDataLimitsRecord.setMonthdayend( raxLocDataLimits.getMonthDayEnd() );
        raxLocDataLimitsRecord.setGross_range_min( raxLocDataLimits.getGrossRangeMin() );
        raxLocDataLimitsRecord.setGross_range_max( raxLocDataLimits.getGrossRangeMax() );
        raxLocDataLimitsRecord.setReason_range_min( raxLocDataLimits.getReasonRangeMin() );
        raxLocDataLimitsRecord.setReason_range_max( raxLocDataLimits.getReasonRangeMax() );
        raxLocDataLimitsRecord.setRoc_max( raxLocDataLimits.getRoc() );
        raxLocDataLimitsRecord.setAlert_limit( raxLocDataLimits.getAlertLimit() );
        raxLocDataLimitsRecord.setAlert_roc_limit( raxLocDataLimits.getAlertRocLimit() );
        raxLocDataLimitsRecord.setAlarm_limit( raxLocDataLimits.getAlarmLimit() );
        raxLocDataLimitsRecord.setAlarm_roc_limit( raxLocDataLimits.getAlarmRocLimit() );

        return raxLocDataLimitsRecord;
    }
    
    public String toString()
    {
        return ( "Lid = " + _lid + " | " + super.toString() );
    }
    
    public String keyString()
    {
        return ( "Lid = " + _lid + " | " + super.keyString() );
    }

    
    public void setLid( String lid )
    {
        _lid = lid;
    }

    public String getLid()
    {
        return _lid;
    }
}
