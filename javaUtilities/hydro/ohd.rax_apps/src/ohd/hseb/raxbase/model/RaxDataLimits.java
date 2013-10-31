package ohd.hseb.raxbase.model;

import ohd.hseb.raxbase.util.PEManager;
import ohd.hseb.raxdb.generated.RaxDataLimitsRecord;
import ohd.hseb.raxdb.generated.RaxLocDataLimitsRecord;


public class RaxDataLimits
{
    private static final short MISSING = -9999;
    
    private String _pe;
    private String _dur;
    private short _idur;
    private String _monthDayStart;
    private String _monthDayEnd;
    private double _grossRangeMin;
    private double _grossRangeMax;
    private double _reasonRangeMin;
    private double _reasonRangeMax;
    private double _roc;
    private double _alertLimit;
    private double _alertRocLimit;
    private double _alarmLimit;
    private double _alarmRocLimit;

    public RaxDataLimits(){}
    
    public RaxDataLimits( RaxDataLimits raxDataLimits )
    {
        setPe( raxDataLimits.getPe() );
        setDur( raxDataLimits.getDur() );
        setIdur( raxDataLimits.getIdur() );
        setMonthDayStart( raxDataLimits.getMonthDayStart() );
        setMonthDayEnd( raxDataLimits.getMonthDayEnd() );
        setGrossRangeMin( raxDataLimits.getGrossRangeMin() );
        setGrossRangeMax( raxDataLimits.getGrossRangeMax() );
        setReasonRangeMin( raxDataLimits.getReasonRangeMin() );
        setReasonRangeMax( raxDataLimits.getReasonRangeMax() );
        setRoc( raxDataLimits.getRoc() );
        setAlertLimit( raxDataLimits.getAlertLimit() );
        setAlertRocLimit( raxDataLimits.getAlertRocLimit() );
        setAlarmLimit( raxDataLimits.getAlarmLimit() );
        setAlarmRocLimit( raxDataLimits.getAlarmRocLimit() );
    }

    public RaxDataLimits( RaxDataLimitsRecord raxDataLimits )
    {
        setPe( PEManager.getPEFromPe1Pe2( raxDataLimits.getPe1(), raxDataLimits.getPe2() ) );
        setDur( raxDataLimits.getDur() );
        setIdur( raxDataLimits.getIdur() );
        setMonthDayStart( raxDataLimits.getMonthdaystart() );
        setMonthDayEnd( raxDataLimits.getMonthdayend() );
        setGrossRangeMin( raxDataLimits.getGross_range_min() );
        setGrossRangeMax( raxDataLimits.getGross_range_max() );
        setReasonRangeMin( raxDataLimits.getReason_range_min() );
        setReasonRangeMax( raxDataLimits.getReason_range_max() );
        setRoc( raxDataLimits.getRoc_max() );
        setAlertLimit( raxDataLimits.getAlert_limit() );
        setAlertRocLimit( raxDataLimits.getAlert_roc_limit() );
        setAlarmLimit( raxDataLimits.getAlarm_limit() );
        setAlarmRocLimit( raxDataLimits.getAlarm_roc_limit() );
    }

    public RaxDataLimits( RaxLocDataLimitsRecord raxLocDataLimits )
    {
        setPe( PEManager.getPEFromPe1Pe2( raxLocDataLimits.getPe1(), raxLocDataLimits.getPe2() ) );
        setDur( raxLocDataLimits.getDur() );
        setIdur( raxLocDataLimits.getIdur() );
        setMonthDayStart( raxLocDataLimits.getMonthdaystart() );
        setMonthDayEnd( raxLocDataLimits.getMonthdayend() );
        setGrossRangeMin( raxLocDataLimits.getGross_range_min() );
        setGrossRangeMax( raxLocDataLimits.getGross_range_max() );
        setReasonRangeMin( raxLocDataLimits.getReason_range_min() );
        setReasonRangeMax( raxLocDataLimits.getReason_range_max() );
        setRoc( raxLocDataLimits.getRoc_max() );
        setAlertLimit( raxLocDataLimits.getAlert_limit() );
        setAlertRocLimit( raxLocDataLimits.getAlert_roc_limit() );
        setAlarmLimit( raxLocDataLimits.getAlarm_limit() );
        setAlarmRocLimit( raxLocDataLimits.getAlarm_roc_limit() );
    }
    
    public String toString()
    {
        return "PE = " + _pe + " | Dur = " + _dur + " | idur = " + _idur + " | MonthDayStart = " + _monthDayStart +
               " | MonthDayEnd = " + _monthDayEnd + " | GrossRangeMin = " + _grossRangeMin + " | GrossRangeMax = " + _grossRangeMax +
               " | ReasonRangeMin = " + _reasonRangeMin + " | ReasonRangeMax = " + _reasonRangeMax + " Roc = " + _roc + 
               " | AlertLimit = " + _alertLimit + " | AlertRocLimit = " + _alertRocLimit + " | AlarmLimit = " + _alarmLimit + " | AlarmRocLimit = " + _alarmRocLimit;
    }

    public String keyString()
    {
        return "PE = " + _pe + " | Dur = " + _dur + " | idur = " + _idur + " | MonthDayStart = " + _monthDayStart;
    }
    
    public static RaxDataLimits getRaxDataLimits( RaxDataLimitsRecord raxDataLimitsRecord )
    {
        return ( new RaxDataLimits( raxDataLimitsRecord ) );
    }
    
    public static RaxDataLimitsRecord getRaxDataLimitsRecord( RaxDataLimits raxDataLimits )
    {
        RaxDataLimitsRecord raxDataLimitsRecord = new RaxDataLimitsRecord();
        
        raxDataLimitsRecord.setPe1( PEManager.getPe1FromPE( raxDataLimits.getPe() ) );
        raxDataLimitsRecord.setPe2( PEManager.getPe2FromPE( raxDataLimits.getPe() ) );
        raxDataLimitsRecord.setDur( raxDataLimits.getDur() );
        raxDataLimitsRecord.setIdur( raxDataLimits.getIdur() );
        raxDataLimitsRecord.setMonthdaystart( raxDataLimits.getMonthDayStart() );
        raxDataLimitsRecord.setMonthdayend( raxDataLimits.getMonthDayEnd() );
        raxDataLimitsRecord.setGross_range_min( raxDataLimits.getGrossRangeMin() );
        raxDataLimitsRecord.setGross_range_max( raxDataLimits.getGrossRangeMax() );
        raxDataLimitsRecord.setReason_range_min( raxDataLimits.getReasonRangeMin() );
        raxDataLimitsRecord.setReason_range_max( raxDataLimits.getReasonRangeMax() );
        raxDataLimitsRecord.setRoc_max( raxDataLimits.getRoc() );
        raxDataLimitsRecord.setAlert_limit( raxDataLimits.getAlertLimit() );
        raxDataLimitsRecord.setAlert_roc_limit( raxDataLimits.getAlertRocLimit() );
        raxDataLimitsRecord.setAlarm_limit( raxDataLimits.getAlarmLimit() );
        raxDataLimitsRecord.setAlarm_roc_limit( raxDataLimits.getAlarmRocLimit() );
    
        return raxDataLimitsRecord;
    }
    
    public void setPe( String pe )
    {
        _pe = pe;
    }
    public String getPe()
    {
        return _pe;
    }
    public void setDur( String dur )
    {
        _dur = dur;
    }
    public String getDur()
    {
        return _dur;
    }
    public void setIdur( short idur )
    {
        _idur = idur;
    }
    public short getIdur()
    {
        return _idur;
    }
    public void setMonthDayStart( String monthdaystart )
    {
        _monthDayStart = monthdaystart;
    }
    public String getMonthDayStart()
    {
        return _monthDayStart;
    }
    public void setMonthDayEnd( String monthdayend )
    {
        _monthDayEnd = monthdayend;
    }
    public String getMonthDayEnd()
    {
        return _monthDayEnd;
    }
    public void setGrossRangeMin( double grossRangeMin )
    {
        _grossRangeMin = grossRangeMin;
    }
    public double getGrossRangeMin()
    {
        return _grossRangeMin;
    }
    public void setGrossRangeMax( double grossRangeMax )
    {
        _grossRangeMax = grossRangeMax;
    }
    public double getGrossRangeMax()
    {
        return _grossRangeMax;
    }
    public void setReasonRangeMin( double reasonRangeMin )
    {
        _reasonRangeMin = reasonRangeMin;
    }
    public double getReasonRangeMin()
    {
        return _reasonRangeMin;
    }
    public void setReasonRangeMax( double reasonRangeMax )
    {
        _reasonRangeMax = reasonRangeMax;
    }
    public double getReasonRangeMax()
    {
        return _reasonRangeMax;
    }
    public void setRoc( double roc )
    {
        _roc = roc;
    }
    public double getRoc()
    {
        return _roc;
    }
    public void setAlertLimit( double alertLimit )
    {
        _alertLimit = alertLimit;
    }
    public double getAlertLimit()
    {
        return _alertLimit;
    }
    public void setAlertRocLimit( double alertRocLimit )
    {
        _alertRocLimit = alertRocLimit;
    }
    public double getAlertRocLimit()
    {
        return _alertRocLimit;
    }
    public void setAlarmLimit( double alarmLimit )
    {
        _alarmLimit = alarmLimit;
    }
    public double getAlarmLimit()
    {
        return _alarmLimit;
    }
    public void setAlarmRocLimit( double alarmRocLimit )
    {
        _alarmRocLimit = alarmRocLimit;
    }
    public double getAlarmRocLimit()
    {
        return _alarmRocLimit;
    }
}
