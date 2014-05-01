package ohd.hseb.raxbase.table;

import ohd.hseb.util.gui.jtable.AbstractJTableRowData;
import ohd.hseb.util.gui.jtable.BaseTableCell;
import ohd.hseb.util.gui.jtable.CellType;


public class ArcBaseDataLimitsJTableRowData extends AbstractJTableRowData
{
    private static final short MISSING = -9999;
    private String _missingRepresentation = "";
    
    private String _lid = null;
    private String _pe = null;
    private String _duration = null;
    private String _monthDayStart = null;
    private String _monthDayEnd = null;
    private double _grossRangeMin = MISSING;
    private double _grossRangeMax = MISSING;
    private double _reasonRangeMin = MISSING;
    private double _reasonRangeMax = MISSING;
    private double _rocMax = MISSING;
    private double _alertLimit = MISSING;
    private double _alertRocLimit = MISSING;
    private double _alarmLimit = MISSING;
    private double _alarmRocLimit = MISSING;
    
    
    public ArcBaseDataLimitsJTableRowData()
    {
        setMissingRepresentation( "" );
    }

    public String toString()
    {
        return "Lid = " + _lid + " PE = " + _pe + " dur = " + _duration;
    }
    
    public void setLid( String lid ) 
    {
        _lid = lid;
    }

    public String getLid() 
    {
        return _lid;
    }

    
    public void setPe( String pe )
    {
        _pe = pe;
    }

    public String getPe()
    {
        return _pe;
    }

    public void setDuration( String duration )
    {
        _duration = duration;
    }

    public String getDuration()
    {
        return _duration;
    }

    public void setMonthDayStart( String monthDayStart )
    {
        _monthDayStart = monthDayStart;
    }

    public String getMonthDayStart()
    {
        return _monthDayStart;
    }

    public void setMonthDayEnd( String monthDayEnd )
    {
        _monthDayEnd = monthDayEnd;
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

    public void setRocMax( double rocMax )
    {
        _rocMax = rocMax;
    }

    public double getRocMax()
    {
        return _rocMax;
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

    /**
     * Creates the individual cells and specifies its properties, like data tpe, value etc.
     *
     */
    public void addAllCellsToMap()
    {
        addCell( new BaseTableCell( "Location", CellType.STRING, getLid(), _missingRepresentation ) );
        addCell( new BaseTableCell( "PE", CellType.STRING, getPe(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Dur", CellType.STRING, getDuration(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Start", CellType.STRING, getMonthDayStart(), _missingRepresentation ) );
        addCell( new BaseTableCell( "End", CellType.STRING, getMonthDayEnd(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Gross Min", CellType.DOUBLE, getGrossRangeMin(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Gross Max", CellType.DOUBLE, getGrossRangeMax(), _missingRepresentation ) );
        addCell( new BaseTableCell( "<HTML>Reasonable<BR>Min</HTML>", CellType.DOUBLE, getReasonRangeMin(), _missingRepresentation ) );
        addCell( new BaseTableCell( "<HTML>Reasonable<BR>Max</HTML>", CellType.DOUBLE, getReasonRangeMax(), _missingRepresentation ) );
        addCell( new BaseTableCell( "<HTML>Rate of<BR>Change</HTML>", CellType.DOUBLE, getRocMax(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Alert Limit", CellType.DOUBLE, getAlertLimit(), _missingRepresentation ) );
        addCell( new BaseTableCell( "<HTML>Alert Limit<BR>ROC</HTML>", CellType.DOUBLE, getAlertRocLimit(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Alarm Limit", CellType.DOUBLE, getAlarmLimit(), _missingRepresentation ) );
        addCell( new BaseTableCell( "<HTML>Alarm Limit<BR>ROC</HTML>", CellType.DOUBLE, getAlarmRocLimit(), _missingRepresentation ) );
    }
}
