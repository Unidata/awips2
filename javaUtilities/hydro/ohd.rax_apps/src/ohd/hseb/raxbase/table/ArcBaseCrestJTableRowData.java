package ohd.hseb.raxbase.table;

import ohd.hseb.util.gui.jtable.AbstractJTableRowData;
import ohd.hseb.util.gui.jtable.BaseTableCell;
import ohd.hseb.util.gui.jtable.CellType;


public class ArcBaseCrestJTableRowData extends AbstractJTableRowData
{
    private static final short MISSING = -9999;
    private String _missingRepresentation = "";
    
    private double _stage = MISSING;
    private double _flow = MISSING;
    private long _date = MISSING;
    private String _time = null;
    
    public ArcBaseCrestJTableRowData()
    {
        setMissingRepresentation( "" );
    }

    public String toString()
    {
        return "Stage = " + _stage + " Flow = " + _flow + " Date = " + _date + " Time = " + _time;
    }
    
    /**
     * Creates the individual cells and specifies its properties, like data tpe, value etc.
     *
     */
    public void addAllCellsToMap()
    {
        addCell( new BaseTableCell( "Stage", CellType.DOUBLE, getStage(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Flow", CellType.DOUBLE, getFlow(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Date", CellType.DATE, getDate(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Time", CellType.STRING, getTime(), _missingRepresentation ) );
    }

    public void setStage( double stage )
    {
        _stage = stage;
    }

    public double getStage()
    {
        return _stage;
    }

    public void setFlow( double flow )
    {
        _flow = flow;
    }

    public double getFlow()
    {
        return _flow;
    }

    public void setDate( long date )
    {
        _date = date;
    }

    public long getDate()
    {
        return _date;
    }

    public void setTime( String time )
    {
        _time = time;
    }

    public String getTime()
    {
        return _time;
    }
}
