package ohd.hseb.raxbase.table;

import ohd.hseb.raxbase.model.RaxAdjustFactor;
import ohd.hseb.util.gui.jtable.AbstractJTableRowData;
import ohd.hseb.util.gui.jtable.BaseTableCell;
import ohd.hseb.util.gui.jtable.CellType;


public class ArcBaseAdjustFactorJTableRowData extends AbstractJTableRowData
{
    private static final short MISSING = -9999;
    private String _missingRepresentation = "";
    
    private RaxAdjustFactor _raxAdjustFactor = null;
/*    
    private String _lid = null;
    private String _pe = null;
    private String _duration = null;
    private String _ts = null;
    private String _extremum = null;
    private long _beginDate = MISSING;
    private double _divisor = MISSING;
    private double _base = MISSING;
    private double _multiplier = MISSING;
    private double _adder = MISSING;
*/   
    public ArcBaseAdjustFactorJTableRowData()
    {
        setMissingRepresentation( "" );
    }

    public String toString()
    {
        return "Lid = " + _raxAdjustFactor.getLid() + " PE = " + _raxAdjustFactor.getPe() + " dur = " + _raxAdjustFactor.getDur();
    }
    
/*   
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

    public void setTs( String ts )
    {
        _ts = ts;
    }

    public String getTs()
    {
        return _ts;
    }

    public void setExtremum( String extremum )
    {
        _extremum = extremum;
    }

    public String getExtremum()
    {
        return _extremum;
    }

    public void setBeginDate( long beginDate )
    {
        _beginDate = beginDate;
    }

    public long getBeginDate()
    {
        return _beginDate;
    }

    public void setDivisor( double divisor )
    {
        _divisor = divisor;
    }

    public double getDivisor()
    {
        return _divisor;
    }

    public void setBase( double base )
    {
        _base = base;
    }

    public double getBase()
    {
        return _base;
    }

    public void setMultiplier( double multiplier )
    {
        _multiplier = multiplier;
    }

    public double getMultiplier()
    {
        return _multiplier;
    }

    public void setAdder( double adder )
    {
        _adder = adder;
    }

    public double getAdder()
    {
        return _adder;
    }
*/

    /**
     * Creates the individual cells and specifies its properties, like data tpe, value etc.
     *
     */
    public void addAllCellsToMap()
    {
        addCell( new BaseTableCell( "Location", CellType.STRING, _raxAdjustFactor.getLid(), _missingRepresentation ) );
        addCell( new BaseTableCell( "PE", CellType.STRING, _raxAdjustFactor.getPe(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Dur", CellType.STRING, _raxAdjustFactor.getIdur() + "/" + _raxAdjustFactor.getDur(), _missingRepresentation ) );
        addCell( new BaseTableCell( "TS", CellType.STRING, _raxAdjustFactor.getTs(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Extremum", CellType.STRING, _raxAdjustFactor.getExtremum(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Begin Date", CellType.DATE, _raxAdjustFactor.getBeginDate(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Divisor", CellType.DOUBLE, _raxAdjustFactor.getDivisor(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Base", CellType.DOUBLE, _raxAdjustFactor.getBase(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Multiplier", CellType.DOUBLE, _raxAdjustFactor.getMultiplier(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Adder", CellType.DOUBLE, _raxAdjustFactor.getAdder(), _missingRepresentation ) );
    }

    public void setRaxAdjustFactor( RaxAdjustFactor raxAdjustFactor )
    {
        _raxAdjustFactor = raxAdjustFactor;
    }

    public RaxAdjustFactor getRaxAdjustFactor()
    {
        return _raxAdjustFactor;
    }
}
