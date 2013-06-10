package ohd.hseb.raxbase.table;

import ohd.hseb.util.gui.jtable.AbstractJTableRowData;
import ohd.hseb.util.gui.jtable.BaseTableCell;
import ohd.hseb.util.gui.jtable.CellType;


public class ArcBaseShefQCJTableRowData extends AbstractJTableRowData
{
    private static final short MISSING = -9999;
    
    private String _missingRepresentation = "";
    
    private String _shefQualifierCode = null;
    private int _power = MISSING;
    private String _name = null;
    
    public ArcBaseShefQCJTableRowData()
    {
        setMissingRepresentation( "" );
    }

    public String toString()
    {
        return "ShefQualifierCode = " + _shefQualifierCode +
               " | Power = " + _power +
               " | Name = " + _name;
    }
    
    /**
     * Creates the individual cells and specifies its properties, like data tpe, value etc.
     *
     */
    public void addAllCellsToMap()
    {
        addCell( new BaseTableCell( "ShefQCCode", CellType.STRING, getShefQualifierCode(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Power", CellType.INTEGER, getPower(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Name", CellType.STRING, getName(), _missingRepresentation ) );
    }

    public void setName( String name )
    {
        _name = name;
    }

    public String getName()
    {
        return _name;
    }

    public void setShefQualifierCode( String shefQualifierCode )
    {
        _shefQualifierCode = shefQualifierCode;
    }

    public String getShefQualifierCode()
    {
        return _shefQualifierCode;
    }

    public void setPower( int power )
    {
        _power = power;
    }

    public int getPower()
    {
        return _power;
    }
}
