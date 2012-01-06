package ohd.hseb.raxbase.table;

import ohd.hseb.util.gui.jtable.AbstractJTableRowData;
import ohd.hseb.util.gui.jtable.BaseTableCell;
import ohd.hseb.util.gui.jtable.CellType;


public class ArcBaseShefTSJTableRowData extends AbstractJTableRowData
{
    private String _missingRepresentation = "";
    
    private String _ts = null;
    private String _name = null;
    
    public ArcBaseShefTSJTableRowData()
    {
        setMissingRepresentation( "" );
    }

    public String toString()
    {
        return "TS = " + _ts +
               " | Name = " + _name;
    }
    
    /**
     * Creates the individual cells and specifies its properties, like data tpe, value etc.
     *
     */
    public void addAllCellsToMap()
    {
        addCell( new BaseTableCell( "TS", CellType.STRING, getTS(), _missingRepresentation ) );
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

    public void setTS( String ts )
    {
        _ts = ts;
    }

    public String getTS()
    {
        return _ts;
    }
}
