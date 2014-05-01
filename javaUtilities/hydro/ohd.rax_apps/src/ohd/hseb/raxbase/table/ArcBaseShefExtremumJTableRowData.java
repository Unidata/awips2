package ohd.hseb.raxbase.table;

import ohd.hseb.util.gui.jtable.AbstractJTableRowData;
import ohd.hseb.util.gui.jtable.BaseTableCell;
import ohd.hseb.util.gui.jtable.CellType;


public class ArcBaseShefExtremumJTableRowData extends AbstractJTableRowData
{
    private String _missingRepresentation = "";
    
    private String _extremum = null;
    private String _name = null;
    
    public ArcBaseShefExtremumJTableRowData()
    {
        setMissingRepresentation( "" );
    }

    public String toString()
    {
        return "Extremum = " + _extremum +
               " | Name = " + _name;
    }
    
    /**
     * Creates the individual cells and specifies its properties, like data tpe, value etc.
     *
     */
    public void addAllCellsToMap()
    {
        addCell( new BaseTableCell( "Extremum", CellType.STRING, getExtremum(), _missingRepresentation ) );
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

    public void setExtremum( String extremum )
    {
        this._extremum = extremum;
    }

    public String getExtremum()
    {
        return _extremum;
    }
}
