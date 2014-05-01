package ohd.hseb.raxbase.table;

import ohd.hseb.util.gui.jtable.AbstractJTableRowData;
import ohd.hseb.util.gui.jtable.BaseTableCell;
import ohd.hseb.util.gui.jtable.CellType;


public class ArcBaseShefDurationJTableRowData extends AbstractJTableRowData
{
    private String _missingRepresentation = "";
    
    private String _duration = null;
    private short _iDuration = 0;
    private String _name = null;
    
    public ArcBaseShefDurationJTableRowData()
    {
        setMissingRepresentation( "" );
    }

    public String toString()
    {
        return "Duration = " + _duration +
               " | IDuration = " + _iDuration +
               " | Name = " + _name;
    }
    
    /**
     * Creates the individual cells and specifies its properties, like data tpe, value etc.
     *
     */
    public void addAllCellsToMap()
    {
        addCell( new BaseTableCell( "Dur", CellType.STRING, getDuration(), _missingRepresentation ) );
        addCell( new BaseTableCell( "IDur", CellType.SHORT, getIDuration(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Name", CellType.STRING, getName(), _missingRepresentation ) );
    }

    public void setDuration( String duration )
    {
        _duration = duration;
    }

    public String getDuration()
    {
        return _duration;
    }

    public void setIDuration( short iDuration )
    {
        _iDuration = iDuration;
    }

    public short getIDuration()
    {
        return _iDuration;
    }

    public void setName( String name )
    {
        _name = name;
    }

    public String getName()
    {
        return _name;
    }
}
