package ohd.hseb.raxbase.table;

import ohd.hseb.util.gui.jtable.AbstractJTableRowData;
import ohd.hseb.util.gui.jtable.BaseTableCell;
import ohd.hseb.util.gui.jtable.CellType;


public class ArcBaseShefPEJTableRowData extends AbstractJTableRowData
{
    private String _missingRepresentation = "";
    
    private String _pe = null;
    private String _name = null;
    private String _metUnit = null;
    private String _engUnit = null;
    
    public ArcBaseShefPEJTableRowData()
    {
        setMissingRepresentation( "" );
    }

    public String toString()
    {
        return "PE = " + _pe +
               " | Name = " + _name + 
               " | MetUnit = " + _metUnit +
               " | EngUnit = " + _engUnit;
    }
    
    /**
     * Creates the individual cells and specifies its properties, like data tpe, value etc.
     *
     */
    public void addAllCellsToMap()
    {
        addCell( new BaseTableCell( "PE", CellType.STRING, getPe(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Name", CellType.STRING, getName(), _missingRepresentation ) );
        addCell( new BaseTableCell( "MetUnit", CellType.STRING, getMetUnit(), _missingRepresentation ) );
        addCell( new BaseTableCell( "EngUnit", CellType.STRING, getEngUnit(), _missingRepresentation ) );
    }

    public void setPe( String pe )
    {
        _pe = pe;
    }

    public String getPe()
    {
        return _pe;
    }

    public void setName( String name )
    {
        _name = name;
    }

    public String getName()
    {
        return _name;
    }

    public void setMetUnit( String metUnit )
    {
        _metUnit = metUnit;
    }

    public String getMetUnit()
    {
        return _metUnit;
    }

    public void setEngUnit( String engUnit )
    {
        _engUnit = engUnit;
    }

    public String getEngUnit()
    {
        return _engUnit;
    }
}
