package ohd.hseb.raxbase.table;

import ohd.hseb.util.gui.jtable.AbstractJTableRowData;
import ohd.hseb.util.gui.jtable.BaseTableCell;
import ohd.hseb.util.gui.jtable.CellType;


public class ArcBaseAgencyJTableRowData extends AbstractJTableRowData
{
    private String _missingRepresentation = "";
    
    private String _agCode = null;
    private String _agLoc = null;
    private String _des = null;
    
    public ArcBaseAgencyJTableRowData()
    {
        setMissingRepresentation( "" );
    }

    public String toString()
    {
        return "AgCode = " + _agCode +
               " | AgLoc = " + _agLoc + 
               " | Des = " + _des;
    }

    
    /**
     * Creates the individual cells and specifies its properties, like data tpe, value etc.
     *
     */
    public void addAllCellsToMap()
    {
        addCell( new BaseTableCell( "AgCode", CellType.STRING, getAgCode(), _missingRepresentation ) );
        addCell( new BaseTableCell( "AgLoc", CellType.STRING, getAgLoc(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Desc", CellType.STRING, getDes(), _missingRepresentation ) );
    }
    public void setAgCode( String agCode )
    {
        _agCode = agCode;
    }

    public String getAgCode()
    {
        return _agCode;
    }

    public void setAgLoc( String agLoc )
    {
        _agLoc = agLoc;
    }

    public String getAgLoc()
    {
        return _agLoc;
    }

    public void setDes( String des )
    {
        _des = des;
    }

    public String getDes()
    {
        return _des;
    }
}
