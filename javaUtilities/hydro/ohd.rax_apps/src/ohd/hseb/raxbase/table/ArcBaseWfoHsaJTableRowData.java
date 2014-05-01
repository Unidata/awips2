package ohd.hseb.raxbase.table;

import ohd.hseb.util.gui.jtable.AbstractJTableRowData;
import ohd.hseb.util.gui.jtable.BaseTableCell;
import ohd.hseb.util.gui.jtable.CellType;


public class ArcBaseWfoHsaJTableRowData extends AbstractJTableRowData
{
    private String _missingRepresentation = "";
    
    private String _wfoHsa = null;
    
    public ArcBaseWfoHsaJTableRowData()
    {
        setMissingRepresentation( "" );
    }

    public String toString()
    {
        return "WfoHsa = " + _wfoHsa;
    }
    
    /**
     * Creates the individual cells and specifies its properties, like data tpe, value etc.
     *
     */
    public void addAllCellsToMap()
    {
        addCell( new BaseTableCell( "WfoHsa", CellType.STRING, getWfoHsa(), _missingRepresentation ) );
    }

    public void setWfoHsa( String wfoHsa )
    {
        this._wfoHsa = wfoHsa;
    }

    public String getWfoHsa()
    {
        return _wfoHsa;
    }
}