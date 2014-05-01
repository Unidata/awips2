package ohd.hseb.raxbase.table;

import ohd.hseb.util.gui.jtable.AbstractJTableRowData;
import ohd.hseb.util.gui.jtable.BaseTableCell;
import ohd.hseb.util.gui.jtable.CellType;

public class ArcBaseRfcJTableRowData extends AbstractJTableRowData
{
    private String _missingRepresentation = "";
    
    private String _rfc = null;
    
    public ArcBaseRfcJTableRowData()
    {
        setMissingRepresentation( "" );
    }

    public String toString()
    {
        return "Rfc = " + _rfc;
    }
    
    /**
     * Creates the individual cells and specifies its properties, like data tpe, value etc.
     *
     */
    public void addAllCellsToMap()
    {
        addCell( new BaseTableCell( "RFC", CellType.STRING, getRfc(), _missingRepresentation ) );
    }

    public void setRfc( String rfc )
    {
        _rfc = rfc;
    }

    public String getRfc()
    {
        return _rfc;
    }
}