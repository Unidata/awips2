package ohd.hseb.raxbase.table;

import ohd.hseb.util.gui.jtable.AbstractJTableRowData;
import ohd.hseb.util.gui.jtable.BaseTableCell;
import ohd.hseb.util.gui.jtable.CellType;


public class ArcBaseShefPE1JTableRowData extends AbstractJTableRowData
{
    private String _missingRepresentation = "";
    
    private String _pe1 = null;
    private String _name = null;
    
    public ArcBaseShefPE1JTableRowData()
    {
        setMissingRepresentation( "" );
    }

    public String toString()
    {
        return "PE1 = " + _pe1 +
               " | Name = " + _name; 
    }
    
    /**
     * Creates the individual cells and specifies its properties, like data tpe, value etc.
     *
     */
    public void addAllCellsToMap()
    {
        addCell( new BaseTableCell( "PE1", CellType.STRING, getPe1(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Name", CellType.STRING, getName(), _missingRepresentation ) );
    }

    public void setPe1( String pe1 )
    {
        _pe1 = pe1;
    }

    public String getPe1()
    {
        return _pe1;
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
