package ohd.hseb.raxbase.table;

import ohd.hseb.util.gui.jtable.AbstractJTableRowData;
import ohd.hseb.util.gui.jtable.BaseTableCell;
import ohd.hseb.util.gui.jtable.CellType;


public class ArcBaseHuc2JTableRowData extends AbstractJTableRowData
{
    private String _missingRepresentation = "";
    
    private String _code2 = null;
    private String _code12 = null;
    private String _desreg = null;
    
    public ArcBaseHuc2JTableRowData()
    {
        setMissingRepresentation( "" );
    }

    public String toString()
    {
        return "Code12 = " + _code12 +
               " | Code2 = " + _code2 +
               " | Desreg = " + _desreg;
    }
    
    /**
     * Creates the individual cells and specifies its properties, like data tpe, value etc.
     *
     */
    public void addAllCellsToMap()
    {
        addCell( new BaseTableCell( "Code12", CellType.STRING, getCode12(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Code2", CellType.STRING, getCode2(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Desc", CellType.STRING, getDesreg(), _missingRepresentation ) );
    }

    public void setCode2( String code2 )
    {
        _code2 = code2;
    }

    public String getCode2()
    {
        return _code2;
    }

    public void setCode12( String code12 )
    {
        _code12 = code12;
    }

    public String getCode12()
    {
        return _code12;
    }

    public void setDesreg( String desreg )
    {
        _desreg = desreg;
    }

    public String getDesreg()
    {
        return _desreg;
    }
    
}
