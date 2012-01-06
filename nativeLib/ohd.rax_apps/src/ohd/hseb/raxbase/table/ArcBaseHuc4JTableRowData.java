package ohd.hseb.raxbase.table;

import ohd.hseb.util.gui.jtable.AbstractJTableRowData;
import ohd.hseb.util.gui.jtable.BaseTableCell;
import ohd.hseb.util.gui.jtable.CellType;


public class ArcBaseHuc4JTableRowData extends AbstractJTableRowData
{
    private String _missingRepresentation = "";
    
    private String _code4 = null;
    private String _code12 = null;
    private String _code34 = null;
    private String _dessubreg = null;
    
    public ArcBaseHuc4JTableRowData()
    {
        setMissingRepresentation( "" );
    }

    public String toString()
    {
        return "Code12 = " + _code12 +
               " | Code34 = " + _code34 +
               " | Code4 = " + _code4 +
               " | Dessubreg = " + _dessubreg;
    }
    
    /**
     * Creates the individual cells and specifies its properties, like data tpe, value etc.
     *
     */
    public void addAllCellsToMap()
    {
        addCell( new BaseTableCell( "Code12", CellType.STRING, getCode12(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Code34", CellType.STRING, getCode34(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Code4", CellType.STRING, getCode4(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Desc", CellType.STRING, getDesreg(), _missingRepresentation ) );
    }

    public void setCode4( String code4 )
    {
        _code4 = code4;
    }

    public String getCode4()
    {
        return _code4;
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
        _dessubreg = desreg;
    }

    public String getDesreg()
    {
        return _dessubreg;
    }

    public void setCode34( String code34 )
    {
        _code34 = code34;
    }

    public String getCode34()
    {
        return _code34;
    }
    
}
