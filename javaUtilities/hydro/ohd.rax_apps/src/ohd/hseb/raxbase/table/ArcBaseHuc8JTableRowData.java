package ohd.hseb.raxbase.table;

import ohd.hseb.util.gui.jtable.AbstractJTableRowData;
import ohd.hseb.util.gui.jtable.BaseTableCell;
import ohd.hseb.util.gui.jtable.CellType;


public class ArcBaseHuc8JTableRowData extends AbstractJTableRowData
{
    private String _missingRepresentation = "";
    
    private String _code8 = null;
    private String _code12 = null;
    private String _code34 = null;
    private String _code56 = null;
    private String _code78 = null;
    private String _descat = null;
    
    public ArcBaseHuc8JTableRowData()
    {
        setMissingRepresentation( "" );
    }

    public String toString()
    {
        return "Code12 = " + _code12 +
               " | Code34 = " + _code34 +
               " | Code56 = " + _code56 +
               " | Code78 = " + _code78 +
               " | Code8 = " + _code8 +
               " | Descat = " + _descat;
    }
    
    /**
     * Creates the individual cells and specifies its properties, like data tpe, value etc.
     *
     */
    public void addAllCellsToMap()
    {
        addCell( new BaseTableCell( "Code12", CellType.STRING, getCode12(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Code34", CellType.STRING, getCode34(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Code56", CellType.STRING, getCode56(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Code78", CellType.STRING, getCode78(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Code8", CellType.STRING, getCode8(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Desc", CellType.STRING, getDescat(), _missingRepresentation ) );
    }

    public void setCode12( String code12 )
    {
        _code12 = code12;
    }

    public String getCode12()
    {
        return _code12;
    }

    public void setCode34( String code34 )
    {
        _code34 = code34;
    }

    public String getCode34()
    {
        return _code34;
    }

    public void setCode56( String code56 )
    {
        _code56 = code56;
    }

    public String getCode56()
    {
        return _code56;
    }

    public void setCode8( String code8 )
    {
        _code8 = code8;
    }

    public String getCode8()
    {
        return _code8;
    }

    public void setCode78( String code78 )
    {
        _code78 = code78;
    }

    public String getCode78()
    {
        return _code78;
    }

    public void setDescat( String descat )
    {
        _descat = descat;
    }

    public String getDescat()
    {
        return _descat;
    }
}
