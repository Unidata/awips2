package ohd.hseb.raxbase.table;

import ohd.hseb.util.gui.jtable.AbstractJTableRowData;
import ohd.hseb.util.gui.jtable.BaseTableCell;
import ohd.hseb.util.gui.jtable.CellType;


public class ArcBaseHuc6JTableRowData extends AbstractJTableRowData
{
    private String _missingRepresentation = "";
    
    private String _code6 = null;
    private String _code12 = null;
    private String _code34 = null;
    private String _code56 = null;
    private String _desacct = null;
    
    public ArcBaseHuc6JTableRowData()
    {
        setMissingRepresentation( "" );
    }

    public String toString()
    {
        return "Code12 = " + _code12 +
               " | Code34 = " + _code34 +
               " | Code56 = " + getCode56() +
               " | Code6 = " + _code6 +
               " | Desacct = " + getDesacct();
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
        addCell( new BaseTableCell( "Code6", CellType.STRING, getCode6(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Desc", CellType.STRING, getDesacct(), _missingRepresentation ) );
    }

    public void setCode6( String code6 )
    {
        _code6 = code6;
    }

    public String getCode6()
    {
        return _code6;
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

    public void setDesacct( String desacct )
    {
        _desacct = desacct;
    }

    public String getDesacct()
    {
        return _desacct;
    }
    
}
