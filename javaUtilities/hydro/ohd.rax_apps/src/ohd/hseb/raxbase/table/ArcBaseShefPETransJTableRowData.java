package ohd.hseb.raxbase.table;

import ohd.hseb.util.gui.jtable.AbstractJTableRowData;
import ohd.hseb.util.gui.jtable.BaseTableCell;
import ohd.hseb.util.gui.jtable.CellType;


public class ArcBaseShefPETransJTableRowData extends AbstractJTableRowData
{
    private String _missingRepresentation = "";
    
    private String _pe = null;
    private String _codePosition = null;
    private int _codedValue = 0;
    
    public ArcBaseShefPETransJTableRowData()
    {
        setMissingRepresentation( "" );
    }

    public String toString()
    {
        return "PE = " + _pe +
               " | CodePosition = " + _codePosition +
               " | CodedValue = " + _codedValue;
    }
    
    /**
     * Creates the individual cells and specifies its properties, like data tpe, value etc.
     *
     */
    public void addAllCellsToMap()
    {
        addCell( new BaseTableCell( "PE", CellType.STRING, getPe(), _missingRepresentation ) );
        addCell( new BaseTableCell( "CodePosition", CellType.STRING, getCodePosition(), _missingRepresentation ) );
        addCell( new BaseTableCell( "CodedValue", CellType.INTEGER, getCodedValue(), _missingRepresentation ) );
    }

    public void setPe( String pe )
    {
        _pe = pe;
    }

    public String getPe()
    {
        return _pe;
    }

    public void setCodePosition( String codePosition )
    {
        _codePosition = codePosition;
    }

    public String getCodePosition()
    {
        return _codePosition;
    }

    public void setCodedValue( int codedValue )
    {
        _codedValue = codedValue;
    }

    public int getCodedValue()
    {
        return _codedValue;
    }
}
