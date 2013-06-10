package ohd.hseb.raxbase.table;

import ohd.hseb.util.gui.jtable.AbstractJTableRowData;
import ohd.hseb.util.gui.jtable.BaseTableCell;
import ohd.hseb.util.gui.jtable.CellType;


public class ArcBaseShefProbJTableRowData extends AbstractJTableRowData
{
    private String _missingRepresentation = "";
    
    private String _p = null;
    private String _name = null;
    private float _probability = 0;
    
    public ArcBaseShefProbJTableRowData()
    {
        setMissingRepresentation( "" );
    }

    public String toString()
    {
        return "P = " + _p +
               " | Probability = " + _probability +
               " | Name = " + _name; 
    }
    
    /**
     * Creates the individual cells and specifies its properties, like data tpe, value etc.
     *
     */
    public void addAllCellsToMap()
    {
        addCell( new BaseTableCell( "P", CellType.STRING, getP(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Probability", CellType.FLOAT, getProbability(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Name", CellType.STRING, getName(), _missingRepresentation ) );
    }

    public void setP( String p )
    {
        _p = p;
    }

    public String getP()
    {
        return _p;
    }

    public void setName( String name )
    {
        _name = name;
    }

    public String getName()
    {
        return _name;
    }

    public void setProbability( float probability )
    {
        _probability = probability;
    }

    public float getProbability()
    {
        return _probability;
    }
}
