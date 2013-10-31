package ohd.hseb.raxbase.table;

import ohd.hseb.util.gui.jtable.AbstractJTableRowData;
import ohd.hseb.util.gui.jtable.BaseTableCell;
import ohd.hseb.util.gui.jtable.CellType;


public class ArcBaseRatingOffsetsJTableRowData extends AbstractJTableRowData
{
    private static final short MISSING = -9999;
    private String _missingRepresentation = "";
    
    private double _stage = MISSING;
    private double _offSet = MISSING;
    
    
    public ArcBaseRatingOffsetsJTableRowData()
    {
        setMissingRepresentation( "" );
    }

    public String toString()
    {
        return "Stage = " + _stage + " Offset = " + _offSet;
    }
    
    /**
     * Creates the individual cells and specifies its properties, like data tpe, value etc.
     *
     */
    public void addAllCellsToMap()
    {
        addCell( new BaseTableCell( "Stage", CellType.DOUBLE, getStage(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Offset", CellType.DOUBLE, getOffset(), _missingRepresentation ) );
    }

    public void setStage( double stage )
    {
        _stage = stage;
    }

    public double getStage()
    {
        return _stage;
    }

    public void setOffset( double offSet )
    {
        _offSet = offSet;
    }

    public double getOffset()
    {
        return _offSet;
    }
}
