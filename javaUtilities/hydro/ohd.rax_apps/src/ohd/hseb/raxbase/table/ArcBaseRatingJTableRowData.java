package ohd.hseb.raxbase.table;

import ohd.hseb.util.gui.jtable.AbstractJTableRowData;
import ohd.hseb.util.gui.jtable.BaseTableCell;
import ohd.hseb.util.gui.jtable.CellType;


public class ArcBaseRatingJTableRowData extends AbstractJTableRowData
{
    private static final short MISSING = -9999;
    private String _missingRepresentation = "";
    
    private double _stage = MISSING;
    private double _shiftedStage = MISSING;
    private double _discharge = MISSING;
    
    
    public ArcBaseRatingJTableRowData()
    {
        setMissingRepresentation( "" );
    }

    public String toString()
    {
        return "Stage = " + _stage + " Shifted Stage = " + _shiftedStage + " Discharge = " + _discharge;
    }
    
    /**
     * Creates the individual cells and specifies its properties, like data tpe, value etc.
     *
     */
    public void addAllCellsToMap()
    {
        addCell( new BaseTableCell( "Stage", CellType.DOUBLE, getStage(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Shifted Stage", CellType.DOUBLE, getShiftedStage(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Discharge", CellType.DOUBLE, getDischarge(), _missingRepresentation ) );
    }

    public void setStage( double stage )
    {
        _stage = stage;
    }

    public double getStage()
    {
        return _stage;
    }

    public void setShiftedStage( double shiftedStage )
    {
        _shiftedStage = shiftedStage;
    }

    public double getShiftedStage()
    {
        return _shiftedStage;
    }

    public void setDischarge( double discharge )
    {
        _discharge = discharge;
    }

    public double getDischarge()
    {
        return _discharge;
    }
}
