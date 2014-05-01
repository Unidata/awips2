package ohd.hseb.raxbase.table;

import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.raxbase.model.RaxLocation;
import ohd.hseb.util.gui.jtable.AbstractJTableRowData;
import ohd.hseb.util.gui.jtable.BaseTableCell;
import ohd.hseb.util.gui.jtable.CellType;

public class ArcBaseLocationJTableRowData extends AbstractJTableRowData
{
    private String _missingRepresentation = "";

    private RaxLocation _raxLocation = null;

    public ArcBaseLocationJTableRowData()
    {
        setMissingRepresentation( "" );
    }

    public void addAllCellsToMap()
    {
        addCell( new BaseTableCell( "Station", CellType.STRING, _raxLocation.getLid(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Begin Date", CellType.DATE, _raxLocation.getBeginDate(), _missingRepresentation ) );
        addCell( new BaseTableCell( "End Date", CellType.DATE, _raxLocation.getEndDate(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Latitude", CellType.DOUBLE, _raxLocation.getLatitude(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Longitude", CellType.DOUBLE, _raxLocation.getLongitude(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Elevation", CellType.INTEGER, _raxLocation.getElevation(), _missingRepresentation ) );
        addCell( new BaseTableCell( "State", CellType.STRING, _raxLocation.getState(), _missingRepresentation ) );
        addCell( new BaseTableCell( "HUC", CellType.STRING, _raxLocation.getHuc(), _missingRepresentation ) );
        addCell( new BaseTableCell( "WFO", CellType.STRING, _raxLocation.getWfo(), _missingRepresentation ) );
        addCell( new BaseTableCell( "RFC", CellType.STRING, _raxLocation.getRfc(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Name", CellType.STRING, _raxLocation.getName(), _missingRepresentation ) );
        addCell( new BaseTableCell( "Detailed Info", CellType.STRING, _raxLocation.getDetailInfo(), _missingRepresentation ) );
        addCell( new BaseTableCell( "HSA", CellType.STRING, _raxLocation.getHsa(), _missingRepresentation ) );
    }

    public String getStringValue(long value, long missingValue)
    {
        String result = getMissingRepresentation();
        if ( value != missingValue )
        {
            result = String.valueOf( DbTimeHelper.getDateStringFromLongTime( value ) );
        }
        return result;
    }

    public void setRaxLocation( RaxLocation raxLocation )
    {
        _raxLocation = raxLocation;
    }

    public RaxLocation getRaxLocation()
    {
        return _raxLocation;
    }
}
