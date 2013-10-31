package ohd.hseb.rivermonlocgroup;

import ohd.hseb.monitor.MonitorCell;
import ohd.hseb.monitor.ThreatLevel;
import ohd.hseb.util.gui.jtable.AbstractJTableRowData;
import ohd.hseb.util.gui.jtable.CellType;

public class RiverMonLocationJTableRowData extends AbstractJTableRowData
{
    private String _lid;
    private String _groupId;
    private int _locationOrdinal;

    public RiverMonLocationJTableRowData(String missingRepresentation)
    {
        setMissingRepresentation(missingRepresentation);
    }

    public String getLid()
    {
        return _lid;
    }

    public void setLid(String lid) 
    {
        _lid = lid;
    }

    public String getGroupId() 
    {
        return _groupId;
    }

    public void setGroupId(String groupId) 
    {
        _groupId = groupId;
    }

    public int getLocationOrdinal() 
    {
        return _locationOrdinal;
    }

    public void setLocationOrdinal(int groupOrdinal) 
    {
        _locationOrdinal = groupOrdinal;
    }

    public void addAllCellsToMap()
    {
        addToCellMap(RiverMonLocationColumns.GROUP_ID, CellType.STRING, getGroupId() );
        addToCellMap(RiverMonLocationColumns.LOCATION_ID, CellType.STRING, getLid() );
        addToCellMap(RiverMonLocationColumns.LOCATION_ORDINAL, CellType.INTEGER, getLocationOrdinal());

    }
    
    // -----------------------------------------------------------------------------------


    private void addToCellMap(String columnName, CellType cellType,
            Object value)
    {
        int decimalPointsForDisplay = 2;
        MonitorCell cell = new MonitorCell (columnName,  cellType,
            value, ThreatLevel.NO_THREAT,
            _missingRepresentation, decimalPointsForDisplay);
        addCell(cell);
        
    }
}
