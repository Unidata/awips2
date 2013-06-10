package ohd.hseb.rivermonlocgroup;

import ohd.hseb.util.gui.jtable.AbstractJTableRowData;
import ohd.hseb.util.gui.jtable.BaseTableCell;
import ohd.hseb.util.gui.jtable.CellType;

public class RiverMonGroupJTableRowData extends AbstractJTableRowData
{
    private String _groupId;
    private String _groupName;
    private int _groupOrdinal;
    private String _hsa;

    public RiverMonGroupJTableRowData(String missingRepresentation)
    {
        setMissingRepresentation(missingRepresentation);
    }

    public String getGroupId()
    {
        return _groupId;
    }

    public void setGroupId(String groupId) 
    {
        _groupId = groupId;
    }

    public String getGroupName() 
    {
        return _groupName;
    }

    public void setGroupName(String groupName) 
    {
        _groupName = groupName;
    }

    public String getHsa() 
    {
        return _hsa;
    }

    public void setHsa(String hsa) 
    {
        _hsa = hsa;
    }

    public int getGroupOrdinal() 
    {
        return _groupOrdinal;
    }

    public void setGroupOrdinal(int groupOrdinal) 
    {
        _groupOrdinal = groupOrdinal;
    }

    public void addAllCellsToMap()
    {
        addToCellMap(RiverMonGroupColumns.GROUP_ID, CellType.STRING, getGroupId() );
        addToCellMap(RiverMonGroupColumns.GROUP_NAME, CellType.STRING, getGroupName() );
        addToCellMap(RiverMonGroupColumns.GROUP_ORDINAL, CellType.INTEGER, getGroupOrdinal());
        addToCellMap(RiverMonGroupColumns.HSA, CellType.STRING, getHsa() );
    }

    // -----------------------------------------------------------------------------------

    // -----------------------------------------------------------------------------------

    private void addToCellMap(String columnName, CellType cellType, 
            Object value)
    {        
        BaseTableCell cell = new BaseTableCell (columnName, cellType, value, getMissingRepresentation());
        addCell(cell);

     }

 /*   private void addToCellMap(String columnName, CellType cellType,
            Object value)
    {
        int decimalPointsForDisplay = 2;
        MonitorCell cell = new MonitorCell (columnName,  cellType,
            value, ThreatLevel.NO_THREAT,
            _missingRepresentation, decimalPointsForDisplay);
        addCell(cell);

    }*/
    
}
