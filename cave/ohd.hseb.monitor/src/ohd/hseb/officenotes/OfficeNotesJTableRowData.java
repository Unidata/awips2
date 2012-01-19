package ohd.hseb.officenotes;

import ohd.hseb.ihfsdb.generated.OfficeNotesRecord;
import ohd.hseb.util.gui.jtable.AbstractJTableRowData;
import ohd.hseb.util.gui.jtable.BaseTableCell;
import ohd.hseb.util.gui.jtable.CellType;

public class OfficeNotesJTableRowData extends AbstractJTableRowData
{
    private String _topic;
    private String _id;
    private long _dataTime;
    private long _postingTime;
    private long _updateTime;
    private long _expireTime;
    private String _note;

    public OfficeNotesJTableRowData(String missingRepresentation)
    {
        setMissingRepresentation(missingRepresentation);
    }

    public String getTopic() 
    {
        return _topic;
    }
    public void setTopic(String topic) 
    {
        _topic = topic;
    }
    public long getDataTime() 
    {
        return _dataTime;
    }
    public void setDataTime(long dataTime) 
    {
        _dataTime = dataTime;
    }
    public long getExpireTime() 
    {
        return _expireTime;
    }
    public void setExpireTime(long expireTime) 
    {
        _expireTime = expireTime;
    }
    public String getId() 
    {
        return _id;
    }
    public void setId(String id) 
    {
        _id = id;
    }
    public String getNote() 
    {
        return _note;
    }
    public void setNote(String note) 
    {
        _note = note;
    }
    public long getPostingTime() 
    {
        return _postingTime;
    }
    public void setPostingTime(long postingTime) 
    {
        _postingTime = postingTime;
    }
    public long getUpdateTime() 
    {
        return _updateTime;
    }
    public void setUpdateTime(long time) 
    {
        _updateTime = time;
    }

    public void setOfficeNotesRecord(OfficeNotesRecord officeNotesRecord)
    {
        setTopic(officeNotesRecord.getTopic());
        setId(officeNotesRecord.getId());
        setDataTime(officeNotesRecord.getDatatime());
        setPostingTime(officeNotesRecord.getPostingtime());
        setUpdateTime(officeNotesRecord.getUpdatetime());
        setExpireTime(officeNotesRecord.getExpiretime());
        setNote(officeNotesRecord.getNote());
    }

    public void addAllCellsToMap()
    {
        addToCellMap(OfficeNotesColumns.TOPIC, CellType.STRING, getTopic() );
        addToCellMap(OfficeNotesColumns.ID, CellType.STRING, getId() );
        addToCellMap(OfficeNotesColumns.NOTE, CellType.STRING, getNote());
        addToCellMap(OfficeNotesColumns.DATA_TIME, CellType.DATE_TIME, getDataTime());
        addToCellMap(OfficeNotesColumns.UPDATE_TIME, CellType.DATE_TIME, getUpdateTime());
        addToCellMap(OfficeNotesColumns.POSTING_TIME, CellType.DATE_TIME, getPostingTime());
        addToCellMap(OfficeNotesColumns.EXPIRE_TIME, CellType.DATE_TIME, getExpireTime());
    }

    // -----------------------------------------------------------------------------------

    private void addToCellMap(String columnName, CellType cellType, 
            Object value)
    {        
        BaseTableCell cell = new BaseTableCell (columnName, cellType, value, getMissingRepresentation(), "MM/dd - HH:mm");
        addCell(cell);

     }
   
}
