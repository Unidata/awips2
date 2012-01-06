package ohd.hseb.monitor;

import java.util.List;

import ohd.hseb.util.gui.jtable.JTableRowData;

public interface DataFilter
{
    /**
     * Returns the filtered row data list
     * @param allRowDataList
     * @return
     */
    List<JTableRowData> filter(List<JTableRowData> allRowDataList);
    
    
}
