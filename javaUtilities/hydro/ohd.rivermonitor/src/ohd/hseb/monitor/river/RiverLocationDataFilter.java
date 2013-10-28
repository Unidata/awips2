package ohd.hseb.monitor.river;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import ohd.hseb.db.DbTable;
import ohd.hseb.monitor.LocationDataFilter;
import ohd.hseb.util.BooleanHolder;
import ohd.hseb.util.gui.jtable.JTableRowData;

public class RiverLocationDataFilter extends LocationDataFilter
{
    
    private BooleanHolder _showMissingBooleanHolder = null;
    
    public RiverLocationDataFilter(Map<String, Boolean> locationDisplayMap, BooleanHolder showMissingBooleanHolder)
    {
       _locationDisplayMap = locationDisplayMap;    
       _showMissingBooleanHolder = showMissingBooleanHolder;
    }
    
    
    public List<JTableRowData> filter(List<JTableRowData> allRowDataList)
    {
        boolean showMissing = _showMissingBooleanHolder.getValue();
        List<JTableRowData> filteredRowDataList = new ArrayList<JTableRowData>();
        
        if (_locationDisplayMap != null)
        {  
            for (int i = 0; i < allRowDataList.size(); i++)
            {   
                RiverMonitorJTableRowData rowData = (RiverMonitorJTableRowData) allRowDataList.get(i);
                String lid = rowData.getLid();
                Boolean shouldDisplay = _locationDisplayMap.get(lid);
                if ((shouldDisplay != null) && (shouldDisplay) ) //based on selection in the tree
                {
                   // String header = "RiverLocationDataFilter.filter(): ";
                   // System.out.println(header + " lid = " + lid + "hsa = " + rowData.getHsa() + " groupId = " + rowData.getGroupId());
                    
                    if (showMissing) //based on menu selection
                    {
                        filteredRowDataList.add(rowData);
                    }
                    else //don't show missing
                    {
                        if (isDataAvailable(rowData))
                        {
                            filteredRowDataList.add(rowData);
                        }
                    }
                }  //end if shouldDisplay
            } //end for
          
        } //end if (_locationDisplayMap != null)
        return filteredRowDataList;
    }
        
    private boolean isDataAvailable(RiverMonitorJTableRowData rowData)
    {
        boolean result = false;
        
        if     (rowData.getLatestObsValue() != DbTable.getNullDouble() ||
                rowData.getMaxFcstValue() != DbTable.getNullDouble())
        {
            result = true;
        }
        
        return result;
    }
 

}

