package ohd.hseb.monitor.precip;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import ohd.hseb.monitor.LocationDataFilter;
import ohd.hseb.util.BooleanHolder;
import ohd.hseb.util.gui.jtable.JTableRowData;

public class PrecipLocationDataFilter extends LocationDataFilter
{
    
    private BooleanHolder _showMissingBooleanHolder = null;
    
    public PrecipLocationDataFilter(Map<String, Boolean> locationDisplayMap, BooleanHolder showMissingBooleanHolder)
    {
       _locationDisplayMap = locationDisplayMap;  
       _showMissingBooleanHolder = showMissingBooleanHolder;
    }
    
    /**
     * Filter the allRowDataList based on the locationDisplayMap
     */
    public List<JTableRowData> filter(List<JTableRowData> allRowDataList)
    {
        boolean showMissing = _showMissingBooleanHolder.getValue();
        List filteredRowDataList = new ArrayList();
        
        if (_locationDisplayMap != null)
        {
            for (int i = 0; i < allRowDataList.size(); i++)
            {   
                PrecipMonitorJTableRowData rowData = (PrecipMonitorJTableRowData) allRowDataList.get(i);
                String lid = rowData.getLid();
                Boolean shouldDisplay = _locationDisplayMap.get(lid);
                if ((shouldDisplay != null) && (shouldDisplay) ) //based on selection in the tree
                {
                    if (showMissing) //based on menu selection
                    {
                        filteredRowDataList.add(rowData);
                    }
                    else //don't show missing
                    {
                        if (rowData.getPrecipData().isDataAvailable())
                        {
                            filteredRowDataList.add(rowData);
                        }
                    }
                }  //end if shouldDisplay
            } //end for
        }


        return filteredRowDataList;
    }
        

}
