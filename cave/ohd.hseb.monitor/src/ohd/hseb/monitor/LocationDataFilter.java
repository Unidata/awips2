package ohd.hseb.monitor;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import ohd.hseb.util.gui.jtable.JTableRowData;

public abstract class LocationDataFilter implements DataFilter
{
    protected Map<String, Boolean> _locationDisplayMap;

    public LocationDataFilter(Map<String, Boolean> locationDisplayMap)
    {
        _locationDisplayMap = locationDisplayMap;
    }
    
    public LocationDataFilter()
    {
        _locationDisplayMap = new HashMap();
    }
    
    public Map<String, Boolean> getLocationDisplayMap()
    {
        return _locationDisplayMap;
    }
    
    /**
     * Set this location to be displayed / not
     * @param locationId
     * @param shouldDisplay - true / false
     */
    public void setDisplayStatus(String locationId, Boolean shouldDisplay)
    {
        _locationDisplayMap.put(locationId, shouldDisplay);   
    }
    
    /**
     * Reset all locations in the location display map 
     *
     */
    public void blockAllLocations()
    {
        Set locationIdSet = _locationDisplayMap.keySet();
        
        Iterator iterator = locationIdSet.iterator();
        while(iterator.hasNext())
        {
            String locationId = iterator.next().toString();
            _locationDisplayMap.put(locationId, false); 
        }
    }

    /**
     * Filter the allRowDataList based on the locationDisplayMap
     */
    public abstract List<JTableRowData> filter(List<JTableRowData> allRowDataList);
    
}
