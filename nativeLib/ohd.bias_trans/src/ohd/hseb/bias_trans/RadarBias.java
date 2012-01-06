package ohd.hseb.bias_trans;

import java.util.*;

public class RadarBias
{
    private String _dateTime;
    private MemorySpanGroup _memorySpanGroup;
    private List _biasDynGroupList;

    public void setMemorySpanGroup(MemorySpanGroup memorySpanGroup)
    {
        _memorySpanGroup = memorySpanGroup;
    }

    public MemorySpanGroup getMemorySpanGroup()
    {
        return _memorySpanGroup;
    }

    public void setBiasDynGroupList(List biasDynGroupList)
    {
        _biasDynGroupList = biasDynGroupList;
    }

    public List getBiasDynGroupList()
    {
        return _biasDynGroupList;
    }
    
    public String toString ()
    {
        String output ="Radar Bias Info: " + "\nDate:" + _dateTime +"\n" + _memorySpanGroup;
        
        for ( int i = 0; i < _biasDynGroupList.size(); ++i )
        {
               output += _biasDynGroupList.get(i);
        }
        
        return output;
    }

    public void setDateTime(String dateTime)
    {
        _dateTime = dateTime;
    }

    public String getDateTime()
    {
        return _dateTime;
    }
    
}
