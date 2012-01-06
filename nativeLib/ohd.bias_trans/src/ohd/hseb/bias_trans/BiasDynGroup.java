package ohd.hseb.bias_trans;
import java.util.*;

public class BiasDynGroup
{
    private String _radarId;
    private List _biasDynSetList;
    
    public BiasDynGroup ( )
    {
    }

    public void setRadarId(String radarId)
    {
        _radarId = radarId;
    }

    public String getRadarId()
    {
        return _radarId;
    }

    public void setBiasDynSetList(List biasDynSetList)
    {
        _biasDynSetList = biasDynSetList;
    }

    public List getBiasDynSetList()
    {
        return _biasDynSetList;
    }
    
    public String toString ()
    {
        
        StringBuffer outputBuffer = new StringBuffer ("_radarId = " + _radarId +"\n");
        
                       
        for ( int i = 0; i < _biasDynSetList.size(); ++i )
        {
            outputBuffer.append ( _biasDynSetList.get(i));
        }
        
        return outputBuffer.toString();
    }
}
