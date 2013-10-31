package ohd.hseb.raxdb_sync;

import java.util.ArrayList;
import java.util.List;

public class DiffSet
{
    private List _differenceList;
    
    public DiffSet()
    {
        // create a List to hold database Differences
        _differenceList = new ArrayList();
    } // end of Constructor
    
    public void addDiffSet(DiffSet anotherDiffSet)
    {
        List diffList = anotherDiffSet.getList();
        RecordDifference recordDifference = null;
        
        for (int i=0; i < diffList.size(); i++)
        {
            recordDifference = (RecordDifference) diffList.get(i);
            addDifference(recordDifference);
        }
    }

    public void addDifference(RecordDifference difference)
    {
        _differenceList.add(difference);
    }

    public void removeDifference(RecordDifference difference)
    {
        _differenceList.remove(difference);
    }

    public List getList()
    {
        return _differenceList;
    }

} // end of DiffSet class
