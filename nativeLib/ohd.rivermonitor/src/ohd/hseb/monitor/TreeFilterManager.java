package ohd.hseb.monitor;

import java.util.List;

import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

import ohd.hseb.util.SessionLogger;

public class TreeFilterManager
{
    private SessionLogger _logger;
    private LocationDataFilter _locationDataFilter;

    public TreeFilterManager(SessionLogger logger, LocationDataFilter locationDataFilter)
    {
        _logger = logger;
        _locationDataFilter = locationDataFilter;
    }

    public LocationDataFilter getLocationDataFilter()
    {
        return _locationDataFilter;
    }
    
    public List filter(List allRowDataList)
    {
        return (_locationDataFilter.filter(allRowDataList));
    }

    public void setDisplayStatus(String location, boolean shouldDisplay)
    {
        _locationDataFilter.setDisplayStatus(location, shouldDisplay);
    }

    public void traversePathsToDisplayLocationSelected(TreePath paths[])
    {
        String header = "TreeFilterManager.parsePathsToDisplayLocationSelected()";

        _locationDataFilter.blockAllLocations();

        if(paths != null)
        {
            for(int i=0; i < paths.length; i++)
            {
                if(paths[i] == null)
                {
                    _logger.log(header + "Invalid path... Check ... ");
                }
                else
                {
                    DefaultMutableTreeNode node = (DefaultMutableTreeNode) paths[i].getLastPathComponent();
                    setAllLocationsUnderThisNodeToBeDisplayed(node);
                }
            }
        }
    }

    public void setAllLocationsUnderThisNodeToBeDisplayed(DefaultMutableTreeNode node)
    {
        if(node != null)
        {
            if(node.isLeaf())
            {
                setDisplayStatus(node.toString(), true);   
            }
            else
            {
                int childCount = node.getChildCount();
                for(int i=0; i < childCount; i++)
                {
                    DefaultMutableTreeNode childNode = (DefaultMutableTreeNode) node.getChildAt(i);
                    setAllLocationsUnderThisNodeToBeDisplayed(childNode);
                }
            }
        }
    }

}

