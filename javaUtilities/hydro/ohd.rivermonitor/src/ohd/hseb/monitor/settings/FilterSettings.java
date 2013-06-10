package ohd.hseb.monitor.settings;

import javax.swing.tree.TreePath;

public class FilterSettings
{
    public static String PATH_TAG = "PATH";
    public static String EXPAND_PATH_TAG = "EXPANDPATH";
    
    public static String TREE_SETTINGS_BEGIN_TAG = "#HSA-GROUP-LOCATION TREE SETTINGS";
    
    private TreePath[] _selectedPaths;
    private TreePath[] _expandedPaths;
    
    public TreePath[] getSelectedPaths() 
    {
        return _selectedPaths;
    }

    public void setSelectedPaths(TreePath[] selectedPaths)
    {
        _selectedPaths = selectedPaths;
    }

    public TreePath[] getExpandedPaths() 
    {
        return _expandedPaths;
    }

    public void setExpandedPaths(TreePath[] expandedPaths)
    {
        _expandedPaths = expandedPaths;
    }

}
