package ohd.hseb.monitor.treefilter;

import java.awt.Color;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.JCheckBox;
import javax.swing.JTree;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;

public class MonitorCheckTreeManager extends MouseAdapter implements TreeSelectionListener
{ 
    private MonitorCheckTreeSelectionModel _selectionModel; 
    private JTree _tree = new JTree(); 
    int _hotspot = new JCheckBox().getPreferredSize().width; 
 
    private List _allAvailablePathsList = null;
    private TreePath[] _allAvailablePaths = null;
    private Map _allAvailablePathsMap = new HashMap();
    
    public MonitorCheckTreeManager(JTree tree, 
                                   TreePath[] preSelectedPaths,
                                   TreePath[] preExpandedPaths,
                                   Map<String, Color> rowToCriticalColorMap,
                                   String iconsDir)
    {
        final String header = "MonitorCheckTreeManager - constructor()";
        
        System.out.println("In "+ header);
        this._tree = tree; 
        _selectionModel = new MonitorCheckTreeSelectionModel(tree.getModel()); 
        tree.setCellRenderer(new MonitorCheckTreeCellRenderer(tree.getCellRenderer(), _selectionModel, rowToCriticalColorMap, iconsDir));

        _allAvailablePathsList = new ArrayList();
        TreeNode root = (TreeNode)tree.getModel().getRoot();
        determineAllAvailablePaths(new TreePath(root));
        _allAvailablePaths = (TreePath[]) (_allAvailablePathsList.toArray(new TreePath[0]));

        printTreePathArray(preSelectedPaths, header);
        if(preSelectedPaths != null)
        {
            setPreSelectedPathsAsSelected(getValidTreePaths(preSelectedPaths));
//            printTreePathArray(getAppropriateTreePaths(preSelectedPaths), header + "getAppPaths(): ");
        }

        if(preExpandedPaths != null)
        {
            expandThesePaths(getValidTreePaths(preExpandedPaths));
        }
       
        tree.addMouseListener(this); 
        _selectionModel.addTreeSelectionListener(this); 
    
    } 

    private void printTreePathArray(TreePath[] treePathArray, String message)
    {
        if (message != null)
        {
            System.out.println(message);
        }
        
        if (treePathArray != null)
        {
            for (TreePath treePath : treePathArray)
            {
                System.out.println(treePath);
            }
        }
        
        return;
    }
    
    public void setRowToCriticalColorMap(Map <String, Color> rowToCriticalColorMap)
    {
        MonitorCheckTreeCellRenderer renderer = (MonitorCheckTreeCellRenderer) _tree.getCellRenderer();
        renderer.setRowToCriticalColorMap(rowToCriticalColorMap);
    }

    public TreePath[] getAllAvailablePaths()
    {
        return _allAvailablePaths;
    }

    public void setPreSelectedPathsAsSelected(TreePath[] preSelectedPathArray)
    {
        String header = "MonitorCheckTreeManager.setPreSelectedPathsAsSelected(): ";

        //This is to prevent a whole bunch of defunct selectionModels from acting as listeners,
        _selectionModel.removeTreeSelectionListener(this);
        
        for(int i=0 ; i < preSelectedPathArray.length; i++)
        {
            selectAPath(preSelectedPathArray[i]);
        }

        _selectionModel.addTreeSelectionListener(this);
        _tree.treeDidChange();
    }

//-------------------------------------------------------------------------------------------------
    
    public TreePath[] getValidTreePaths(TreePath desiredPathArray[])
    {
        String header = "MonitorCheckTreeManager.getAppropriateTreePaths(): ";
        
        if (desiredPathArray == null)
            return null;

        TreePath pathsToReturn[] = null;
        Set pathsToReturnSet = new HashSet();
        
        // if the desiredPath is [HSA] then return all valid paths
        if(desiredPathArray.length == 1 && desiredPathArray[0].toString().equals("[HSA]"))
        {
            for(int i=0 ; i< _allAvailablePathsList.size(); i++)
            {
                TreePath path = (TreePath) _allAvailablePathsList.get(i);
                pathsToReturnSet.add(path);
            } 
        }
        else
        {
            //Create a set to search
            Set<String> desiredPathStringSet = new HashSet<String>();      
            for (TreePath desiredPath : desiredPathArray)
            {
                desiredPathStringSet.add(desiredPath.toString());
            }

            //check each path
            for(int i=0 ; i< _allAvailablePathsList.size(); i++)
            {
                TreePath path = (TreePath) _allAvailablePathsList.get(i);
                if ( withinTreeSet(path, desiredPathStringSet) )
                {
                    pathsToReturnSet.add(path);
                }
            }
        }
        
        pathsToReturn = (TreePath[]) pathsToReturnSet.toArray(new TreePath[0]);
        
    //    printTreePathArray(pathsToReturn, header);
           
        return pathsToReturn;
    }

//  -------------------------------------------------------------------------------------------------
    
    private void removeSelectedPaths()
    {
        _selectionModel.removeTreeSelectionListener(this);
        
        TreePath paths[] = _selectionModel.getSelectionPaths();
        if(paths != null)
        {
            for(int i=0; i < paths.length; i++)
            {
              
                try
                {
                    _selectionModel.removeSelectionPath(paths[i]);
                }
                finally
                {
              
                    _tree.treeDidChange();
                }
            }
        }
        
        _selectionModel.addTreeSelectionListener(this); 
    }

    private void selectAPath(TreePath path)
    {
        boolean selected = _selectionModel.isPathSelected(path, true);
        
        //TODO - What does the following line do?
        //This was to prevent a whole bunch of defunct selectionModels from acting as listeners,
       //although, it seems like overkill to be called that many times.
        //   _selectionModel.removeTreeSelectionListener(this);
        try
        { 
            if(!selected)
            {
                _selectionModel.addSelectionPath(path);
            }
        }
        finally
        {

        }

    }

    public void mouseClicked(MouseEvent me)
    {  
        TreePath path = _tree.getPathForLocation(me.getX(), me.getY()); 
        if(path==null) 
            return; 
        if(me.getX()>_tree.getPathBounds(path).x+_hotspot) 
            return; 

        boolean selected = _selectionModel.isPathSelected(path, true); 
        _selectionModel.removeTreeSelectionListener(this); 
        try
        { 
            if(selected) 
                _selectionModel.removeSelectionPath(path); 
            else 
                _selectionModel.addSelectionPath(path); 
        } 
        finally
        { 
            _selectionModel.addTreeSelectionListener(this); 
            _tree.treeDidChange(); 
        } 
    } 

    public MonitorCheckTreeSelectionModel getSelectionModel()
    { 
        return _selectionModel; 
    }

    public void valueChanged(TreeSelectionEvent e) 
    {
        // Auto-generated method stub
    }


    private void determineAllAvailablePaths(TreePath parent) 
    {
        // Traverse children
        TreeNode node = (TreeNode) parent.getLastPathComponent();
        if (node.getChildCount() >= 0) 
        {
            for (Enumeration e = node.children(); e.hasMoreElements();) 
            {
                TreeNode n = (TreeNode) e.nextElement();

                TreePath path = parent.pathByAddingChild(n);
                determineAllAvailablePaths(path);

                if(_allAvailablePathsList != null)
                {
                    _allAvailablePathsList.add(path);
                    _allAvailablePathsMap.put(path.toString(), path);
                }
            }
        }
        
    }

    private boolean withinTreeSet(TreePath path, Set<String> treePathStringSet)
    {
        boolean result = false;
        
        result = treePathStringSet.contains(path.toString());
         
        return result;
    }
    
    
    private boolean withinTree(TreePath path, TreePath[] pathArray)
    {
        boolean result = false;
        
        Set<String> set = new HashSet<String>();
        
        for (TreePath pathFromArray: pathArray)
        {
            set.add(pathFromArray.toString());
        }
        
        result = set.contains(path.toString());
         
        return result;
    }
    
    public void collapseEntireTree()
    {
        for(int i = 0; i < _allAvailablePathsList.size(); i++)
        {
            TreePath treePath = (TreePath) _allAvailablePathsList.get(i);
          
            Object[] pathArray =  treePath.getPath();
            if (pathArray.length == 2) //just collapse the 2nd level.  Collapsing this level collapses the rest.
            {
                _tree.collapsePath(treePath);
            }
            
        }
    }

    public void expandThesePaths(TreePath[] paths)
    {   String header = "MonitorCheckTreeManager.expandThesePaths(): ";
        try
        {
            for(int i=0; i< paths.length ; i++)
            {
               _tree.expandPath(paths[i]);
            }
           
        }
        catch(Exception e)
        {
            System.out.println("Exception :"+e);
        }
        finally
        {
        }
        
        
    }

    public TreePath[] getAllExpandedPaths()
    {
        TreePath[] allExpandedPaths = null;
        List allExpandedPathList = new ArrayList();

        for(int i=0; i < _allAvailablePaths.length; i++)
        {
            TreePath path = _allAvailablePaths[i];
            if(_tree.isExpanded(path))
            {
                allExpandedPathList.add(path);

            }
            else
                continue;
        }
        
        allExpandedPaths = (TreePath[]) allExpandedPathList.toArray(new TreePath[0]);
       
        return allExpandedPaths;
    }

}

