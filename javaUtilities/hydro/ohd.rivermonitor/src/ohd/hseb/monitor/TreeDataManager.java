package ohd.hseb.monitor;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

public class TreeDataManager
{

    private Map<String,DefaultMutableTreeNode> _nodeMap = new HashMap<String, DefaultMutableTreeNode>();

    public TreeDataManager()
    {

    }

    private void addNode(DefaultMutableTreeNode baseNode, TreePath nodePath)
    {
        String header = "TreeDataManager.addNode(): ";
        DefaultMutableTreeNode node = null;
        DefaultMutableTreeNode parentNode = null;

        if(isRootPath(baseNode, nodePath))
        {
//          is root node, so done
            _nodeMap.put(nodePath.toString(), baseNode);      
        }
        else //is not root node
        {
            //find the parent first
            TreePath parentPath = nodePath.getParentPath();    
            parentNode = findNode(baseNode, parentPath);
            
            if (parentNode != null) //found the node, so don't add it
            {
                //check to make sure this node does not already exist
                node = findNode(parentNode, nodePath); //just search under parentNode for the child
       
                if (node == null) 
                {
                    DefaultMutableTreeNode childNode = new DefaultMutableTreeNode(nodePath.getLastPathComponent());
                    parentNode.add(childNode);
                    
                    TreePath childNodePath = getTreePathForNode(childNode);
                    
                    //System.out.println(header + "Inserting  " + childNodePath.toString() + ", " + childNode + " into nodeMap");
                    _nodeMap.put(childNodePath.toString(), childNode);
                }
            }
            else //parentNode is null 
            {
                //add the parent node
                
               // System.out.println(header + "Adding parent node  " + parentPath.toString() + ", " + baseNode);
     
                addNode(baseNode, parentPath);

                //add the child node
                
             //   System.out.println(header + "Adding childt node  " + nodePath.toString() + ", " + baseNode);
                
                addNode(baseNode, nodePath);
            }
        }
    }

    private boolean isRootPath(DefaultMutableTreeNode node, TreePath path)
    {
        boolean result = false;

        if(path != null) // is valid path
        {
            int pathCount = path.getPathCount();

            if(pathCount == 1)
            {
                if (node.toString().equals(path.getPathComponent(0).toString()))
                {
                    result = true;
                }
            }
        }

        return result;
    }

    private DefaultMutableTreeNode oldFindNode(DefaultMutableTreeNode node, TreePath targetPath)
    {
        DefaultMutableTreeNode resultNode = null;
        DefaultMutableTreeNode testNode = null;

        if(node != null)
        {
            if(isNodeAMatchForPath(node, targetPath))
            { 
                resultNode = node;
            }
            else
            {
                if(isNodeInPath(node, targetPath))
                {
                    int childCount = node.getChildCount();
                    for(int i=0; i < childCount; i++)
                    {
                        DefaultMutableTreeNode childNode = (DefaultMutableTreeNode) node.getChildAt(i);

                        testNode = findNode(childNode, targetPath);
                        if (testNode != null)
                        {
                            resultNode = testNode;
                            break;
                        }
                    }
                }
            }
        }
        return  (DefaultMutableTreeNode ) resultNode;
    }

    private DefaultMutableTreeNode findNode2(DefaultMutableTreeNode node, TreePath targetPath)
    {
        String header = "TreeDataManager.findNode(): ";
        DefaultMutableTreeNode resultNode = null;
        DefaultMutableTreeNode testNode = null;

        if(node == null)
        {
            return null;
        } 

        if(isNodeInPath(node, targetPath))
        {

            if(isNodeAMatchForPath(node, targetPath))
            { 
                resultNode = node;
            }
            else //not a match
            {

                int childCount = node.getChildCount();
                for(int i=0; i < childCount; i++)
                {
                    DefaultMutableTreeNode childNode = (DefaultMutableTreeNode) node.getChildAt(i);

                    if(isNodeInPath(childNode, targetPath))
                    {
                        if(isNodeAMatchForPath(childNode, targetPath))
                        { 
                            //   System.out.println(header + " childNode = " + 
                            //           childNode.toString() + " matches  " + targetPath);


                            resultNode = childNode;
                            break;
                        }
                        else //not complete match, so keep looking from here on down
                        {
                            //    System.out.println(header + " childNode = " + 
                            //            childNode.toString() + " is in  " + targetPath);

                            resultNode = findNode2(childNode, targetPath);
                        }
                        break;
                    }

                    else //not a match and not in path
                    {
                        //  System.out.println(header + " childNode = " + 
                        //          childNode.toString() + " is not in  " + targetPath);
                        //do nothing
                    }

                } //end for 
            } //end else

        }

        return  (DefaultMutableTreeNode ) resultNode;
    }

    
    private DefaultMutableTreeNode findNode(DefaultMutableTreeNode node, TreePath targetPath)
    {
 //       String header = "TreeDataManager.findNode(): ";
        DefaultMutableTreeNode resultNode = _nodeMap.get(targetPath.toString());
        
        return  resultNode;
    }

 
    private boolean isNodeInPath(DefaultMutableTreeNode node, TreePath targetPath)
    {
        boolean isInPath = false;

        TreePath nodePath = getTreePathForNode(node);
        isInPath = isPathBeginningOfTargetPath(nodePath, targetPath);

        return isInPath;
    }

    private boolean isPathBeginningOfTargetPath(TreePath nodePath, TreePath targetPath)
    {
        boolean isBeginning = false;

        String nodePathString = nodePath.toString();
        String targetPathString = targetPath.toString();

        int length = nodePathString.length();
        nodePathString = nodePathString.substring(0, length - 1);

        isBeginning = targetPathString.startsWith(nodePathString);

        return isBeginning;
    }

    private boolean isNodeAMatchForPath(DefaultMutableTreeNode node, TreePath targetPath)
    {
        TreePath nodePath = getTreePathForNode(node);
        boolean isTargetNode = false;

        if (targetPath.toString().equals(nodePath.toString()))
        {
            isTargetNode = true;
        }

        return isTargetNode;
    }

    private TreePath getTreePathForNode(DefaultMutableTreeNode node)
    {
        TreePath nodePath = new TreePath(node.getPath());
        return nodePath;
    }

    private List<TreePath> createTreePathList(List<String[]> stringArrayList)
    {
        List<TreePath> treePathList = new ArrayList<TreePath>();

        for(int i=0; i < stringArrayList.size(); i++)
        {
            String[] stringArray = (String[]) stringArrayList.get(i);
            DefaultMutableTreeNode[] nodeArray = new DefaultMutableTreeNode[stringArray.length];
            for(int j=0; j < stringArray.length; j++)
            {
                nodeArray[j] = new DefaultMutableTreeNode(stringArray[j]);
            }
            TreePath path = new TreePath(nodeArray);
            treePathList.add(path);
        }

        return treePathList;
    }

    public JTree createFilterTreeFromList(DefaultMutableTreeNode rootNode, List<String[]> stringArrayList)
    {
        String header = "TreeDataManager.createFilterTreeFromList(): ";
        
        _nodeMap.clear();
        
        List<TreePath> treePathList = createTreePathList(stringArrayList);
        
        for(int i=0; i < treePathList.size(); i++)
        {
            TreePath path = treePathList.get(i);

            addNode(rootNode, path);
        }

         JTree tree = new JTree(rootNode);

        return tree;
    }

}
