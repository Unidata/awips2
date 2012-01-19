package ohd.hseb.monitor.treefilter;

import javax.swing.*;
import javax.swing.event.ChangeListener;
import javax.swing.plaf.ActionMapUIResource;
import java.awt.event.*;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Image;
import java.awt.Toolkit;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JPanel;
import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.TreeCellRenderer;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;

import ohd.hseb.util.CodeTimer;

public class MonitorCheckTreeCellRenderer extends JPanel implements TreeCellRenderer
{
    /** This is a type-safe enumerated type */
    public static class State { private State() { } }
    public static final State NOT_SELECTED = new State();
    public static final State SELECTED = new State();
    public static final State DONT_CARE = new State();

    private MonitorCheckTreeSelectionModel _selectionModel; 
    private TreeCellRenderer _delegate; 
    private TristateCheckBox _checkBox = new TristateCheckBox(); 

    private String _greenLocationIconFile ;
    private String _redLocationIconFile ;
    private String _yellowLocationIconFile;
    private String _darkGreenLocationIconFile;
    private String _grayLocationIconFile;

    private String _greenGroupIconFile;
    private String _yellowGroupIconFile;
    private String _redGroupIconFile;
    private String _darkGreenGroupIconFile;
    private String _grayGroupIconFile;

    private String _greenHsaIconFile;
    private String _yellowHsaIconFile;
    private String _redHsaIconFile;
    private String _darkGreenHsaIconFile;
    private String _grayHsaIconFile;

    private Icon _greenLocationIcon ;
    private Icon _redLocationIcon ;
    private Icon _yellowLocationIcon;
    private Icon _darkGreenLocationIcon;
    private Icon _grayLocationIcon;

    private Icon _greenGroupIcon ;
    private Icon _redGroupIcon ;
    private Icon _yellowGroupIcon;
    private Icon _darkGreenGroupIcon;
    private Icon _grayGroupIcon;

    private Icon _greenHsaIcon ;
    private Icon _redHsaIcon ;
    private Icon _yellowHsaIcon;
    private Icon _darkGreenHsaIcon;
    private Icon _grayHsaIcon;

    private JLabel _redLocationLabel = null;
    private JLabel _greenLocationLabel = null;
    private JLabel _yellowLocationLabel = null;
    private JLabel _darkGreenLocationLabel = null;
    private JLabel _grayLocationLabel = null;
    private JLabel _redGroupLabel = null;
    private JLabel _greenGroupLabel = null;
    private JLabel _yellowGroupLabel = null;
    private JLabel _darkGreenGroupLabel = null;
    private JLabel _grayGroupLabel = null;
    private JLabel _redHsaLabel = null;
    private JLabel _greenHsaLabel = null;
    private JLabel _yellowHsaLabel = null;
    private JLabel _darkGreenHsaLabel = null;
    private JLabel _grayHsaLabel = null;

  //  private List _rowDataForAllLocationsList;
    private Map _rowToCriticalColorMap = null;
    
    public MonitorCheckTreeCellRenderer(TreeCellRenderer delegate, MonitorCheckTreeSelectionModel selectionModel, 
                                       Map<String, Color> rowToCriticalColorMap,  String iconsDir)
    { 
        this._delegate = delegate; 
        this._selectionModel = selectionModel;
        _rowToCriticalColorMap = rowToCriticalColorMap;
        setLayout(new BorderLayout()); 
        setOpaque(false); 
        _checkBox.setOpaque(false); 

        //createIcons(iconsDir);
        createLabels();
    }

    public void setRowToCriticalColorMap(Map<String,Color> rowToCriticalColorMap)
    {
        _rowToCriticalColorMap = rowToCriticalColorMap;
    }
 
    private void createLabels()
    {
        _redLocationLabel = new JLabel();
        _redLocationLabel.setIcon(_redLocationIcon);
        _redLocationLabel.setForeground(Color.RED);
        _greenLocationLabel = new JLabel();
        _greenLocationLabel.setIcon(_greenLocationIcon);
        _greenLocationLabel.setForeground(Color.GREEN);
        _yellowLocationLabel = new JLabel();
        _yellowLocationLabel.setIcon(_yellowLocationIcon);
        _yellowLocationLabel.setForeground(Color.YELLOW);
        _darkGreenLocationLabel = new JLabel();
        _darkGreenLocationLabel.setIcon(_darkGreenLocationIcon);
        _darkGreenLocationLabel.setForeground(new Color(0, 153, 0));
        _grayLocationLabel = new JLabel();
        _grayLocationLabel.setIcon(_grayLocationIcon);
        _grayLocationLabel.setForeground(Color.LIGHT_GRAY);

        _redGroupLabel = new JLabel();
        _redGroupLabel.setIcon(_redGroupIcon);
        _redGroupLabel.setForeground(Color.RED);
        _yellowGroupLabel = new JLabel();
        _yellowGroupLabel.setIcon(_yellowGroupIcon);
        _yellowGroupLabel.setForeground(Color.YELLOW);
        _greenGroupLabel = new JLabel();
        _greenGroupLabel.setIcon(_greenGroupIcon);
        _greenGroupLabel.setForeground(Color.GREEN);
        _darkGreenGroupLabel = new JLabel();
        _darkGreenGroupLabel.setIcon(_darkGreenGroupIcon);
        _darkGreenGroupLabel.setForeground(new Color(0, 153, 0));
        _grayGroupLabel = new JLabel();
        _grayGroupLabel.setIcon(_grayGroupIcon);
        _grayGroupLabel.setForeground(Color.LIGHT_GRAY);

        _redHsaLabel = new JLabel();
        _redHsaLabel.setIcon(_redHsaIcon);
        _redHsaLabel.setForeground(Color.RED);
        _yellowHsaLabel = new JLabel();
        _yellowHsaLabel.setIcon(_yellowHsaIcon);
        _yellowHsaLabel.setForeground(Color.YELLOW);
        _greenHsaLabel = new JLabel();
        _greenHsaLabel.setIcon(_greenHsaIcon);
        _greenHsaLabel.setForeground(Color.GREEN);
        _darkGreenHsaLabel = new JLabel();
        _darkGreenHsaLabel.setIcon(_darkGreenHsaIcon);
        _darkGreenHsaLabel.setForeground(new Color(0, 153, 0));
        _grayHsaLabel = new JLabel();
        _grayHsaLabel.setIcon(_grayHsaIcon);
        _grayHsaLabel.setForeground(Color.LIGHT_GRAY);
    }


    private void createIcons(String iconsDir)
    {
        _greenHsaIconFile = iconsDir + "GreenHsa.GIF";
        _redHsaIconFile = iconsDir + "RedHsa.GIF";
        _yellowHsaIconFile = iconsDir + "YellowHsa.GIF";
        _darkGreenHsaIconFile = iconsDir + "DarkGreenHsa.GIF";
        _grayHsaIconFile = iconsDir + "GrayHsa.GIF";

        _greenGroupIconFile = iconsDir + "GreenGroup.GIF";
        _redGroupIconFile = iconsDir + "RedGroup.GIF";
        _yellowGroupIconFile = iconsDir + "YellowGroup.GIF";
        _darkGreenGroupIconFile = iconsDir + "DarkGreenGroup.GIF";
        _grayGroupIconFile = iconsDir + "GrayGroup.GIF";

        _greenLocationIconFile = iconsDir + "GreenLocation.GIF";
        _redLocationIconFile = iconsDir + "RedLocation.GIF";
        _yellowLocationIconFile = iconsDir + "YellowLocation.GIF";
        _darkGreenLocationIconFile = iconsDir + "DarkGreenLocation.GIF";
        _grayLocationIconFile = iconsDir + "GrayLocation.GIF";

        _greenLocationIcon = createIcon(_greenLocationIconFile);
        _redLocationIcon = createIcon(_redLocationIconFile);
        _yellowLocationIcon = createIcon(_yellowLocationIconFile);
        _darkGreenLocationIcon = createIcon(_darkGreenLocationIconFile);
        _grayLocationIcon = createIcon(_grayLocationIconFile);

        _greenGroupIcon= createIcon(_greenGroupIconFile);
        _yellowGroupIcon= createIcon(_yellowGroupIconFile);
        _redGroupIcon= createIcon(_redGroupIconFile);
        _darkGreenGroupIcon= createIcon(_darkGreenGroupIconFile);
        _grayGroupIcon= createIcon(_grayGroupIconFile);

        _greenHsaIcon= createIcon(_greenHsaIconFile);
        _yellowHsaIcon= createIcon(_yellowHsaIconFile);
        _redHsaIcon= createIcon(_redHsaIconFile);
        _darkGreenHsaIcon= createIcon(_darkGreenHsaIconFile);
        _grayHsaIcon= createIcon(_grayHsaIconFile);

    }

    public Component getTreeCellRendererComponent(JTree tree, Object value,
                                                  boolean selected, boolean expanded,
                                                  boolean leaf, int row, boolean hasFocus)
    { 
        Component renderer = _delegate.getTreeCellRendererComponent(tree, value, selected, expanded, leaf, row, hasFocus);
        TreePath path = tree.getPathForRow(row); 
        Component returnedComponent = null;
        DefaultTreeCellRenderer tempRenderer = (DefaultTreeCellRenderer) renderer;
        returnedComponent = getAppropriateLabel(leaf, path, tempRenderer, value);
        if(path!=null)
        { 
            if(_selectionModel.isPathSelected(path, true))
            {
                _checkBox.setState(SELECTED);
            }
            else 
            {
                _checkBox.setState(_selectionModel.isPartiallySelected(path) ? null : NOT_SELECTED);
            }
        } 
        removeAll(); 
        add(_checkBox, BorderLayout.WEST); 
        add(returnedComponent, BorderLayout.CENTER);
        return this; 
    } 

    private JLabel getAppropriateLabelOld(boolean isLeaf, TreePath path, DefaultTreeCellRenderer renderer, Object value)
    {
        String header = "MonitorCheckTreeCellRenderer.getAppropriateLabelOld(): ";
        
        JLabel returnedComponent = null;
        renderer.setLeafIcon(null);
        renderer.setClosedIcon(null);
        renderer.setOpenIcon(null);
        CodeTimer timer = new CodeTimer();
        timer.restart();
        
        if (path == null)
        {
            System.out.println(header + "null value for value = " + value);
        }
          
        if (isLeaf) 
        {
          //  System.out.println(header + "Leaf: value = " + value);
            
            returnedComponent = getLabelComponentForChild(value);
            if (returnedComponent == null)
            {
               
                
                _greenLocationLabel.setText(value.toString());
                returnedComponent = _greenLocationLabel;
            }
        }
        else
        {
            if(path != null)
            {
                if(path.getPathCount() == 3) // group node
                {
                    DefaultMutableTreeNode node = (DefaultMutableTreeNode) path.getLastPathComponent();

                    JLabel groupLabel = null;
                    JLabel locationLabel = getLocationLabelDenotingCurrentStateForParentNode(node);
                    if (locationLabel == _redLocationLabel)
                    {
                        groupLabel = _redGroupLabel;
                    }
                    else if (locationLabel == _yellowLocationLabel)
                    {
                        groupLabel = _yellowGroupLabel;
                    }
                    else if (locationLabel == _grayLocationLabel)
                    {
                       // groupLabel = _grayGroupLabel;
                        groupLabel = _greenGroupLabel;
                    }
                    else if (locationLabel == _darkGreenLocationLabel)
                    {
                        groupLabel = _darkGreenGroupLabel;
                    }
                    else
                    {
                        groupLabel = _greenGroupLabel;
                    }
                    groupLabel.setText(value.toString());
                    returnedComponent = groupLabel;
                }// else if pathCount == 3 ends here
                else if(path.getPathCount() == 2) //hsa node
                {
                    DefaultMutableTreeNode node = (DefaultMutableTreeNode) path.getLastPathComponent();
                    int groupCount = node.getChildCount();
                    JLabel hsaLabel = null;
                    for(int j=0; j< groupCount; j++)
                    {
                        DefaultMutableTreeNode groupNode = (DefaultMutableTreeNode) node.getChildAt(j);
                        JLabel locationLabel = getLocationLabelDenotingCurrentStateForParentNode(groupNode);
                        if(hsaLabel == null)
                        {
                            if(locationLabel == _redLocationLabel)
                            {
                                hsaLabel = _redHsaLabel;
                                break;
                            }
                        }
                    }
                    if(hsaLabel == null)
                    {
                        for(int j=0; j< groupCount; j++)
                        {
                            DefaultMutableTreeNode groupNode = (DefaultMutableTreeNode) node.getChildAt(j);
                            JLabel locationLabel = getLocationLabelDenotingCurrentStateForParentNode(groupNode);
                            if(hsaLabel == null)
                            {
                                if(locationLabel == _yellowLocationLabel)
                                {
                                    hsaLabel = _yellowHsaLabel;
                                    break;
                                }
                            }
                        }
                    }
                    if(hsaLabel == null)
                    {
                        for(int j=0; j< groupCount; j++)
                        {
                            DefaultMutableTreeNode groupNode = (DefaultMutableTreeNode) node.getChildAt(j);
                            JLabel locationLabel = getLocationLabelDenotingCurrentStateForParentNode(groupNode);
                            if(hsaLabel == null)
                            {
                                if(locationLabel == _grayLocationLabel)
                                {
                                    hsaLabel = _greenHsaLabel;
                                  //  hsaLabel = _grayHsaLabel;
                                    break;
                                }
                            }
                        }
                    }
                    if(hsaLabel == null)
                    {
                        for(int j=0; j< groupCount; j++)
                        {
                            DefaultMutableTreeNode groupNode = (DefaultMutableTreeNode) node.getChildAt(j);
                            JLabel locationLabel = getLocationLabelDenotingCurrentStateForParentNode(groupNode);
                            if(hsaLabel == null)
                            {
                                if(locationLabel == _darkGreenLocationLabel)
                                {
                                    hsaLabel = _darkGreenHsaLabel;
                                    break;
                                }
                            }
                        }
                    }
                    if(hsaLabel == null)
                    {
                        hsaLabel = _greenHsaLabel;
                    }
                    hsaLabel.setText(value.toString());
                    returnedComponent = hsaLabel;
                }// else if pathCount == 2 ends here
            }// path != null ends here

            if(returnedComponent == null)
                returnedComponent = renderer;
        }
    //    timer.stop("Time elapsed in getAppropriateLabel():");
        return returnedComponent;
    }

    // -------------------------------------------------------------------------------------------------------------
    
    private void analyzePathInfo(boolean isLeaf, TreePath path, Object value)
    {
        //This method is just for debugging purposes and can be commented out at will
        String header = "MonitorCheckTreeCellRenderer.analyzePathInfo(): ";
           
        boolean path2 = false;
        boolean path3 = false;
        boolean path4 = false;
        boolean nullPath = false;
        
        int pathCount = -1;
        String pathString = null;
        if (path != null)
        {
            pathCount = path.getPathCount();
            pathString = path.toString();
        }

        
        System.out.println(header + "isLeaf = " + isLeaf +
                " pathCount = " + pathCount + 
                " treePath = " + pathString +
                " for nodeValue = " + value);

        /*
        if (path != null)
        {
            pathCount = path.getPathCount();
            
            switch(pathCount)
            {
                case 2:
                     path2 = true;
                     break;
                case 3:
                    path3 = true;
                    break;
                case 4:
                    path4 = true;
                    break;      
            }
        }
        else // (path == null)    
        {
            nullPath = true;
        }
        
        if (isLeaf != path4)
        {
            
            System.out.print(header + "isLeaf = " + isLeaf +
                               " but path4 =  " + path4 +
                               " pathCount = " + pathCount + 
                               " treePath = " + path.toString() +
                               " for nodeValue = " + value);
            
            if (nullPath)
            {
                System.out.println(" nullPath is true");
            }
            else
            {
                System.out.println();
            }
            
        }
        else //isLeaf == path4
        {
           // System.out.print(header + "isLeaf = " + isLeaf +
           //         " AND path4 =  " + path4 +
           //         " for nodeValue = " + value);
            
            if (nullPath)
            {
                System.out.println(" nullPath is true");
            }
            else
            {
                System.out.println();
            }
        }
        
        */
        
    }
   
    // -------------------------------------------------------------------------------------------------------------
    
    
    private JLabel getAppropriateLabel(boolean isLeaf, TreePath path, DefaultTreeCellRenderer renderer, Object value)
    {
        String header = "MonitorCheckTreeCellRenderer.getAppropriateLabel(): ";
        
        JLabel returnedComponent = null;
        renderer.setLeafIcon(null);
        renderer.setClosedIcon(null);
        renderer.setOpenIcon(null);
        CodeTimer timer = new CodeTimer();
        timer.restart();
        
      
        int pathCount = -1;
        if (path != null)
        {
            pathCount = path.getPathCount();
        }
        
       // analyzePathInfo(isLeaf, path, value);
        
        if (isLeaf)
       // if (path != null  && path.getPathCount() == 4)
        {
          //  System.out.println(header + "Leaf: value = " + value);
            
            if (pathCount == 4)
            {

                returnedComponent = getLabelComponentForChild(value);
                if (returnedComponent == null)
                {
                    System.out.println(header + "********** - got here **************");
                    _greenLocationLabel.setText(value.toString());
                    returnedComponent = _greenLocationLabel;
                }
            }
            else if (pathCount == 3)
            {
                returnedComponent = getGroupLabel(path, value);
            }
            else if (pathCount == 2)
            {
                returnedComponent = getHsaLabel(path, value);
            }
            else if (pathCount == -1)
            {
                returnedComponent = getLabelComponentForChild(value);
            }
        }
        else //not a leaf
        {
            if(path != null)
            {
                if(path.getPathCount() == 3) // group node
                {         
                    returnedComponent = getGroupLabel(path, value);
                }
                else if(path.getPathCount() == 2) //hsa node
                {
                    returnedComponent = getHsaLabel(path, value);
 
                }
            }// path != null ends here
            else //path == null
            {
                System.out.println(header + "value = " + value + " and the path == null !!!!!!!!!!!!!!!!!!!!!!!!");
            }

            if(returnedComponent == null)
                returnedComponent = renderer;
        }
    //    timer.stop("Time elapsed in getAppropriateLabel():");
        return returnedComponent;
    }
    
    // ---------------------------------------------------------------------------------------------------------------
    
    private JLabel getGroupLabel(TreePath treePath, Object value)
    {
        DefaultMutableTreeNode node = (DefaultMutableTreeNode) treePath.getLastPathComponent();

        JLabel groupLabel = null;
    
        JLabel locationLabel = getLocationLabelDenotingCurrentStateForParentNode(node);
        if (locationLabel == _redLocationLabel)
        {
            groupLabel = _redGroupLabel;
        }
        else if (locationLabel == _yellowLocationLabel)
        {
            groupLabel = _yellowGroupLabel;
        }
        else if (locationLabel == _grayLocationLabel)
        {
           // groupLabel = _grayGroupLabel;
            groupLabel = _greenGroupLabel;
        }
        else if (locationLabel == _darkGreenLocationLabel)
        {
            groupLabel = _darkGreenGroupLabel;
        }
        else
        {
            groupLabel = _greenGroupLabel;
        }
        groupLabel.setText(value.toString());
        
        
        return groupLabel;
    }
    
    // -------------------------------------------------------------------------------------------------------
    
    private JLabel getHsaLabel(TreePath treePath, Object value)
    {
        JLabel hsaLabel = null;
        
        DefaultMutableTreeNode node = (DefaultMutableTreeNode) treePath.getLastPathComponent();
        int groupCount = node.getChildCount();
       
        hsaLabel =  getHsaLocationLabelChildByColorLabelMatch(hsaLabel, _redLocationLabel, _redHsaLabel, node, groupCount);
        hsaLabel =  getHsaLocationLabelChildByColorLabelMatch(hsaLabel, _yellowLocationLabel, _yellowHsaLabel, node, groupCount);
        hsaLabel =  getHsaLocationLabelChildByColorLabelMatch(hsaLabel, _grayLocationLabel, _greenHsaLabel, node, groupCount);     //gray to green on purpose  
        hsaLabel =  getHsaLocationLabelChildByColorLabelMatch(hsaLabel, _darkGreenLocationLabel, _darkGreenHsaLabel, node, groupCount);
    
        if(hsaLabel == null)
        {
            hsaLabel = _greenHsaLabel;
        }
        hsaLabel.setText(value.toString());
       
        
        return hsaLabel;
    
    }
    //  ---------------------------------------------------------------------------------------------------------------
    private JLabel getHsaLabelOld(TreePath treePath, Object value)
    {
        JLabel hsaLabel = null;
        
        DefaultMutableTreeNode node = (DefaultMutableTreeNode) treePath.getLastPathComponent();
        int groupCount = node.getChildCount();
       
        for(int j=0; j< groupCount; j++)
        {
            DefaultMutableTreeNode groupNode = (DefaultMutableTreeNode) node.getChildAt(j);
            JLabel locationLabel = getLocationLabelDenotingCurrentStateForParentNode(groupNode);
            if(hsaLabel == null)
            {
                if(locationLabel == _redLocationLabel)
                {
                    hsaLabel = _redHsaLabel;
                    break;
                }
            }
        }
        if(hsaLabel == null)
        {
            for(int j=0; j< groupCount; j++)
            {
                DefaultMutableTreeNode groupNode = (DefaultMutableTreeNode) node.getChildAt(j);
                JLabel locationLabel = getLocationLabelDenotingCurrentStateForParentNode(groupNode);
                if(hsaLabel == null)
                {
                    if(locationLabel == _yellowLocationLabel)
                    {
                        hsaLabel = _yellowHsaLabel;
                        break;
                    }
                }
            }
        }
        if(hsaLabel == null)
        {
            for(int j=0; j< groupCount; j++)
            {
                DefaultMutableTreeNode groupNode = (DefaultMutableTreeNode) node.getChildAt(j);
                JLabel locationLabel = getLocationLabelDenotingCurrentStateForParentNode(groupNode);
                if(hsaLabel == null)
                {
                    if(locationLabel == _grayLocationLabel)
                    {
                        hsaLabel = _greenHsaLabel;
                      //  hsaLabel = _grayHsaLabel;
                        break;
                    }
                }
            }
        }
        if(hsaLabel == null)
        {
            for(int j=0; j< groupCount; j++)
            {
                DefaultMutableTreeNode groupNode = (DefaultMutableTreeNode) node.getChildAt(j);
                JLabel locationLabel = getLocationLabelDenotingCurrentStateForParentNode(groupNode);
                if(hsaLabel == null)
                {
                    if(locationLabel == _darkGreenLocationLabel)
                    {
                        hsaLabel = _darkGreenHsaLabel;
                        break;
                    }
                }
            }
        }
        if(hsaLabel == null)
        {
            hsaLabel = _greenHsaLabel;
        }
        hsaLabel.setText(value.toString());
       
        
        return hsaLabel;
    
    }
    //  ---------------------------------------------------------------------------------------------------------------
    private JLabel getHsaLocationLabelChildByColorLabelMatch(JLabel resultHsaLabel, JLabel targetLocationLabel,
                                                             JLabel targetHsaLabel, TreeNode node, int groupCount )
    {
        if (resultHsaLabel == null)
        {
            for(int j=0; j< groupCount; j++)
            {
                DefaultMutableTreeNode groupNode = (DefaultMutableTreeNode) node.getChildAt(j);
                JLabel locationLabel = getLocationLabelDenotingCurrentStateForParentNode(groupNode);
                if(resultHsaLabel == null)
                {
                    if(locationLabel == targetLocationLabel)
                    {
                        resultHsaLabel = targetHsaLabel;
                        break;
                    }
                }
            } //end for
        }
        
        return resultHsaLabel;
    }
    
    //  ---------------------------------------------------------------------------------------------------------------
    
    private JLabel getLocationLabelDenotingCurrentStateForParentNode(DefaultMutableTreeNode groupNode)
    {
        //    CodeTimer timer = new CodeTimer();
        //  timer.restart();
        int locationCount = groupNode.getChildCount();
        JLabel locationLabelDenotingCurrentState = null;
        for(int i=0; i < locationCount; i++)
        {
            DefaultMutableTreeNode childNode = (DefaultMutableTreeNode) groupNode.getChildAt(i);
            locationLabelDenotingCurrentState = getLabelComponentForChild(childNode);

            if(locationLabelDenotingCurrentState == _redLocationLabel)
            {
                return locationLabelDenotingCurrentState;
            }
        }
        if(locationLabelDenotingCurrentState != _redLocationLabel)
        {
            for(int i=0; i < locationCount; i++)
            {
                DefaultMutableTreeNode childNode = (DefaultMutableTreeNode) groupNode.getChildAt(i);
                locationLabelDenotingCurrentState = getLabelComponentForChild(childNode);
                if(locationLabelDenotingCurrentState == _yellowLocationLabel)
                {
                    return locationLabelDenotingCurrentState;
                }
            }
        }
        if(locationLabelDenotingCurrentState != _yellowLocationLabel)
        {
            for(int i=0; i < locationCount; i++)
            {
                DefaultMutableTreeNode childNode = (DefaultMutableTreeNode) groupNode.getChildAt(i);
                locationLabelDenotingCurrentState = getLabelComponentForChild(childNode);
                if(locationLabelDenotingCurrentState == _grayLocationLabel)
                {
                    return locationLabelDenotingCurrentState;
                }
            }
        }
        if(locationLabelDenotingCurrentState != _grayLocationLabel)
        {
            for(int i=0; i < locationCount; i++)
            {
                DefaultMutableTreeNode childNode = (DefaultMutableTreeNode) groupNode.getChildAt(i);
                locationLabelDenotingCurrentState = getLabelComponentForChild(childNode);
                if(locationLabelDenotingCurrentState == _darkGreenLocationLabel)
                {
                    return locationLabelDenotingCurrentState;
                }
            }
        }
        locationLabelDenotingCurrentState = _greenLocationLabel;
        //  timer.stop("Time elapsed in getLocationLabelDenotingCurrentStateForParentNode():");
        return locationLabelDenotingCurrentState;
    }

    private JLabel getLabelComponentForChild(Object value)
    {
        //  _timer.restart();
        JLabel label = null;
        Color darkGreen = new Color(0, 153, 0);
        String lid = value.toString();
    
        Color criticalColor = (Color) _rowToCriticalColorMap.get(lid);
        if(criticalColor == Color.RED)
        {
            _redLocationLabel.setText(value.toString());
            label = _redLocationLabel;
        }
        else if(criticalColor == Color.YELLOW)
        {
            _yellowLocationLabel.setText(value.toString());
            label = _yellowLocationLabel;
        }
        else if(criticalColor == Color.GRAY)
        {
            _grayLocationLabel.setText(value.toString());
            label = _grayLocationLabel;
        }
        //     else if(criticalColor.equals(new Color(0, 153, 0)))
        else if(criticalColor == darkGreen)
        {
            _darkGreenLocationLabel.setText(value.toString());
            label = _darkGreenLocationLabel;
        }
        else
        {
            _greenLocationLabel.setText(value.toString());
            label = _greenLocationLabel;
        }
      
        //  _timer.stop("Time elapsed in getLabelComponentForChild()");
        return label;
    }

    /*   private JLabel getLabelComponentForChild(Object value)
    {
        //  _timer.restart();
        JLabel label = null;
        //List allRowDataList = _tableManager.getAllRowDataList();
        List rowDataForAllLocationsList = _rowDataForAllLocationsList;
        for(int i=0; i < rowDataForAllLocationsList.size(); i++)
        {
            RiverMonitorJTableRowData2 rowData = (RiverMonitorJTableRowData2) rowDataForAllLocationsList.get(i);
            if(rowData.getDataValue("Location Id").toString().equals(value.toString()))
            {
                Color criticalColor = rowData.getCriticalColorForThisRow();
                if(criticalColor == Color.RED)
                {
                    _redLocationLabel.setText(value.toString());
                    label = _redLocationLabel;
                    break;
                }
                else if(criticalColor == Color.YELLOW)
                {
                    _yellowLocationLabel.setText(value.toString());
                    label = _yellowLocationLabel;
                    break;
                }
                else if(criticalColor == Color.GRAY)
                {
                    _grayLocationLabel.setText(value.toString());
                    label = _grayLocationLabel;
                    break;
                }
                else if(criticalColor.equals(new Color(0, 153, 0)))
                {
                    _darkGreenLocationLabel.setText(value.toString());
                    label = _darkGreenLocationLabel;
                    break;
                }
                else
                {
                    _greenLocationLabel.setText(value.toString());
                    label = _greenLocationLabel;
                    break;
                }
            }
        }
        //  _timer.stop("Time elapsed in getLabelComponentForChild()");
        return label;
    }
     */

    private Icon createIcon(String fileName)
    {
        Icon icon = null;
        int width = 15;
        int height = 15;
        Image image = Toolkit.getDefaultToolkit().getImage(fileName);
        Image newImage = image.getScaledInstance(width, height, Image.SCALE_FAST);
        icon = new ImageIcon(newImage);
        return icon;
    }


    /**
     * Maintenance tip - There were some tricks to getting this code
     * working:
     * 
     * 1. You have to overwite addMouseListener() to do nothing
     * 2. You have to add a mouse event on mousePressed by calling
     * super.addMouseListener()
     * 3. You have to replace the UIActionMap for the keyboard event
     * "pressed" with your own one.
     * 4. You have to remove the UIActionMap for the keyboard event
     * "released".
     * 5. You have to grab focus when the next state is entered,
     * otherwise clicking on the component won't get the focus.
     * 6. You have to make a TristateDecorator as a button model that
     * wraps the original button model and does state management.
     */
    public class TristateCheckBox extends JCheckBox {

        private final TristateDecorator model;

        public TristateCheckBox(String text, Icon icon, State initial){
            super(text, icon);
            // Add a listener for when the mouse is pressed
            super.addMouseListener(new MouseAdapter() {
                public void mousePressed(MouseEvent e) {
                    grabFocus();
                    model.nextState();
                }
            });
            // Reset the keyboard action map
            ActionMap map = new ActionMapUIResource();
            map.put("pressed", new AbstractAction() {
                public void actionPerformed(ActionEvent e) {
                    grabFocus();
                    model.nextState();
                }
            });
            map.put("released", null);
            SwingUtilities.replaceUIActionMap(this, map);
            // set the model to the adapted model
            model = new TristateDecorator(getModel());
            setModel(model);
            setState(initial);
        }
        public TristateCheckBox(String text, State initial) {
            this(text, null, initial);
        }
        public TristateCheckBox(String text) {
            this(text, DONT_CARE);
        }
        public TristateCheckBox() {
            this(null);
        }

        /** No one may add mouse listeners, not even Swing! */
        public void addMouseListener(MouseListener l) { }
        /**
         * Set the new state to either SELECTED, NOT_SELECTED or
         * DONT_CARE.  If state == null, it is treated as DONT_CARE.
         */
        public void setState(State state) { model.setState(state); }
        /** Return the current state, which is determined by the
         * selection status of the model. */
        public State getState() { return model.getState(); }
        public void setSelected(boolean b) {
            if (b) {
                setState(SELECTED);
            } else {
                setState(NOT_SELECTED);
            }
        }
        /**
         * Exactly which Design Pattern is this?  Is it an Adapter,
         * a Proxy or a Decorator?  In this case, my vote lies with the
         * Decorator, because we are extending functionality and
         * "decorating" the original model with a more powerful model.
         */
        private class TristateDecorator implements ButtonModel {
            private final ButtonModel other;
            private TristateDecorator(ButtonModel other) {
                this.other = other;
            }
            private void setState(State state) {
                if (state == NOT_SELECTED) {
                    other.setArmed(false);
                    setPressed(false);
                    setSelected(false);
                } else if (state == SELECTED) {
                    other.setArmed(false);
                    setPressed(false);
                    setSelected(true);
                } else { // either "null" or DONT_CARE
                    other.setArmed(true);
                    setPressed(true);
                    setSelected(true);
                }
            }
            /**
             * The current state is embedded in the selection / armed
             * state of the model.
             * 
             * We return the SELECTED state when the checkbox is selected
             * but not armed, DONT_CARE state when the checkbox is
             * selected and armed (grey) and NOT_SELECTED when the
             * checkbox is deselected.
             */
            private State getState() {
                if (isSelected() && !isArmed()) {
                    // normal black tick
                    return SELECTED;
                } else if (isSelected() && isArmed()) {
                    // don't care grey tick
                    return DONT_CARE;
                } else {
                    // normal deselected
                    return NOT_SELECTED;
                }
            }
            /** We rotate between NOT_SELECTED, SELECTED and DONT_CARE.*/
            private void nextState() {
                State current = getState();
                if (current == NOT_SELECTED) {
                    setState(SELECTED);
                } else if (current == SELECTED) {
                    setState(DONT_CARE);
                } else if (current == DONT_CARE) {
                    setState(NOT_SELECTED);
                }
            }
            /** Filter: No one may change the armed status except us. */
            public void setArmed(boolean b) {
            }
            /** We disable focusing on the component when it is not
             * enabled. */
            public void setEnabled(boolean b) {
                setFocusable(b);
                other.setEnabled(b);
            }
            /** All these methods simply delegate to the "other" model
             * that is being decorated. */
            public boolean isArmed() { return other.isArmed(); }
            public boolean isSelected() { return other.isSelected(); }
            public boolean isEnabled() { return other.isEnabled(); }
            public boolean isPressed() { return other.isPressed(); }
            public boolean isRollover() { return other.isRollover(); }
            public void setSelected(boolean b) { other.setSelected(b); }
            public void setPressed(boolean b) { other.setPressed(b); }
            public void setRollover(boolean b) { other.setRollover(b); }
            public void setMnemonic(int key) { other.setMnemonic(key); }
            public int getMnemonic() { return other.getMnemonic(); }
            public void setActionCommand(String s) {
                other.setActionCommand(s);
            }
            public String getActionCommand() {
                return other.getActionCommand();
            }
            public void setGroup(ButtonGroup group) {
                other.setGroup(group);
            }
            public void addActionListener(ActionListener l) {
                other.addActionListener(l);
            }
            public void removeActionListener(ActionListener l) {
                other.removeActionListener(l);
            }
            public void addItemListener(ItemListener l) {
                other.addItemListener(l);
            }
            public void removeItemListener(ItemListener l) {
                other.removeItemListener(l);
            }
            public void addChangeListener(ChangeListener l) {
                other.addChangeListener(l);
            }
            public void removeChangeListener(ChangeListener l) {
                other.removeChangeListener(l);
            }
            public Object[] getSelectedObjects() {
                return other.getSelectedObjects();
            }
        }
    }
} 

