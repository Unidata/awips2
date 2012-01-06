package ohd.hseb.monitor.river.manager;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.List;
import java.util.Map;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTree;
import javax.swing.SwingUtilities;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

import ohd.hseb.monitor.MonitorFrame;
import ohd.hseb.monitor.MonitorMessage;
import ohd.hseb.monitor.TreeDataManager;
import ohd.hseb.monitor.manager.BaseManager;
import ohd.hseb.monitor.manager.Receiver;
import ohd.hseb.monitor.messaging.MessageSystemManager;
import ohd.hseb.monitor.messaging.MessageType;
import ohd.hseb.monitor.river.RiverMonitorDataManager;
import ohd.hseb.monitor.settings.FilterSettings;
import ohd.hseb.monitor.treefilter.MonitorCheckTreeManager;
import ohd.hseb.monitor.treefilter.MonitorCheckTreeSelectionModel;
import ohd.hseb.monitor.util.HydroappsDefaultDirs;
import ohd.hseb.util.AppsDefaults;
import ohd.hseb.util.SessionLogger;

public class RiverMonitorFilterManager extends BaseManager
{
    private SessionLogger _logger;

    private RiverMonitorDataManager _riverMonitorDataManager;
    private FilterSettings _filterSettings;

    private JSplitPane _splitPane;

    private AppsDefaults _appsDefaults;

    private MonitorCheckTreeManager _checkTreeManager;

    private JScrollPane _treeScrollPane;

    private JPopupMenu _popupMenuForTree;
    private JMenuItem _expandTreeMenuItem;
    private JMenuItem _collapseTreeMenuItem;

    private MonitorFrame _mainFrame;
    private String _iconsDir;


    public RiverMonitorFilterManager(MessageSystemManager msgSystemManager,SessionLogger logger, 
            MonitorFrame mainFrame, RiverMonitorDataManager riverMonitorDataManager, JSplitPane splitPane,
            AppsDefaults appsDefaults, FilterSettings filterSettings)
    {
        _riverMonitorDataManager = riverMonitorDataManager;
        _splitPane = splitPane;
        _appsDefaults = appsDefaults;
        _mainFrame = mainFrame; 
        _filterSettings = filterSettings;
        _iconsDir = _appsDefaults.getToken("rivermon_config_dir", 
        	HydroappsDefaultDirs.RIVERMON_CONFIG_DIR);

        createPopupMenuForTree();

        setMessageSystemManager(msgSystemManager);
        _msgSystemManager.registerReceiver(new UpdateDisplayWithSettingsReceiver(), MessageType.UPDATE_DISPLAY_WITH_SETTINGS);
        _msgSystemManager.registerReceiver(new RefreshDisplayReceiver(), MessageType.REFRESH_DISPLAY);
    }

    private void createTreeAndApplySettings(MonitorMessage message)
    {
        final String header = "RiverMonitorFilterManager.createTreeAndApplySettings(): ";
        System.out.println(header + " thread = " + Thread.currentThread().getId());
        
        _mainFrame.setWaitCursor();
        if(message.getMessageSource() != this)
        {
            System.out.println(header +" Invoked" + "...Source:" + message.getMessageSource() + " Type:" + message.getMessageType());
            List stringArrayListOfFilterItems = _riverMonitorDataManager.createStringArrayListOfFilterItems();

            TreeDataManager treeDataManager = new TreeDataManager();

            DefaultMutableTreeNode rootNode = new DefaultMutableTreeNode("HSA");

            JTree tree = treeDataManager.createFilterTreeFromList(rootNode, stringArrayListOfFilterItems);
             
            tree.setBackground(Color.BLACK);

            TreePath[] preSelectedPaths = _filterSettings.getSelectedPaths();
            
         //   printTreePathArray(preSelectedPaths, "CreateTree--- preSelectedPaths :");
            
            TreePath[] preExpandedPaths = _filterSettings.getExpandedPaths();
            
          //  printTreePathArray(preSelectedPaths, "CreateTree--- preExpandedPaths :");
            
            Map<String, Color> lidToCriticalColorMap = _riverMonitorDataManager.getLidToCriticalColorMap();

            _checkTreeManager = new MonitorCheckTreeManager(tree, preSelectedPaths, preExpandedPaths, lidToCriticalColorMap, _iconsDir);
            CheckTreeSelectionListener treeListener = new CheckTreeSelectionListener();
            tree.addMouseListener(treeListener);

            _treeScrollPane = new JScrollPane(tree);
            _treeScrollPane.setPreferredSize(new Dimension(300,645));

            _splitPane.setLeftComponent(_treeScrollPane);

        //    printTreePathArray(_checkTreeManager.getSelectionModel().getSelectionPaths(), "Create Tree: ");
            
            _riverMonitorDataManager.getTreeFilterManager().traversePathsToDisplayLocationSelected(_checkTreeManager.getValidTreePaths(preSelectedPaths));

        }
        _mainFrame.setDefaultCursor();
    }

    private void highlightThisRow()
    {
        String header = "RiverMonitorFilterManager.highlightThisRow(): ";

        System.out.print(header );
        send(this, MessageType.FILTER_SELECT_ITEM);
    }

    private void sendRefreshDisplayMessage()
    {
        String header = "RiverMonitorFilterManager.sendRefreshDisplayMessage(): ";
        
        System.out.print(header);
        send(this, MessageType.REFRESH_DISPLAY);
    }
    
    private void setFilterSettings()
    {
        String header = "RiverMonitorFilterManager.setFilterSettings(): ";
        System.out.println(header);
   
        TreePath[] paths = _checkTreeManager.getSelectionModel().getSelectionPaths();
        _filterSettings.setSelectedPaths(paths);
        
     //   printTreePathArray(paths, header + " selected path in ");
        
        TreePath[] expandedPaths = _checkTreeManager.getAllExpandedPaths();
        _filterSettings.setExpandedPaths(expandedPaths);
    
    }
 
    private void createPopupMenuForTree()
    {
        _popupMenuForTree = new JPopupMenu();
        _expandTreeMenuItem = new JMenuItem("Expand Entire Tree");
        _expandTreeMenuItem.setMnemonic('E');
        _collapseTreeMenuItem = new JMenuItem("Collapse Entire Tree");
        _collapseTreeMenuItem.setMnemonic('C');
        _popupMenuForTree.add(_expandTreeMenuItem);
        _popupMenuForTree.add(_collapseTreeMenuItem);

        TreePopupMenuListener treePopupMenuListener = new TreePopupMenuListener();
        _expandTreeMenuItem.addActionListener(treePopupMenuListener);
        _collapseTreeMenuItem.addActionListener(treePopupMenuListener);
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
    
    private class UpdateDisplayWithSettingsReceiver implements Receiver
    {
        public void receive(MonitorMessage message)
        {
            createTreeAndApplySettings(message);
        }
    }

    private class RefreshDisplayReceiver implements Receiver
    {
        public void receive(MonitorMessage message)
        {
            createTreeAndApplySettings(message);
        }
    }

    private class TreePopupMenuListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e) 
        {
         
            TreePath[] allAvailablePaths = _checkTreeManager.getAllAvailablePaths();
            if(e.getSource().toString().contains("Expand Entire Tree"))
            {
                _checkTreeManager.expandThesePaths(allAvailablePaths);
            }
            else //collapse tree
            {
                _checkTreeManager.collapseEntireTree();
            }
                    
            setFilterSettings();
        }
    }

    class CheckTreeSelectionListener implements MouseListener
    {
        public void mouseClicked(MouseEvent e)
        {
            
            MonitorCheckTreeSelectionModel selectionModel = (MonitorCheckTreeSelectionModel) (_checkTreeManager.getSelectionModel());

            selectionModel.printInstanceId();

            TreePath paths[] = selectionModel.getSelectionPaths();

            //    printTreePathArray(paths, "CheckTreeSelectionListener.mouseClicked(): ");

            setFilterSettings();

            _riverMonitorDataManager.getTreeFilterManager().traversePathsToDisplayLocationSelected(paths);
            sendRefreshDisplayMessage();

            if(SwingUtilities.isLeftMouseButton(e)) 
            {
                JTree tree = (JTree) e.getSource();
                int row = tree.getRowForLocation(e.getX(), e.getY());
                TreePath path  = tree.getPathForRow(row);
                String itemClickedOnJtree = null;
                if(path != null)
                {
                    if(path.getPathCount() == 4)
                    {
                        itemClickedOnJtree = path.getLastPathComponent().toString();
                        System.out.println("Item of interest:"+ itemClickedOnJtree);
                        _riverMonitorDataManager.setLidOfCurrentInterest(itemClickedOnJtree);
                        highlightThisRow();
                    }
                }
            }
        }
        public void mousePressed(MouseEvent e)
        {
            if(e.isPopupTrigger())
            {
                _popupMenuForTree.show(_treeScrollPane, e.getX(), e.getY());
            }
        }

        public void mouseReleased(MouseEvent e)
        {
            if(e.isPopupTrigger())
            {
                _popupMenuForTree.show(_treeScrollPane, e.getX(), e.getY());
            }
        }
        public void mouseEntered(MouseEvent e){}
        public void mouseExited(MouseEvent e){} 
    }

}
