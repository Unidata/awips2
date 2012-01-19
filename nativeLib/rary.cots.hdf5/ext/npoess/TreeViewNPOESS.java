/*****************************************************************************
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of the HDF Java Products distribution.                  *
 * The full copyright notice, including terms governing use, modification,   *
 * and redistribution, is contained in the files COPYING and Copyright.html. *
 * COPYING can be found at the root of the source code distribution tree.    *
 * Or, see http://hdfgroup.org/products/hdf-java/doc/Copyright.html.         *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 ****************************************************************************/

package ext.npoess;

import ncsa.hdf.object.*;
import ncsa.hdf.view.*;

import javax.swing.*;
import javax.swing.tree.*;
import java.util.*;
import java.io.*;
import java.lang.reflect.*;
import java.awt.Component;
import java.awt.BorderLayout;
import java.awt.Toolkit;
import java.awt.event.*;
import java.awt.Cursor;

/**
 *
 * <p>TreeView defines APIs for open a file and display the file structure in tree structure.</p>
 *
 * <p>TreeView uses folders and leaf nodes to represent groups and data objects in the
 * file. You can expand or collapse folders to navigate data objects in the file.</p>
 *
 * <p>From the TreeView, you can open data content or metadata of selected object.
 * You can selet object(s) to delete or add new object to the file.</p>
 *
 * @author Peter X. Cao
 * @version 2.4 9/6/2007
 */
public class TreeViewNPOESS extends JPanel
	implements TreeView, ActionListener 
{
	public static final long serialVersionUID = HObject.serialVersionUID;

    /** the owner of this treeview */
    private ViewManager viewer;

    /**
     * The super root of tree: all open files start at this root.
     */
    private final DefaultMutableTreeNode root;

    /**
     * The tree which holds file structures.
     */
    private final JTree tree;

    /**
     * The tree model
     */
    private final DefaultTreeModel treeModel;

    /** A list open files. */
    private final List fileList;

    private final Toolkit toolkit;

    /** Selected file */
    private FileFormat selectedFile;

    /** The current selected node. */
    private DefaultMutableTreeNode selectedNode;

    /** the current selected object */
    private HObject selectedObject;

    /** flag to indicate if the dataset is displayed as default */
    private boolean isDefaultDisplay;

    /**
     * The popup menu used to display user choice of actions on data object.
     */
    private final JPopupMenu popupMenu;

    /** a list of editing GUI components */
    private List editGUIs;

    /** the list of current selected objects */
    private List objectsToCopy;

    private JMenuItem addTableMenuItem;

    private JMenuItem addDatatypeMenuItem;

    private JMenuItem addLinkMenuItem;

    public TreeViewNPOESS(ViewManager theView) {
        viewer = theView;

        root = new DefaultMutableTreeNode() {
        	public static final long serialVersionUID = HObject.serialVersionUID;

            public boolean isLeaf() { return false; }
        };

        fileList = new Vector();
        toolkit = Toolkit.getDefaultToolkit();
        editGUIs = new Vector();
        objectsToCopy = null;
        isDefaultDisplay = true;

        addTableMenuItem = new JMenuItem( "Table", ViewProperties.getTableIcon());
        addTableMenuItem.addActionListener(this);
        addTableMenuItem.setActionCommand("Add table");

        addDatatypeMenuItem = new JMenuItem( "Datatype", ViewProperties.getDatatypeIcon());
        addDatatypeMenuItem.addActionListener(this);
        addDatatypeMenuItem.setActionCommand("Add datatype");

        addLinkMenuItem = new JMenuItem( "Link", ViewProperties.getLinkIcon());
        addLinkMenuItem.addActionListener(this);
        addLinkMenuItem.setActionCommand("Add link");

        // initialize the tree and root
        treeModel = new DefaultTreeModel(root);
        tree = new JTree(treeModel);

        tree.setLargeModel(true);
        tree.setCellRenderer(new HTreeCellRenderer());
        tree.addMouseListener(new HTreeMouseAdapter());
        tree.setRootVisible(false);
        //tree.setShowsRootHandles(true);
        int rowheight = 23 + (int)((tree.getFont().getSize()-12)*0.5);
        tree.setRowHeight(rowheight);
        
        // create the popupmenu
        popupMenu = createPopupMenu();

        // reset the scroll increament
        // layout GUI component
        this.setLayout( new BorderLayout() );
        this.add(tree, BorderLayout.CENTER);
    }

    /**
     * Insert a node into the tree.
     * @param node the node to insert.
     * @param pnode the parent node.
     */
    private void insertNode(TreeNode node, TreeNode pnode)
    {
        if ((node == null) || (pnode==null)) {
            return;
        }

        treeModel.insertNodeInto((DefaultMutableTreeNode)node,
            (DefaultMutableTreeNode)pnode,
            pnode.getChildCount());
    }

     /**
     * Checks if a file is already opoen.
     */
    private boolean isFileOpen(String filename)
    {
        boolean isOpen = false;

        // find the file by matching its file name and close the file
        FileFormat theFile = null;
        Iterator iterator = fileList.iterator();
        while(iterator.hasNext())
        {
            theFile = (FileFormat)iterator.next();
            if (theFile.getFilePath().equals(filename))
            {
                isOpen = true;
                break;
            }
        } // while(iterator.hasNext())

        return isOpen;
    }

    /** creates a popup menu for a right mouse click on a data object */
    private JPopupMenu createPopupMenu()
    {
        JPopupMenu menu = new JPopupMenu();
        JMenuItem item;

        item = new JMenuItem( "Open");
        item.setMnemonic(KeyEvent.VK_O);
        item.addActionListener(this);
        item.setActionCommand("Open data");
        menu.add(item);

        item = new JMenuItem( "Open As");
        item.setMnemonic(KeyEvent.VK_A);
        item.addActionListener(this);
        item.setActionCommand("Open data as");
        menu.add(item);

        menu.addSeparator();

        JMenu newOjbectMenu = new JMenu("New");
        menu.add(newOjbectMenu);
        editGUIs.add(newOjbectMenu);

        item = new JMenuItem( "Group", ViewProperties.getFoldercloseIcon());
        item.addActionListener(this);
        item.setActionCommand("Add group");
        newOjbectMenu.add(item);

        item = new JMenuItem( "Dataset", ViewProperties.getDatasetIcon());
        item.addActionListener(this);
        item.setActionCommand("Add dataset");
        newOjbectMenu.add(item);

        item = new JMenuItem( "Image", ViewProperties.getImageIcon());
        item.addActionListener(this);
        item.setActionCommand("Add image");
        newOjbectMenu.add(item);

        newOjbectMenu.add(addTableMenuItem);
        newOjbectMenu.add(addDatatypeMenuItem);
        newOjbectMenu.add(addLinkMenuItem);

        menu.addSeparator();

        item = new JMenuItem( "Copy");
        item.setMnemonic(KeyEvent.VK_C);
        item.addActionListener(this);
        item.setActionCommand("Copy object");
        menu.add(item);

        item = new JMenuItem( "Paste");
        item.setMnemonic(KeyEvent.VK_P);
        item.addActionListener(this);
        item.setActionCommand("Paste object");
        menu.add(item);
        editGUIs.add(item);

        item = new JMenuItem( "Delete");
        item.setMnemonic(KeyEvent.VK_D);
        item.addActionListener(this);
        item.setActionCommand("Cut object");
        menu.add(item);
        editGUIs.add(item);

        menu.addSeparator();

        item = new JMenuItem( "Save to");
        item.setMnemonic(KeyEvent.VK_S);
        item.addActionListener(this);
        item.setActionCommand("Save object to file");
        menu.add(item);

        item = new JMenuItem( "Rename");
        item.setMnemonic(KeyEvent.VK_R);
        item.addActionListener(this);
        item.setActionCommand("Rename object");
        menu.add(item);
        editGUIs.add(item);

        menu.addSeparator();

        item = new JMenuItem( "Show Properties");
        item.addActionListener(this);
        item.setActionCommand("Show object properties");
        menu.add(item);

        item = new JMenuItem( "Show Properties As");
        item.addActionListener(this);
        item.setActionCommand("Show object properties as");
        menu.add(item);

        menu.addSeparator();

        item = new JMenuItem( "Close File");
        item.setMnemonic(KeyEvent.VK_F);
        item.addActionListener(this);
        item.setActionCommand("Close file");
        menu.add(item);

        return menu;
    }

    /** display the popupmenu of data properties */
    private void showPopupMenu(MouseEvent e)
    {
        int x = e.getX();
        int y = e.getY();

        HObject selectedObject = ((HObject)(selectedNode.getUserObject()));
        boolean isReadOnly = selectedObject.getFileFormat().isReadOnly();

        setEnabled(editGUIs, !isReadOnly);

        boolean isWritable = !selectedObject.getFileFormat().isReadOnly();
        if (selectedObject instanceof Group)
        {
            popupMenu.getComponent(0).setEnabled(false); // "open" menuitem
            popupMenu.getComponent(1).setEnabled(false); // "open as" menuitem

            boolean state = !(((Group)selectedObject).isRoot());
            popupMenu.getComponent(5).setEnabled(state); // "Copy" menuitem
            popupMenu.getComponent(6).setEnabled(isWritable); // "Paste" menuitem
            popupMenu.getComponent(7).setEnabled(state && isWritable); // "Delete" menuitem
            popupMenu.getComponent(9).setEnabled(state); // "save to" menuitem
            popupMenu.getComponent(10).setEnabled(state && isWritable); // "rename" menuitem
        }
        else {
            popupMenu.getComponent(0).setEnabled(true);
            popupMenu.getComponent(1).setEnabled(true);
            popupMenu.getComponent(5).setEnabled(true); // "Copy" menuitem
            popupMenu.getComponent(6).setEnabled(isWritable); // "Paste" menuitem
            popupMenu.getComponent(7).setEnabled(isWritable); // "Delete" menuitem
            popupMenu.getComponent(9).setEnabled(true); // "save to" menuitem
            popupMenu.getComponent(10).setEnabled(isWritable); // "rename" menuitem
        }

        // adding table is only supported by HDF5
        if ((selectedFile != null) &&
            selectedFile.isThisType(FileFormat.getFileFormat(FileFormat.FILE_TYPE_HDF5)))
        {
            addTableMenuItem.setVisible(true);
            addDatatypeMenuItem.setVisible(true);
            addLinkMenuItem.setVisible(true);
        } else
        {
            addTableMenuItem.setVisible(false);
            addDatatypeMenuItem.setVisible(false);
            addLinkMenuItem.setVisible(false);
        }

        popupMenu.show((JComponent)e.getSource(), x, y);
    }

    /** disable/enable GUI components */
    private static void setEnabled(List list, boolean b)
    {
        Component item = null;
        Iterator it = list.iterator();
        while (it.hasNext())
        {
            item = (Component)it.next();
            item.setEnabled(b);
        }
    }

    /** Save the current file into HDF4.
     *  Since HDF4 does not support packing. The source file is
     *  copied into the new file with the exact same content.
     */
    private final void saveAsHDF4(FileFormat srcFile)
    {
        if(srcFile == null)
        {
            toolkit.beep();
            JOptionPane.showMessageDialog(
            this,
            "Select a file to save.",
            "HDFView",
            JOptionPane.ERROR_MESSAGE);
            return;
        }

        JFrame owner = (viewer == null) ? new JFrame() : (JFrame)viewer;
        String currentDir = srcFile.getParent();
        NewFileDialog dialog = new NewFileDialog(owner, currentDir, FileFormat.FILE_TYPE_HDF4, getCurrentFiles());
        //dialog.show();

        if (!dialog.isFileCreated()) {
            return;
        }

        String filename = dialog.getFile();

        // since cannot pack hdf4, simple copy the whole phyisical file
        int length = 0;
        int bsize = 512;
        byte[] buffer;
        BufferedInputStream bi = null;
        BufferedOutputStream bo = null;

        try {
            bi = new BufferedInputStream(new FileInputStream(srcFile.getFilePath()));
        }
        catch (Exception ex )
        {
            toolkit.beep();
            JOptionPane.showMessageDialog(
            this,
            ex.getMessage()+"\n"+filename,
            "HDFView",
            JOptionPane.ERROR_MESSAGE);
            return;
        }

        try {
            bo = new BufferedOutputStream( new FileOutputStream (filename));
        }
        catch (Exception ex )
        {
            try { bi.close(); } catch (Exception ex2 ) {}
            toolkit.beep();
            JOptionPane.showMessageDialog(
            this,
            ex,
            "HDFView",
            JOptionPane.ERROR_MESSAGE);
            return;
        }

        buffer = new byte[bsize];
        try { length = bi.read(buffer,0,bsize); }
        catch (Exception ex ) { length = 0; }
        while ( length > 0 )
        {
            try {
            bo.write(buffer, 0, length);
            length = bi.read(buffer,0,bsize);
            }
            catch (Exception ex ) { length = 0; }
        }

        try { bo.flush(); } catch (Exception ex ) {}
        try { bi.close(); } catch (Exception ex ) {}
        try { bo.close(); } catch (Exception ex ) {}

        try {
            FileFormat newFile = openFile(filename, FileFormat.WRITE);
        } catch (Exception ex) {
            toolkit.beep();
            JOptionPane.showMessageDialog(
            this,
            ex.getMessage()+"\n"+filename,
            "HDFView",
            JOptionPane.ERROR_MESSAGE);
        }
    }

    /**
     * Copy the current file into a new file. The new file does not
     * include the inaccessible objects. Values of reference dataset
     * are not updated in the new file.
     */
    private void saveAsHDF5(FileFormat srcFile)
    {
        if (srcFile == null) {
            toolkit.beep();
            JOptionPane.showMessageDialog(
            this,
            "Select a file to save.",
            "HDFView",
            JOptionPane.ERROR_MESSAGE);
            return;
        }

        TreeNode root = srcFile.getRootNode();
        if(root == null)
        {
            toolkit.beep();
            JOptionPane.showMessageDialog(
            this,
            "The file is empty.",
            "HDFView",
            JOptionPane.ERROR_MESSAGE);
            return;
        }

        JFrame owner = (viewer == null) ? new JFrame() : (JFrame)viewer;
        NewFileDialog dialog = new NewFileDialog(owner, srcFile.getParent(),
            FileFormat.FILE_TYPE_HDF5, getCurrentFiles());
        //dialog.show();

        if (!dialog.isFileCreated()) {
            return;
        }

        String filename = dialog.getFile();

        int n = root.getChildCount();
        Vector objList = new Vector(n);
        DefaultMutableTreeNode node = null;
        for (int i=0; i<n; i++)
        {
            node = (DefaultMutableTreeNode)root.getChildAt(i);
            objList.add(node.getUserObject());
        }

        FileFormat newFile = null;
        try {
            newFile = openFile(filename, FileFormat.WRITE);
        } catch (Exception ex) {
            toolkit.beep();
            JOptionPane.showMessageDialog(
            this,
            ex.getMessage()+"\n"+filename,
            "HDFView",
            JOptionPane.ERROR_MESSAGE);
            return;
        }

        if (newFile == null) {
            return;
        }

        TreeNode pnode = newFile.getRootNode();

        pasteObject(objList, pnode, newFile);
        objList.setSize(0);

        Group srcGroup = (Group) ((DefaultMutableTreeNode)root).getUserObject();
        Group dstGroup = (Group) ((DefaultMutableTreeNode)newFile.getRootNode()).getUserObject();
        Object[] parameter = new Object[2];
        Class classHOjbect = null;
        Class[] parameterClass = new Class[2];
        Method method = null;

        // copy attributes of the root group
        try {
            parameter[0] = srcGroup;
            parameter[1] = dstGroup;
            classHOjbect = Class.forName("ncsa.hdf.object.HObject");
            parameterClass[0] = parameterClass[1] = classHOjbect;
            method = newFile.getClass().getMethod("copyAttributes", parameterClass);
            method.invoke(newFile, parameter);
        } catch (Exception ex) {
            toolkit.beep();
            JOptionPane.showMessageDialog(
            this,
            ex,
            "HDFView",
            JOptionPane.ERROR_MESSAGE);
        }

        // update reference datasets
        parameter[0] = srcGroup.getFileFormat();
        parameter[1] = newFile;
        parameterClass[0] = parameterClass[1] = parameter[0].getClass();
        try {
            method = newFile.getClass().getMethod("updateReferenceDataset", parameterClass);
            method.invoke(newFile, parameter);
        } catch (Exception ex) {
            toolkit.beep();
            JOptionPane.showMessageDialog(
            this,
            ex,
            "HDFView",
            JOptionPane.ERROR_MESSAGE);
        }
    }

    /** copy selected objects */
    private void copyObject()
    {
        objectsToCopy = getSelectedObjects();
    }

    /** paste selected objects */
    private void pasteObject()
    {
        TreeNode pnode = selectedNode;

        if ((objectsToCopy == null) ||
            (objectsToCopy.size() <=0) ||
            (pnode == null)) {
            return;
        }

        FileFormat srcFile = ((HObject)objectsToCopy.get(0)).getFileFormat();
        FileFormat dstFile = getSelectedFile();
        FileFormat h5file = FileFormat.getFileFormat(FileFormat.FILE_TYPE_HDF5);
        FileFormat h4file = FileFormat.getFileFormat(FileFormat.FILE_TYPE_HDF4);

        if (srcFile == null)
        {
            toolkit.beep();
            JOptionPane.showMessageDialog(
                this,
                "Source file is null.",
                "HDFView",
                JOptionPane.ERROR_MESSAGE);
            return;
        }
        else if (dstFile == null)
        {
            toolkit.beep();
            JOptionPane.showMessageDialog(
                this,
                "Destination file is null.",
                "HDFView",
                JOptionPane.ERROR_MESSAGE);
            return;
        }
        else if (srcFile.isThisType(h4file) && dstFile.isThisType(h5file))
        {
            toolkit.beep();
            JOptionPane.showMessageDialog(
                this,
                "Unsupported operation: cannot copy HDF4 object to HDF5 file",
                "HDFView",
                JOptionPane.ERROR_MESSAGE);
            return;
        }
        else if (srcFile.isThisType(h5file) && dstFile.isThisType(h4file))
        {
            toolkit.beep();
            JOptionPane.showMessageDialog(
                this,
                "Unsupported operation: cannot copy HDF5 object to HDF4 file",
                "HDFView",
                JOptionPane.ERROR_MESSAGE);
            return;
        }

        if (pnode.isLeaf()) {
            pnode = pnode.getParent();
        }
        Group pgroup = (Group)((DefaultMutableTreeNode)pnode).getUserObject();
        String fullPath = pgroup.getPath()+pgroup.getName();
        if (pgroup.isRoot()) {
            fullPath = HObject.separator;
        }

        String msg = "";
        int msgType = JOptionPane.QUESTION_MESSAGE;
        if (srcFile.isThisType(h4file))
        {
            msg = "WARNING: object can not be deleted after it is copied.\n\n";
            msgType = JOptionPane.WARNING_MESSAGE;
        }

        msg += "Do you want to copy the selected object(s) to \nGroup: "+
            fullPath + "\nFile: "+ dstFile.getFilePath();

        int op = JOptionPane.showConfirmDialog(this,
            msg,
            "Copy object",
            JOptionPane.YES_NO_OPTION,
            msgType);

        if (op == JOptionPane.NO_OPTION) {
            return;
        }

        pasteObject(objectsToCopy, pnode, dstFile);

        //objectsToCopy = null;
    }

    /** paste selected objects */
    private void pasteObject(List objList, TreeNode pnode, FileFormat dstFile)
    {
        if ((objList == null) ||
            (objList.size() <=0) ||
            (pnode == null)) {
            return;
        }

        FileFormat srcFile = ((HObject)objList.get(0)).getFileFormat();
        Group pgroup = (Group)((DefaultMutableTreeNode)pnode).getUserObject();

        HObject theObj=null;
        TreeNode newNode = null;
        Iterator iterator = objList.iterator();
        while (iterator.hasNext())
        {
            newNode = null;
            theObj = (HObject)iterator.next();

            if ( (theObj instanceof Group) && ((Group)theObj).isRoot())
            {
                toolkit.beep();
                JOptionPane.showMessageDialog(
                this,
                "Unsupported operation: cannot copy the root group",
                "HDFView",
                JOptionPane.ERROR_MESSAGE);
                return;
            }

            // check if it creates infinite loop
            Group pg = pgroup;
            while (!pg.isRoot())
            {
                if ( theObj.equals(pg))
                {
                    toolkit.beep();
                    JOptionPane.showMessageDialog(
                    this,
                    "Unsupported operation: cannot copy a group to itself.",
                    "HDFView",
                    JOptionPane.ERROR_MESSAGE);
                    return;
                }
                pg = pg.getParent();
            }

            try {
                newNode = dstFile.copy(theObj, pgroup);
            } catch (Exception ex)
            {
                toolkit.beep();
                JOptionPane.showMessageDialog(
                this,
                ex,
                "HDFView",
                JOptionPane.ERROR_MESSAGE);
                //newNode = null;
            }

            // add the node to the tree
            if (newNode != null) {
                insertNode(newNode, pnode);
            }

        } // while (iterator.hasNext())
    }

    private void removeSelectedObjects()
    {
        FileFormat theFile = getSelectedFile();
        if (theFile.isThisType(FileFormat.getFileFormat(FileFormat.FILE_TYPE_HDF4)))
        {
            toolkit.beep();
            JOptionPane.showMessageDialog(
                this,
                "Unsupported operation: cannot delete HDF4 object.",
                "HDFView",
                JOptionPane.ERROR_MESSAGE);
            return;
        }

        TreePath[] currentSelections = tree.getSelectionPaths();
        if ((currentSelections == null) || (currentSelections.length <=0)) {
            return;
        }

        int op = JOptionPane.showConfirmDialog(this,
            "Do you want to remove all the selected object(s) ?",
            "Remove object",
            JOptionPane.YES_NO_OPTION);

        if (op == JOptionPane.NO_OPTION) {
            return;
        }

        String frameName = "";
        HObject theObj = null;
        for (int i=0; i< currentSelections.length; i++) {
            DefaultMutableTreeNode currentNode = (DefaultMutableTreeNode) (currentSelections[i].getLastPathComponent());
            theObj = (HObject)currentNode.getUserObject();

            // cannot delete root
            if (theObj instanceof Group) {
                Group g = (Group)theObj;
                if (g.isRoot()) {
                    toolkit.beep();
                    JOptionPane.showMessageDialog(
                        this,
                        "Unsupported operation: cannot delete the file root.",
                        "HDFView",
                        JOptionPane.ERROR_MESSAGE);
                    return;
                }
            }

            if (isObjectOpen(theObj)) {
                toolkit.beep();
                JOptionPane.showMessageDialog(
                    this,
                    "Cannot delete the selected object: "+theObj+
                    "\nThe dataset or dataset in the group is in use."+
                    "\n\nPlease close the dataset(s) and try again.\n",
                     "HDFView",
                    JOptionPane.ERROR_MESSAGE);
                continue;
            }

            try {
                theFile.delete(theObj);
            } catch (Exception ex) {
                toolkit.beep();
                JOptionPane.showMessageDialog(
                    this,
                    ex,
                    "HDFView",
                    JOptionPane.ERROR_MESSAGE);
                continue;
            }

            if (theObj.equals(selectedObject)) {
                selectedObject = null;
            }

            removeNode(currentNode);
        } //for (int i=0; i< currentSelections.length; i++) {
    }

    private void removeNode(DefaultMutableTreeNode node)
    {
        if (node == null) {
            return;
        }

        DefaultMutableTreeNode parentNode = (DefaultMutableTreeNode) (node.getParent());
        if (parentNode != null) {
            treeModel.removeNodeFromParent(node);

            // add the two lines to fix bug in HDFView 1.2. Delete a subgroup and
            // then copy the group to another group, the deleted group still exists.
            Group pgroup = (Group)parentNode.getUserObject();
            pgroup.removeFromMemberList((HObject)node.getUserObject());

            if (node.equals(selectedNode)) {
                selectedNode = null;
                selectedFile = null;
            }
        } // if (parentNode != null) {
    }

    private boolean isObjectOpen(HObject obj)
    {
        boolean isOpen = false;

        if (obj instanceof Group) {
            Group g = (Group) obj;
            List members = g.getMemberList();
            if ((members == null) || (members.size() == 0)) {
                isOpen = false;
            } else {
                int n = members.size();
                for (int i=0; i<n; i++) {
                    HObject theObj = (HObject) members.get(i);
                    isOpen = (viewer.getDataView(theObj) != null);
                    if (isOpen) {
                        break;
                    }
                }
            }
        }
        else {
            if (viewer.getDataView(obj) == null) {
                isOpen = false;
            } else {
                isOpen = true;
            }
        }

        return isOpen;
    }

    /**
     * Returns a list of all user objects that traverses the subtree rooted
     * at this node in breadth-first order..
     * @param node the node to start with.
     */
    private final List breadthFirstUserObjects(TreeNode node)
    {
        if (node == null) {
            return null;
        }

        Vector list = new Vector();
        DefaultMutableTreeNode theNode = null;
        Enumeration local_enum = ((DefaultMutableTreeNode)node).breadthFirstEnumeration();
        while(local_enum.hasMoreElements())
        {
            theNode = (DefaultMutableTreeNode)local_enum.nextElement();
            list.add(theNode.getUserObject());
        }

        return list;
    }

    private void addGroup()
    {
        if ((selectedObject == null) || (selectedNode == null)) {
            return;
        }

       Group pGroup = null;
        if (selectedObject instanceof Group) {
            pGroup = (Group)selectedObject;
        } else {
            pGroup = (Group)((DefaultMutableTreeNode)selectedNode.getParent()).getUserObject();
        }

        NewGroupDialog dialog = new NewGroupDialog(
            (JFrame)viewer,
            pGroup,
            breadthFirstUserObjects(selectedObject.getFileFormat().getRootNode()));
        dialog.setVisible(true);

        HObject obj = (HObject)dialog.getObject();
        if (obj == null) {
            return;
        }

        Group pgroup = dialog.getParentGroup();
        try { this.addObject(obj, pgroup); }
        catch (Exception ex) {
            toolkit.beep();
            JOptionPane.showMessageDialog(
                this,
                ex,
                "HDFView",
                JOptionPane.ERROR_MESSAGE);
            return;
        }
    }

    private void addDataset()
    {
        if ((selectedObject == null) || (selectedNode == null)) {
            return;
        }

        Group pGroup = null;
        if (selectedObject instanceof Group) {
            pGroup = (Group)selectedObject;
        } else {
            pGroup = (Group)((DefaultMutableTreeNode)selectedNode.getParent()).getUserObject();
        }

        NewDatasetDialog dialog = new NewDatasetDialog(
            (JFrame)viewer,
            pGroup,
            breadthFirstUserObjects(selectedObject.getFileFormat().getRootNode()));
        dialog.setVisible(true);

        HObject obj = (HObject)dialog.getObject();
        if (obj == null) {
            return;
        }

        Group pgroup = dialog.getParentGroup();
        try { addObject(obj, pgroup); }
        catch (Exception ex) {
            toolkit.beep();
            JOptionPane.showMessageDialog(
                this,
                ex,
                "HDFView",
                JOptionPane.ERROR_MESSAGE);
            return;
        }
    }

    private void addImage()
    {
        if ((selectedObject == null) || (selectedNode == null)) {
            return;
        }

        Group pGroup = null;
        if (selectedObject instanceof Group) {
            pGroup = (Group)selectedObject;
        } else {
            pGroup = (Group)((DefaultMutableTreeNode)selectedNode.getParent()).getUserObject();
        }

        NewImageDialog dialog = new NewImageDialog(
            (JFrame)viewer,
            pGroup,
            breadthFirstUserObjects(selectedObject.getFileFormat().getRootNode()));
        dialog.setVisible(true);

        HObject obj = (HObject)dialog.getObject();
        if (obj == null) {
            return;
        }

        Group pgroup = dialog.getParentGroup();
        try { this.addObject(obj, pgroup); }
        catch (Exception ex) {
            toolkit.beep();
            JOptionPane.showMessageDialog(
                this,
                ex,
                "HDFView",
                JOptionPane.ERROR_MESSAGE);
            return;
        }
    }

    private void addTable()
    {
        if ((selectedObject == null) || (selectedNode == null)) {
            return;
        }

        Group pGroup = null;
        if (selectedObject instanceof Group) {
            pGroup = (Group)selectedObject;
        } else {
            pGroup = (Group)((DefaultMutableTreeNode)selectedNode.getParent()).getUserObject();
        }

        NewTableDataDialog dialog = new NewTableDataDialog(
            (JFrame)viewer,
            pGroup,
            breadthFirstUserObjects(selectedObject.getFileFormat().getRootNode()));
        dialog.setVisible(true);

        HObject obj = (HObject)dialog.getObject();
        if (obj == null) {
            return;
        }

        Group pgroup = dialog.getParentGroup();
        try { addObject(obj, pgroup); }
        catch (Exception ex) {
            toolkit.beep();
            JOptionPane.showMessageDialog(
                this,
                ex,
                "HDFView",
                JOptionPane.ERROR_MESSAGE);
            return;
        }
    }

    private void addDatatype()
    {
        if ((selectedObject == null) || (selectedNode == null)) {
            return;
        }

        Group pGroup = null;
        if (selectedObject instanceof Group) {
            pGroup = (Group)selectedObject;
        } else {
            pGroup = (Group)((DefaultMutableTreeNode)selectedNode.getParent()).getUserObject();
        }

        NewDatatypeDialog dialog = new NewDatatypeDialog(
            (JFrame)viewer,
            pGroup,
            breadthFirstUserObjects(selectedObject.getFileFormat().getRootNode()));
        dialog.setVisible(true);

        HObject obj = (HObject)dialog.getObject();
        if (obj == null) {
            return;
        }

        Group pgroup = dialog.getParentGroup();
        try { addObject(obj, pgroup); }
        catch (Exception ex) {
            toolkit.beep();
            JOptionPane.showMessageDialog(
                this,
                ex,
                "HDFView",
                JOptionPane.ERROR_MESSAGE);
            return;
        }
    }

    private void addLink()
    {
        if ((selectedObject == null) || (selectedNode == null)) {
            return;
        }

        Group pGroup = null;
        if (selectedObject instanceof Group) {
            pGroup = (Group)selectedObject;
        } else {
            pGroup = (Group)((DefaultMutableTreeNode)selectedNode.getParent()).getUserObject();
        }

        NewLinkDialog dialog = new NewLinkDialog(
            (JFrame)viewer,
            pGroup,
            breadthFirstUserObjects(selectedObject.getFileFormat().getRootNode()));
        dialog.setVisible(true);

        HObject obj = (HObject)dialog.getObject();
        if (obj == null) {
            return;
        }

        Group pgroup = dialog.getParentGroup();
        try { addObject(obj, pgroup); }
        catch (Exception ex) {
            toolkit.beep();
            JOptionPane.showMessageDialog(
                this,
                ex,
                "HDFView",
                JOptionPane.ERROR_MESSAGE);
            return;
        }
    }

    private void renameObject()
    {
        if (selectedObject == null) {
            return;
        }

        if ((selectedObject instanceof Group) &&
            ((Group)selectedObject).isRoot())
        {
            toolkit.beep();
            JOptionPane.showMessageDialog(this,
                "Cannot rename the root.", "HDFView", JOptionPane.ERROR_MESSAGE);
            return;
        }

        boolean isH4 = selectedObject.getFileFormat().isThisType(FileFormat.getFileFormat(FileFormat.FILE_TYPE_HDF4));

        if (isH4)
        {
            toolkit.beep();
            JOptionPane.showMessageDialog(this, "Cannot rename HDF4 object.",
                "HDFView", JOptionPane.ERROR_MESSAGE);
            return;
        }

        String oldName = selectedObject.getName();
        String newName = JOptionPane.showInputDialog(this, "Rename \""+ oldName + "\" to:",
                "Rename...", JOptionPane.INFORMATION_MESSAGE);

        if (newName == null) {
            return;
        }

        newName = newName.trim();
        if ((newName == null) ||
            (newName.length()==0) ||
            newName.equals(oldName)) {
            return;
        }

        try { selectedObject.setName(newName); }
        catch (Exception ex)
        {
            toolkit.beep();
            JOptionPane.showMessageDialog(this, ex.getMessage(), "HDFView", JOptionPane.ERROR_MESSAGE);
        }
    }

    // Implementing java.io.ActionListener
    public void actionPerformed(ActionEvent e)
    {
        String cmd = e.getActionCommand();

        if (cmd.equals("Close file")) {
            ((HDFView)viewer).actionPerformed(e);
        }
        else if (cmd.equals("Add group")) {
            addGroup();
        }
        else if (cmd.equals("Add dataset")) {
            addDataset();
        }
        else if (cmd.equals("Add image")) {
            addImage();
        }
        else if (cmd.equals("Add table")) {
            addTable();
        }
        else if (cmd.equals("Add datatype")) {
            addDatatype();
        }
        else if (cmd.equals("Add link")) {
            addLink();
        }
        else if (cmd.startsWith("Open data"))
        {
            if (cmd.equals("Open data")) {
                isDefaultDisplay = true;
            } else {
                isDefaultDisplay = false;
            }

            try { showDataContent(selectedObject); }
            catch (Throwable err)
            {
                toolkit.beep();
                JOptionPane.showMessageDialog(this,
                    err,
                    "HDFView",
                    JOptionPane.ERROR_MESSAGE);
                return;
            }
        }
        else if (cmd.equals("Copy object")) {
            copyObject();
        }
        else if (cmd.equals("Paste object")) {
            pasteObject();
        }
        else if (cmd.equals("Cut object")) {
            removeSelectedObjects();
        }
        else if (cmd.equals("Save object to file")) {
            if (selectedObject == null) {
                return;
            }

            if ((selectedObject instanceof Group) &&
                ((Group)selectedObject).isRoot()) {
                toolkit.beep();
                JOptionPane.showMessageDialog(
                    this,
                    "Cannot save the root group.\nUse \"Save As\" from file menu to save the whole file",
                    "HDFView",
                    JOptionPane.ERROR_MESSAGE);
                return;
            }

            String filetype = FileFormat.FILE_TYPE_HDF4;
            boolean isH5 = selectedObject.getFileFormat().isThisType(FileFormat.getFileFormat(FileFormat.FILE_TYPE_HDF5));
            if (isH5) {
                filetype = FileFormat.FILE_TYPE_HDF5;
            }

            NewFileDialog dialog = new NewFileDialog(
                (JFrame)viewer,
                selectedObject.getFileFormat().getParent(),
                filetype,
                fileList);
            //dialog.show();

            if (!dialog.isFileCreated()) {
                return;
            }

            String filename = dialog.getFile();
            FileFormat dstFile = null;
            try {
                dstFile = openFile(filename, FileFormat.WRITE);
            } catch (Exception ex)
            {
                toolkit.beep();
                JOptionPane.showMessageDialog(
                    this,
                    ex.getMessage()+"\n"+filename,
                    "HDFView",
                    JOptionPane.ERROR_MESSAGE);
            }
            List objList = new Vector(2);
            objList.add(selectedObject);
            pasteObject(objList, dstFile.getRootNode(), dstFile);
        }
        else if (cmd.equals("Rename object")) {
            renameObject();
        }
        else if (cmd.startsWith("Show object properties")) {
            if (cmd.equals("Show object properties")) {
                isDefaultDisplay = true;
            } else {
                isDefaultDisplay = false;
            }

            try {
                MetaDataView theView = showMetaData(selectedObject);
            }
            catch (Exception ex) {
                toolkit.beep();
                JOptionPane.showMessageDialog(
                    this,
                    ex,
                    "HDFView",
                    JOptionPane.ERROR_MESSAGE);
            }
        }
    }

    /**
     * Opens a file and retrieves the file structure of the file.
     * It also can be used to create a new file by setting the accessID to
     * FileFormat.CREATE.
     *
     * <p>
     * Subclasses must implement this function to take appropriate steps to
     * open a file.</p>
     *
     * @param filename the name of the file to open.
     * @param accessID identifier for the file access. Valid value of accessID is:
     * <ul>
     * <li>FileFormat.READ --- allow read-only access to file.</li>
     * <li>FileFormat.WRITE --- allow read and write access to file.</li>
     * <li>FileFormat.CREATE --- create a new file.</li>
     * </ul>
     *
     * @return the FileFormat of this file if successful; otherwise returns null.
     */
    public FileFormat openFile(String filename, int accessID)
        throws Exception
    {
        FileFormat fileFormat = null;
        MutableTreeNode fileRoot = null;
        String msg = "";

        if (isFileOpen(filename))
        {
            viewer.showStatus("File is in use");
            return null;
            //throw new UnsupportedOperationException("File is in use.");
        }

        File tmpFile = new File(filename);
        if (!tmpFile.exists()) {
            throw new UnsupportedOperationException("File does not exist.");
        }

        if (!tmpFile.canWrite()) {
            accessID = FileFormat.READ;
        }

        Enumeration keys = FileFormat.getFileFormatKeys();

        String theKey = null;
        while (keys.hasMoreElements())
        {
            theKey = (String)keys.nextElement();
            if (theKey.equals(FileFormat.FILE_TYPE_HDF4))
            {
            	try {
                    FileFormat h4format = FileFormat.getFileFormat(FileFormat.FILE_TYPE_HDF4);
                    if ((h4format !=null) && h4format.isThisType(filename)) {
                        fileFormat = h4format.open(filename, accessID);
                        break;
                    }
            	} catch (Throwable err) {}
                continue;
            }
            else if (theKey.equals(FileFormat.FILE_TYPE_HDF5))
            {
            	try {
                	FileFormat h5format = FileFormat.getFileFormat(FileFormat.FILE_TYPE_HDF5);
                    if ((h5format !=null) && h5format.isThisType(filename)) {
                        fileFormat = h5format.open(filename, accessID);
                        break;
                    }
            	} catch (Throwable err) {}
                continue;
            }
            else
            {
            	try {

                    FileFormat theformat = FileFormat.getFileFormat(theKey);
                    if (theformat.isThisType(filename)) {
                        fileFormat = theformat.open(filename, accessID);
                        break;
                    }
            	} catch (Throwable err) {}
            }
        }

        if (fileFormat == null) {
            throw new java.io.IOException("Unsupported fileformat - "+filename);
        }

        ((JFrame)viewer).setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        try {
            fileFormat.setMaxMembers(ViewProperties.getMaxMembers());
            fileFormat.setStartMembers(ViewProperties.getStartMembers());
            fileFormat.open();
        } finally {
            ((JFrame)viewer).setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
        }

        fileRoot = (MutableTreeNode)fileFormat.getRootNode();
        if (fileRoot != null) {
            insertNode(fileRoot, root);

            int currentRowCount = tree.getRowCount();
            if (currentRowCount>0) {
                tree.expandRow(tree.getRowCount()-1);
            }

            fileList.add(fileFormat);
        }

        return fileFormat;
    }

    /**
     * close a file
     * @param file the file to close
     */
    public void closeFile(FileFormat file) throws Exception
    {
        if (file == null) {
            return;
        }

        // find the file node in the tree and removed it from the tree first
        FileFormat theFile = null;
        DefaultMutableTreeNode theNode = null;
        Enumeration enumeration = root.children();
        while(enumeration.hasMoreElements())
        {
            theNode = (DefaultMutableTreeNode)enumeration.nextElement();
            Group g = (Group)theNode.getUserObject();
            theFile = g.getFileFormat();

            if (theFile.equals(file)) {
                treeModel.removeNodeFromParent(theNode);
                try { theFile.close(); } catch (Exception ex) {;}
                fileList.remove(theFile);
                if (theFile.equals(selectedFile)) {
                    selectedFile = null;
                    selectedNode = null;
                }
                break;
            }
        } //while(enumeration.hasMoreElements())
    }

    /**
     * save a file
     * @param file the file to save
     */
    public void saveFile(FileFormat file) throws Exception {
        if (file == null) {
            toolkit.beep();
            JOptionPane.showMessageDialog(
            this,
            "Select a file to save.",
            "HDFView",
            JOptionPane.ERROR_MESSAGE);
            return;
        }

        boolean isH4 = file.isThisType(FileFormat.getFileFormat(FileFormat.FILE_TYPE_HDF4));
        boolean isH5 = file.isThisType(FileFormat.getFileFormat(FileFormat.FILE_TYPE_HDF5));

        if (!(isH4 || isH5))
        {
            toolkit.beep();
            JOptionPane.showMessageDialog(
            this,
            "Saving file is not supported for this file type",
            "HDFView",
            JOptionPane.ERROR_MESSAGE);
            return;
        }

        // write the change of the data into file before save the file
        List views = ((HDFView)viewer).getDataViews();
        Object theView = null;
        TableView tableView = null;
        TextView textView = null;
        FileFormat theFile = null;
        if (views != null)
        {
            int n = views.size();
            for (int i=0; i<n; i++)
            {
                theView = views.get(i);
                if (theView instanceof TableView)
                {
                    tableView = (TableView)theView;
                    theFile = tableView.getDataObject().getFileFormat();
                    if (file.equals(theFile))
                    {
                        tableView.updateValueInFile();
                    }
                }
                else if (theView instanceof TextView)
                {
                    textView = (TextView)theView;
                    theFile = textView.getDataObject().getFileFormat();
                    if (file.equals(theFile))
                    {
                        textView.updateValueInFile();
                    }
                }
            }
        }

        if (isH5) {
            saveAsHDF5(file);
        } else if (isH4) {
            saveAsHDF4(file);
        }
    }


    /**
     * Gets the selected the file.
     * When multiple files are open, we need to know which file is currently
     * selected.
     *
     * @return the FileFormat of the selected file.
     */
    public FileFormat getSelectedFile() {
        return selectedFile;
    }

    /**
     * Gets a list of selected object in the tree.
     * Obtaining a list of current selected objects is necessary for copy/paste/delete
     * objects.
     *
     * @return a list of selected object in the tree.
     */
    public List getSelectedObjects() {
        TreePath[] paths = tree.getSelectionPaths();
        if ((paths == null) || (paths.length <=0)) {
            return null;
        }

        List objs = new Vector(paths.length);
        HObject theObject=null, parentObject;
        DefaultMutableTreeNode currentNode=null, parentNode=null;
        for (int i=0; i<paths.length; i++)
        {
            currentNode = (DefaultMutableTreeNode) (paths[i].getLastPathComponent());
            theObject = (HObject)currentNode.getUserObject();

            if (theObject != null)
            {
                objs.add(theObject);
                // removed the group from the selected list if some of its members are selected
                // to avoid duplicated copy/paste when a group is pasted.
                parentNode = (DefaultMutableTreeNode)currentNode.getParent();
                parentObject = (HObject)parentNode.getUserObject();
                objs.remove(parentObject);
            }
        }

        return objs;
    }

    /**
     * @return the current selected object in the tree.
     */
    public HObject getCurrentObject() {
        return selectedObject;
    }

    /**
     * Dispaly the content of a data object.
     * @param dataObject the data object
     * @return the dataview that displays the data content
     * @throws Exception
     */
    public DataView showDataContent(HObject dataObject)
        throws Exception
    {
        if ((dataObject == null) ||!(dataObject instanceof Dataset)) {
            return null; // can only display dataset
        }

        Dataset d = (Dataset)dataObject;

        if (d.getRank() <= 0) {
            d.init();
        }
        boolean isText = ((d instanceof ScalarDS) && ((ScalarDS)d).isText());
        boolean isImage = ((d instanceof ScalarDS) && ((ScalarDS)d).isImage());
        boolean isDisplayTypeChar = false;
        boolean isTransposed = false;
        BitSet bitmask = null;
        String dataViewName = null;

        JInternalFrame theFrame = (JInternalFrame)viewer.getDataView(d);

        if (isDefaultDisplay) {

            if (theFrame != null) {
                theFrame.toFront();
                return null;
            }

            if (isText) {
                dataViewName = (String)HDFView.getListOfTextView().get(0);
            }
            else if (isImage) {
                dataViewName = (String)HDFView.getListOfImageView().get(0);
            }
            else {
                dataViewName = (String)HDFView.getListOfTableView().get(0);
            }
        } else {
            DataOptionNPOESS dialog = new DataOptionNPOESS(viewer, d);

            dialog.setVisible(true);
            if (dialog.isCancelled()) {
                return null;
            }

            // allow to display the same dataset multiple times
            // modified by Peter Cao, May 8, 2009
            //  if (theFrame != null) {
            //     ((DataView)theFrame).dispose();
            //  }
            
            isImage = dialog.isImageDisplay();
            isDisplayTypeChar = dialog.isDisplayTypeChar();
            dataViewName = dialog.getDataViewName();
            isTransposed = dialog.isTransposed();
            bitmask = dialog.getBitmask();
        }

        // enables use of JHDF5 in JNLP (Web Start) applications, the system class loader with reflection first.
        Class theClass = null;
        try { theClass = Class.forName(dataViewName); }
        catch (Exception ex) { 
        	try {
        		theClass = ViewProperties.loadExtClass().loadClass(dataViewName); 
        	} catch (Exception ex2) { theClass = null;}
        }
        
        //  use default dataview
        if (theClass == null) {
            if (isText)
                dataViewName = "ncsa.hdf.view.DefaultTextView";
            else if (isImage)
                dataViewName = "ncsa.hdf.view.DefaultImageView";
            else
                dataViewName = "ncsa.hdf.view.DefaultTableView";
            try { theClass = Class.forName(dataViewName); } catch (Exception ex){}
        }

        Object theView = null;
        Object[] initargs = {viewer};
        HashMap map = new HashMap(4);
        if (bitmask != null) {
            map.put(ViewProperties.DATA_VIEW_KEY.BITMASK, bitmask);

            // create a copy of dataset
            ScalarDS d_copy = null;
            Constructor constructor = null;
            Object[] paramObj = null;
            try {
                Class[] paramClass = {FileFormat.class, String.class, String.class};
                constructor = d.getClass().getConstructor(paramClass);
                paramObj = new Object[] {d.getFileFormat(), d.getName(), d.getPath()};
            } catch (Exception ex) { constructor = null; }

            try { d_copy = (ScalarDS)constructor.newInstance(paramObj); }
            catch (Exception ex) { d_copy = null; }
            if (d_copy!= null) {
                try {
                    d_copy.init();
                    int rank = d.getRank();
                    System.arraycopy(d.getDims(), 0, d_copy.getDims(), 0, rank);
                    System.arraycopy(d.getStartDims(), 0, d_copy.getStartDims(), 0, rank);
                    System.arraycopy(d.getSelectedDims(), 0, d_copy.getSelectedDims(), 0, rank);
                    System.arraycopy(d.getStride(), 0, d_copy.getStride(), 0, rank);
                    System.arraycopy(d.getSelectedIndex(), 0, d_copy.getSelectedIndex(), 0, 3);
                } catch (Throwable ex) {}

                map.put(ViewProperties.DATA_VIEW_KEY.OBJECT, d_copy);
            }
        }
            
        if (dataViewName.startsWith("ncsa.hdf.view.DefaultTableView")) {
            map.put(ViewProperties.DATA_VIEW_KEY.CHAR, new Boolean(isDisplayTypeChar));
            map.put(ViewProperties.DATA_VIEW_KEY.TRANSPOSED, new Boolean(isTransposed));
        } else if (dataViewName.startsWith("ncsa.hdf.view.DefaultImageView")) {
            map.put(ViewProperties.DATA_VIEW_KEY.CONVERTBYTE, new Boolean((bitmask != null)));
        }


        Object[] tmpargs = {viewer, map};
        initargs = tmpargs;

        ((JFrame)viewer).setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        try {
            theView = Tools.newInstance(theClass, initargs);
            viewer.addDataView((DataView)theView);
        }finally {
            ((JFrame)viewer).setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
        }

        return (DataView)theView;
    }

    /**
     * Displays the meta data of a data object.
     * @param dataObject teh data object
     * @return the MetaDataView that displays the MetaData of the data object
     * @throws Exception
     */
    public MetaDataView showMetaData(HObject dataObject)
        throws Exception
    {
        if (dataObject == null) {
            return null;
        }

        List metaDataViewList = HDFView.getListOfMetaDataView();
        if ((metaDataViewList == null) || (metaDataViewList.size() <=0)) {
            return null;
        }

        int n = metaDataViewList.size();
        Class viewClass = null;
        String className = (String)metaDataViewList.get(0);

        if (!isDefaultDisplay && (n>1)) {
            className = (String) JOptionPane.showInputDialog (
                this,
                "Select MetaDataView",
                "HDFView",
                JOptionPane.INFORMATION_MESSAGE,
                null,
                metaDataViewList.toArray(),
                className);
        }

        // enables use of JHDF5 in JNLP (Web Start) applications, the system class loader with reflection first.
        Class theClass = null;
        try { theClass = Class.forName(className); }
        catch (Exception ex) { theClass = ViewProperties.loadExtClass().loadClass(className); }

        Object[] initargs = {viewer};
        MetaDataView dataView = (MetaDataView)Tools.newInstance(theClass, initargs);

        return dataView;
    }

    /**
     * Adds a new data object to the file.
     * @param newObject the new object to add.
     * @param parentGroup the parent group the object is to add to.
     * @throws Exception
     */
    public void addObject(HObject newObject, Group parentGroup)
        throws Exception {
        if ((newObject == null) || (parentGroup==null)) {
            return;
        }

        TreeNode pnode = findTreeNode(parentGroup);
        TreeNode newnode = null;
        if (newObject instanceof Group) {
            newnode = new DefaultMutableTreeNode(newObject) {
            	public static final long serialVersionUID = HObject.serialVersionUID;

                public boolean isLeaf() { return false; }
            };
        } else {
            newnode = new DefaultMutableTreeNode(newObject);
        }

        treeModel.insertNodeInto((DefaultMutableTreeNode)newnode,
            (DefaultMutableTreeNode)pnode, pnode.getChildCount());
    }

    /**
     * Returns the JTree which holds the file structure.
     * @return the JTree which holds the file structure.
     */
    public JTree getTree(){
        return tree;
    }

    /**
     * Returns the list of current open files..
     */
    public List getCurrentFiles() {
        return fileList;
    }

    /**
     * Returns the tree node that contains the given data object.
     */
    public TreeNode findTreeNode(HObject obj)
    {
        if (obj == null) {
            return null;
        }

        TreeNode theFileRoot = obj.getFileFormat().getRootNode();
        if (theFileRoot == null) {
            return null;
        }

        DefaultMutableTreeNode theNode = null;
        HObject theObj = null;
        Enumeration local_enum = ((DefaultMutableTreeNode)theFileRoot).breadthFirstEnumeration();
        while(local_enum.hasMoreElements())
        {
            theNode = (DefaultMutableTreeNode)local_enum.nextElement();
            theObj = (HObject)theNode.getUserObject();
            if (theObj == null) {
                continue;
            } else if (theObj.equals(obj)) {
                return theNode;
            }
        }

        return null;
    }

    /**
     * This class is used to change the default icons for tree nodes.
     * @see javax.swing.tree.DefaultTreeCellRenderer
     */
    private class HTreeCellRenderer extends DefaultTreeCellRenderer
    {
    	public static final long serialVersionUID = HObject.serialVersionUID;

        private Icon h4Icon, h5Icon,
                datasetIcon, imageIcon, tableIcon, textIcon,
                openFolder, closeFolder,
                datasetIconA, imageIconA, tableIconA, textIconA,
                openFolderA, closeFolderA, datatypeIcon, datatypeIconA;

        private HTreeCellRenderer()
        {
            super();

            openFolder = ViewProperties.getFolderopenIcon();
            closeFolder = ViewProperties.getFoldercloseIcon();
            datasetIcon = ViewProperties.getDatasetIcon();
            imageIcon = ViewProperties.getImageIcon();
            h4Icon = ViewProperties.getH4Icon();
            h5Icon = ViewProperties.getH5Icon();
            tableIcon = ViewProperties.getTableIcon();
            textIcon = ViewProperties.getTextIcon();

            openFolderA = ViewProperties.getFolderopenIconA();
            closeFolderA = ViewProperties.getFoldercloseIconA();
            datasetIconA = ViewProperties.getDatasetIconA();
            imageIconA = ViewProperties.getImageIconA();
            tableIconA = ViewProperties.getTableIconA();
            textIconA = ViewProperties.getTextIconA();
            datatypeIcon = ViewProperties.getDatatypeIcon();
            datatypeIconA = ViewProperties.getDatatypeIconA();

            if (openFolder != null) {
                openIcon = openFolder;
            } else {
                openFolder = this.openIcon;
            }

            if (closeFolder != null) {
                closedIcon = closeFolder;
            } else {
                closeFolder = closedIcon;
            }

            if (datasetIcon == null) {
                datasetIcon = leafIcon;
            }
            if (imageIcon == null) {
                imageIcon = leafIcon;
            }
            if (tableIcon == null) {
                tableIcon = leafIcon;
            }
            if (textIcon == null) {
                textIcon = leafIcon;
            }
            if (h4Icon == null) {
                h4Icon = leafIcon;
            }
            if (h5Icon == null) {
                h5Icon = leafIcon;
            }
            if (datatypeIcon == null) {
                datatypeIcon = leafIcon;
            }

            if (openFolderA == null) {
                openFolderA = openFolder;
            }
            if (closeFolderA == null) {
                closeFolderA = closeFolder;
            }
            if (datasetIconA == null) {
                datasetIconA = datasetIcon;
            }
            if (imageIconA == null) {
                imageIconA = imageIcon;
            }
            if (tableIconA == null) {
                tableIconA = tableIcon;
            }
            if (textIconA == null) {
                textIconA = textIcon;
            }
            if (datatypeIconA == null) {
                datatypeIconA = datatypeIcon;
            }
        }

        public Component getTreeCellRendererComponent(
            JTree tree,
            Object value,
            boolean selected,
            boolean expanded,
            boolean leaf,
            int row,
            boolean hasFocus)
        {
            HObject theObject = (HObject)((DefaultMutableTreeNode)value).getUserObject();

            boolean hasAttribute = false;
            if (theObject instanceof Dataset)
            {
                if (theObject instanceof ScalarDS)
                {
                    ScalarDS sd = (ScalarDS)theObject;
                    hasAttribute = sd.hasAttribute();
                    if (sd.isImage())
                    {
                        if (hasAttribute) {
                            leafIcon = imageIconA;
                        } else {
                            leafIcon = imageIcon;
                        }
                    }
                    else if (sd.isText())
                    {
                        if (hasAttribute) {
                            leafIcon = textIconA;
                        } else {
                            leafIcon = textIcon;
                        }
                    }
                    else
                    {
                        if (hasAttribute) {
                            leafIcon = datasetIconA;
                        } else {
                            leafIcon = datasetIcon;
                        }
                   
                    }
                }
                else if (theObject instanceof CompoundDS)
                {
                    if (theObject.hasAttribute()) {
                        leafIcon = tableIconA;
                    } else {
                        leafIcon = tableIcon;
                    }
                }
            }
            else if (theObject instanceof Group)
            {
                Group g = (Group)theObject;

                if (g.hasAttribute())
                {
                    openIcon = openFolderA;
                    closedIcon = closeFolderA;
                } else
                {
                    openIcon = openFolder;
                    closedIcon = closeFolder;
                }

                if (g.isRoot())
                {
                    if ( g.getFileFormat().isThisType(FileFormat.getFileFormat(FileFormat.FILE_TYPE_HDF5))) {
                        openIcon = closedIcon = h5Icon;
                    } else if ( g.getFileFormat().isThisType(FileFormat.getFileFormat(FileFormat.FILE_TYPE_HDF4))) {
                        openIcon = closedIcon = h4Icon;
                    }
                }
            }
            else if (theObject instanceof Datatype)
            {
                Datatype t = (Datatype)theObject;

                if (t.hasAttribute()) {
                    leafIcon = datatypeIconA;
                } else {
                    leafIcon = datatypeIcon;
                }
            }

            return super.getTreeCellRendererComponent(
                tree,
                value,
                selected,
                expanded,
                leaf,
                row,
                hasFocus);
        }
    } // private class HTreeCellRenderer

    /**
     * Handle mouse clicks on data object in the tree view.
     * A right mouse-click to show the popup menu for user choice.
     * A doulbe left-mouse-click to display the data content.
     * A single left-mouse-click to select the current data object.
     */
    private class HTreeMouseAdapter extends MouseAdapter
    {
        //public void mousePressed(MouseEvent e)
        public void mouseReleased(MouseEvent e)
        {
            TreePath selPath = tree.getPathForLocation(e.getX(), e.getY());
            if (selPath == null) {
                return;
            }

            DefaultMutableTreeNode theNode = (DefaultMutableTreeNode)selPath.getLastPathComponent();
            if (!theNode.equals(selectedNode))
            {
                selectedNode = theNode;
                selectedObject = ((HObject)(selectedNode.getUserObject()));
                FileFormat theFile = selectedObject.getFileFormat();
                if ((theFile!= null) && !theFile.equals(selectedFile))
                {
                    // a different file is selected, handle only one file a time
                    selectedFile = theFile;
                    tree.clearSelection();
                    tree.setSelectionPath(selPath);
                }
                
                viewer.mouseEventFired(e);
            }

            // ***************************************************************
            // Different platforms have different ways to show popups
            // if (e.getModifiers() == MouseEvent.BUTTON3_MASK) works for all but mac
            // mouseReleased() and e.isPopupTrigger() work on windows and mac but not unix,
            // mouseClicked() and e.isPopupTrigger() work on unix and mac but not windows,
            // to solve the problem, we use both.
            // 7/25/06 bug 517. e.isPopupTrigger does not work on one mouse Mac.
            // add (MouseEvent.BUTTON1_MASK|MouseEvent.CTRL_MASK) for MAC
            int eMod = e.getModifiers();
            if (e.isPopupTrigger() || (eMod == MouseEvent.BUTTON3_MASK) ||
                (System.getProperty("os.name").startsWith("Mac") &&
                (eMod == (MouseEvent.BUTTON1_MASK|MouseEvent.CTRL_MASK))))
            {
                int selRow = tree.getRowForLocation(e.getX(), e.getY());

                if (!tree.isRowSelected(selRow))
                {
                    // reselect the node
                    tree.clearSelection();
                    tree.setSelectionRow(selRow);
                }
                showPopupMenu(e);
            }
            // double click to open data content
            else if (e.getClickCount() == 2)
            {
                isDefaultDisplay = true;
                try { showDataContent(selectedObject); }
                catch (Exception ex) {}
            }
        } // public void mousePressed(MouseEvent e)
    } // private class HTreeMouseAdapter extends MouseAdapter
}
