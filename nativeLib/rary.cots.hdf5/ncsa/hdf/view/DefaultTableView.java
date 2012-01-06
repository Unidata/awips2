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

package ncsa.hdf.view;

import ncsa.hdf.object.*;

import javax.swing.*;
import javax.swing.table.*;
import javax.swing.event.*;
import javax.swing.tree.*;
import java.awt.event.*;

import javax.swing.border.*;
import java.awt.datatransfer.*;
import java.util.*;
import java.io.*;
import java.lang.reflect.Array;
import java.lang.reflect.Constructor;
import java.math.BigInteger;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.MouseEvent;
import java.awt.Toolkit;
import java.awt.Color;
import java.awt.Insets;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.awt.GridLayout;
import java.awt.Point;

/**
 * TableView displays an HDF dataset as a two-dimensional table.
 * 
 * @author Peter X. Cao
 * @version 2.4 9/6/2007
 */
public class DefaultTableView extends JInternalFrame
implements TableView, ActionListener, MouseListener
{
	private static final long serialVersionUID = HObject.serialVersionUID;

    /**
     * The main HDFView.
     */
    private final ViewManager viewer;

    /** Numerical data type.
      B = byte array,
      S = short array,
      I = int array,
      J = long array,
      F = float array, and
      D = double array.
     */
    private char NT = ' ';

    /**
     * The Scalar Dataset.
     */
    private Dataset dataset;

    /**
     * The value of the dataset.
     */
    private Object dataValue;

    /**
     * The table used to hold the table data.
     */
    private JTable table;

    /** Label to indicate the current cell location. */
    private JLabel cellLabel;

    /** Text field to display the value of of the current cell. */
    private JTextArea cellValueField;

    private boolean isValueChanged;

    private final Toolkit toolkit;

    private boolean isReadOnly;

    private boolean isDisplayTypeChar;

    private boolean isDataTransposed;
    
    private boolean isRegRef, isObjRef;
    
    private final JCheckBoxMenuItem checkFixedDataLength;
    private int fixedDataLength;
    private final JCheckBoxMenuItem checkScientificNotation, checkHex, checkBin; 

    private final DecimalFormat scientificFormat = new DecimalFormat ("###.#####E0#");
    private final NumberFormat normalFormat = null;//NumberFormat.getInstance();
    private NumberFormat numberFormat = normalFormat;
    private boolean showAsHex = false, showAsBin = false;
    private final boolean startEditing[] = {false};
    private JPopupMenu popupMenu;
    private enum ViewType { TABLE, IMAGE, TEXT }
    private ViewType viewType;

    private JTextField frameField;

    private long curFrame=0, maxFrame=1;
    
    private Object fillValue = null;
    
    private BitSet bitmask;
 
     /**
     * Constructs an TableView.
     * <p>
     * @param theView the main HDFView.
     */
    public DefaultTableView(ViewManager theView) {
        this(theView, null);
    }

    /**
     * Constructs an TableView.
     * <p>
     * @param theView the main HDFView.
     * @param map the properties on how to show the data. The map is used to 
     *        allow applications to pass properties on how to display the data, 
     *        such as, transposing data, showing data as character, applying 
     *        bitmask, and etc. Predefined keys are listed at ViewProperties.DATA_VIEW_KEY.
     */
    public DefaultTableView(ViewManager theView, HashMap map)
    {
        super();

        setDefaultCloseOperation(JInternalFrame.DISPOSE_ON_CLOSE);

        viewer = theView;
        toolkit = Toolkit.getDefaultToolkit();
        isValueChanged = false;
        isReadOnly = false;
        isRegRef = false;
        isObjRef = false;
        viewType = ViewType.TABLE;
        fixedDataLength = -1;
        HObject hobject = null;
        popupMenu = null;
        bitmask = null;
        
        checkFixedDataLength = new JCheckBoxMenuItem("Fixed Data Length", false);
        checkScientificNotation = new JCheckBoxMenuItem("Show Scientific Notation", false);
        checkHex = new JCheckBoxMenuItem("Show Hexadecimal", false);
        checkBin = new JCheckBoxMenuItem("Show Binary", false);

        if (map != null) {
            hobject = (HObject)map.get(ViewProperties.DATA_VIEW_KEY.OBJECT);
            
            bitmask = (BitSet)map.get(ViewProperties.DATA_VIEW_KEY.BITMASK);
            
            Boolean b = (Boolean) map.get(ViewProperties.DATA_VIEW_KEY.CHAR);
            if (b != null)
                isDisplayTypeChar = b.booleanValue();
            
            b = (Boolean) map.get(ViewProperties.DATA_VIEW_KEY.TRANSPOSED);
            if (b != null)
                isDataTransposed = b.booleanValue();
        }

        if (hobject == null)
            hobject = (HObject)viewer.getTreeView().getCurrentObject();
        
        if ((hobject == null) || !(hobject instanceof Dataset)) {
             return;
        }

        dataset = (Dataset)hobject;
        isReadOnly = dataset.getFileFormat().isReadOnly();


        // cannot edit hdf4 vdata
        if (dataset.getFileFormat().isThisType(FileFormat.getFileFormat(FileFormat.FILE_TYPE_HDF4)) &&
            (dataset instanceof CompoundDS)) {
            isReadOnly = true;
        }

        // disable edit feature for szip compression when encode is not enabled
        if (!isReadOnly)
        {
            String compression = dataset.getCompression();
            if ((compression != null) && compression.startsWith("SZIP"))
            {
                if (!compression.endsWith("ENCODE_ENABLED")) {
                    isReadOnly = true;
                }
            }
        }

        Datatype dtype = dataset.getDatatype();
        isDisplayTypeChar = (isDisplayTypeChar && 
               (dtype.getDatatypeSize()==1 || 
               (dtype.getDatatypeClass() == Datatype.CLASS_ARRAY &&
                dtype.getBasetype().getDatatypeClass()==Datatype.CLASS_CHAR)));
        
        dataset.setEnumConverted(ViewProperties.isConvertEnum());
        
        // create the table and its columnHeader
        if (dataset instanceof CompoundDS)
        {
            isDataTransposed = false; // disable transpose for compound dataset
            this.setFrameIcon(ViewProperties.getTableIcon());
            table = createTable((CompoundDS)dataset);
        }
        else /*if (dataset instanceof ScalarDS) */
        {
            this.setFrameIcon(ViewProperties.getDatasetIcon());
            table = createTable( (ScalarDS)dataset);
            
            if (dtype.getDatatypeClass() == Datatype.CLASS_REFERENCE) 
            {
                table.addMouseListener(this);
                
                if (dtype.getDatatypeSize() > 8) {
                    isReadOnly = true;
                    isRegRef = true;
                }
                else
                    isObjRef = true;
            }
        }

        if (table == null)
        {
            viewer.showStatus("Creating table failed - "+dataset.getName());
            dataset = null;
            return;
        }
        
        ColumnHeader columnHeaders = new ColumnHeader(table);
        table.setTableHeader(columnHeaders);
        table.setCellSelectionEnabled(true);
        table.setAutoResizeMode( JTable.AUTO_RESIZE_OFF );
        table.setGridColor(Color.gray);

        // add the table to a scroller
        JScrollPane scrollingTable = new JScrollPane(table);
        scrollingTable.getVerticalScrollBar().setUnitIncrement(100);
        scrollingTable.getHorizontalScrollBar().setUnitIncrement(100);

        // create row headers and add it to the scroller
        RowHeader rowHeaders = new RowHeader( table, dataset );

        JViewport viewp = new JViewport();
        viewp.add( rowHeaders );
        viewp.setPreferredSize( rowHeaders.getPreferredSize() );
        scrollingTable.setRowHeader( viewp );
         
        cellLabel = new JLabel("");
        cellLabel.setBorder(new EtchedBorder(EtchedBorder.LOWERED) );
        Dimension dim = cellLabel.getPreferredSize();
        dim.width = 75;
        cellLabel.setPreferredSize( dim );
        cellLabel.setHorizontalAlignment(JLabel.RIGHT);

        cellValueField = new JTextArea();
        cellValueField.setLineWrap(true);
        cellValueField.setWrapStyleWord(true);
        cellValueField.setEditable(false);
        cellValueField.setBackground(new Color(255, 255, 240));

        JScrollPane scrollingcellValue = new JScrollPane(cellValueField);
        scrollingcellValue.getVerticalScrollBar().setUnitIncrement(50);
        scrollingcellValue.getHorizontalScrollBar().setUnitIncrement(50);

        JPanel valuePane = new JPanel();
        valuePane.setLayout(new BorderLayout());
        valuePane.add(cellLabel, BorderLayout.WEST);
        valuePane.add (scrollingcellValue, BorderLayout.CENTER);
       

        JSplitPane splitPane = new JSplitPane(
            JSplitPane.VERTICAL_SPLIT,
            valuePane, scrollingTable);
        splitPane.setDividerLocation(25);
        JPanel contentPane = (JPanel)getContentPane();
        contentPane.add(splitPane);

        // set title
        StringBuffer sb = new StringBuffer("TableView  -  ");
        sb.append(hobject.getName());
        sb.append("  -  ");
        sb.append(hobject.getPath());
        sb.append("  -  ");
        sb.append(dataset.getFile());
        setTitle(sb.toString());

        // setup subset information
        int rank = dataset.getRank();
        int[] selectedIndex = dataset.getSelectedIndex();
        long[] count = dataset.getSelectedDims();
        long[] stride = dataset.getStride();
        long[] dims = dataset.getDims();
        long[] start = dataset.getStartDims();
        int n = Math.min(3, rank);
        if (rank>2) {
            curFrame = start[selectedIndex[2]];
            maxFrame = dims[selectedIndex[2]];
        }

        sb.append(" [ dims");
        sb.append(selectedIndex[0]);
        for (int i=1; i<n; i++)
        {
            sb.append("x");
            sb.append(selectedIndex[i]);
        }
        sb.append(", start");
        sb.append(start[selectedIndex[0]]);
        for (int i=1; i<n; i++)
        {
            sb.append("x");
            sb.append(start[selectedIndex[i]]);
        }
        sb.append(", count");
        sb.append(count[selectedIndex[0]]);
        for (int i=1; i<n; i++)
        {
            sb.append("x");
            sb.append(count[selectedIndex[i]]);
        }
        sb.append(", stride");
        sb.append(stride[selectedIndex[0]]);
        for (int i=1; i<n; i++)
        {
            sb.append("x");
            sb.append(stride[selectedIndex[i]]);
        }
        sb.append(" ] ");

        setJMenuBar(createMenuBar());
        viewer.showStatus(sb.toString());

        // set cell height for large fonts
		int cellRowHeight = table.getFontMetrics(table.getFont()).getHeight();
        rowHeaders.setRowHeight(cellRowHeight);
        table.setRowHeight(cellRowHeight);
        
        // create popup menu for reg. ref.
        if (isRegRef || isObjRef)
            popupMenu = createPopupMenu();
    }

    private JMenuBar createMenuBar() {
        JMenuBar bar = new JMenuBar();
        JButton button;
        boolean isEditable = !isReadOnly;
        boolean is3D = (dataset.getRank() > 2);

        JMenu menu = new JMenu("Table", false);
        menu.setMnemonic('T');
        bar.add(menu);

        JMenuItem item = new JMenuItem( "Export Data to File");
        item.addActionListener(this);
        item.setActionCommand("Save table as text");
        menu.add(item);
        
        menu.addSeparator();

        item = new JMenuItem( "Import Data from File");
        item.addActionListener(this);
        item.setActionCommand("Import data from file");
        item.setEnabled(isEditable);
        menu.add(item);
        
        item = checkFixedDataLength;
        item.addActionListener(this);
        item.setActionCommand("Fixed data length");
        if (dataset instanceof ScalarDS) {
            menu.add(item);
        }

        menu.addSeparator();

        item = new JMenuItem( "Copy");
        item.addActionListener(this);
        item.setActionCommand("Copy data");
        item.setAccelerator( KeyStroke.getKeyStroke(KeyEvent.VK_C, KeyEvent.CTRL_MASK, true));
        menu.add(item);

        item = new JMenuItem( "Paste");
        item.addActionListener(this);
        item.setActionCommand("Paste data");
        item.setAccelerator( KeyStroke.getKeyStroke(KeyEvent.VK_V, KeyEvent.CTRL_MASK, true));
        item.setEnabled(isEditable);
        menu.add(item);

        menu.addSeparator();

        item = new JMenuItem( "Copy to New Dataset");
        item.addActionListener(this);
        item.setActionCommand("Write selection to dataset");
        item.setEnabled(isEditable && (dataset instanceof ScalarDS));
        menu.add(item);

        item = new JMenuItem( "Save to File");
        item.addActionListener(this);
        item.setActionCommand("Save dataset");
        item.setEnabled(isEditable);
        item.setAccelerator( KeyStroke.getKeyStroke(KeyEvent.VK_U, KeyEvent.CTRL_MASK, true));
        menu.add(item);

        menu.addSeparator();

        item = new JMenuItem( "Select All");
        item.addActionListener(this);
        item.setActionCommand("Select all data");
        item.setAccelerator( KeyStroke.getKeyStroke(KeyEvent.VK_A, KeyEvent.CTRL_MASK, true));
        menu.add(item);

        menu.addSeparator();

        item = new JMenuItem( "Show Lineplot");
        item.addActionListener(this);
        item.setActionCommand("Show chart");
        menu.add(item);

        item = new JMenuItem( "Show Statistics");
        item.addActionListener(this);
        item.setActionCommand("Show statistics");
        menu.add(item);

        menu.addSeparator();

        item = new JMenuItem( "Math Conversion");
        item.addActionListener(this);
        item.setActionCommand("Math conversion");
        item.setEnabled(isEditable);
        menu.add(item);

        menu.addSeparator();

        item = checkScientificNotation;
        item.addActionListener(this);
        item.setActionCommand("Show scientific notation");
        if (dataset instanceof ScalarDS) {
            menu.add(item);
        }

        boolean isInt = (NT == 'B' || NT == 'S' || NT == 'I' || NT == 'J');
        item = checkHex;
        item.addActionListener(this);
        item.setActionCommand("Show hexadecimal");
        if ( (dataset instanceof ScalarDS) && isInt ) {
            menu.add(item);
        }
        
        item = checkBin;
        item.addActionListener(this);
        item.setActionCommand("Show binary");
        if ( (dataset instanceof ScalarDS) && isInt ) {
            menu.add(item);
        }        
        
        menu.addSeparator();

        item = new JMenuItem( "Close");
        item.addActionListener(this);
        item.setActionCommand("Close");
        menu.add(item);

        bar.add( new JLabel("     ") );

        // add icons to the menubar

        Insets margin = new Insets( 0, 2, 0, 2 );

        // chart button
        button = new JButton( ViewProperties.getChartIcon() );
        bar.add( button );
        button.setToolTipText( "Line Plot" );
        button.setMargin( margin );
        button.addActionListener( this );
        button.setActionCommand( "Show chart" );

        if (is3D) {
            bar.add( new JLabel("     ") );

            // first button
            button = new JButton( ViewProperties.getFirstIcon() );
            bar.add( button );
            button.setToolTipText( "First" );
            button.setMargin( margin );
            button.addActionListener( this );
            button.setActionCommand( "First page" );

            // previous button
            button = new JButton( ViewProperties.getPreviousIcon() );
            bar.add( button );
            button.setToolTipText( "Previous" );
            button.setMargin( margin );
            button.addActionListener( this );
            button.setActionCommand( "Previous page" );

            frameField = new JTextField(String.valueOf(curFrame));
            frameField.setMaximumSize(new Dimension(50,30));
            bar.add( frameField );
            frameField.setMargin( margin );
            frameField.addActionListener( this );
            frameField.setActionCommand( "Go to frame" );

            JLabel tmpField = new JLabel(String.valueOf(maxFrame),SwingConstants.CENTER);
            tmpField.setMaximumSize(new Dimension(50,30));
            bar.add( tmpField );

            // next button
            button = new JButton( ViewProperties.getNextIcon() );
            bar.add( button );
            button.setToolTipText( "Next" );
            button.setMargin( margin );
            button.addActionListener( this );
            button.setActionCommand( "Next page" );

            // last button
            button = new JButton( ViewProperties.getLastIcon() );
            bar.add( button );
            button.setToolTipText( "Last" );
            button.setMargin( margin );
            button.addActionListener( this );
            button.setActionCommand( "Last page" );
        }

        return bar;
    }

    public void actionPerformed(ActionEvent e)
    {
        try { 
            setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));

            Object source = e.getSource();
            String cmd = e.getActionCommand();

            if (cmd.equals("Close")) {
                dispose();  // terminate the application
            }
            else if (cmd.equals("Save table as text")) {
                try { saveAsText(); }
                catch (Exception ex) {
                    toolkit.beep();
                    JOptionPane.showMessageDialog((JFrame)viewer,
                            ex,
                            getTitle(),
                            JOptionPane.ERROR_MESSAGE);
                }
            }
            else if (cmd.equals("Copy data")) {
                copyData();
            }
            else if (cmd.equals("Paste data")) {
                pasteData();
            }
            else if (cmd.equals("Import data from file")) {
                String currentDir = dataset.getFileFormat().getParent();
                JFileChooser fchooser = new JFileChooser(currentDir);
                fchooser.setFileFilter(DefaultFileFilter.getFileFilterText());
                int returnVal = fchooser.showOpenDialog(this);

                if(returnVal != JFileChooser.APPROVE_OPTION) {
                    return;
                }

                File choosedFile = fchooser.getSelectedFile();
                if (choosedFile == null) {
                    return;
                }

                String txtFile = choosedFile.getAbsolutePath();
                importTextData(txtFile);
            }
            else if (cmd.equals("Write selection to dataset"))
            {
                JTable jtable = getTable();
                if ((jtable.getSelectedColumnCount() <=0) ||
                        (jtable.getSelectedRowCount() <=0) )
                {
                    JOptionPane.showMessageDialog(
                            this,
                            "Select table cells to write.",
                            "HDFView",
                            JOptionPane.INFORMATION_MESSAGE);
                    return;
                }

                TreeView treeView = viewer.getTreeView();
                TreeNode node = viewer.getTreeView().findTreeNode(dataset);
                Group pGroup = (Group)((DefaultMutableTreeNode)node.getParent()).getUserObject();
                TreeNode root = dataset.getFileFormat().getRootNode();

                if (root == null) {
                    return;
                }

                Vector list = new Vector(dataset.getFileFormat().getNumberOfMembers()+5);
                DefaultMutableTreeNode theNode = null;
                Enumeration local_enum = ((DefaultMutableTreeNode)root).depthFirstEnumeration();
                while(local_enum.hasMoreElements()) {
                    theNode = (DefaultMutableTreeNode)local_enum.nextElement();
                    list.add(theNode.getUserObject());
                }

                NewDatasetDialog dialog = new NewDatasetDialog(
                        (JFrame)viewer,
                        pGroup,
                        list,
                        this);
                dialog.setVisible(true);

                HObject obj = (HObject)dialog.getObject();
                if (obj != null) {
                    Group pgroup = dialog.getParentGroup();
                    try { treeView.addObject(obj, pgroup); }
                    catch (Exception ex) {}
                }

                list.setSize(0);
            }
            else if (cmd.equals("Save dataset")) {
                try { updateValueInFile(); }
                catch (Exception ex) {
                    toolkit.beep();
                    JOptionPane.showMessageDialog((JFrame)viewer,
                            ex,
                            getTitle(),
                            JOptionPane.ERROR_MESSAGE);
                }
            }
            else if (cmd.equals("Select all data")) {
                try { selectAll(); }
                catch (Exception ex) {
                    toolkit.beep();
                    JOptionPane.showMessageDialog((JFrame)viewer,
                            ex,
                            getTitle(),
                            JOptionPane.ERROR_MESSAGE);
                }
            }
            else if (cmd.equals("Show chart")) {
                showLineplot();
            }
            else if (cmd.equals("First page")) {
                firstPage();
            }
            else if (cmd.equals("Previous page")) {
                previousPage();
            }
            else if (cmd.equals("Next page")) {
                nextPage();
            }
            else if (cmd.equals("Last page")) {
                lastPage();
            }
            else if (cmd.equals("Show statistics")) {
                try {
                    Object theData = null;

                    if (dataset instanceof CompoundDS)
                    {
                        theData = getSelectedData();
                        int cols = table.getSelectedColumnCount();
                        //if (!(dataset instanceof ScalarDS))  return;
                        if ((dataset instanceof CompoundDS) && (cols>1))
                        {
                            JOptionPane.showMessageDialog(this,
                                    "Please select one colunm a time for compound dataset.",
                                    getTitle(),
                                    JOptionPane.ERROR_MESSAGE);
                            return;
                        }
                        else if (theData == null)
                        {
                            JOptionPane.showMessageDialog(this,
                                    "Select a column to show statistics.",
                                    getTitle(),
                                    JOptionPane.ERROR_MESSAGE);
                            return;
                        }
                    } else {
                        theData = dataValue;
                    }

                    double[] minmax = new double[2];
                    double[] stat = new double[2];
                    Tools.findMinMax(theData, minmax, fillValue);
                    if (Tools.computeStatistics(theData, stat, fillValue) > 0) {
                        String statistics = "Min                      = "+minmax[0] +
                        "\nMax                      = "+minmax[1] +
                        "\nMean                     = "+stat[0] +
                        "\nStandard deviation = "+stat[1];
                        JOptionPane.showMessageDialog(this, statistics, "Statistics", JOptionPane.INFORMATION_MESSAGE);
                    }

                    theData = null;
                    System.gc();
                } catch (Exception ex) {
                    toolkit.beep();
                    JOptionPane.showMessageDialog((JFrame)viewer,
                            ex,
                            getTitle(),
                            JOptionPane.ERROR_MESSAGE);
                }
            }
            else if (cmd.equals("Math conversion")) {
                try { mathConversion(); }
                catch (Exception ex) {
                    toolkit.beep();
                    JOptionPane.showMessageDialog((JFrame)viewer,
                            ex,
                            getTitle(),
                            JOptionPane.ERROR_MESSAGE);
                }
            }
            else if (cmd.startsWith("Go to frame"))
            {
                int page = 0;
                try { page = Integer.parseInt(frameField.getText().trim()); }
                catch (Exception ex) { page = -1; }

                gotoPage(page);
            }
            else if (cmd.equals("Show scientific notation"))
            {
                if (checkScientificNotation.isSelected()) {
                    numberFormat = scientificFormat;
                    checkHex.setSelected(false);
                    checkBin.setSelected(false);
                    showAsHex = false;
                    showAsBin = false;
                }
                else 
                    numberFormat = normalFormat;
                this.updateUI();
            }
            else if (cmd.equals("Show hexadecimal"))
            {
                showAsHex = checkHex.isSelected();
                if (showAsHex) {
                    checkScientificNotation.setSelected(false);
                    checkBin.setSelected(false);
                    showAsBin = false;
                    numberFormat = normalFormat;
                }
                this.updateUI();
            }
            else if (cmd.equals("Show binary"))
            {
                showAsBin = checkBin.isSelected();
                if (showAsBin) {
                    checkScientificNotation.setSelected(false);
                    checkHex.setSelected(false);
                    showAsHex = false;
                    numberFormat = normalFormat;
                }
                this.updateUI();
            }            
            else if (cmd.equals("Fixed data length"))
            {
                if (!checkFixedDataLength.isSelected()) {
                    fixedDataLength = -1;
                    this.updateUI();
                    return;
                }

                String str = JOptionPane.showInputDialog(this, "Enter fixed data length when importing text data\n\n"+
                        "For example, for a text string of \"12345678\"\n\t\tenter 2, the data will be 12, 34, 56, 78\n\t\tenter 4, the data will be 1234, 5678\n", "");

                if ((str == null) || (str.length()<1)) {
                    checkFixedDataLength.setSelected(false);
                    return;
                }

                try { fixedDataLength = Integer.parseInt(str); }
                catch (Exception ex) { fixedDataLength = -1; }

                if (fixedDataLength<1) {
                    checkFixedDataLength.setSelected(false);
                    return;
                }
            } else if (cmd.startsWith("Show data as")) {
                // show data pointed by reg. ref.
                if (cmd.endsWith("table"))
                    viewType = ViewType.TABLE;
                else if (cmd.endsWith("image"))
                    viewType = ViewType.IMAGE;
                else
                    viewType = ViewType.TABLE;
                
                Object theData = getSelectedData();
                if (theData == null) {
                    toolkit.beep();
                    JOptionPane.showMessageDialog(
                        this,
                        "No data selected.",
                        getTitle(),
                        JOptionPane.ERROR_MESSAGE);
                    return;
                    
                }
       
                int len = Array.getLength(theData);
                for (int i=0; i<len; i++) {
                    if (isRegRef)
                        showRegRefData((String)Array.get(theData, i));
                    else if (isObjRef)
                        showObjRefData(Array.getLong(theData, i));
                }
            }  
        }
        finally { setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR)); }
    }

    // Implementing DataView.
    public HObject getDataObject() {
        return dataset;
    }

    public void dispose()
    {
        if (isValueChanged && !isReadOnly) {
            int op = JOptionPane.showConfirmDialog(this,
                    "\""+ dataset.getName() +"\" has changed.\n"+
                    "Do you want to save the changes?",
                    getTitle(),
                    JOptionPane.YES_NO_OPTION);

            if (op == JOptionPane.YES_OPTION) {
                updateValueInFile();
            } else 
                dataset.clearData(); // reload data
            
        }

        if (dataset instanceof ScalarDS)
        {
            ScalarDS sds = (ScalarDS)dataset;
            // reload the data when it is displayed next time
            // because the display type (table or image) may be
            // different.

            if (sds.isImage()) {
                sds.clearData();
            }

            dataValue = null;
            table = null;
        }

        viewer.removeDataView(this);
        
        super.dispose();
    }

    // Implementing DataObserver.
    private void previousPage()
    {
        int rank = dataset.getRank();

        if (rank < 3) {
            return;
        }

        long[] start = dataset.getStartDims();
        long[] dims = dataset.getDims();
        int[] selectedIndex = dataset.getSelectedIndex();
        long idx = start[selectedIndex[2]];
        if (idx == 0) {
            return; // current page is the first page
        }

        gotoPage(start[selectedIndex[2]]-1);
    }

    // Implementing DataObserver.
    private void nextPage()
    {
        int rank = dataset.getRank();

        if (rank < 3) {
            return;
        }

        long[] start = dataset.getStartDims();
        int[] selectedIndex = dataset.getSelectedIndex();
        long[] dims = dataset.getDims();
        long idx = start[selectedIndex[2]];
        if (idx == dims[selectedIndex[2]]-1) {
            return; // current page is the last page
        }

        gotoPage(start[selectedIndex[2]]+1);
    }

    // Implementing DataObserver.
    private void firstPage()
    {
        int rank = dataset.getRank();

        if (rank < 3) {
            return;
        }

        long[] start = dataset.getStartDims();
        int[] selectedIndex = dataset.getSelectedIndex();
        long[] dims = dataset.getDims();
        long idx = start[selectedIndex[2]];
        if (idx == 0) {
            return; // current page is the first page
        }

        gotoPage(0);
    }

    // Implementing DataObserver.
    private void lastPage()
    {
        int rank = dataset.getRank();

        if (rank < 3) {
            return;
        }

        long[] start = dataset.getStartDims();
        int[] selectedIndex = dataset.getSelectedIndex();
        long[] dims = dataset.getDims();
        long idx = start[selectedIndex[2]];
        if (idx == dims[selectedIndex[2]]-1) {
            return; // current page is the last page
        }

        gotoPage(dims[selectedIndex[2]]-1);
    }

    // Implementing TableObserver.
    public JTable getTable() {
        return table;
    }

    // Implementing TableObserver.
    private void showLineplot()
    {
        int[] rows = table.getSelectedRows();
        int[] cols = table.getSelectedColumns();

        if ((rows == null) ||
            (cols == null) ||
            (rows.length <=0) ||
            (cols.length  <=0) )
        {
            toolkit.beep();
            JOptionPane.showMessageDialog(this,
            "Select rows/columns to draw line plot.",
            getTitle(),
            JOptionPane.ERROR_MESSAGE);
            return;
        }

/*
        Object[] options = { "Column", "Row", "X-axis", "Cancel"};
        int option = JOptionPane.showOptionDialog(
                this,
                "Do you want to draw line plot by column or row?",
                getTitle(),
                JOptionPane.YES_NO_CANCEL_OPTION,
                JOptionPane.QUESTION_MESSAGE,
                null,
                options,
                options[0]);
        if (option == 3)
            return; // cancel the line plot action
        boolean isRowPlot = (option == 1);
*/
        int nrow = table.getRowCount();
        int ncol = table.getColumnCount();

        LineplotOption lpo = new LineplotOption((JFrame)viewer, "Line Plot Options -- "+dataset.getName(), nrow, ncol);
        lpo.setVisible(true);

        int plotType = lpo.getPlotBy();
        if (plotType == LineplotOption.NO_PLOT) {
            return;
        }

        boolean isRowPlot = (plotType==LineplotOption.ROW_PLOT);
        int xIndex = lpo.getXindex();

        // figure out to plot data by row or by column
        // Plot data by rows if all columns are selected and part of
        // rows are selected, otherwise plot data by column
        double[][] data = null;
        int nLines = 0;
        String title = "Lineplot - "+dataset.getPath()+dataset.getName();
        String[] lineLabels = null;
        double[] yRange = {Double.POSITIVE_INFINITY, Double.NEGATIVE_INFINITY};
        double xData[] = null;

        if (isRowPlot)
        {
            title +=" - by row";
            nLines = rows.length;
            if (nLines > 10)
            {
                toolkit.beep();
                nLines = 10;
                JOptionPane.showMessageDialog(this,
                "More than 10 rows are selected.\n"+
                "The first 10 rows will be displayed.",
                getTitle(),
                JOptionPane.WARNING_MESSAGE);
            }
            lineLabels = new String[nLines];
            data = new double[nLines][cols.length];

            for (int i=0; i<nLines; i++)
            {
                lineLabels[i] = String.valueOf(rows[i]);
                for (int j=0; j<cols.length; j++)
                {
                    try {
                        data[i][j] = Double.parseDouble(
                        table.getValueAt(rows[i], cols[j]).toString());
                        yRange[0] = Math.min(yRange[0], data[i][j]);

                        yRange[1] = Math.max(yRange[1], data[i][j]);
                    } catch (NumberFormatException ex)
                    {
                        data[i][j] = 0;
                    }
                } // for (int j=0; j<ncols; j++)
            } // for (int i=0; i<rows.length; i++)

            if (xIndex >= 0)
            {
                xData = new double[cols.length];
                for (int j=0; j<cols.length; j++)
                {
                    try { xData[j] = Double.parseDouble( table.getValueAt(xIndex, j).toString()); }
                    catch (NumberFormatException ex) { xData[j] = 0; }
                }
            }
        } // if (isRowPlot)
        else
        {
            title +=" - by column";
            nLines = cols.length;
            if (nLines > 10)
            {
                toolkit.beep();
                nLines = 10;
                JOptionPane.showMessageDialog(this,
                "More than 10 columns are selected.\n"+
                "The first 10 columns will be displayed.",
                getTitle(),
                JOptionPane.WARNING_MESSAGE);
            }
            lineLabels = new String[nLines];
            data = new double[nLines][rows.length];
            for (int j=0; j<nLines; j++)
            {
                lineLabels[j] = table.getColumnName(cols[j]);
                for (int i=0; i<rows.length; i++)
                {
                    try {
                        data[j][i] = Double.parseDouble(
                        table.getValueAt(rows[i], cols[j]).toString());
                        yRange[0] = Math.min(yRange[0], data[j][i]);
                        yRange[1] = Math.max(yRange[1], data[j][i]);
                    } catch (NumberFormatException ex)
                    {
                        data[j][i] = 0;
                    }
                } // for (int j=0; j<ncols; j++)
            } // for (int i=0; i<rows.length; i++)

            if (xIndex >= 0)
            {
                xData = new double[rows.length];
                for (int j=0; j<rows.length; j++)
                {
                    try { xData[j] = Double.parseDouble( table.getValueAt(j, xIndex).toString()); }
                    catch (NumberFormatException ex) { xData[j] = 0; }
                }
            }
        } // else

        if ((yRange[0] == Double.POSITIVE_INFINITY) ||
            (yRange[1] == Double.NEGATIVE_INFINITY) ||
            (yRange[0] == yRange[1]))
        {
            toolkit.beep();
            JOptionPane.showMessageDialog(
                this,
                "Cannot show line plot for the selected data. \n"+
                "Please check the data range.",
                getTitle(),
                JOptionPane.ERROR_MESSAGE);
            data = null;
            return;
        }

        Chart cv = new Chart((JFrame)viewer, title, Chart.LINEPLOT, data, xData, yRange);
        cv.setLineLabels(lineLabels);

        String cname = dataValue.getClass().getName();
        char dname = cname.charAt(cname.lastIndexOf("[")+1);
        if ((dname == 'B') || (dname == 'S') || (dname == 'I') || (dname == 'J')) {
            cv.setTypeToInteger();
        }

        cv.setVisible(true);
    }

    /**
     * Returns the selected data values.
     */
    public Object getSelectedData()
    {
        if (dataset instanceof CompoundDS) {
            return getSelectedCompoundData();
        } else {
            return getSelectedScalarData();
        }
    }

    /**
     * Returns the selected data values.
     */
    private Object getSelectedScalarData()
    {
        Object selectedData = null;

        int[] selectedRows = table.getSelectedRows();
        int[] selectedCols = table.getSelectedColumns();
        if (selectedRows == null || selectedRows.length <=0 ||
            selectedCols == null || selectedCols.length <=0)
        {
            return null;
        }
   
        int size = selectedCols.length*selectedRows.length;


        // the whole table is selected
        if ((table.getColumnCount() == selectedCols.length) &&
            (table.getRowCount() == selectedRows.length)) {
            return dataValue;
        }
        
        selectedData = null;
        if (isRegRef) {
            // reg. ref data are stored in strings
            selectedData = new String[size];
        } else {
            switch (NT) {
            case 'B':
                selectedData = new byte[size];
                break;
            case 'S':
                selectedData = new short[size];
                break;
            case 'I':
                selectedData = new int[size];
                break;
            case 'J':
                selectedData = new long[size];
                break;
            case 'F':
                selectedData = new float[size];
                break;
            case 'D':
                selectedData = new double[size];
                break;
            default:
                selectedData = null;
                break;
            }
        }

        if (selectedData == null)
        {
            toolkit.beep();
            JOptionPane.showMessageDialog(this,
            "Unsupported data type.",
            getTitle(),
            JOptionPane.ERROR_MESSAGE);
            return null;
        }

        
        int r0 = table.getSelectedRow();
        int c0 = table.getSelectedColumn();
        int w = table.getColumnCount();
        int idx_src=0, idx_dst=0;
        for (int i=0; i<selectedRows.length; i++)
        {
            for (int j=0; j<selectedCols.length; j++) {
                idx_src = selectedRows[i]*w+selectedCols[j];
                Array.set(selectedData, idx_dst, Array.get(dataValue, idx_src));
                idx_dst++;
                
            }
        }
        
        // this only works for continuous cells
//        for (int i=0; i<rows; i++)
//        {
//            idx_src = (r0+i)*w+c0;
//            System.arraycopy(dataValue, idx_src, selectedData, idx_dst, cols);
//            idx_dst += cols;
//        }


        return selectedData;
    }

    /**
     * Returns the selected data values.
     */
    private Object getSelectedCompoundData()
    {
        Object selectedData = null;

        int cols = table.getSelectedColumnCount();
        int rows = table.getSelectedRowCount();

        if ((cols <=0) || (rows <= 0))
        {
            toolkit.beep();
            JOptionPane.showMessageDialog(this,
            "No data is selected.",
            getTitle(),
            JOptionPane.ERROR_MESSAGE);
            return null;
        }

        Object colData = null;
        try {
            colData = ((List)dataset.getData()).get(table.getSelectedColumn());
        } catch (Exception ex) {return null;}

        int size = Array.getLength(colData);
        String cName = colData.getClass().getName();
        int cIndex = cName.lastIndexOf("[");
        char nt = ' ';
        if (cIndex >= 0 ) {
            nt = cName.charAt(cIndex+1);
        }

        if (nt == 'B') {
            selectedData = new byte[size];
        } else if (nt == 'S') {
            selectedData = new short[size];
        } else if (nt == 'I') {
            selectedData = new int[size];
        } else if (nt == 'J') {
            selectedData = new long[size];
        } else if (nt == 'F') {
            selectedData = new float[size];
        } else if (nt == 'D') {
            selectedData = new double[size];
        } else
        {
            toolkit.beep();
            JOptionPane.showMessageDialog(this,
            "Unsupported data type.",
            getTitle(),
            JOptionPane.ERROR_MESSAGE);
            return null;
        }

        System.arraycopy(colData, 0, selectedData, 0, size);

        return selectedData;
    }

    /**
     * Creates a JTable to hold a scalar dataset.
     */
    private JTable createTable(ScalarDS d)
    {
        JTable theTable = null;
        int rows=0, cols=0;

        int rank = d.getRank();
        if (rank <= 0)
        {
            d.init();
            rank = d.getRank();
        }
        long[] dims = d.getSelectedDims();

        rows = (int)dims[0];
        cols = 1;
        if (rank > 1)
        {
            rows = d.getHeight();
            cols = d.getWidth();
        }

        dataValue = null;
        try {
            dataValue = d.getData();
            
            if (Tools.applyBitmask(dataValue, bitmask) ) {
                isReadOnly = true;
                Border border = BorderFactory.createCompoundBorder(
                        BorderFactory.createRaisedBevelBorder(),
                        BorderFactory.createTitledBorder(
                                BorderFactory.createLineBorder(Color.BLUE, 3),
                                "By bitmask "+bitmask, 
                                TitledBorder.RIGHT, 
                                TitledBorder.TOP,
                                this.getFont(),
                                Color.RED)); 
                this.setBorder(border);
            }

            d.convertFromUnsignedC();
            dataValue = d.getData();
            
            if (Array.getLength(dataValue)<=rows)
                cols = 1;
        }
        catch (Exception ex)
        {
            JOptionPane.showMessageDialog(
                this,
                ex,
                getTitle(),
                JOptionPane.ERROR_MESSAGE);
            dataValue = null;
        }

        if (dataValue == null) {
            return null;
        }

        fillValue = d.getFillValue();
        
        String cName = dataValue.getClass().getName();
        int cIndex = cName.lastIndexOf("[");
        if (cIndex >= 0 ) {
            NT = cName.charAt(cIndex+1);
        }
        boolean isVL = cName.startsWith("[Ljava.lang.String;");

        // convert nubmerical data into char
        // only possible cases are byte[] and short[] (converted from unsigned byte)
        if (isDisplayTypeChar && ((NT == 'B') || (NT == 'S')))
        {
            int n = Array.getLength(dataValue);
            char[] charData = new char[n];
            for (int i=0; i<n; i++)
            {
                if (NT == 'B') {
                    charData[i] = (char)Array.getByte(dataValue, i);
                } else if (NT == 'S') {
                    charData[i] = (char)Array.getShort(dataValue, i);
                }
            }

            dataValue = charData;
        } else if ((NT == 'B') && dataset.getDatatype().getDatatypeClass()==Datatype.CLASS_ARRAY) {
            Datatype baseType = dataset.getDatatype().getBasetype();
            if (baseType.getDatatypeClass()==Datatype.CLASS_STRING) {
                dataValue = Dataset.byteToString((byte[])dataValue, baseType.getDatatypeSize());
            }
        }

        final String columnNames[] = new String[cols];
        final int rowCount = rows;
        final int colCount = cols;
        final long[] startArray = dataset.getStartDims();
        final long[] strideArray = dataset.getStride();
        int[] selectedIndex = dataset.getSelectedIndex();
        final int rowStart = (int)startArray[selectedIndex[0]];
        final int rowStride = (int)strideArray[selectedIndex[0]];
        int start = 0;
        int stride = 1;

        if (rank > 1)
        {
            start = (int)startArray[selectedIndex[1]];
            stride = (int)strideArray[selectedIndex[1]];
        }

        for (int i=0; i<cols; i++)
        {
            columnNames[i] = String.valueOf(start+i*stride);
        }

        AbstractTableModel tm =  new AbstractTableModel()
        {
        	private static final long serialVersionUID = HObject.serialVersionUID;
            private final StringBuffer stringBuffer = new StringBuffer();
            private final Datatype dtype = dataset.getDatatype();
            private final Datatype btype = dtype.getBasetype();
            private final int typeSize = dtype.getDatatypeSize();
            private final boolean isArray = (dtype.getDatatypeClass()==Datatype.CLASS_ARRAY);
            private final boolean isStr = (NT == 'L');
            private final boolean isInt = (NT == 'B' || NT == 'S' || NT == 'I' || NT == 'J');
            private final boolean isUINT64 = (dtype.isUnsigned() && (NT == 'J'));
            private Object theValue;
            
            public int getColumnCount() {
            	return columnNames.length;
            }

            public int getRowCount() {
            	return rowCount;
            }

            public String getColumnName(int col) {
            	return columnNames[col];
            }

            public Object getValueAt(int row, int column)
            {
            	if (startEditing[0])
            		return "";
            	
                if (isArray) {
                    // ARRAY dataset
                    int arraySize = dtype.getDatatypeSize()/btype.getDatatypeSize();
                    stringBuffer.setLength(0); // clear the old string
                    int i0 = (row*colCount+column)*arraySize;
                    int i1 = i0+arraySize;

                    if (isDisplayTypeChar) {
                        for (int i=i0; i<i1; i++) {
                            stringBuffer.append(Array.getChar(dataValue, i));
                            if(stringBuffer.length()>0 && i<(i1-1))
                            stringBuffer.append(", ");
                        }
                    } else {
                        for (int i=i0; i<i1; i++) {
                            stringBuffer.append(Array.get(dataValue, i));
                            if(stringBuffer.length()>0 && i<(i1-1))
                                stringBuffer.append(", ");
                        }
                    }
                    theValue = stringBuffer;
               } else {
                    if (isDataTransposed) {
                        theValue = Array.get(dataValue, column*rowCount+row);
                    } else {
                        theValue = Array.get(dataValue, row*colCount+column);
                    }
                    
                    if (isStr) 
                        return theValue;
                    
                    if (isUINT64) {
                        Long l = (Long)theValue;
                        if ( l< 0) {
                            l = (l << 1)>>1;
                            BigInteger big1 = new BigInteger("9223372036854775808"); // 2^65
                            BigInteger big2 = new BigInteger(l.toString()); 
                            BigInteger big = big1.add(big2);
                            theValue = big.toString();
                        }
                    } else if (showAsHex && isInt) {
                        // show in Hexadecimal
                        theValue = Long.toHexString(Long.valueOf(theValue.toString()));
                    }
                    else if (showAsBin && isInt) {
                        theValue = Tools.toBinaryString(Long.valueOf(theValue.toString()), typeSize);
                        //theValue = Long.toBinaryString(Long.valueOf(theValue.toString()));
                    }
                    else if (numberFormat!=null){
                        // show in scientific format
                        theValue = numberFormat.format(theValue);
                    } 
                }
                
                return theValue;
            } // getValueAt(int row, int column)
        };
        
        theTable = new JTable(tm)
        {
        	private static final long serialVersionUID = HObject.serialVersionUID;
        	private final Datatype dtype = dataset.getDatatype();
        	private final boolean isArray = (dtype.getDatatypeClass()==Datatype.CLASS_ARRAY);

        	public boolean isCellEditable( int row, int col )
        	{
        		if (isReadOnly || isDisplayTypeChar || isArray || showAsBin || showAsHex) {
        			return false;
        		} else {
        			return true;
        		}
        	}

        	public boolean editCellAt(int row, int column, java.util.EventObject e) 
        	{

                if (!isCellEditable(row, column)) {
                    return super.editCellAt(row, column, e);
                }
                
				if (e instanceof KeyEvent) {
					KeyEvent ke = (KeyEvent)e;
					if (ke.getID()==KeyEvent.KEY_PRESSED) 
						startEditing[0] = true;
				}
				
                return super.editCellAt(row, column, e);
            }
            
            public void editingStopped(ChangeEvent e)
            {
                int row = getEditingRow();
                int col = getEditingColumn();
                super.editingStopped(e);
 				startEditing[0] = false;
 
                Object source = e.getSource();

                if (source instanceof CellEditor)
                {
                    CellEditor editor = (CellEditor)source;
                    String cellValue = (String)editor.getCellEditorValue();

                    try {
                        updateValueInMemory(cellValue, row, col);
                    }
                    catch (Exception ex)
                    {
                        toolkit.beep();
                        JOptionPane.showMessageDialog(
                        this,
                        ex,
                        getTitle(),
                        JOptionPane.ERROR_MESSAGE);
                    }
                } // if (source instanceof CellEditor)
            }

            public boolean isCellSelected(int row, int column)
            {
                if ((getSelectedRow()==row) && (getSelectedColumn()==column))
                {
                    cellLabel.setText(
                        String.valueOf(rowStart+row*rowStride)+
                        ", "+
                        table.getColumnName(column)+
                        "  =  ");
                    
                    Object val = getValueAt(row, column);
                    String strVal = null;
                    
                    if (isRegRef) {
                        String reg = (String) val;
                        String oidStr = reg.substring(reg.indexOf(':')+1, reg.indexOf(' '));
                        long oid[] = {-1};
                        
                        // decode object ID
                        try { 
                            oid[0] = Long.valueOf(oidStr);
                            HObject obj = FileFormat.findObject(dataset.getFileFormat(), oid);
                            strVal = obj.getFullName() + " " + reg.substring(reg.indexOf("{"));
                        }
                        catch (Exception ex) { strVal = null; }
                    } else if (isObjRef) {
                        Long ref = (Long) val;
                        long oid[] = {ref.longValue()};
                        
                        // decode object ID
                        try { 
                            HObject obj = FileFormat.findObject(dataset.getFileFormat(), oid);
                            strVal = obj.getFullName();
                        }
                        catch (Exception ex) { strVal = null; }
                    }
                    
                    if (strVal == null)
                        strVal = val.toString();
                    
                    cellValueField.setText(strVal);
                }

                return super.isCellSelected(row, column);
            }
        };

        return theTable;
    }

    /**
     * Creates a JTable to hold a compound dataset.
     */
    private JTable createTable(CompoundDS d)
    {
        JTable theTable = null;

        int rank = d.getRank();
        if (rank <=0 ) {
            d.init();
        }

        long[] startArray = d.getStartDims();
        long[] strideArray = d.getStride();
        int[] selectedIndex = d.getSelectedIndex();
        final int rowStart = (int)startArray[selectedIndex[0]];
        final int rowStride = (int)strideArray[selectedIndex[0]];

        // use lazy convert for large number of strings
        if (d.getHeight() > 10000) {
            d.setConvertByteToString(false);
        }

        dataValue = null;
        try { dataValue = d.getData(); }
        catch (Exception ex)
        {
            toolkit.beep();
            JOptionPane.showMessageDialog(
                this,
                ex,
                getTitle(),
                JOptionPane.ERROR_MESSAGE);
            dataValue = null;
        }

        if ((dataValue == null) ||
           !(dataValue instanceof List) ) {
            return null;
        }

        final int rows = d.getHeight();
        int cols = d.getSelectedMemberCount();
        String[] columnNames = new String[cols];

        int idx = 0;
        String[] columnNamesAll = d.getMemberNames();
        for (int i=0; i<columnNamesAll.length; i++)
        {
            if (d.isMemberSelected(i)) {
                columnNames[idx] = columnNamesAll[i];
                columnNames[idx] = columnNames[idx].replaceAll(CompoundDS.separator, "->");
                idx++;
            }
        }

        String[] subColumnNames = columnNames;
        int columns = d.getWidth();
        if (columns > 1) {
            // multi-dimension compound dataset
            subColumnNames = new String[columns*columnNames.length];
            int halfIdx = columnNames.length/2;
            for (int i=0; i<columns; i++) {
                for (int j=0; j<columnNames.length; j++) {
                    // display column index only once, in the middle of the compound fields
                    if (j == halfIdx) {
                        subColumnNames[i*columnNames.length+j] = (i+1)+"\n "+columnNames[j];
                    } else {
                        subColumnNames[i*columnNames.length+j] = " \n "+columnNames[j];
                    }
                }
            }
        }

        final String[] allColumnNames = subColumnNames;
        AbstractTableModel tm =  new AbstractTableModel()
        {
            private static final long serialVersionUID = HObject.serialVersionUID;

            List list = (List)dataValue;
            CompoundDS compound = (CompoundDS)dataset;
            int orders[] = compound.getSelectedMemberOrders();
            Datatype types[] = compound.getSelectedMemberTypes();
            StringBuffer stringBuffer = new StringBuffer();
            int nFields = list.size();
            int nRows = getRowCount();
            int nSubColumns = (nFields>0) ? getColumnCount()/nFields : 0;
            
            public int getColumnCount() {
            	return allColumnNames.length;
            }

            public int getRowCount() {
            	return rows;
            }

            public String getColumnName(int col) {
            	return allColumnNames[col];
            }
            
            public Object getValueAt(int row, int col)
            {
            	if (startEditing[0])
            		return "";

                int fieldIdx = col;
                int rowIdx = row;

                if (nSubColumns > 1) // multi-dimension compound dataset
                {
                    int colIdx = col/nFields;
                    fieldIdx = col - colIdx*nFields;
                    //BUG 573: rowIdx = row*orders[fieldIdx] + colIdx*nRows*orders[fieldIdx];
                    rowIdx = row*orders[fieldIdx]*nSubColumns+colIdx*orders[fieldIdx];;
                }
                else {
                    rowIdx = row*orders[fieldIdx];
                }

                Object colValue = list.get(fieldIdx);
                if (colValue == null) {
                    return "Null";
                }

                stringBuffer.setLength(0); // clear the old string
                int[] mdim = compound.getMemeberDims(fieldIdx);
                if (mdim == null)
                {
                    // member is not an ARRAY datatype
                    int strlen = types[fieldIdx].getDatatypeSize();
                    boolean isString = (types[fieldIdx].getDatatypeClass() == Datatype.CLASS_STRING);
                    

                    if ((orders[fieldIdx] <= 1) && isString && (strlen>0) && !compound.getConvertByteToString())
                    {
                        // original data is a char array
                        String str = new String(((byte[])colValue), rowIdx*strlen, strlen);
                        int idx = str.indexOf('\0');
                        if (idx > 0) {
                            str = str.substring(0, idx);
                        }
                        stringBuffer.append(str.trim());
                    } else {
                        stringBuffer.append(Array.get(colValue, rowIdx));
                    }

                    for (int i=1; i<orders[fieldIdx]; i++) {
                        stringBuffer.append(", ");
                        stringBuffer.append(Array.get(colValue, rowIdx+i));
                    }
                } else
                {
                    // member is an ARRAY datatype
                    for (int i=0; i<orders[fieldIdx]; i++) {
                        stringBuffer.append(Array.get(colValue, rowIdx+i));
                        stringBuffer.append(", ");
                    }
                }

                return stringBuffer;
            }
        };

        theTable = new JTable(tm)
        {
            private static final long serialVersionUID = HObject.serialVersionUID;

            int lastSelectedRow = -1;
            int lastSelectedColumn = -1;

            public boolean isCellEditable(int row, int column)
            {
                return !isReadOnly;
            }

            public boolean editCellAt(int row, int column, java.util.EventObject e) 
            {
            	
                if (!isCellEditable(row, column)) {
                    return super.editCellAt(row, column, e);
                }
                
				if (e instanceof KeyEvent) {
					KeyEvent ke = (KeyEvent)e;
					if (ke.getID()==KeyEvent.KEY_PRESSED) 
						startEditing[0] = true;
				}
				
                return super.editCellAt(row, column, e);
            }

            public void editingStopped(ChangeEvent e)
            {
                int row = getEditingRow();
                int col = getEditingColumn();
                super.editingStopped(e);
                startEditing[0] = false;

                Object source = e.getSource();

                if (source instanceof CellEditor)
                {
                    CellEditor editor = (CellEditor)source;
                    String cellValue = (String)editor.getCellEditorValue();

                    try { updateValueInMemory(cellValue, row, col); }
                    catch (Exception ex)
                    {
                        toolkit.beep();
                        JOptionPane.showMessageDialog(
                        this,
                        ex,
                        getTitle(),
                        JOptionPane.ERROR_MESSAGE);
                    }
                } // if (source instanceof CellEditor)
            }

            public boolean isCellSelected(int row, int column)
            {
                if ((lastSelectedRow == row) &&
                    (lastSelectedColumn == column)) {
                    return super.isCellSelected(row, column);
                }

                lastSelectedRow = row;
                lastSelectedColumn = column;
                if ((getSelectedRow()==row) && (getSelectedColumn()==column))
                {
                    cellLabel.setText(
                        String.valueOf(rowStart+row*rowStride)+
                        ", "+ table.getColumnName(column)+
                        "  =  ");
                    cellValueField.setText(getValueAt(row, column).toString());
                }

                return super.isCellSelected(row, column);
            }
        };

        if (columns > 1) {
            // multi-dimension compound dataset
            MultiLineHeaderRenderer renderer = new MultiLineHeaderRenderer(
                    columns, columnNames.length);
            Enumeration local_enum = theTable.getColumnModel().getColumns();
            while (local_enum.hasMoreElements())
            {
                ((TableColumn)local_enum.nextElement()).setHeaderRenderer(renderer);
            }
        }

        return theTable;
    } /* createTable */
    
    private void gotoPage(long idx)
    {
        if (dataset.getRank() < 3) {
            return;
        }

        if (isValueChanged) {
            updateValueInFile();
        }

        long[] start = dataset.getStartDims();
        int[] selectedIndex = dataset.getSelectedIndex();
        long[] dims = dataset.getDims();

        if ((idx <0) || (idx >= dims[selectedIndex[2]])) {
            toolkit.beep();
            JOptionPane.showMessageDialog(this,
                "Frame number must be between 0 and "+(dims[selectedIndex[2]]-1),
                getTitle(),
                JOptionPane.ERROR_MESSAGE);
            return;
        }

        start[selectedIndex[2]] = idx;
        curFrame = idx;
        dataset.clearData();

        setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));

        try {
            dataValue = dataset.getData();
            if (dataset instanceof ScalarDS)
            {
                ((ScalarDS)dataset).convertFromUnsignedC();
                dataValue = dataset.getData();
            }
        }
        catch (Exception ex)
        {
        	setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
            dataValue = null;
            JOptionPane.showMessageDialog(this, ex, getTitle(), JOptionPane.ERROR_MESSAGE);
            return;
        }

    	setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
        
        frameField.setText(String.valueOf(curFrame));
        updateUI();
    }

    /** copy data from the spreadsheet to the system clipboard. */
    private void copyData()
    {
        StringBuffer sb = new StringBuffer();

        int r0 = table.getSelectedRow();     // starting row
        int c0 = table.getSelectedColumn();  // starting column

        if ((r0<0) || (c0 <0)) {
            return;
        }

        int r1 = r0 + table.getSelectedRowCount();     // finish row
        int c1 = c0 + table.getSelectedColumnCount();  // finshing column

        for (int i=r0; i<r1; i++)
        {
            sb.append(table.getValueAt(i, c0).toString());
            for (int j=c0+1; j<c1; j++)
            {
                sb.append("\t");
                sb.append(table.getValueAt(i, j).toString());
            }
            sb.append("\n");
        }

        Clipboard cb =  Toolkit.getDefaultToolkit().getSystemClipboard();
        StringSelection contents = new StringSelection(sb.toString());
        cb.setContents(contents, null);
    }

    /** paste data from the system clipboard to the spreadsheet. */
    private void pasteData()
    {
        int pasteDataFlag = JOptionPane.showConfirmDialog(this,
            "Do you want to paste selected data ?",
            this.getTitle(),
            JOptionPane.YES_NO_OPTION);
        if (pasteDataFlag == JOptionPane.NO_OPTION) {
            return;
        }

        int cols = table.getColumnCount();
        int rows = table.getRowCount();
        int r0 = table.getSelectedRow();
        int c0 = table.getSelectedColumn();

        if (c0 < 0) {
            c0 = 0;
        }
        if (r0 < 0) {
            r0 = 0;
        }
        int r = r0;
        int c = c0;

        Clipboard cb =  Toolkit.getDefaultToolkit().getSystemClipboard();
        Transferable content = cb.getContents(this);
        String line = "";
        try {
            String s =(String)content.getTransferData(DataFlavor.stringFlavor);

            StringTokenizer st = new StringTokenizer(s, "\n");
            // read line by line
            while (st.hasMoreTokens() && (r < rows))
            {
                line = st.nextToken();

                if (fixedDataLength < 1)
                {
                    // separate by delimiter
                    StringTokenizer lt = new StringTokenizer(line, "\t");
                    while (lt.hasMoreTokens() && (c < cols))
                    {
                        try {
                            updateValueInMemory(lt.nextToken(), r, c);
                        } catch (Exception ex) { continue; }
                        c++;
                    }
                    r = r+1;
                    c = c0;
                } else
                {
                    // the data has fixed length
                    int n = line.length();
                    String theVal;
                    for (int i = 0; i<n; i=i+fixedDataLength)
                    {
                        try {
                            theVal = line.substring(i, i+fixedDataLength);
                            updateValueInMemory(theVal, r, c);
                        } catch (Exception ex) { continue; }
                        c ++;
                    }
                }
            }
        }	catch (Throwable ex) {
            toolkit.beep();
            JOptionPane.showMessageDialog(
                this,
                ex,
                getTitle(),
                JOptionPane.ERROR_MESSAGE);
        }

        table.updateUI();
    }

    /**
     *  import data values from text file.
     */
    private void importTextData(String fname)
    {
        int pasteDataFlag = JOptionPane.showConfirmDialog(this,
            "Do you want to paste selected data ?",
            this.getTitle(),
            JOptionPane.YES_NO_OPTION);
        if (pasteDataFlag == JOptionPane.NO_OPTION) {
            return;
        }

        int cols = table.getColumnCount();
        int rows = table.getRowCount();
        int r0 = table.getSelectedRow();
        int c0 = table.getSelectedColumn();

        if (c0 < 0) {
            c0 = 0;
        }
        if (r0 < 0) {
            r0 = 0;
        }
        
        // start at the first column for compound datasets
        if (dataset instanceof CompoundDS) 
            c0 = 0;

        BufferedReader in = null;
        try { in = new BufferedReader(new FileReader(fname));
        } catch (FileNotFoundException ex) { return; }

        String line = null;
        StringTokenizer tokenizer1=null;

        try { line = in.readLine(); }
        catch (IOException ex)
        {
            try { in.close(); } catch (IOException ex2) {}
            return;
        }

        String delName = ViewProperties.getDataDelimiter();
        String delimiter = "\t";
        
        // delimiter must include a tab to be consistent with copy/paste (
        if (delName.equalsIgnoreCase(ViewProperties.DELIMITER_SPACE)) {
            delimiter = " "+delimiter;
        } else if (delName.equalsIgnoreCase(ViewProperties.DELIMITER_COMMA)) {
            delimiter = ","+delimiter;
        } else if (delName.equalsIgnoreCase(ViewProperties.DELIMITER_COLON)) {
            delimiter = ":"+delimiter;
        } else if (delName.equalsIgnoreCase(ViewProperties.DELIMITER_SEMI_COLON)) {
            delimiter = ";"+delimiter;
        }

        String token=null;
        int r = r0, c = c0;
        while ((line != null) && (r < rows)) {
            if (fixedDataLength > 0)
            {
                // the data has fixed length
                int n = line.length();
                String theVal;
                for (int i = 0; i<n; i=i+fixedDataLength)
                {
                    try {
                        theVal = line.substring(i, i+fixedDataLength);
                        updateValueInMemory(theVal, r, c);
                    } catch (Exception ex) { continue; }
                    c ++;
                }
            } else {
                try {
                    tokenizer1 = new StringTokenizer(line, delimiter);
                    while (tokenizer1.hasMoreTokens() && (c < cols)) {
                        token = tokenizer1.nextToken();
                        if (dataset instanceof ScalarDS) {
                            StringTokenizer tokenizer2 = new StringTokenizer(token);
                            while (tokenizer2.hasMoreTokens() && (c < cols)) {
                                updateValueInMemory(tokenizer2.nextToken(), r, c);
                                c++;
                            }
                        }
                        else {
                            updateValueInMemory(token, r, c);
                            c++;    
                        }
                    } // while (tokenizer1.hasMoreTokens() && index < size)
                }
                catch (Exception ex)
                {
                    JOptionPane.showMessageDialog(this, ex, getTitle(), JOptionPane.ERROR_MESSAGE);
                    try { in.close(); } catch (IOException ex2) {}
                    return;
                }
            }


            try { line = in.readLine(); }
            catch (IOException ex) { line = null; }
            c = 0;
            r++;
        } // while ((line != null) && (r < rows))

        try { in.close(); } catch (IOException ex) {}

        table.updateUI();
    }

    /** Save data as text. */
    private void saveAsText() throws Exception
    {
        final JFileChooser fchooser = new JFileChooser(dataset.getFile());
        fchooser.setFileFilter(DefaultFileFilter.getFileFilterText());
        //fchooser.changeToParentDirectory();
        fchooser.setDialogTitle("Save Current Data To Text File --- "+dataset.getName());

        File choosedFile = new File(dataset.getName()+".txt");;
        fchooser.setSelectedFile(choosedFile);
        int returnVal = fchooser.showSaveDialog(this);

        if(returnVal != JFileChooser.APPROVE_OPTION) {
            return;
        }

        choosedFile = fchooser.getSelectedFile();
        if (choosedFile == null) {
            return;
        }
        String fname = choosedFile.getAbsolutePath();

        // check if the file is in use
        List fileList = viewer.getTreeView().getCurrentFiles();
        if (fileList != null)
        {
            FileFormat theFile = null;
            Iterator iterator = fileList.iterator();
            while(iterator.hasNext())
            {
                theFile = (FileFormat)iterator.next();
                if (theFile.getFilePath().equals(fname))
                {
                    toolkit.beep();
                    JOptionPane.showMessageDialog(this,
                        "Unable to save data to file \""+fname+"\". \nThe file is being used.",
                        getTitle(),
                        JOptionPane.ERROR_MESSAGE);
                    return;
                }
            }
        }

        if (choosedFile.exists())
        {
            int newFileFlag = JOptionPane.showConfirmDialog(this,
                "File exists. Do you want to replace it ?",
                this.getTitle(),
                JOptionPane.YES_NO_OPTION);
            if (newFileFlag == JOptionPane.NO_OPTION) {
                return;
            }
        }

        PrintWriter out = new PrintWriter(
            new BufferedWriter(new FileWriter(choosedFile)));

        String delName = ViewProperties.getDataDelimiter();
        String delimiter = "\t";
        
        // delimiter must include a tab to be consistent with copy/paste (
        if (delName.equalsIgnoreCase(ViewProperties.DELIMITER_SPACE)) {
            delimiter = " "+delimiter;
        } else if (delName.equalsIgnoreCase(ViewProperties.DELIMITER_COMMA)) {
            delimiter = ","+delimiter;
        } else if (delName.equalsIgnoreCase(ViewProperties.DELIMITER_COLON)) {
            delimiter = ":"+delimiter;
        } else if (delName.equalsIgnoreCase(ViewProperties.DELIMITER_SEMI_COLON)) {
            delimiter = ";"+delimiter;
        }

        int cols = table.getColumnCount();
        int rows = table.getRowCount();

        for (int i=0; i<rows; i++)
        {
            out.print(table.getValueAt(i, 0));
            for (int j=1; j<cols; j++)
            {
                out.print(delimiter);
                out.print(table.getValueAt(i, j));
            }
            out.println();
        }

        out.flush();
        out.close();

        viewer.showStatus("Data save to: "+fname);

//        try {
//            RandomAccessFile rf = new RandomAccessFile(choosedFile, "r");
//            long size = rf.length();
//            rf.close();
//            viewer.showStatus("File size (bytes): "+size);
//        } catch (Exception ex) {}
    }

    /** update dataset value in file.
     *  The change will go to file.
     */
    public void updateValueInFile()
    {
        if (isReadOnly || showAsBin || showAsHex) {
            return;
        }

        //if (!(dataset instanceof ScalarDS) || !isValueChanged)
        if (!isValueChanged) {
            return;
        }

        try { dataset.write(); }
        catch (Exception ex)
        {
            toolkit.beep();
            JOptionPane.showMessageDialog(
                this,
                ex,
                getTitle(),
                JOptionPane.ERROR_MESSAGE);
            return;
        }

        isValueChanged = false;
    }

    /**
     * Selects all rows, columns, and cells in the table.
     */
    private void selectAll() throws Exception
    {
        table.selectAll();
    }

    /**
     * Converting selected data based on predefined math functions.
     */
    private void mathConversion() throws Exception
    {
        if (isReadOnly) {
            return;
        }

        int cols = table.getSelectedColumnCount();
        //if (!(dataset instanceof ScalarDS))  return;
        if ((dataset instanceof CompoundDS) && (cols>1))
        {
            toolkit.beep();
            JOptionPane.showMessageDialog(this,
            "Please select one colunm a time for math conversion for compound dataset.",
            getTitle(),
            JOptionPane.ERROR_MESSAGE);
            return;
        }

        Object theData = getSelectedData();
        if (theData == null)
        {
            toolkit.beep();
            JOptionPane.showMessageDialog(this,
            "No data is selected.",
            getTitle(),
            JOptionPane.ERROR_MESSAGE);
            return;
        }

        MathConversionDialog dialog = new MathConversionDialog((JFrame)viewer, theData);
        dialog.setVisible(true);

        if (dialog.isConverted())
        {
            if (dataset instanceof CompoundDS)
            {
                Object colData = null;
                try {
                    colData = ((List)dataset.getData()).get(table.getSelectedColumn());
                } catch (Exception ex) {;}

                if (colData != null)
                {
                    int size = Array.getLength(theData);
                    System.arraycopy(theData, 0, colData, 0, size);
                }
            }
            else
            {
                int rows = table.getSelectedRowCount();
                int r0 = table.getSelectedRow();
                int c0 = table.getSelectedColumn();
                int w = table.getColumnCount();
                int idx_src=0, idx_dst=0;
                for (int i=0; i<rows; i++)
                {
                    idx_dst = (r0+i)*w+c0;
                    System.arraycopy(theData, idx_src, dataValue, idx_dst, cols);
                    idx_src += cols;
                }
            }

            theData = null;
            System.gc();
            table.updateUI();
            isValueChanged = true;
        }

    }

    /** update cell value in memory.
     *  It does not change the dataset value in file.
     *  @param cellValue the string value of input.
     *  @param row the row of the editing cell.
     *  @param col the column of the editing cell.
     */
    private void updateValueInMemory(String cellValue, int row, int col)
    throws Exception
    {
        if (dataset instanceof ScalarDS) {
            updateScalarData(cellValue, row, col);
        } else if (dataset instanceof CompoundDS) {
            updateCompoundData(cellValue, row, col);
        }
    }

    /** update cell value in memory.
     *  It does not change the dataset value in file.
     *  @param cellValue the string value of input.
     *  @param row the row of the editing cell.
     *  @param col the column of the editing cell.
     */
    private void updateScalarData(String cellValue, int row, int col)
    throws Exception
    {
        if (!(dataset instanceof ScalarDS) ||
            (cellValue == null) ||
            ((cellValue=cellValue.trim()) == null) ||
            showAsBin || showAsHex) {
            return;
        }

        int i = 0;
        if (isDataTransposed) {
            i = col*table.getRowCount()+row;
        } else {
            i = row*table.getColumnCount()+col;
        }

        ScalarDS sds = (ScalarDS)dataset;
        boolean isUnsigned = sds.isUnsigned();

        // check data range for unsigned datatype
        if (isUnsigned)
        {
            long lvalue = -1;
            long maxValue = Long.MAX_VALUE;
            
            lvalue = Long.parseLong(cellValue);

            if (lvalue < 0)
            {
                throw new NumberFormatException("Negative value for unsigned integer: "+lvalue);
            }

            if (NT=='S') {
                maxValue = 255;
            } else if (NT=='I') {
                maxValue = 65535;
            } else if (NT=='J') {
                maxValue = 4294967295L;
            }

            if ((lvalue < 0) || (lvalue > maxValue))
            {
                throw new NumberFormatException("Data value is out of range: "+lvalue);
            }
        }

        switch (NT) {
        case 'B':
            byte bvalue = 0;
            bvalue = Byte.parseByte(cellValue);
            Array.setByte(dataValue, i, bvalue);
            break;
        case 'S':
            short svalue = 0;
            svalue = Short.parseShort(cellValue);
            Array.setShort(dataValue, i, svalue);
            break;
        case 'I':
            int ivalue = 0;
            ivalue = Integer.parseInt(cellValue);
            Array.setInt(dataValue, i, ivalue);
            break;
        case 'J':
            long lvalue = 0;
            lvalue = Long.parseLong(cellValue);
            Array.setLong(dataValue, i, lvalue);
            break;
        case 'F':
            float fvalue = 0;
            fvalue = Float.parseFloat(cellValue);
            Array.setFloat(dataValue, i, fvalue);
            break;
        case 'D':
            double dvalue = 0;
            dvalue = Double.parseDouble(cellValue);
            Array.setDouble(dataValue, i, dvalue);
            break;
        }
        
        isValueChanged = true;
    }

    private void updateCompoundData(String cellValue, int row, int col)
    throws Exception
    {
        if (!(dataset instanceof CompoundDS) ||
            (cellValue == null) ||
            ((cellValue=cellValue.trim()) == null)) {
            return;
        }

        CompoundDS compDS = (CompoundDS)dataset;
        List cdata = (List)compDS.getData();
        int orders[] = compDS.getSelectedMemberOrders();
        Datatype types[] = compDS.getSelectedMemberTypes();
        int nFields = cdata.size();
        int nSubColumns = table.getColumnCount()/nFields;
        int nRows = table.getRowCount();
        int column = col;
        int offset = 0;
        int morder = 1;
        
        if (nSubColumns > 1) // multi-dimension compound dataset
        {
            int colIdx = col/nFields;
            column = col - colIdx*nFields;
            ////BUG 573: offset = row*orders[column] + colIdx*nRows*orders[column];
            offset = row*orders[column]*nSubColumns+colIdx*orders[column];
        }
        else {
            offset = row*orders[column];
        }
        morder = orders[column];

        Object mdata = cdata.get(column);

        // strings
        if (Array.get(mdata, 0) instanceof String)
        {
            Array.set(mdata, offset, cellValue);
            isValueChanged = true;
            return;
        } else if (types[column].getDatatypeClass() == Datatype.CLASS_STRING)
        {
            // it is string but not converted, still byte array
            int strlen = types[column].getDatatypeSize();
            offset *= strlen;
            byte[] bytes = cellValue.getBytes();
            byte[] bData = (byte[])mdata;
            int n = Math.min(strlen, bytes.length);
            System.arraycopy(bytes, 0, bData, offset, n);
            offset += n;
            n = strlen-bytes.length;
            // space padding
            for (int i=0; i<n; i++) {
                bData[offset+i] = ' ';
            }
            isValueChanged = true;
            return;
        }

        // numveric data
        char mNT = ' ';
        String cName = mdata.getClass().getName();
        int cIndex = cName.lastIndexOf("[");
        if (cIndex >= 0 ) {
            mNT = cName.charAt(cIndex+1);
        }

        StringTokenizer st = new StringTokenizer(cellValue, ",");
        if (st.countTokens() < morder)
        {
            toolkit.beep();
            JOptionPane.showMessageDialog(this,
            "Number of data point < "+morder+".",
            getTitle(),
            JOptionPane.ERROR_MESSAGE);
            return;
        }

        String token = "";
        isValueChanged = true;
        switch (mNT)
        {
            case 'B':
                byte bvalue = 0;
                for (int i=0; i<morder; i++)
                {
                    token = st.nextToken().trim();
                    bvalue = Byte.parseByte(token);
                    Array.setByte(mdata, offset+i, bvalue);
                }
                break;
            case 'S':
                short svalue = 0;
                for (int i=0; i<morder; i++)
                {
                    token = st.nextToken().trim();
                    svalue = Short.parseShort(token);
                    Array.setShort(mdata, offset+i, svalue);
                }
                break;
            case 'I':
                int ivalue = 0;
                for (int i=0; i<morder; i++)
                {
                    token = st.nextToken().trim();
                    ivalue = Integer.parseInt(token);
                    Array.setInt(mdata, offset+i, ivalue);
                }
                break;
            case 'J':
                long lvalue = 0;
                for (int i=0; i<morder; i++)
                {
                    token = st.nextToken().trim();
                    lvalue = Long.parseLong(token);
                    Array.setLong(mdata, offset+i, lvalue);
                }
                break;
            case 'F':
                float fvalue = 0;
                for (int i=0; i<morder; i++)
                {
                    token = st.nextToken().trim();
                    fvalue = Float.parseFloat(token);
                    Array.setFloat(mdata, offset+i, fvalue);
                }
                break;
            case 'D':
                double dvalue = 0;
                for (int i=0; i<morder; i++)
                {
                    token = st.nextToken().trim();
                    dvalue = Double.parseDouble(token);
                    Array.setDouble(mdata, offset+i, dvalue);
                }
                break;
            default:
                isValueChanged = false;
        }
    }

    private class LineplotOption extends JDialog implements ActionListener, ItemListener
    {
    	private static final long serialVersionUID = HObject.serialVersionUID;

        public static final int NO_PLOT = -1;
        public static final int ROW_PLOT = 0;
        public static final int COLUMN_PLOT = 1;

        private int idx_xaxis=-1, nRows=0, nCols=0, plotType=-1;
        private JRadioButton rowButton, colButton;
        private JComboBox rowBox, colBox;

        public LineplotOption(JFrame owner, String title, int nrow, int ncol)
        {
            super(owner, title, true);

            nRows = nrow;
            nCols = ncol;

            rowBox = new JComboBox();
            rowBox.setEditable(false);
            colBox = new JComboBox();
            colBox.setEditable(false);

            JPanel contentPane = (JPanel)this.getContentPane();
            contentPane.setPreferredSize(new Dimension(400, 150));
            contentPane.setLayout(new BorderLayout(10, 10));
            
            long[] startArray = dataset.getStartDims();
            long[] strideArray = dataset.getStride();
            int[] selectedIndex = dataset.getSelectedIndex();
            int start = (int)startArray[selectedIndex[0]];
            int stride = (int)strideArray[selectedIndex[0]];
            
            rowBox.addItem("array index");
            for ( int i = 0; i < nrow;  i++ ) {
                rowBox.addItem("row "+(start+i*stride));
            }

            colBox.addItem("array index");
            for (int i=0; i<ncol; i++) {
                colBox.addItem ("column "+table.getColumnName(i));
            }

            rowButton = new JRadioButton("Row");
            colButton = new JRadioButton("Column", true);
            rowButton.addItemListener(this);
            colButton.addItemListener(this);
            ButtonGroup rgroup = new ButtonGroup();
            rgroup.add(rowButton);
            rgroup.add(colButton);

            JPanel p1 = new JPanel();
            p1.setLayout(new GridLayout(2,1,5,5));
            p1.add(new JLabel(" Series in:", SwingConstants.RIGHT));
            p1.add(new JLabel(" For abscissa use:", SwingConstants.RIGHT));


            JPanel p2 = new JPanel();
            p2.setLayout(new GridLayout(2,1,5,5));
            //p2.setBorder(new LineBorder(Color.lightGray));
            p2.add(colButton);
            p2.add(colBox);
            
            JPanel p3 = new JPanel();
            p3.setLayout(new GridLayout(2,1,5,5));
            //p3.setBorder(new LineBorder(Color.lightGray));
            p3.add(rowButton);
            p3.add(rowBox);
            
            JPanel p = new JPanel();
            p.setBorder(new LineBorder(Color.lightGray));
            p.setLayout(new GridLayout(1,3, 20, 5));
            p.add(p1);
            p.add(p2);
            p.add(p3);
            
            JPanel bp = new JPanel();

            JButton okButton = new JButton("Ok");
            okButton.addActionListener(this);
            okButton.setActionCommand("Ok");
            bp.add(okButton);

            JButton cancelButton = new JButton("Cancel");
            cancelButton.addActionListener(this);
            cancelButton.setActionCommand("Cancel");
            bp.add(cancelButton);
           
            contentPane.add(new JLabel(" Select plot options:"), BorderLayout.NORTH);
            contentPane.add(p, BorderLayout.CENTER);
            contentPane.add(bp, BorderLayout.SOUTH);
            
            colBox.setEnabled(colButton.isSelected());
            rowBox.setEnabled(rowButton.isSelected());

            Point l = getParent().getLocation();
            l.x += 450;
            l.y += 200;
            setLocation(l);
            pack();
        }

        int getXindex() { return idx_xaxis; }
        int getPlotBy() { return plotType; }

        public void actionPerformed(ActionEvent e)
        {
            Object source = e.getSource();
            String cmd = e.getActionCommand();

            if (cmd.equals("Cancel")) {
                plotType = NO_PLOT;
                this.dispose();  // terminate the application
            }
            else if (cmd.equals("Ok")) {
                if (colButton.isSelected()) {
                    idx_xaxis = colBox.getSelectedIndex()-1;
                    plotType = COLUMN_PLOT;
                }
                else {
                    idx_xaxis = rowBox.getSelectedIndex()-1;
                    plotType = ROW_PLOT;
                }

                this.dispose();  // terminate the application
            }
        }
        
        public void itemStateChanged(ItemEvent e)
        {
            Object source = e.getSource();

            if (source.equals(colButton) || source.equals(rowButton)) {
                colBox.setEnabled(colButton.isSelected());
                rowBox.setEnabled(rowButton.isSelected());
            }
        }
    }

    private class ColumnHeader extends JTableHeader
    {
    	private static final long serialVersionUID = HObject.serialVersionUID;

        private int currentColumnIndex = -1;
        private int lastColumnIndex = -1;
        private JTable parentTable;

        public ColumnHeader(JTable theTable)
        {
            super(theTable.getColumnModel());

            parentTable = theTable;
            setReorderingAllowed(false);
        }

        protected void processMouseMotionEvent(MouseEvent e)
        {
            super.processMouseMotionEvent(e);

            if (e.getID() == MouseEvent.MOUSE_DRAGGED)
            {
                // do not do anything, just resize the column
                if (getResizingColumn() != null)
                    return;
                
                int colEnd = columnAtPoint(e.getPoint());

                if (colEnd < 0) {
                    colEnd = 0;
                }
                if (currentColumnIndex < 0 ) {
                    currentColumnIndex = 0;
                }

                parentTable.clearSelection();

                if (colEnd > currentColumnIndex) {
                    parentTable.setColumnSelectionInterval(currentColumnIndex, colEnd);
                } else {
                    parentTable.setColumnSelectionInterval(colEnd, currentColumnIndex);
                }

                parentTable.setRowSelectionInterval(0, parentTable.getRowCount()-1);
            }
        }

        protected void processMouseEvent(MouseEvent e)
        {
            super.processMouseEvent(e);

            int mouseID = e.getID();

            if (mouseID == MouseEvent.MOUSE_CLICKED)
            {
                if (currentColumnIndex < 0 ) {
                    return;
                }

                if(e.isControlDown())
                {
                    // select discontinuous columns
                    parentTable.addColumnSelectionInterval(currentColumnIndex, currentColumnIndex);
                }
                else if (e.isShiftDown())
                {
                    // select continuous columns
                    if (lastColumnIndex < 0) {
                        parentTable.addColumnSelectionInterval(0, currentColumnIndex);
                    } else if (lastColumnIndex < currentColumnIndex) {
                        parentTable.addColumnSelectionInterval(lastColumnIndex, currentColumnIndex);
                    } else {
                        parentTable.addColumnSelectionInterval(currentColumnIndex, lastColumnIndex);
                    }
                }
                else
                {
                    // clear old selection and set new column selection
                    parentTable.clearSelection();
                    parentTable.setColumnSelectionInterval(currentColumnIndex, currentColumnIndex);
                }

                lastColumnIndex = currentColumnIndex;
                parentTable.setRowSelectionInterval(0, parentTable.getRowCount()-1);
            }
            else if (mouseID == MouseEvent.MOUSE_PRESSED)
            {
                currentColumnIndex = columnAtPoint(e.getPoint());
            }
        }
    } // private class ColumnHeader

    /** RowHeader defines the row header component of the Spreadsheet. */
    private class RowHeader extends JTable
    {
    	private static final long serialVersionUID = HObject.serialVersionUID;

        private int currentRowIndex = -1;
        private int lastRowIndex = -1;
        private JTable parentTable;

        public RowHeader(JTable pTable, Dataset dset)
        {
            // Create a JTable with the same number of rows as
            // the parent table and one column.
            //super( pTable.getRowCount(), 1 );

            final long[] startArray = dset.getStartDims();
            final long[] strideArray = dset.getStride();
            final int[] selectedIndex = dset.getSelectedIndex();
            final int start = (int)startArray[selectedIndex[0]];
            final int stride = (int)strideArray[selectedIndex[0]];
            final int rowCount = pTable.getRowCount();
            parentTable = pTable;
            
            AbstractTableModel tm =  new AbstractTableModel()
            {
                public int getColumnCount() {
                	return 1;
                }

                public int getRowCount() {
                	return rowCount;
                }

                public String getColumnName(int col) {
                	return " ";
                }
				public Object getValueAt(int row, int column)
                {
                	return String.valueOf(start+row*stride);
                }
            };
           
            this.setModel(tm);

            // Get the only table column.
            TableColumn col = getColumnModel().getColumn( 0 );

            // Use the cell renderer in the column.
            col.setCellRenderer( new RowHeaderRenderer() );
        }

        /** Overridden to return false since the headers are not editable. */
        public boolean isCellEditable( int row, int col )
        {
            return false;
        }

        /** This is called when the selection changes in the row headers. */
        public void valueChanged( ListSelectionEvent e )
        {
            if (parentTable == null) {
                return;
            }

            int rows[] = getSelectedRows();
            if ((rows== null) || (rows.length == 0)) {
                return;
            }

            parentTable.clearSelection();
            parentTable.setRowSelectionInterval(rows[0], rows[rows.length-1]);
            parentTable.setColumnSelectionInterval(0, parentTable.getColumnCount()-1);
        }

        protected void processMouseMotionEvent(MouseEvent e)
        {
            if (e.getID() == MouseEvent.MOUSE_DRAGGED)
            {
                int colEnd = rowAtPoint(e.getPoint());

                if (colEnd < 0) {
                    colEnd = 0;
                }
                if (currentRowIndex < 0 ) {
                    currentRowIndex = 0;
                }

                parentTable.clearSelection();

                if (colEnd > currentRowIndex) {
                    parentTable.setRowSelectionInterval(currentRowIndex, colEnd);
                } else {
                    parentTable.setRowSelectionInterval(colEnd, currentRowIndex);
                }

                parentTable.setColumnSelectionInterval(0, parentTable.getColumnCount()-1);
            }
        }

        protected void processMouseEvent(MouseEvent e)
        {
            int mouseID = e.getID();

            if (mouseID == MouseEvent.MOUSE_CLICKED)
            {
                if (currentRowIndex < 0 ) {
                    return;
                }

                if(e.isControlDown())
                {
                    // select discontinguous rows
                    parentTable.addRowSelectionInterval(currentRowIndex, currentRowIndex);
                }
                else if (e.isShiftDown())
                {
                    // select continguous columns
                    if (lastRowIndex < 0) {
                        parentTable.addRowSelectionInterval(0, currentRowIndex);
                    } else if (lastRowIndex < currentRowIndex) {
                        parentTable.addRowSelectionInterval(lastRowIndex, currentRowIndex);
                    } else {
                        parentTable.addRowSelectionInterval(currentRowIndex, lastRowIndex);
                    }
                }
                else
                {
                    // clear old selection and set new column selection
                    parentTable.clearSelection();
                    parentTable.setRowSelectionInterval(currentRowIndex, currentRowIndex);
                }

                lastRowIndex = currentRowIndex;

                parentTable.setColumnSelectionInterval(0, parentTable.getColumnCount()-1);
            }
            else if (mouseID == MouseEvent.MOUSE_PRESSED)
            {
                currentRowIndex = rowAtPoint(e.getPoint());
            }
        }
    } // private class RowHeader extends JTable

    /** RowHeaderRenderer is a custom cell renderer that displays cells as buttons. */
    private class RowHeaderRenderer extends JLabel implements TableCellRenderer
    {
    	private static final long serialVersionUID = HObject.serialVersionUID;

        public RowHeaderRenderer()
        {
            super();
            setHorizontalAlignment(JLabel.CENTER);

            setOpaque(true);
            setBorder(UIManager.getBorder("TableHeader.cellBorder"));
            setBackground(Color.lightGray);
        }

        /** Configures the button for the current cell, and returns it. */
        public Component getTableCellRendererComponent (
            JTable table,
            Object value,
            boolean isSelected,
            boolean hasFocus,
            int row,
            int column )
        {
            setFont(table.getFont());

            if ( value != null ) {
                setText( value.toString() );
            }

            return this;
        }
    } //private class RowHeaderRenderer extends JLabel implements TableCellRenderer

    private class MultiLineHeaderRenderer extends JList implements TableCellRenderer
    {
    	private static final long serialVersionUID = HObject.serialVersionUID;

        private final CompoundBorder subBorder = new CompoundBorder(
                new MatteBorder(1, 0, 1, 0, java.awt.Color.darkGray),
                new MatteBorder(1, 0, 1, 0, java.awt.Color.white));
        private final CompoundBorder majorBorder = new CompoundBorder(
                new MatteBorder(1, 1, 1, 0, java.awt.Color.darkGray),
                new MatteBorder(1, 2, 1, 0, java.awt.Color.white));
        Vector lines = new Vector();
        int nMajorcolumns = 1;
        int nSubcolumns = 1;

        public MultiLineHeaderRenderer(int majorColumns, int subColumns)
        {
            nMajorcolumns = majorColumns;
            nSubcolumns = subColumns;
            setOpaque(true);
            setForeground(UIManager.getColor("TableHeader.foreground"));
            setBackground(UIManager.getColor("TableHeader.background"));
        }

        public Component getTableCellRendererComponent(JTable table, Object value,
               boolean isSelected, boolean hasFocus, int row, int column)
        {
            setFont(table.getFont());
            String str = (value == null) ? "" : value.toString();
            BufferedReader br = new BufferedReader(new StringReader(str));
            String line;

            lines.clear();
            try {
                while ((line = br.readLine()) != null) {
                    lines.addElement(line);
                }
            } catch (IOException ex) {}

            if ((column/nSubcolumns)*nSubcolumns == column)
            {
                setBorder(majorBorder);
            }
            else {
                setBorder(subBorder);
            }
            setListData(lines);

            return this;
        }
    }

    //////////////////////////////////////////////////////////////////////////
    //                                                                      //
    //        The code below was added to deal with region references       //
    //                      Peter Cao, 4/30/2009                            //
    //                                                                      //
    //////////////////////////////////////////////////////////////////////////
    
    public void mouseClicked(MouseEvent e) 
    {
        // only deal with reg. ref 
        if (! (isRegRef || isObjRef))
            return;
        
        int eMod = e.getModifiers();

        // provide two options here: double click to show data in table, or
        // right mouse to choose to show data in table or in image
        
        // right mouse click
        if (e.isPopupTrigger() || (eMod == InputEvent.BUTTON3_MASK) ||
            (System.getProperty("os.name").startsWith("Mac") &&
            (eMod == (InputEvent.BUTTON1_MASK|InputEvent.CTRL_MASK))))
        {
            if (popupMenu != null) {
                popupMenu.show((JComponent)e.getSource(), e.getX(), e.getY());
            }
        } else if (e.getClickCount() == 2) {
            // double click
            viewType = ViewType.TABLE;
            Object theData = getSelectedData();
            
            if (theData == null) {
                toolkit.beep();
                JOptionPane.showMessageDialog(
                    this,
                    "No data selected.",
                    getTitle(),
                    JOptionPane.ERROR_MESSAGE);
                return;
                
            }
   
            int len = Array.getLength(theData);
            for (int i=0; i<len; i++) {
                if (isRegRef)
                    showRegRefData((String)Array.get(theData, i));
                else if (isObjRef)
                    showObjRefData(Array.getLong(theData, i));
            }
        }

    }
    
    public void mouseEntered(MouseEvent e) {
        // TODO Auto-generated method stub
        
    }

    public void mouseExited(MouseEvent e) {
        // TODO Auto-generated method stub
        
    }

    public void mousePressed(MouseEvent e) {
        // TODO Auto-generated method stub
        
    }

    public void mouseReleased(MouseEvent e) {
        // TODO Auto-generated method stub
    }
    
    /** creates a popup menu for a right mouse click on a data object */
    private JPopupMenu createPopupMenu()
    {
        JPopupMenu menu = new JPopupMenu();
        JMenuItem item;

        item = new JMenuItem( "Show As Table");
        item.setMnemonic(KeyEvent.VK_T);
        item.addActionListener(this);
        item.setActionCommand("Show data as table");
        menu.add(item);

        item = new JMenuItem( "Show As Image");
        item.setMnemonic(KeyEvent.VK_I);
        item.addActionListener(this);
        item.setActionCommand("Show data as image");
        menu.add(item);

//        item = new JMenuItem( "Show As Text");
//        item.setMnemonic(KeyEvent.VK_I);
//        item.addActionListener(this);
//        item.setActionCommand("Show data as text");
//        menu.add(item);

        return menu;
    }
    
    /**
     * Display data pointed by object references. Data of each object is shown
     * in a separate spreadsheet. 
     * @param ref the array of strings that contain the reg. ref information.
     * 
     */
    private void showObjRefData(long ref) {
        long[] oid = {ref};
        
        HObject obj = FileFormat.findObject(dataset.getFileFormat(), oid);
        if (obj == null || !(obj instanceof ScalarDS))
            return;
        
        ScalarDS dset = (ScalarDS) obj;
        ScalarDS dset_copy = null;

        // create an instance of the dataset constructor
        Constructor constructor = null;
        Object[] paramObj = null;
        Object data = null;
        
        try {
            Class[] paramClass = {FileFormat.class, String.class, String.class};
            constructor = dset.getClass().getConstructor(paramClass);
            paramObj = new Object[] {dset.getFileFormat(), dset.getName(), dset.getPath()};
            dset_copy = (ScalarDS)constructor.newInstance(paramObj);
            data = dset_copy.getData();
        } catch (Exception ex) { data = null; }
        
        if (data == null)
            return;        
        
        // load each selection into a separate dataset and display it in 
        // a separate spreadsheet
        StringBuffer titleSB = new StringBuffer();
        Font font = this.getFont();
        CompoundBorder border = BorderFactory.createCompoundBorder(
                BorderFactory.createRaisedBevelBorder(),
                BorderFactory.createTitledBorder(
                        BorderFactory.createLineBorder(Color.BLUE, 3),
                        "Data pointed by object reference", 
                        TitledBorder.RIGHT, 
                        TitledBorder.TOP,
                        font,
                        Color.RED));  
        
        JInternalFrame dataView = null;
        HashMap map = new HashMap(1);
        map.put(ViewProperties.DATA_VIEW_KEY.OBJECT, dset_copy);
        switch (viewType) {
        case TEXT:
            dataView = new DefaultTextView(viewer, map);
            break;
        case IMAGE:
            dataView = new DefaultImageView(viewer, map);
             break;
        default:
            dataView = new DefaultTableView(viewer, map);
            break;
        }
        
        if (dataView != null) {
            viewer.addDataView((DataView)dataView);
            titleSB.append(dset_copy.getName());
            titleSB.append("  -  ");
            titleSB.append(dset_copy.getPath());
            titleSB.append("  -  ");
            titleSB.append(dataset.getFile());
            dataView.setTitle(titleSB.toString());
            
            dataView.setBorder(border);
        }
    }

    
    /**
     * Display data pointed by region references. Data of each region is shown
     * in a separate spreadsheet. The reg. ref. information is stored in strings
     * of the format below:
     * <p />
     * <ul> 
     *   <li> For point selections: "file_id:obj_id { <point1> <point2> ...) }",
     *        where <point1> is in the form of (location_of_dim0, location_of_dim1, ...). 
     *        For example, 0:800 { (0,1)  (2,11)  (1,0)  (2,4) } </li>
     *   <li> For rectangle selections: "file_id:obj_id { <corner coordinates1> <corner coordinates2> ... }",
     *        where <corner coordinates1> is in the form of (start_corner)-(oposite_corner).
     *        For example, 0:800 { (0,0)-(0,2)  (0,11)-(0,13)  (2,0)-(2,2)  (2,11)-(2,13) }</li>
     * </ul>
     *  
     * @param reg the array of strings that contain the reg. ref information.
     * 
     */
    private void showRegRefData(String reg) 
    {
        boolean isPointSelection = false;
        
        if (reg == null || reg.length() <=0)
            return;
        
        isPointSelection = (reg.indexOf('-')<=0);
        
        // find the object id
        String oidStr = reg.substring(reg.indexOf(':')+1, reg.indexOf(' '));
        long oid[] = {-1};
        
        // decode object ID
        try { oid[0] = Long.valueOf(oidStr); }
        catch (Exception ex) { return; }
         
        // decode the region selection
        String regStr = reg.substring(reg.indexOf('{')+1, reg.indexOf('}'));
        if (regStr==null || regStr.length()<=0)
            return; // no selection
        
        StringTokenizer st = new StringTokenizer(regStr);
        int nSelections = st.countTokens();
        if (nSelections <= 0)
            return; // no selection
        
        HObject obj = FileFormat.findObject(dataset.getFileFormat(), oid);
        if (obj == null || !(obj instanceof ScalarDS))
            return;
        
        ScalarDS dset = (ScalarDS) obj;
        ScalarDS dset_copy = null;

        // create an instance of the dataset constructor
        Constructor constructor = null;
        Object[] paramObj = null;
        try {
            Class[] paramClass = {FileFormat.class, String.class, String.class};
            constructor = dset.getClass().getConstructor(paramClass);
            paramObj = new Object[] {dset.getFileFormat(), dset.getName(), dset.getPath()};
        } catch (Exception ex) { constructor = null; }
        
        // load each selection into a separate dataset and display it in 
        // a separate spreadsheet
        StringBuffer titleSB = new StringBuffer();
        Font font = this.getFont();
        CompoundBorder border = BorderFactory.createCompoundBorder(
                BorderFactory.createRaisedBevelBorder(),
                BorderFactory.createTitledBorder(
                        BorderFactory.createLineBorder(Color.BLUE, 3),
                        "Data pointed by region reference", 
                        TitledBorder.RIGHT, 
                        TitledBorder.TOP,
                        font,
                        Color.RED));   

        while (st.hasMoreTokens()) {
            try { dset_copy = (ScalarDS)constructor.newInstance(paramObj); }
            catch (Exception ex) {continue; }
            
            if (dset_copy == null)
                continue;
            
            try { dset_copy.init(); } catch (Exception ex) {continue; }
            
            int rank = dset_copy.getRank();
            long start[] = dset_copy.getStartDims();
            long count[] = dset_copy.getSelectedDims();
            
            // set the selected dimension sizes based on the region selection info.
            int idx = 0;
            String sizeStr = null;
            String token = st.nextToken();
            
            titleSB.setLength(0);
            titleSB.append(token);
            titleSB.append(" at ");
            
            token = token.replace('(', ' ');
            token = token.replace(')', ' ');
            if (isPointSelection) {
                // point selection
                StringTokenizer tmp = new StringTokenizer(token, ",");
                while (tmp.hasMoreTokens()) {
                    count[idx] = 1;
                    sizeStr = tmp.nextToken().trim();
                    start[idx] = Long.valueOf(sizeStr);
                    idx++;
                }
            } else {
                // rectangle selection
                String startStr = token.substring(0, token.indexOf('-'));
                String endStr = token.substring(token.indexOf('-')+1);
                StringTokenizer tmp = new StringTokenizer(startStr, ",");
                while (tmp.hasMoreTokens()) {
                    sizeStr = tmp.nextToken().trim();
                    start[idx] = Long.valueOf(sizeStr);
                    idx++;
                }
                
                idx = 0;
                tmp = new StringTokenizer(endStr, ",");
                while (tmp.hasMoreTokens()) {
                    sizeStr = tmp.nextToken().trim();
                    count[idx] = Long.valueOf(sizeStr)-start[idx]+1;
                    idx++;
                }
            }
            
            Object data = null;
            try { data = dset_copy.getData(); }
            catch (Exception ex) {data = null; }
            
            JInternalFrame dataView = null;
            HashMap map = new HashMap(1);
            map.put(ViewProperties.DATA_VIEW_KEY.OBJECT, dset_copy);
            switch (viewType) {
            case TEXT:
                dataView = new DefaultTextView(viewer, map);
                break;
            case IMAGE:
                dataView = new DefaultImageView(viewer, map);
                 break;
            default:
                dataView = new DefaultTableView(viewer, map);
                break;
            }
            
            if (dataView != null) {
                viewer.addDataView((DataView)dataView);
                titleSB.append(dset_copy.getName());
                titleSB.append("  -  ");
                titleSB.append(dset_copy.getPath());
                titleSB.append("  -  ");
                titleSB.append(dataset.getFile());
                dataView.setTitle(titleSB.toString());
                
                dataView.setBorder(border);
            }
        } // while (st.hasMoreTokens())
    } // private void showRegRefData(String reg) 
}
