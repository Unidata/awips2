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
import javax.print.*;
import java.util.*;
import java.io.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ListSelectionEvent;

import java.awt.event.*;
import java.awt.Color;
import java.awt.Component;
import javax.swing.table.*;



/**
 * TextView displays an HDF string dataset in text.
 * 
 * @author Peter X. Cao
 * @version 2.4 9/6/2007
 */
public class DefaultTextView extends JInternalFrame
implements TextView, ActionListener, KeyListener
{
	public static final long serialVersionUID = HObject.serialVersionUID;

    /**
     * The main HDFView.
     */
    private final ViewManager viewer;

    /**
     * The Scalar Dataset.
     */
    private ScalarDS dataset;

    /**
     * The string text.
     */
    private String[] text;
    
    /** The table to display the text content */
    private JTable table;

    //Text areas to hold the text.
    //private JTextArea[] textAreas;

    private boolean isReadOnly = false;

    private boolean isTextChanged = false;
    
    private TextAreaEditor textEditor = null;
    
    private RowHeader rowHeaders = null;

    /**
     * Constructs an TextView.
     * <p>
     * @param theView the main HDFView.
     */
    public DefaultTextView(ViewManager theView)
    {
        this(theView, null);
    }
    
    /**
     * Constructs an TextView.
     * <p>
     * @param theView the main HDFView.
     * @param map the properties on how to show the data. The map is used to 
     *        allow applications to pass properties on how to display the data, 
     *        such as, transposing data, showing data as character, applying 
     *        bitmask, and etc. Predefined keys are listed at ViewProperties.DATA_VIEW_KEY.
     */
    public DefaultTextView(ViewManager theView, HashMap map)
    {
        viewer = theView;
        text = null;
        table = null;
        dataset = null;
        textEditor = new TextAreaEditor(this);
        
        HObject hobject = null;
        if (map != null) 
            hobject = (HObject)map.get(ViewProperties.DATA_VIEW_KEY.OBJECT);
        else 
            hobject = (HObject)theView.getTreeView().getCurrentObject();
        
        if (!(hobject instanceof ScalarDS)) {
            return;
        }

        dataset = (ScalarDS)hobject;

        if (!dataset.isText()) {
            viewer.showStatus("Cannot display non-text dataset in text view.");
            dataset = null;
            return;
        }

        isReadOnly = dataset.getFileFormat().isReadOnly();

        try {
            text = (String[])dataset.getData();
        } catch (Exception ex) {
            viewer.showStatus(ex.toString());
            text = null;
        }

        if (text == null) {
            viewer.showStatus("Loading text dataset failed - "+dataset.getName());
            dataset = null;
            return;
        }

        String fname = new java.io.File(dataset.getFile()).getName();
        this.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        this.setTitle("TextView  -  "+dataset.getName()+"  -  "+ dataset.getPath()+"  -  "+  fname);
        this.setFrameIcon(ViewProperties.getTextIcon());

        table = createTable();
        table.getTableHeader().setReorderingAllowed(false);
        table.getTableHeader().setBackground(Color.black);
        rowHeaders = new RowHeader( table, dataset );
        
        // add the table to a scroller
        JScrollPane scrollingTable = new JScrollPane(table);
        scrollingTable.getVerticalScrollBar().setUnitIncrement(100);
        scrollingTable.getHorizontalScrollBar().setUnitIncrement(100);

        JViewport viewp = new JViewport();
        viewp.add( rowHeaders );
        viewp.setPreferredSize( rowHeaders.getPreferredSize() );
        scrollingTable.setRowHeader( viewp );
        
        
        TableColumnModel cmodel = table.getColumnModel();
        TextAreaRenderer textAreaRenderer = new TextAreaRenderer();

        cmodel.getColumn(0).setCellRenderer(textAreaRenderer);
        cmodel.getColumn(0).setCellEditor(textEditor);


        ((JPanel)getContentPane()).add (scrollingTable);

        setJMenuBar(createMenuBar());
    }

    public void actionPerformed(ActionEvent e)
    {
        Object source = e.getSource();
        String cmd = e.getActionCommand();

        if (cmd.equals("Close")) {
            dispose();  // terminate the application
        } else if (cmd.equals("Save to text file")) {
            try { saveAsText(); }
            catch (Exception ex) {
                JOptionPane.showMessageDialog((JFrame)viewer,
                        ex,
                        getTitle(),
                        JOptionPane.ERROR_MESSAGE);
            }
        } else if (cmd.equals("Save changes")) {
            updateValueInFile();
        }
        else if (cmd.equals("Print")) {
            print();
        }
    }

    /**
     * Creates a JTable to hold a compound dataset.
     */
    private JTable createTable()
    {
        JTable theTable = null;

        int rows = text.length;
        theTable = new JTable(rows, 1)
        {
            public static final long serialVersionUID = HObject.serialVersionUID;

            public Object getValueAt(int row, int col)
            {
                return text[row];
            }
            
            public boolean isCellEditable(int row, int column)
            {
                return !isReadOnly;
            }

            public void editingStopped(ChangeEvent e)
            {
                int row = getEditingRow();
                int col = getEditingColumn();
                super.editingStopped(e);

                Object source = e.getSource();

                if (source instanceof CellEditor)
                {
                    CellEditor editor = (CellEditor)source;
                    String cellValue = (String)editor.getCellEditorValue();
                    text[row] = cellValue;
                } // if (source instanceof CellEditor)
            }

        };
        
        return theTable;
    }

    public void keyPressed(KeyEvent e){}
    public void keyReleased(KeyEvent e){}
    public void keyTyped(KeyEvent e)
    {
        isTextChanged = true;
    }

    private JMenuBar createMenuBar() {
        JMenuBar bar = new JMenuBar();
        JMenu menu = new JMenu("Text", false);
        menu.setMnemonic('T');
        bar.add(menu);

        JMenuItem item = new JMenuItem( "Save To Text File");
        //item.setMnemonic(KeyEvent.VK_T);
        item.addActionListener(this);
        item.setActionCommand("Save to text file");
        menu.add(item);

        menu.addSeparator();

        item = new JMenuItem( "Save Changes");
        item.addActionListener(this);
        item.setActionCommand("Save changes");
        menu.add(item);

        menu.addSeparator();

        menu.addSeparator();

        item = new JMenuItem( "Close");
        item.addActionListener(this);
        item.setActionCommand("Close");
        menu.add(item);

        return bar;
    }

    /** update dataset value in file.
     *  The change will go to file.
     */
    public void updateValueInFile()
    {
        if (isReadOnly) {
            return;
        }
        
        if (!(dataset instanceof ScalarDS)) {
            return;
        }

        if (!isTextChanged) {
            return;
        }
        
        int row = table.getEditingRow();
        if (row >= 0 ) {
            // make sure to update the current row
            String cellValue = (String)textEditor.getCellEditorValue();
            text[row] = cellValue;
        }

        try { dataset.write(); }
        catch (Exception ex)
        {
             JOptionPane.showMessageDialog(
                this,
                ex,
                getTitle(),
                JOptionPane.ERROR_MESSAGE);
            return;
        }
        isTextChanged = false;

    }

    /** Save data as text. */
    private void saveAsText() throws Exception
    {
        final JFileChooser fchooser = new JFileChooser(dataset.getFile());
        fchooser.setFileFilter(DefaultFileFilter.getFileFilterText());
        fchooser.changeToParentDirectory();
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

        int rows = text.length;
        for (int i=0; i<rows; i++)
        {
            out.print(text[i].trim());
            out.println();
            out.println();
        }

        out.flush();
        out.close();

        viewer.showStatus("Data save to: "+fname);

        try {
            RandomAccessFile rf = new RandomAccessFile(choosedFile, "r");
            long size = rf.length();
            rf.close();
            viewer.showStatus("File size (bytes): "+size);
        } catch (Exception ex) {}
    }

    public void dispose()
    {
        if (isTextChanged && !isReadOnly) {
            int op = JOptionPane.showConfirmDialog(this,
                    "\""+ dataset.getName() +"\" has changed.\n"+
                    "Do you want to save the changes?",
                    getTitle(),
                    JOptionPane.YES_NO_OPTION);

            if (op == JOptionPane.YES_OPTION) {
                updateValueInFile();
            }
        }

        viewer.removeDataView(this);

        super.dispose();
    }

    // Implementing DataView.
    public HObject getDataObject() {
        return dataset;
    }

    // Implementing TextView.
    public String[] getText()  {
        return text;
    }

    // print the table
    private void print() {
        StreamPrintServiceFactory[] spsf = StreamPrintServiceFactory.lookupStreamPrintServiceFactories(null, null);
        for (int i = 0; i<spsf.length; i++) {
            System.out.println(spsf[i]);
        }
        DocFlavor[] docFlavors = spsf[0].getSupportedDocFlavors();
        for (int i = 0; i<docFlavors.length; i++) {
            System.out.println(docFlavors[i]);
        }

        // Get a text DocFlavor
        InputStream is = null;
        try { is = new BufferedInputStream( new java.io.FileInputStream("e:\\temp\\t.html"));}
        catch (Exception ex) {}
        DocFlavor flavor = DocFlavor.STRING.TEXT_HTML;

        // Get all available print services
        PrintService[] services = PrintServiceLookup.lookupPrintServices(null, null);

        // Print this job on the first print server
        DocPrintJob job = services[0].createPrintJob();
        Doc doc = new SimpleDoc(is, flavor, null);

        // Print it
        try { job.print(doc, null); }
        catch (Exception ex) {System.out.println(ex);}
    }
    
    private class TextAreaRenderer extends JTextArea implements TableCellRenderer 
    {
        public static final long serialVersionUID = HObject.serialVersionUID;

        private final DefaultTableCellRenderer adaptee = new DefaultTableCellRenderer();
        
        /** map from table to map of rows to map of column heights */
        private final Map cellSizes = new HashMap();

        public TextAreaRenderer() {
          setLineWrap(true);
          setWrapStyleWord(true);
        }

        public Component getTableCellRendererComponent(//
                JTable table, Object obj, boolean isSelected,
                boolean hasFocus, int row, int column) 
        {
            // set the colours, etc. using the standard for that platform
            adaptee.getTableCellRendererComponent(table, obj,
                isSelected, hasFocus, row, column);
            setForeground(adaptee.getForeground());
            setBackground(adaptee.getBackground());
            setBorder(adaptee.getBorder());
            setFont(adaptee.getFont());
            setText(adaptee.getText());

            // This line was very important to get it working with JDK1.4
            TableColumnModel columnModel = table.getColumnModel();
            setSize(columnModel.getColumn(column).getWidth(), 100000);
            int height_wanted = (int) getPreferredSize().getHeight();
            addSize(table, row, column, height_wanted);
            height_wanted = findTotalMaximumRowSize(table, row);
            if (height_wanted != table.getRowHeight(row)) {
              table.setRowHeight(row, height_wanted);
              rowHeaders.setRowHeight(row, height_wanted)   ;     
              
            }
            return this;
        }

        private void addSize(JTable table, int row, int column, int height) 
        {
            Map rows = (Map) cellSizes.get(table);
            if (rows == null) {
                cellSizes.put(table, rows = new HashMap());
            }
            Map rowheights = (Map) rows.get(new Integer(row));
            if (rowheights == null) {
                rows.put(new Integer(row), rowheights = new HashMap());
            }
            rowheights.put(new Integer(column), new Integer(height));
        }

        /**
         * Look through all columns and get the renderer.  If it is
         * also a TextAreaRenderer, we look at the maximum height in
         * its hash table for this row.
         */
        private int findTotalMaximumRowSize(JTable table, int row) 
        {
            int maximum_height = 0;
            Enumeration columns = table.getColumnModel().getColumns();
            while (columns.hasMoreElements()) {
              TableColumn tc = (TableColumn) columns.nextElement();
              TableCellRenderer cellRenderer = tc.getCellRenderer();
              if (cellRenderer instanceof TextAreaRenderer) {
                TextAreaRenderer tar = (TextAreaRenderer) cellRenderer;
                maximum_height = Math.max(maximum_height,
                    tar.findMaximumRowSize(table, row));
              }
            }
            return maximum_height;
        }

        private int findMaximumRowSize(JTable table, int row) 
        {
            Map rows = (Map) cellSizes.get(table);
            if (rows == null) {
                return 0;
            }
            Map rowheights = (Map) rows.get(new Integer(row));
            if (rowheights == null) {
                return 0;
            }
            int maximum_height = 0;
            for (Iterator it = rowheights.entrySet().iterator(); it.hasNext();) 
            {
                Map.Entry entry = (Map.Entry) it.next();
                int cellHeight = ((Integer) entry.getValue()).intValue();
                maximum_height = Math.max(maximum_height, cellHeight);
            }
            return maximum_height;
          }
    }

    private class TextAreaEditor extends DefaultCellEditor 
    {
        public static final long serialVersionUID = HObject.serialVersionUID;
        
        public TextAreaEditor(KeyListener keyListener) 
        {
            super(new JTextField());
            
            final JTextArea textArea = new JTextArea();

            textArea.addKeyListener(keyListener);
            textArea.setWrapStyleWord(true);
            textArea.setLineWrap(true);
            JScrollPane scrollPane = new JScrollPane(textArea);
            scrollPane.setBorder(null);
            editorComponent = scrollPane;
            delegate = new DefaultCellEditor.EditorDelegate() 
            {
                public static final long serialVersionUID = HObject.serialVersionUID;
                public void setValue(Object value) {
                   textArea.setText((value != null) ? value.toString() : "");
                 }
                 public Object getCellEditorValue() {
                   return textArea.getText();
                 }
            };
        }
    }

    /** RowHeader defines the row header component of the Spreadsheet. */
    private class RowHeader extends JTable
    {
        public static final long serialVersionUID = HObject.serialVersionUID;

        private int currentRowIndex = -1;
        private int lastRowIndex = -1;
        private JTable parentTable;

        public RowHeader(JTable pTable, Dataset dset)
        {
            // Create a JTable with the same number of rows as
            // the parent table and one column.
            super( pTable.getRowCount(), 1 );

            long[] startArray = dset.getStartDims();
            long[] strideArray = dset.getStride();
            int[] selectedIndex = dset.getSelectedIndex();
            int start = (int)startArray[selectedIndex[0]];
            int stride = (int)strideArray[selectedIndex[0]];

            // Store the parent table.
            parentTable = pTable;

            // Set the values of the row headers starting at 0.
            int n = parentTable.getRowCount();
            for ( int i = 0; i < n;  i++ )
            {
                setValueAt( new Integer(start+i*stride), i, 0 );
            }

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
        public static final long serialVersionUID = HObject.serialVersionUID;

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
    
}


