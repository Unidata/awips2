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

import java.awt.event.*;
import javax.swing.*;
import javax.swing.table.*;
import javax.swing.border.TitledBorder;
import java.awt.Color;
import java.awt.Point;
import java.awt.Dimension;
import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.awt.Toolkit;
import java.util.*;

import ncsa.hdf.object.*;

/**
 * NewTableDataDialog shows a message dialog requesting user input for creating
 * a new HDF4/5 dataset.
 * 
 * @author Peter X. Cao
 * @version 2.4 9/6/2007
 */
public class NewTableDataDialog extends JDialog
implements ActionListener, ItemListener
{
	public static final long serialVersionUID = HObject.serialVersionUID;

    private static final String[] DATATYPE_NAMES = {
        "byte (8-bit)",                // 0
        "short (16-bit)",              // 1
        "int (32-bit)",                // 2
        "unsigned byte (8-bit)",       // 3
        "unsigned short (16-bit)",     // 4
        "unsigned int (32-bit)",       // 5
        "long (64-bit)",               // 6
        "float",                       // 7
        "double",                      // 8
        "string",                      // 9
        "enum"                         // 10
    };

    private FileFormat fileformat;

    private JComboBox parentChoice, nFieldBox, templateChoice;

    private boolean isH5;

    /** a list of current groups */
    private Vector groupList, compoundDSList;

    private HObject newObject;

    private final Toolkit toolkit;

    private final DataView dataView;

    private int numberOfMembers;

    private JTable table;

    private DefaultTableModel tableModel;

    private RowEditorModel rowEditorModel;

    private DefaultCellEditor cellEditor;

    private JTextField nameField, currentSizeField, maxSizeField, chunkSizeField;
    private JComboBox compressionLevel, rankChoice, memberTypeChoice;
    private JCheckBox checkCompression;
    private JRadioButton checkContinguous, checkChunked;

    /** Constructs NewTableDataDialog with specified list of possible parent groups.
     *  @param owner the owner of the input
     *  @param pGroup the parent group which the new group is added to.
     *  @param objs the list of all objects.
     */
    public NewTableDataDialog(JFrame owner, Group pGroup, List objs)
    {
        super (owner, "New Compound Dataset...", true);

        newObject = null;
        dataView = null;
        numberOfMembers = 2;
        fileformat = pGroup.getFileFormat();

        memberTypeChoice = new JComboBox(DATATYPE_NAMES);
        cellEditor = new DefaultCellEditor(memberTypeChoice);
        rowEditorModel = new RowEditorModel(numberOfMembers, cellEditor);
        String[] colNames = {"Name", "Datatype", "Array size / String length / Enum names"};
        tableModel =  new DefaultTableModel( colNames, numberOfMembers);
        table = new JTable(tableModel)
        {
        	public static final long serialVersionUID = HObject.serialVersionUID;

            RowEditorModel rm = rowEditorModel;
            public TableCellEditor getCellEditor(int row, int col)
            {
                TableCellEditor cellEditor = rm.getEditor(row);

                if ((cellEditor==null) || !(col==1)) {
                    cellEditor =  super.getCellEditor(row,col);
                }

                return cellEditor;
            }
        };
        table.setRowSelectionAllowed(false);
        table.setColumnSelectionAllowed(false);
        
        // set cell height for large fonts
		int cellRowHeight = Math.max(16, table.getFontMetrics(table.getFont()).getHeight());
        table.setRowHeight(cellRowHeight);

        toolkit = Toolkit.getDefaultToolkit();
        isH5 = pGroup.getFileFormat().isThisType(FileFormat.getFileFormat(FileFormat.FILE_TYPE_HDF5));

        parentChoice = new JComboBox();
        String[] memberSizes = new String[100];
        for (int i=0; i<100; i++) {
            memberSizes[i] = String.valueOf(i+1);
        }

        nFieldBox = new JComboBox(memberSizes);
        nFieldBox.setEditable(true);
        nFieldBox.addActionListener(this);
        nFieldBox.setActionCommand("Change number of members");
        nFieldBox.setSelectedItem(String.valueOf(numberOfMembers));

        groupList = new Vector(objs.size());
        Object obj = null;
        Iterator iterator = objs.iterator();

        compoundDSList = new Vector(objs.size());

        while (iterator.hasNext())
        {
            obj = iterator.next();
            if (obj instanceof Group)
            {
                Group g = (Group)obj;
                groupList.add(obj);
                if (g.isRoot()) {
                    parentChoice.addItem(HObject.separator);
                } else {
                    parentChoice.addItem(g.getPath()+g.getName()+HObject.separator);
                }
            }
            else if (obj instanceof CompoundDS)
            {
                compoundDSList.add(obj);
            }
        }

        templateChoice = new JComboBox(compoundDSList);
        templateChoice.setSelectedIndex(-1);
        templateChoice.addItemListener(this);

        if (pGroup.isRoot()) {
            parentChoice.setSelectedItem(HObject.separator);
        } else {
            parentChoice.setSelectedItem(pGroup.getPath()+pGroup.getName()+HObject.separator);
        }

        JPanel contentPane = (JPanel)getContentPane();
        contentPane.setLayout(new BorderLayout(5,5));
        contentPane.setBorder(BorderFactory.createEmptyBorder(15,5,5,5));
        int w = 700 + (ViewProperties.getFontSize()-12)*15;
        int h = 500 + (ViewProperties.getFontSize()-12)*10;
        contentPane.setPreferredSize(new Dimension(w, h));

        JButton okButton = new JButton("   Ok   ");
        okButton.setActionCommand("Ok");
        okButton.setMnemonic(KeyEvent.VK_O);
        okButton.addActionListener(this);

        JButton cancelButton = new JButton("Cancel");
        cancelButton.setMnemonic(KeyEvent.VK_C);
        cancelButton.setActionCommand("Cancel");
        cancelButton.addActionListener(this);

        // set NAME and PARENT GROUP panel
        JPanel namePanel = new JPanel();
        namePanel.setLayout(new BorderLayout(5,5));
        JPanel tmpP = new JPanel();
        tmpP.setLayout(new GridLayout(3,1));
        tmpP.add(new JLabel("   Dataset name: "));
        tmpP.add(new JLabel("   Parent group: "));
        tmpP.add(new JLabel("Import template: "));
        namePanel.add(tmpP, BorderLayout.WEST);
        tmpP = new JPanel();
        tmpP.setLayout(new GridLayout(3,1));
        tmpP.add(nameField=new JTextField());
        tmpP.add(parentChoice);
        tmpP.add(templateChoice);
        namePanel.add(tmpP, BorderLayout.CENTER);

        // set DATATSPACE
        JPanel spacePanel = new JPanel();
        spacePanel.setLayout(new GridLayout(2,3,15,3));
        TitledBorder border = new TitledBorder("Dataspace");
        border.setTitleColor(Color.blue);
        spacePanel.setBorder(border);


        rankChoice = new JComboBox();
        for (int i=1; i<33; i++) {
            rankChoice.addItem(String.valueOf(i));
        }
        rankChoice.setSelectedIndex(0);

        currentSizeField = new JTextField("1");
        maxSizeField = new JTextField("0");
        spacePanel.add(new JLabel("No. of dimensions"));
        spacePanel.add(new JLabel("Current size"));
        spacePanel.add(new JLabel("Max size (-1 for unlimited)"));
        spacePanel.add(rankChoice);
        spacePanel.add(currentSizeField);
        spacePanel.add(maxSizeField);

        // set storage layout and data compression
        JPanel layoutPanel = new JPanel();
        layoutPanel.setLayout(new BorderLayout());
        border = new TitledBorder("Data Layout and Compression");
        border.setTitleColor(Color.BLUE);
        layoutPanel.setBorder(border);

        checkContinguous = new JRadioButton("Contiguous");
        checkContinguous.setSelected(true);
        checkChunked = new JRadioButton("Chunked");
        ButtonGroup bgroup = new ButtonGroup();
        bgroup.add(checkChunked);
        bgroup.add(checkContinguous);
        chunkSizeField = new JTextField("1");
        chunkSizeField.setEnabled(false);
        checkCompression = new JCheckBox("gzip");

        compressionLevel = new JComboBox();
        for (int i=0; i<10; i++) {
            compressionLevel.addItem(String.valueOf(i));
        }
        compressionLevel.setSelectedIndex(6);
        compressionLevel.setEnabled(false);

        tmpP = new JPanel();
        tmpP.setLayout(new GridLayout(2, 1));
        tmpP.add(new JLabel("Storage layout:  "));
        tmpP.add(new JLabel("Compression:  "));
        layoutPanel.add(tmpP, BorderLayout.WEST);

        tmpP = new JPanel();
        tmpP.setLayout(new GridLayout(2, 1));

        JPanel tmpP0 = new JPanel();
        tmpP0.setLayout(new GridLayout(1, 2));
        tmpP0.add(checkContinguous);

        JPanel tmpP00 = new JPanel();
        tmpP00.setLayout(new GridLayout(1, 3));
        tmpP00.add(checkChunked);
        tmpP00.add(new JLabel("          Size: "));
        tmpP00.add(chunkSizeField);
        tmpP0.add(tmpP00);

        tmpP.add(tmpP0);

        tmpP0 = new JPanel();
        tmpP0.setLayout(new GridLayout(1, 7));
        tmpP0.add(checkCompression);
        tmpP0.add(new JLabel("      Level: "));
        tmpP0.add(compressionLevel);
        tmpP0.add(new JLabel(""));
        tmpP0.add(new JLabel(""));
        tmpP0.add(new JLabel(""));
        tmpP0.add(new JLabel(""));
        tmpP.add(tmpP0);

        layoutPanel.add(tmpP, BorderLayout.CENTER);

        // add name, space and layout panels
        tmpP = new JPanel();
        tmpP.setLayout(new BorderLayout(5,5));
        tmpP.add(namePanel, BorderLayout.NORTH);
        tmpP.add(spacePanel, BorderLayout.CENTER);
        tmpP.add(layoutPanel, BorderLayout.SOUTH);

        contentPane.add(tmpP, BorderLayout.NORTH);

        // add field table
        tmpP = new JPanel();
        tmpP.setLayout(new BorderLayout(5,5));
        tmpP0 = new JPanel();
        tmpP0.setLayout(new BorderLayout(5,5));
        tmpP0.add(new JLabel(" Number of Members:"), BorderLayout.WEST);
        tmpP0.add(nFieldBox, BorderLayout.CENTER);
        tmpP.add(tmpP0, BorderLayout.NORTH);
        JScrollPane scroller = new JScrollPane(table);
        border = new TitledBorder("Compound Datatype Properties");
        border.setTitleColor(Color.BLUE);
        tmpP.setBorder(border);
        tmpP.add(scroller, BorderLayout.CENTER);
        contentPane.add(tmpP, BorderLayout.CENTER);

        // set OK and CANCEL buttons
        JPanel buttonPanel = new JPanel();
        buttonPanel.add(okButton);
        buttonPanel.add(cancelButton);
        contentPane.add(buttonPanel, BorderLayout.SOUTH);

        rankChoice.addItemListener(this);
        checkCompression.addItemListener(this);
        checkContinguous.addItemListener(this);
        checkChunked.addItemListener(this);
        memberTypeChoice.addItemListener(this);

        // locate the H5Property dialog
        Point l = owner.getLocation();
        l.x += 250;
        l.y += 120;
        setLocation(l);
        validate();
        pack();
    }

    public void actionPerformed(ActionEvent e)
    {
        Object source = e.getSource();
        String cmd = e.getActionCommand();

        if (cmd.equals("Ok"))
        {
            try { newObject = createCompoundDS(); }
            catch (Exception ex)
            {
                JOptionPane.showMessageDialog(this,
                    ex,
                    getTitle(),
                    JOptionPane.ERROR_MESSAGE);
            }

            if (newObject != null) {
                dispose();
            }
        }
        else if (cmd.equals("Cancel"))
        {
            newObject = null;
            dispose();
            (groupList).setSize(0);
        }
        else if (cmd.equals("Change number of members"))
        {
            int n = 0;

            try { n = Integer.valueOf((String)nFieldBox.getSelectedItem()).intValue(); }
            catch (Exception ex) {}

            if (n == numberOfMembers) {
                return;
            }

            tableModel.setRowCount(n);
            for (int i=numberOfMembers; i<n; i++)
            {
                rowEditorModel.addEditorForRow(i, cellEditor);
            }
            numberOfMembers = n;
        }
    }

    public void itemStateChanged(ItemEvent e)
    {
        Object source = e.getSource();

        if (source.equals(rankChoice))
        {
            int rank = rankChoice.getSelectedIndex()+1;
            String currentSizeStr = "1";
            String maxSizeStr = "0";

            for (int i=1; i<rank; i++)
            {
                currentSizeStr += " x 1";
                maxSizeStr += " x 0";
            }

            currentSizeField.setText(currentSizeStr);
            maxSizeField.setText(maxSizeStr);

            String currentStr = currentSizeField.getText();
            int idx = currentStr.lastIndexOf("x");
            String chunkStr = "1";

            if (rank <=1) {
                chunkStr = currentStr;
            } else
            {
                for (int i=1; i<rank-1; i++) {
                    chunkStr += " x 1";
                }
                if (idx >0) {
                    chunkStr += " x "+currentStr.substring(idx+1);
                }
            }

            chunkSizeField.setText(chunkStr);
        }
        else if (source.equals(checkContinguous)) {
            chunkSizeField.setEnabled(false);
        } else if (source.equals(checkChunked))
        {
            chunkSizeField.setEnabled(true);
            String currentStr = currentSizeField.getText();
            int idx = currentStr.lastIndexOf("x");
            String chunkStr = "1";

            int rank = rankChoice.getSelectedIndex()+1;
            if (rank <=1) {
                chunkStr = currentStr;
            } else
            {
                for (int i=1; i<rank-1; i++) {
                    chunkStr += " x 1";
                }
                if (idx >0) {
                    chunkStr += " x "+currentStr.substring(idx+1);
                }
            }

            chunkSizeField.setText(chunkStr);
        }
        else if (source.equals(checkCompression))
        {
            boolean isCompressed = checkCompression.isSelected();

            if (isCompressed)
            {
                if (!checkChunked.isSelected())
                {
                    String currentStr = currentSizeField.getText();
                    int idx = currentStr.lastIndexOf("x");
                    String chunkStr = "1";

                    int rank = rankChoice.getSelectedIndex()+1;
                    if (rank <=1) {
                        chunkStr = currentStr;
                    } else
                    {
                        for (int i=1; i<rank-1; i++) {
                            chunkStr += " x 1";
                        }
                        if (idx >0) {
                            chunkStr += " x "+currentStr.substring(idx+1);
                        }
                    }

                    chunkSizeField.setText(chunkStr);
                }
                compressionLevel.setEnabled(true);
                checkContinguous.setEnabled(false);
                checkChunked.setSelected(true);
                chunkSizeField.setEnabled(true);
            }
            else
            {
                compressionLevel.setEnabled(false);
                checkContinguous.setEnabled(true);
            }
        }
        else if (source.equals(memberTypeChoice))
        {
            String item = (String)memberTypeChoice.getSelectedItem();
            if ((item == null) || !item.equals("enum")) {
                return;
            }

            int row = table.getSelectedRow();
            table.setValueAt("mb1=0,mb=1,...", row, 2);
        }
        else if (source.equals(templateChoice))
        {
            Object obj = templateChoice.getSelectedItem();
            if ( !(obj instanceof CompoundDS)) {
                return;
            }

            CompoundDS dset = (CompoundDS)obj;
            int rank = dset.getRank();
            if (rank < 1) {
                dset.init();
            }

            rank = dset.getRank();
            rankChoice.setSelectedIndex(rank-1);
            long[] dims = dset.getDims();
            String[] mNames = dset.getMemberNames();
            int[] mOrders = dset.getMemberOrders();
            Datatype[] mTypes = dset.getMemberTypes();

            String sizeStr = String.valueOf(dims[0]);
            for (int i=1; i<rank; i++) {
                sizeStr += "x"+dims[i];
            }
            currentSizeField.setText(sizeStr);

            try { dset.getMetadata(); } // get chunking and compression info
            catch (Exception ex) {}
            long[] chunks = dset.getChunkSize();
            if (chunks != null) {
                checkChunked.setSelected(true);
                sizeStr = String.valueOf(chunks[0]);
                for (int i=1; i<rank; i++) {
                    sizeStr += "x"+chunks[i];
                }
                chunkSizeField.setText(sizeStr);
            }

            String compression = dset.getCompression();
            if (compression != null) {
                int idx = compression.indexOf("GZIP: level = ");
                int clevel = -1;
                try { clevel = Integer.parseInt(compression.substring(idx+14, idx+15)); }
                catch (NumberFormatException ex) { clevel = -1; }

                if (clevel>0) {
                    checkCompression.setSelected(true);
                    compressionLevel.setSelectedIndex(clevel);
                }
            }

            numberOfMembers = dset.getMemberCount();
            nFieldBox.setSelectedIndex(numberOfMembers-1);
            tableModel.setRowCount(numberOfMembers);
            for (int i=0; i<numberOfMembers; i++)
            {
                rowEditorModel.addEditorForRow(i, cellEditor);

                tableModel.setValueAt(mNames[i], i, 0);

                int typeIdx = -1;
                int tclass = mTypes[i].getDatatypeClass();
                int tsize = mTypes[i].getDatatypeSize();
                if (tclass == Datatype.CLASS_INTEGER)
                {
                    int tsigned = mTypes[i].getDatatypeSign();
                    if (tsigned == Datatype.SIGN_NONE)
                    {
                        if (tsize == 1) {
                            typeIdx = 3;
                        } else if (tsize == 2) {
                            typeIdx = 4;
                        } else if (tsize == 4) {
                            typeIdx = 5;
                        }
                    }
                    else
                    {
                        if (tsize == 1) {
                            typeIdx = 0;
                        } else if (tsize == 2) {
                            typeIdx = 1;
                        } else if (tsize == 4) {
                            typeIdx = 2;
                        } else {
                            typeIdx = 6;
                        }
                    }
                }
                else if (tclass == Datatype.CLASS_FLOAT)
                {
                    if (tsize == 4) {
                        typeIdx = 7;
                    } else {
                        typeIdx = 8;
                    }
                }
                else if (tclass == Datatype.CLASS_STRING)
                {
                    typeIdx = 9;
                }
                else if (tclass == Datatype.CLASS_ENUM)
                {
                    typeIdx = 10;
                }

                if (typeIdx < 0) {
                    continue;
                }

                memberTypeChoice.setSelectedIndex(typeIdx);
                tableModel.setValueAt(memberTypeChoice.getSelectedItem(), i, 1);

                if (tclass == Datatype.CLASS_STRING) {
                    tableModel.setValueAt(String.valueOf(tsize), i, 2);
                } else if (tclass == Datatype.CLASS_ENUM) {
                    tableModel.setValueAt(mTypes[i].getEnumMembers(), i, 2);
                } else {
                    tableModel.setValueAt(String.valueOf(mOrders[i]), i, 2);
                }

            } // for (int i=0; i<numberOfMembers; i++)
        } // else if (source.equals(templateChoice))
    }

    private HObject createCompoundDS() throws Exception
    {
        HObject obj = null;
        long dims[], maxdims[], chunks[];
        int rank;

        // stop editing the last selected cell
        int row = table.getSelectedRow();
        int col = table.getSelectedColumn();
        if ((row>=0) && (col>-0))
        {
            TableCellEditor ed = table.getCellEditor(row, col);
            if (ed != null) {
                ed.stopCellEditing();
            }
        }

        maxdims= chunks = null;
        String dname = nameField.getText();
        if ((dname == null) || (dname.length()<=0)) {
            throw new IllegalArgumentException("Dataset name is empty");
        }

        Group pgroup = (Group)groupList.get(parentChoice.getSelectedIndex());
        if (pgroup == null) {
            throw new IllegalArgumentException("Invalid parent group");
        }

        int n = table.getRowCount();
        if (n<=0) {
            return null;
        }

        String[] mNames = new String[n];
        Datatype[] mDatatypes = new Datatype[n];
        int[] mOrders = new int[n];

        for (int i=0; i<n; i++)
        {
            String name = (String)table.getValueAt(i, 0);
            if ((name == null) || (name.length() <=0)) {
                throw new IllegalArgumentException("Member name is empty");
            }
            mNames[i] = name;

            int order = 1;
            String orderStr = (String)table.getValueAt(i, 2);
            if (orderStr != null) {
                try { order = Integer.parseInt(orderStr); }
                catch (Exception ex) {}
            }
            mOrders[i] = order;

            String typeName = (String)table.getValueAt(i, 1);
            Datatype type = null;
            if (DATATYPE_NAMES[0].equals(typeName))
            {
                type = fileformat.createDatatype(Datatype.CLASS_INTEGER, 1, Datatype.NATIVE, Datatype.NATIVE);
            }
            else if (DATATYPE_NAMES[1].equals(typeName))
            {
                type = fileformat.createDatatype(Datatype.CLASS_INTEGER, 2, Datatype.NATIVE, Datatype.NATIVE);
            }
            else if (DATATYPE_NAMES[2].equals(typeName))
            {
                type = fileformat.createDatatype(Datatype.CLASS_INTEGER, 4, Datatype.NATIVE, Datatype.NATIVE);
            }
            else if (DATATYPE_NAMES[3].equals(typeName))
            {
                type = fileformat.createDatatype(Datatype.CLASS_INTEGER, 1, Datatype.NATIVE, Datatype.SIGN_NONE);
            }
            else if (DATATYPE_NAMES[4].equals(typeName))
            {
                type = fileformat.createDatatype(Datatype.CLASS_INTEGER, 2, Datatype.NATIVE, Datatype.SIGN_NONE);
            }
            else if (DATATYPE_NAMES[5].equals(typeName))
            {
                type = fileformat.createDatatype(Datatype.CLASS_INTEGER, 4, Datatype.NATIVE, Datatype.SIGN_NONE);
            }
            else if (DATATYPE_NAMES[6].equals(typeName))
            {
                type = fileformat.createDatatype(Datatype.CLASS_INTEGER, 8, Datatype.NATIVE, Datatype.NATIVE);
            }
            else if (DATATYPE_NAMES[7].equals(typeName))
            {
                type = fileformat.createDatatype(Datatype.CLASS_FLOAT, 4, Datatype.NATIVE, Datatype.NATIVE);
            }
            else if (DATATYPE_NAMES[8].equals(typeName))
            {
                type = fileformat.createDatatype(Datatype.CLASS_FLOAT, 8, Datatype.NATIVE, Datatype.NATIVE);
            }
            else if (DATATYPE_NAMES[9].equals(typeName))
            {
                type = fileformat.createDatatype(Datatype.CLASS_STRING, order, Datatype.NATIVE, Datatype.NATIVE);
            }
            else if (DATATYPE_NAMES[10].equals(typeName)) // enum
            {
                type = fileformat.createDatatype(Datatype.CLASS_ENUM, 4, Datatype.NATIVE, Datatype.NATIVE);
                if ((orderStr==null) || (orderStr.length()<1) || orderStr.endsWith("..."))
                {
                    toolkit.beep();
                    JOptionPane.showMessageDialog(this,
                            "Invalid member values: "+orderStr,
                            getTitle(),
                            JOptionPane.ERROR_MESSAGE);
                    return null;
                } else {
                    type.setEnumMembers(orderStr);
                }
            }
            else
            {
                throw new IllegalArgumentException("Invalid data type.");
            }
            mDatatypes[i] = type;
        } // for (int i=0; i<n; i++)

        rank = rankChoice.getSelectedIndex()+1;
        StringTokenizer st = new StringTokenizer(currentSizeField.getText(), "x");
        if (st.countTokens() < rank)
        {
            toolkit.beep();
            JOptionPane.showMessageDialog(this,
                "Number of values in the current dimension size is less than "+rank,
                getTitle(),
                JOptionPane.ERROR_MESSAGE);
            return null;
        }

        long l = 0;
        dims = new long[rank];
        String token = null;
        for (int i=0; i<rank; i++)
        {
            token = st.nextToken().trim();
            try { l = Long.parseLong(token); }
            catch (NumberFormatException ex)
            {
                toolkit.beep();
                JOptionPane.showMessageDialog(this,
                    "Invalid dimension size: "+currentSizeField.getText(),
                    getTitle(),
                    JOptionPane.ERROR_MESSAGE);
                return null;
            }

            if (l <=0)
            {
                toolkit.beep();
                JOptionPane.showMessageDialog(this,
                    "Dimension size must be greater than zero.",
                    getTitle(),
                    JOptionPane.ERROR_MESSAGE);
                return null;
            }

            dims[i] = l;
        }

        st = new StringTokenizer(maxSizeField.getText(), "x");
        if (st.countTokens() < rank)
        {
            toolkit.beep();
            JOptionPane.showMessageDialog(this,
                "Number of values in the max dimension size is less than "+rank,
                getTitle(),
                JOptionPane.ERROR_MESSAGE);
            return null;
        }

        l = 0;
        maxdims = new long[rank];
        for (int i=0; i<rank; i++)
        {
            token = st.nextToken().trim();
            try { l = Long.parseLong(token); }
            catch (NumberFormatException ex)
            {
                toolkit.beep();
                JOptionPane.showMessageDialog(this,
                    "Invalid max dimension size: "+maxSizeField.getText(),
                    getTitle(),
                    JOptionPane.ERROR_MESSAGE);
                return null;
            }

            if (l < -1)
            {
                toolkit.beep();
                JOptionPane.showMessageDialog(this,
                    "Dimension size cannot be less than -1.",
                    getTitle(),
                    JOptionPane.ERROR_MESSAGE);
                return null;
            }
            else if ( l == 0) {
                l = dims[i];
            }

            maxdims[i] = l;
        }

        chunks = null;
        if (checkChunked.isSelected())
        {
            st = new StringTokenizer(chunkSizeField.getText(), "x");
            if (st.countTokens() < rank)
            {
                toolkit.beep();
                JOptionPane.showMessageDialog(this,
                    "Number of values in the chunk size is less than "+rank,
                    getTitle(),
                    JOptionPane.ERROR_MESSAGE);
                return null;
            }

            l = 0;
            chunks = new long[rank];
            token = null;
            for (int i=0; i<rank; i++)
            {
                token = st.nextToken().trim();
                try { l = Long.parseLong(token); }
                catch (NumberFormatException ex)
                {
                    toolkit.beep();
                    JOptionPane.showMessageDialog(this,
                        "Invalid chunk dimension size: "+chunkSizeField.getText(),
                        getTitle(),
                        JOptionPane.ERROR_MESSAGE);
                    return null;
                }

                if (l < 1)
                {
                    toolkit.beep();
                    JOptionPane.showMessageDialog(this,
                        "Chunk size cannot be less than 1.",
                        getTitle(),
                        JOptionPane.ERROR_MESSAGE);
                    return null;
                }

                chunks[i] = l;
            } // for (int i=0; i<rank; i++)

            long tchunksize=1, tdimsize=1;
            for (int i=0; i<rank; i++)
            {
                tchunksize *= chunks[i];
                tdimsize *= dims[i];
            }

            if (tchunksize >= tdimsize)
            {
                toolkit.beep();
                int status = JOptionPane.showConfirmDialog(
                    this,
                    "Chunk size is equal/greater than the current size. "+
                    "\nAre you sure you want to set chunk size to "+
                    chunkSizeField.getText()+"?",
                    getTitle(),
                    JOptionPane.YES_NO_OPTION);
                if (status == JOptionPane.NO_OPTION) {
                    return null;
                }
            }

            if (tchunksize == 1)
            {
                toolkit.beep();
                int status = JOptionPane.showConfirmDialog(
                    this,
                     "Chunk size is one, which may cause large memory overhead for large dataset."+
                    "\nAre you sure you want to set chunk size to "+
                    chunkSizeField.getText()+"?",
                    getTitle(),
                    JOptionPane.YES_NO_OPTION);
                if (status == JOptionPane.NO_OPTION) {
                    return null;
                }
            }

        } // if (checkChunked.isSelected())

        int gzip = 0;
        if (checkCompression.isSelected()) {
            gzip = compressionLevel.getSelectedIndex();
        }

        if (checkChunked.isSelected()) {
            obj = fileformat.createCompoundDS(dname, pgroup, dims, maxdims, chunks,
                gzip, mNames, mDatatypes, mOrders, null);
        } else {
            obj = fileformat.createCompoundDS(dname, pgroup, dims, maxdims, null,
                -1, mNames, mDatatypes, mOrders, null);
        }

        return obj;
    }

    /** Returns the new dataset created. */
    public DataFormat getObject() { return newObject; }

    /** Returns the parent group of the new dataset. */
    public Group getParentGroup() {
        return (Group)groupList.get(parentChoice.getSelectedIndex());
    }

    private class RowEditorModel
    {
        private Hashtable data;

        public RowEditorModel()
        {
            data = new Hashtable();
        }

        // all rows has the same cell editor
        public RowEditorModel(int rows, TableCellEditor ed)
        {
            data = new Hashtable();
            for (int i=0; i<rows; i++) {
                data.put(new Integer(i), ed);
            }
        }

        public void addEditorForRow(int row, TableCellEditor e )
        {
            data.put(new Integer(row), e);
        }

        public void removeEditorForRow(int row)
        {
            data.remove(new Integer(row));
        }

        public TableCellEditor getEditor(int row)
        {
            return (TableCellEditor)data.get(new Integer(row));
        }
    }
}
