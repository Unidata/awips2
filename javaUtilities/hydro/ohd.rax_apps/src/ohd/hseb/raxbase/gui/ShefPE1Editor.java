package ohd.hseb.raxbase.gui;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import ohd.hseb.raxbase.RaxBaseDataMgr;
import ohd.hseb.raxbase.model.ShefPE1;
import ohd.hseb.raxbase.table.ArcBaseShefPE1JTableRowData;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.DialogHelper;
import ohd.hseb.util.gui.LabeledTextField;
import ohd.hseb.util.gui.jtable.ComplexJTableManager;
import ohd.hseb.util.gui.jtable.JTableColumnDescriptor;
import ohd.hseb.util.gui.jtable.JTableManager;

public class ShefPE1Editor extends JDialog
{
    private Container _dialogContentPane = getContentPane();
    
    private RaxBaseDataMgr _dataMgr = null;
    
//  JTable variables
    private JTableManager _shefPe1TableManager = null;
    private List _shefPe1ColumnDescriptorList = null;
    private List _shefPe1RowDataList = null;
    private JScrollPane _tableScrollPane = null;
    private JPanel _shefPe1SelectionPanel = new JPanel( new GridBagLayout() );

    private JPanel _selectedItemPanel = new JPanel( new GridBagLayout() );
    private LabeledTextField _pe1LTF = new LabeledTextField( "PE1:", "", "SHEF PE code", 3 );
    private LabeledTextField _nameLTF = new LabeledTextField( "Name:", "", "Description (mixed case)", 21 );
    
    private JPanel _buttonPanel = new JPanel( new GridBagLayout() );
    private JButton _saveAndCloseButton = new JButton( "Save and Close" );
    private JButton _saveButton = new JButton( "Save" );
    private JButton _closeButton = new JButton( "Close" );
    private JButton _deleteButton = new JButton( "Delete" );

    private Map _shefPe1RowDataToShefPe1Map = null;

    public ShefPE1Editor( JFrame frame, RaxBaseDataMgr dataMgr )
    {
        super( frame, "ShefPE1 Editor", true );
        _dataMgr = dataMgr;
    }
    
    public void displayGUI()
    {
        setPreferredSize( new Dimension ( 600, 400 ) );
        initGUI();
    }
    
    private void initGUI()
    {
        setLayout( new GridBagLayout() );
        initShefPe1SelectionPanel();
        initSelectedItemPanel();
        initButtonPanel();
        initFrameComponents();
        initHashMaps();
        addListeners();
        pack();
        setVisible( true );
    }
    
    private void initShefPe1SelectionPanel()
    {
        initJTable();
    }
    
    private void initFrameComponents()
    {
//                                                                                         X,   Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _dialogContentPane, _shefPe1SelectionPanel,     0,  0,     4,     20, 1, 1, GridBagConstraints.VERTICAL );
        ComponentHelper.addFrameComponent( _dialogContentPane, _selectedItemPanel,         0,  21,    4,     1, 1, 1, GridBagConstraints.BOTH );
        ComponentHelper.addFrameComponent( _dialogContentPane, _buttonPanel,               0,  22,    4,     1, 1, 1, GridBagConstraints.BOTH );

    }
    private void initButtonPanel()
    {
//                                                                              X,  Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _buttonPanel, _saveAndCloseButton,   0,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _buttonPanel, _saveButton,           1,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _buttonPanel, _deleteButton,         2,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _buttonPanel, _closeButton,          1,  1,    1,     1, GridBagConstraints.NONE );
    }

    private void initSelectedItemPanel()
    {
        _selectedItemPanel.setBorder( BorderFactory.createTitledBorder( "Info for Selected ShefPE1" ) );
        JPanel vPanel = new JPanel();
        
//                                                                                X,  Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _selectedItemPanel, _pe1LTF,            0,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _nameLTF,  0,  1,    1,     1, GridBagConstraints.NONE );
    }


    private void initHashMaps()
    {
        _shefPe1RowDataToShefPe1Map = _dataMgr.getShefPe1RowDataToShefPe1Map();
    }

    private void initJTable() 
    {
        _shefPe1RowDataList = _dataMgr.getShefPe1RowDataList();
        setCEColumnDescriptorList();
        _shefPe1TableManager = new ComplexJTableManager( _shefPe1ColumnDescriptorList, _shefPe1RowDataList, ListSelectionModel.SINGLE_SELECTION );
        String columnsSelected[] = _shefPe1TableManager.getColumnNamesThatAreCurrentlyDisplayed();
        _shefPe1TableManager.setDisplayableColumns( columnsSelected, true, true );
        _tableScrollPane = _shefPe1TableManager.getJScrollPane();
        _tableScrollPane.setPreferredSize( new Dimension( 320, 100 ) );
//                                                                                    X,   Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _shefPe1SelectionPanel, _tableScrollPane,    0,   1,    1,     1, 1, 1, GridBagConstraints.VERTICAL );
    }

    private void updateJTable()
    {
        _dataMgr.initShefPeList();
        _shefPe1RowDataList = _dataMgr.getShefPe1RowDataList();
        _shefPe1TableManager.setChangedAllRowDataList( _shefPe1RowDataList );
        _shefPe1TableManager.refreshDisplay();
    }

    private void setCEColumnDescriptorList()
    {
        _shefPe1ColumnDescriptorList = new ArrayList();

        _shefPe1ColumnDescriptorList.add(new JTableColumnDescriptor( "PE1", true, 50, "center" ) );
        _shefPe1ColumnDescriptorList.add(new JTableColumnDescriptor( "Name", true, 250, "center" ) );
    }
    
    private void populateDataInputPanel( ShefPE1 shefPe1 )
    {
        if ( shefPe1 != null )
        {
            _pe1LTF.setTextField( shefPe1.getPe1() );
            _nameLTF.setTextField( shefPe1.getName() );
        }
    }
    
    private void clearDataInputPanel()
    {
        _pe1LTF.setTextField( "" );
        _nameLTF.setTextField( "" );
    }
    
    private boolean saveShefPe1()
    {
        boolean saved = false;
        
        if ( ! _pe1LTF.getTextFieldText().equalsIgnoreCase( "" ) )  
        {
            ShefPE1 shefPe1 = new ShefPE1();

            shefPe1.setPe1( _pe1LTF.getTextFieldText() );
            shefPe1.setName( _nameLTF.getTextFieldText() );
            
            saved = _dataMgr.saveShefPe1( shefPe1 );
            
            updateJTable();
        }
        else
        {
            DialogHelper.displayErrorDialog( this, "Please enter a value for PE1", "Save ShefPE1" );
        }

        return saved;
    }
    
    private void deleteShefPe1()
    {
        ShefPE1 shefPe1 = null;
        List selectedRowsData = _shefPe1TableManager.getSelectedRowsData();
        
        if ( ! selectedRowsData.isEmpty() )
        {
            ArcBaseShefPE1JTableRowData rowData = (ArcBaseShefPE1JTableRowData) selectedRowsData.get( 0 );
            shefPe1 = (ShefPE1) _shefPe1RowDataToShefPe1Map.get( rowData );
            if ( DialogHelper.displayConfirmDialog( this, "Are you sure you want to delete " + shefPe1.keyString(), "Delete ShefPE1" ) )
            {
                _dataMgr.deleteShefPe1FromDataBase( shefPe1 );
                clearDataInputPanel();
                updateJTable();
            }
        }
        else
        {
            DialogHelper.displayErrorDialog( this, "You must select a ShefPE1", "Delete ShefPE1" );
        }
    }
    
    private void addListeners()
    {
        WindowCloserListener windowCloser = new WindowCloserListener();

        _closeButton.addActionListener( windowCloser );
        addWindowListener( windowCloser );
        _shefPe1TableManager.addTableListener( new MCETableListener() );
        
        _saveButton.addActionListener( new SaveButtonListener() );
        _saveAndCloseButton.addActionListener( new SaveAndCloseButtonListener() );
        _deleteButton.addActionListener( new DeleteButtonListener() );
    }
    
    private class SaveButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            saveShefPe1();
        }
    }
    
    private class SaveAndCloseButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            if ( saveShefPe1() )   // If the save is successful, close the window
            {
                closeWindow();
            }
        }
    }
    
    
    private class DeleteButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            deleteShefPe1();
        }
    }
    
    private class MCETableListener implements ListSelectionListener
    {
        public void valueChanged( ListSelectionEvent e )
        {
            if ( e.getValueIsAdjusting() )
            {
                ShefPE1 shefPe1 = null;

                ArcBaseShefPE1JTableRowData rowData = (ArcBaseShefPE1JTableRowData) _shefPe1TableManager.getSelectedRowsData().get( 0 );
                shefPe1 = (ShefPE1) _shefPe1RowDataToShefPe1Map.get( rowData );
                populateDataInputPanel( shefPe1 );
            }
        }
    }

    private class WindowCloserListener extends WindowAdapter implements ActionListener
    {
        public void windowClosing ( WindowEvent e )
        {
            closeWindow();
        }
        
        public void actionPerformed( ActionEvent e )
        {
            closeWindow();
        }
    }

    private void closeWindow()
    /********************
        Purpose: Exits the program gracefully 
     *********************/
    {
        this.dispose();
    }

    public static void main( String args[] )
    {
        JFrame frame = new JFrame();
        frame.setSize( new Dimension( 1024, 768 ) );
        RaxBaseDataMgr dataMgr = new RaxBaseDataMgr( "jdbc:postgresql://ax2:5432/adb_ob72krf?user=pguser" );

        ShefPE1Editor shefPE1Editor = new ShefPE1Editor( frame, dataMgr );
        shefPE1Editor.displayGUI();
    }
}
